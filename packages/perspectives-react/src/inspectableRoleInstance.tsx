import React from "react";
import {
	Accordion,
	Button,
	Card,
	Form,
	ListGroup,
	OverlayTrigger,
	Tooltip,
} from "react-bootstrap";
import { InspectableRole, RoleInstanceT, ContextInstanceT } from "perspectives-proxy";

export type InspectableRoleProps = {
	data: InspectableRole;
	showRole: (roleInstanceId: RoleInstanceT) => void;
	showContext: (contextInstanceId: ContextInstanceT) => void;
};

// Stateless component that renders a read-only view for a single InspectableRole.
export function InspectableRoleInstanceView({ data, showRole, showContext }: InspectableRoleProps) {
	const { title, rtype } = data;

	return (
		<Card>
			<Card.Body>
				<Card.Title as="h3">
					<OverlayTrigger placement="right" overlay={<Tooltip>{rtype}</Tooltip>}>
						<span>{title}</span>
					</OverlayTrigger>
				</Card.Title>

				{/* Index and IsMe */}
				<Form>
					<Form.Group className="mb-3" controlId="role-index">
						<Form.Label>Position in list of instances</Form.Label>
						<Form.Control readOnly value={String(data.index)} />
					</Form.Group>
					<Form.Group className="mb-3" controlId="role-isMe">
						<Form.Label>Is me</Form.Label>
						<Form.Check type="switch" disabled checked={!!data.isMe} />
					</Form.Group>

					{/* Context button */}
					<Form.Group className="mb-3" controlId="role-context">
						<Form.Label>Context</Form.Label>
						<div>
							<Button variant="outline-secondary" onClick={() => showContext((data.context as unknown as { _id: ContextInstanceT })._id)}>
								{data.context.title}
							</Button>
						</div>
					</Form.Group>

					{/* Properties */}
					{Object.entries(data.properties).map(([readablePropFqn, prop]) => (
						<Form.Group className="mb-3" key={readablePropFqn} controlId={`prop-${readablePropFqn}`}>
							<Form.Label>
								<OverlayTrigger placement="right" overlay={<Tooltip>{readablePropFqn}</Tooltip>}>
									<span>{prop.translatedProperty}</span>
								</OverlayTrigger>
							</Form.Label>
							<Form.Control readOnly value={String(prop.value ?? "")} />
						</Form.Group>
					))}
				</Form>

				{/* Filler */}
				<Form className="mb-3">
					<Form.Group controlId="role-filler">
						<Form.Label>Filler</Form.Label>
						<div>
							<Button variant="outline-primary" onClick={() => showRole(data.filler._id)}>
								{data.filler.title}
							</Button>
						</div>
					</Form.Group>
				</Form>

				{/* Filled roles accordion per context */}
				<Accordion alwaysOpen className="mb-3">
					{Object.entries(data.filledRoles).map(([contextFQN, info], idx) => (
						<Accordion.Item eventKey={String(idx)} key={contextFQN}>
							<Accordion.Header>{info.contextTitle}</Accordion.Header>
							<Accordion.Body>
								<ListGroup>
									{Object.entries(info.roleInstances).map(([readableRoleFqn, group]) => (
										<ListGroup.Item key={`${contextFQN}-${readableRoleFqn}`}>
											<div className="d-flex justify-content-between align-items-center mb-2">
												<OverlayTrigger placement="right" overlay={<Tooltip>{readableRoleFqn}</Tooltip>}>
													<span className="fw-semibold">{group.translatedRole}</span>
												</OverlayTrigger>
											</div>
											<div className="d-flex flex-wrap gap-2">
												{group.instances.map((ri) => (
													<Button key={`${ri._id}-${ri.title}`} size="sm" variant="outline-primary" onClick={() => showRole(ri._id)}>
														{ri.title}
													</Button>
												))}
											</div>
										</ListGroup.Item>
									))}
								</ListGroup>
							</Accordion.Body>
						</Accordion.Item>
					))}
				</Accordion>

				{/* Types list */}
				<Card className="mb-3">
					<Card.Header>Types</Card.Header>
					<ListGroup variant="flush">
						{Object.entries(data.types).map(([readableRoleFqn, translatedName]) => (
							<OverlayTrigger key={readableRoleFqn} placement="right" overlay={<Tooltip>{readableRoleFqn}</Tooltip>}>
								<ListGroup.Item>{translatedName}</ListGroup.Item>
							</OverlayTrigger>
						))}
					</ListGroup>
				</Card>

				{/* States list */}
				<Card>
					<Card.Header>States</Card.Header>
					<ListGroup variant="flush">
						{Object.entries(data.states).map(([readableStateFqn, translatedState]) => (
							<OverlayTrigger key={readableStateFqn} placement="right" overlay={<Tooltip>{readableStateFqn}</Tooltip>}>
								<ListGroup.Item>{translatedState}</ListGroup.Item>
							</OverlayTrigger>
						))}
					</ListGroup>
				</Card>
			</Card.Body>
		</Card>
	);
}

export default InspectableRoleInstanceView;


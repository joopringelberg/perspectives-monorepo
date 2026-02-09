import React from "react";
import {
	Accordion,
	Button,
	Card,
	Form,
	ListGroup,
	OverlayTrigger,
	Tooltip,
	Row,
	Col,
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

				{/* Context and Properties */}
				<Form>
					{/* Properties */}
					<Card className="mb-3">
						<Card.Header className="py-2">Properties</Card.Header>
						<Card.Body className="py-2">
							{Object.entries(data.properties).map(([readablePropFqn, prop]) => (
								<Form.Group as={Row} className="mb-2 align-items-center" key={readablePropFqn} controlId={`prop-${readablePropFqn}`}>
									<Form.Label column sm={4}>
										<OverlayTrigger placement="right" overlay={<Tooltip>{readablePropFqn}</Tooltip>}>
											<span>{prop.translatedProperty}</span>
										</OverlayTrigger>
									</Form.Label>
									<Col sm={8}>
										<Form.Control readOnly value={String(prop.value ?? "")} />
									</Col>
								</Form.Group>
							))}
						</Card.Body>
					</Card>
				</Form>

				{/* Filler */}
				<Form className="mb-3">
					<Form.Group as={Row} className="align-items-center" controlId="role-filler">
						<Form.Label column sm={4}>Filler</Form.Label>
						<Col sm={8}>
							{data.filler ? (
								<Button variant="outline-primary" onClick={() => showRole(data.filler!._id)}>
									{data.filler.title}
								</Button>
							) : (
								<span>No filler</span>
							)}
						</Col>
					</Form.Group>
				</Form>

				{/* Filled roles accordion per context */}
				<Card className="mb-3">
					<Card.Header className="py-2">Filled Roles</Card.Header>
					<Card.Body className="py-2">
						<Accordion alwaysOpen>
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
					</Card.Body>
				</Card>

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
				<Card className="mb-3">
					<Card.Header>States</Card.Header>
					<ListGroup variant="flush">
						{Object.entries(data.states).map(([readableStateFqn, translatedState]) => (
							<OverlayTrigger key={readableStateFqn} placement="right" overlay={<Tooltip>{readableStateFqn}</Tooltip>}>
								<ListGroup.Item>{translatedState}</ListGroup.Item>
							</OverlayTrigger>
						))}
					</ListGroup>
				</Card>

				{/* Index and IsMe at bottom */}
				<Form>
					<Form.Group as={Row} className="mb-2 align-items-center" controlId="role-index">
						<Form.Label column sm={4}>Position in list of instances</Form.Label>
						<Col sm={8}>
							<Form.Control readOnly value={String(data.index)} />
						</Col>
					</Form.Group>
					<Form.Group as={Row} className="mb-2 align-items-center" controlId="role-isMe">
						<Form.Label column sm={4}>Is me</Form.Label>
						<Col sm={8}>
							<Form.Check type="switch" disabled checked={!!data.isMe} />
						</Col>
					</Form.Group>
					{/* Context button at bottom */}
					<Form.Group as={Row} className="align-items-center" controlId="role-context-bottom">
						<Form.Label column sm={4}>Context</Form.Label>
						<Col sm={8}>
							<Button variant="primary" onClick={() => showContext((data.context as unknown as { _id: ContextInstanceT })._id)}>
								{data.context.title}
							</Button>
						</Col>
					</Form.Group>
				</Form>
			</Card.Body>
		</Card>
	);
}

export default InspectableRoleInstanceView;


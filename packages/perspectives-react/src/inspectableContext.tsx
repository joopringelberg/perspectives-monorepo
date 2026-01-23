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
import { InspectableContext, RoleInstanceT } from "perspectives-proxy";

export type InspectableContextProps = {
	data: InspectableContext;
	showRole: (roleInstanceId: RoleInstanceT) => void;
};

// Stateless component that renders a read-only view for a single InspectableContext.
export function InspectableContextView({ data, showRole }: InspectableContextProps) {
	const { title, ctype } = data;

	return (
		<Card>
			<Card.Body>
				<Card.Title as="h3">
					<OverlayTrigger placement="right" overlay={<Tooltip>{ctype}</Tooltip>}>
						<span>{title}</span>
					</OverlayTrigger>
				</Card.Title>

				{/* Properties */}
				<Form>
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

					{/* Me + My type */}
					{data.me && (
						<>
							<Form.Group className="mb-3" controlId="me">
								<Form.Label>Me</Form.Label>
								<Form.Control readOnly value={data.me.title} />
							</Form.Group>
							<Form.Group className="mb-3" controlId="my-type">
								<Form.Label>My type</Form.Label>
								<Form.Control readOnly value={data.me.roleType} />
							</Form.Group>
						</>
					)}
				</Form>

				{/* Roles accordion */}
				<Accordion alwaysOpen className="mb-3">
					{Object.entries(data.roles).map(([readableRoleFqn, roleGroup], idx) => (
						<Accordion.Item eventKey={String(idx)} key={readableRoleFqn}>
							<Accordion.Header>
								<OverlayTrigger placement="right" overlay={<Tooltip>{readableRoleFqn}</Tooltip>}>
									<span>{roleGroup.translatedRole}</span>
								</OverlayTrigger>
							</Accordion.Header>
							<Accordion.Body>
								<ListGroup>
									{roleGroup.instances.map((ri) => (
										<ListGroup.Item key={`${ri._id}-${ri.title}`} className="d-flex justify-content-between align-items-center">
											<span>{ri.title}</span>
											<Button size="sm" variant="outline-primary" onClick={() => showRole(ri._id)}>
												Show
											</Button>
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
						{Object.entries(data.types).map(([readableContextFqn, translatedName]) => (
							<OverlayTrigger key={readableContextFqn} placement="right" overlay={<Tooltip>{readableContextFqn}</Tooltip>}>
								<ListGroup.Item>{translatedName}</ListGroup.Item>
							</OverlayTrigger>
						))}
					</ListGroup>
				</Card>

				{/* External role button */}
				<div className="mb-3">
					<Button variant="primary" onClick={() => showRole(data.externalRole)}>
						ExternalRole
					</Button>
				</div>

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

export default InspectableContextView;


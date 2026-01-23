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

				{/* Properties */}
				<Form>
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

					{/* Me + My type */}
					{data.me && (
						<>
							<Form.Group as={Row} className="mb-2 align-items-center" controlId="me">
								<Form.Label column sm={4}>Me</Form.Label>
								<Col sm={8}>
									<Form.Control readOnly value={data.me.title} />
								</Col>
							</Form.Group>
							<Form.Group as={Row} className="mb-2 align-items-center" controlId="my-type">
								<Form.Label column sm={4}>My type</Form.Label>
								<Col sm={8}>
									<Form.Control readOnly value={data.me.roleType} />
								</Col>
							</Form.Group>
						</>
					)}
				</Form>

				{/* Roles accordion */}
				<Card className="mb-3">
					<Card.Header className="py-2">Roles</Card.Header>
					<Card.Body className="py-2">
						<Accordion alwaysOpen>
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
					</Card.Body>
				</Card>

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

				{/* External role button at bottom */}
				<Form>
					<Form.Group as={Row} className="align-items-center" controlId="external-role">
						<Form.Label column sm={4}>ExternalRole</Form.Label>
						<Col sm={8}>
							<Button variant="primary" onClick={() => showRole(data.externalRole)}>
								ExternalRole
							</Button>
						</Col>
					</Form.Group>
				</Form>
			</Card.Body>
		</Card>
	);
}

export default InspectableContextView;


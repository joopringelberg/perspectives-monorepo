/*
  This module provides functionality in the form of a.o. a component that should be used to let the user make a choice.
  Embed the UserChoice component in a component that is rendered in the application. When its ChoiceMessage prop is set
  to a value that has a message, the dialog will be rendered.
*/
import React, { useEffect } from "react";
import { Card, ListGroup, Modal } from "react-bootstrap";
import { externalRole } from "./urifunctions";


export interface ChoiceMessage {
  title: string;
  message?: string;
  choices: Record<string, string>;
  chosen: (value: string) => void;
}

interface UserChoiceProps {
  message: ChoiceMessage;
}

export const UserChoice: React.FC<UserChoiceProps> = ({message}) => {
  const [show, setShow] = React.useState(message.message !== undefined);
  useEffect(() => {
    setShow(message.message !== undefined);
  }, [message.message]);
  return (<Modal show={show} onHide={() => setShow(false)}>
    <Modal.Header>
      <Modal.Title>{message.title || "A choice"}</Modal.Title>
    </Modal.Header>
    <Modal.Body>
      <Card>
        <Card.Body>
          <Card.Text>
          {message.message}
          </Card.Text>
        </Card.Body>
      </Card>
      <ListGroup>
        {Object.keys(message.choices).map((choice) => {
          const namePartMatch = choice.match(/\$(.*)/);
          return <ListGroup.Item 
            action 
            onClick={ () => { message.chosen(externalRole ( message.choices[choice] )); setShow(false)}}
            >{ 
              namePartMatch && namePartMatch[1] ? namePartMatch[1] : choice
            }</ListGroup.Item>
        })}
      </ListGroup>
    </Modal.Body>
    </Modal>);
}


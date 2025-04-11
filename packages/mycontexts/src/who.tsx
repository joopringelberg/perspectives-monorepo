import React from "react";
import { Who as WhoDef } from "perspectives-proxy";
import { Component } from "react";
import { TableForms } from "./tableForms";
import { ChatComponent, externalRole, PSContext } from "perspectives-react";
import { Accordion } from "react-bootstrap";

interface WhoProps {
  screenelements: WhoDef;
  showTablesAndForm: boolean;
}
export class Who extends Component<WhoProps> {
  render() {
    const defaultActiveKey = this.props.screenelements.chats.length > 0 ? this.props.screenelements.chats[0].fields.chatRole : undefined;
    return <PSContext.Consumer>{ value => 
      <>
        <Accordion defaultActiveKey={defaultActiveKey} flush>
        {this.props.screenelements.chats.map((chat) => (
          chat.fields.chatInstance ? 
            <Accordion.Item eventKey={chat.fields.chatRole} key={chat.fields.chatRole}>
            <Accordion.Header>{chat.fields.title}</Accordion.Header>
            <Accordion.Body>
              <ChatComponent 
                key={chat.fields.chatRole}
                externalrole={ externalRole( value.contextinstance )}
                roleinstance={chat.fields.chatInstance!}
                roletype={chat.fields.chatRole}
                messagesproperty={chat.fields.messageProperty}
                mediaproperty={chat.fields.mediaProperty}
                myroletype={value.myroletype}
              />
            </Accordion.Body>
            </Accordion.Item>
          : null))
        }
        </Accordion>
        <TableForms screenelements={this.props.screenelements.userRoles} showTablesAndForm={this.props.showTablesAndForm} doubleclickOpensDetails={true} />
      </>
      }</PSContext.Consumer>;
  }
}

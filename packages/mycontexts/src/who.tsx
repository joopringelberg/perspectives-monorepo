import * as React from "react";
const { Component } = React;
import { Who as WhoDef } from "perspectives-proxy";
import { TableForms } from "./tableForms";
import { buildMarkDown, ChatComponent, externalRole, PSContext } from "perspectives-react";
import { Accordion } from "react-bootstrap";

interface WhoProps {
  screenelements: WhoDef;
  showTablesAndForm: boolean;
}
export class Who extends Component<WhoProps> {
  render() {
    const defaultActiveKey = this.props.screenelements.chats[0]?.fields.chatInstance;
    return <PSContext.Consumer>{ value => 
      <div className="content-top-aligned px-0">{ defaultActiveKey ?
        <Accordion defaultActiveKey={defaultActiveKey} flush>
        {this.props.screenelements.chats.map((chat) => (
          chat.fields.chatInstance ? 
            <Accordion.Item eventKey={chat.fields.chatInstance} key={chat.fields.chatInstance}>
              <Accordion.Header>{chat.fields.title}</Accordion.Header>
              <Accordion.Body className="chat-height">
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
        : null}
        {this.props.screenelements.markdown.map((markdown, index) => 
          <div key={index} className="markdown">{ buildMarkDown(value.contextinstance, value.myroletype, markdown) }</div>
        )}
        <TableForms screenelements={this.props.screenelements.userRoles} showTablesAndForm={this.props.showTablesAndForm} doubleclickOpensDetails={false} />
      </div>
      }</PSContext.Consumer>;
  }
}

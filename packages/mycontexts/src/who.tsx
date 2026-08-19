import * as React from "react";
const { Component, createRef } = React;
import { ContextType, Who as WhoDef, WhoWhatWhereScreenDef } from "perspectives-proxy";
import { TableForms } from "./tableForms";
import { buildMarkDown, ChatComponent, externalRole, PSContext } from "perspectives-react";
import { Accordion } from "react-bootstrap";
import { NavigationGraphView } from "./NavigationGraphView";

interface WhoProps {
  screenelements: WhoDef;
  showTablesAndForm: boolean;
  /** Full screen definition used to populate the navigation graph. */
  whoWhatWhereScreen?: WhoWhatWhereScreenDef;
  /** The context type currently open (graph highlight). */
  currentContextType?: ContextType;
  /** Human-readable label for the current context type. */
  currentContextLabel?: string;
}
export class Who extends Component<WhoProps> {
  private hostRef = createRef<HTMLDivElement>();

  render() {
    const { whoWhatWhereScreen, currentContextType, currentContextLabel } = this.props;
    const defaultActiveKey = this.props.screenelements.chats[0]?.fields.chatInstance;
    return <PSContext.Consumer>{ value => 
      <div className="content-top-aligned px-0" ref={this.hostRef}>{ defaultActiveKey ?
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
        <TableForms screenelements={this.props.screenelements.userRoles} showTablesAndForm={this.props.showTablesAndForm} />
        { whoWhatWhereScreen && currentContextType ? (
          <NavigationGraphView
            currentContextType={currentContextType}
            currentContextLabel={currentContextLabel ?? ""}
            currentScreen={whoWhatWhereScreen}
            hostRef={this.hostRef}
          />
        ) : null }
      </div>
      }</PSContext.Consumer>;
  }
}

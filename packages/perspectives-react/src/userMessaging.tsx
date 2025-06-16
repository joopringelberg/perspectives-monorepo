
// This module exports three values:
//     * a promise UserMessagingPromise for an instantiated, stateful UserMessaging object;
//     * a function initUserMessaging to initiate this instance with two functions.
//     * a component EndUserNotifier to render in the App. it will show end user notifications.
// The instantiated UserMessaging object can be used in many modules. It connects them with the
// user-facing EndUserNotifier component that notifies the end user.
// Modules can add messages in either of these two categories to the UserMessaging object.
// It buffers them and notifies the user one by one.
// This is realised by using the functions provided on initialization. 
// Either function should accept a message (an object with three members, see below) and return a promise whose fulfillment signals 
// acknowledgment (e.g. by the end user pressing a button in a model dialog). 
//
// Here is a skeleton for the notifyEndUser function to be passed to the initUserMessaging function.
// It should be defined in a scope where it can access App state that causes the EndUserNotifier props to be set.
// function ( message )
//   {
//     const p = new Promise(function(resolve)
//       { 
//         message.acknowledge = resolve
//       });
//     app.setState( {endUserMessage: message});
//     return p.then( function()
//     {
//       app.setState( { endUserMessage: {}} );
//     })
//   };
//
// In this example, the EndUserNotifier would have been incorporated as:
//    <EndUserNotifier message={app.state.endUserMessage}/>



import { Button, Card, Modal } from "react-bootstrap";
import PerspectivesComponent from "./perspectivesComponent.js";

export interface UserMessagingMessage {
  title: string;
  message?: string;
  acknowledge?: (value: boolean) => void;
  error?: string;
}

export type NotifyFunction = (message: UserMessagingMessage) => Promise<void>;

class UserMessaging
{
    oldMessages: UserMessagingMessage[];
    newMessages: UserMessagingMessage[];
    notifyEndUser: NotifyFunction;
    notifyDeveloper?: NotifyFunction;
    showing: boolean;

    constructor(notifyEndUser: NotifyFunction, notifyDeveloper?: NotifyFunction) {
      // A history of messages.
      this.oldMessages = [];
      this.newMessages = [];
      // A function that accepts a message and that somehow notifies the end user.
      // The function should return a promise whose fulfillment signals that the user has acknowledged the message.
      this.notifyEndUser = notifyEndUser;
      notifyDeveloper ? this.notifyDeveloper = notifyDeveloper : null;
      // A state variable. If true, showMessages is still waiting for the acknowledgement of the user of a message.
      this.showing = false;
    }


  // Add a message for the end user to be notified with.
  addMessageForEndUser (m : UserMessagingMessage)
  {
    this.newMessages.push(m)
    if (!this.showing)
    {
      this.showMessages();
    }
  }

  showMessages()
  {
    const component = this;
    let next : UserMessagingMessage | undefined;
    if (this.newMessages.length > 0)
    {
      this.showing = true;
      next = this.newMessages.pop();
      if (next)
      {
        this.oldMessages.push( next );
      this.notifyEndUser( next ).then( () => component.showMessages() );
      }
    }
    else
    {
      this.showing = false;
    }
  }
}

let messagingResolver: (value: UserMessaging) => void;

// Use this function in the constructor of the application component. 
// It should be applied only once!
interface InitUserMessaging {
  (notifyEndUser: NotifyFunction, notifyDeveloper?: NotifyFunction): void;
}

export const initUserMessaging: InitUserMessaging = (notifyEndUser, notifyDeveloper?) => {
  messagingResolver(new UserMessaging(notifyEndUser, notifyDeveloper));
};

// Use this promise in any component that must notify the end user or the developer.
// It resolves to the instantiated UserMessaging object.
// Use method addMessageForEndUser on it.
export const UserMessagingPromise = new Promise<UserMessaging>(
  function (resolve) {
    messagingResolver = resolve;
  }
);

// Render in the App. 
// To trigger a message, set message and acknowledge.
// the acknowledge function should
//  - resolve a promise that is used to signal acknowledgement to the UserMessaging instance;
//  - set props.message to undefined, so the Modal is no longer shown.
interface EndUserNotifierState {
  showErrorMessage: boolean;
}

interface EndUserNotifierProps {
  message: UserMessagingMessage;
}

export class EndUserNotifier extends PerspectivesComponent<EndUserNotifierProps, EndUserNotifierState>
{
  state: EndUserNotifierState;

  constructor(props: EndUserNotifierProps)
  {
    super(props);
    this.state = {
      showErrorMessage: false
    };
  }
  toggleShow()
  {
    this.setState({showErrorMessage: !this.state.showErrorMessage})
  }
  render()
  {
    const component = this;
    const acknowledge = component.props.message.acknowledge ? component.props.message.acknowledge : () => undefined;
    return <Modal
      show={component.props.message.message !== undefined}
      onHide={ () => acknowledge( true )}>
      <Modal.Header closeButton>
        <Modal.Title>{component.props.message.title || "A message"}</Modal.Title>
      </Modal.Header>
      <Modal.Body>
        <Card>
          <Card.Body>
            <Card.Text>
              {component.props.message.message}
            </Card.Text>
          </Card.Body>
        </Card>
        { component.state.showErrorMessage ? <pre>{component.props.message.error}</pre> : null}
      </Modal.Body>
      <Modal.Footer>
        <Button variant="primary" onClick={ () => acknowledge(true)}>
          OK
        </Button>
        {
          component.props.message.error ?
          <Button 
            variant="secondary" 
            onClick={ () => component.toggleShow()}
            >
              {component.state.showErrorMessage ? "Hide details" : "Show details"}
            </Button>
          : null
        }
      </Modal.Footer>
    </Modal>
  }
}

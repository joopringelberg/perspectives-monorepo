// OBSOLETE

import React, { forwardRef } from "react";
import PropTypes from "prop-types";

import
  { Button
  , Form
  , Row
  , Col
  , Card
  } from "react-bootstrap";

import {PSView, PSRol} from "../reactcontexts.js";
import {ViewOnExternalRole} from "../views.js";
import SetBoolAsCheckbox from "./setboolascheckbox.js";
import RolInstance from "../roleinstance.js";
import {makeRoleInListPresentation} from "../cards.js";
import ExternalRole from "../externalrole.js";
import View from "../view.js";
import RoleInstance from "../roleinstance.js";
import RoleDropZone from "../roleDropzone.js";
import { BackButton } from "./perspectivescontainer.js";
import { addFillARole } from "../cardbehaviour.js";
import {addBehaviour} from "../behaviourcomponent.js";

import {ArrowRightIcon} from '@primer/octicons-react';

export function Invitation()
{
  function InvitationCard(props)
  {
    function download(filename, text) {
        var element = document.createElement('a');
        element.setAttribute('href', 'data:text/json;charset=utf-8,' + encodeURIComponent(text));
        element.setAttribute('download', filename);

        element.style.display = 'none';
        document.body.appendChild(element);

        element.click();

        document.body.removeChild(element);
    }

    if ( props.required )
    {
      return  <>
                <Form.Group as={Row}>
                  <Form.Label column sm="3" id="MessageId">Message:</Form.Label>
                  <Col sm="9">
                    <PSView.Consumer>
                      {props => <Form.Control aria-labelledby="MessageId" aria-describedby="messageDescriptionID" defaultValue={props.propval("Message")} onBlur={e => props.propset("Message", e.target.value)} />}
                    </PSView.Consumer>
                  </Col>
                  <Col sm="9">
                    <Form.Text className="text-muted" id="messageDescriptionID">
                      Enter a text message to invite your contact with.
                    </Form.Text>
                  </Col>
                </Form.Group>
                <Row>
                  <Col>
                    <Button
                      variant="primary"
                      onClick={ () => download("invitation.json", props.serialisation) }
                      disabled={props.serialisation[0] ? null : true}
                      >Download invitation file</Button>
                  </Col>
                  <Col><p>Click the button to download an invitation file. An invitation file is personal! Send it to the person you want to invite to connect, through a secure channel.</p></Col>
                </Row>
              </>;
    }
    else
    {
      return <div/>;
      }
  }

  return  <ViewOnExternalRole viewname="allProperties">
            <PSView.Consumer>
              {value => (
                  <>
                    <SetBoolAsCheckbox
                      propertyname="IWantToInviteAnUnconnectedUser"
                      label="I want to invite someone I have no contact card for"
                    />
                    <InvitationCard required={value.propval("IWantToInviteAnUnconnectedUser")[0] == ["true"]} serialisation={value.propval("SerialisedInvitation") }/>
                  </>)}
            </PSView.Consumer>
          </ViewOnExternalRole>;

}

function Message()
{
  return <Form.Group as={Row}>
            <Form.Label column sm="3">You are invited:</Form.Label>
            <Col sm="9">
              <ExternalRole>
                <View viewname="allProperties">
                  <PSView.Consumer>
                    {props => <Form.Control className="font-italic" plaintext readOnly defaultValue={props.propval("Message")}/>}
                  </PSView.Consumer>
                </View>
              </ExternalRole>
            </Col>
          </Form.Group>;
}

export function ViewIncomingInvitation(props)
{
  const ContactCard = addBehaviour( makeRoleInListPresentation(
    // eslint-disable-next-line react/display-name
    forwardRef( function(props, ref)
    {
      // eslint-disable-next-line react/prop-types
      return  <Card ref={ref} tabIndex={props.tabIndex} aria-label={props["aria-label"]}>
                <Card.Text>Contact card of {
                  // eslint-disable-next-line react/prop-types
                  props.propval("FirstName")}.</Card.Text>
              </Card>;
    }))
    , [addFillARole]);
  return (<>
    <Message/>
    <section aria-label="Received invitation">
      <Form.Group as={Row} controlId="initiator" className="align-items-center">
        <Col sm="4">
          <RolInstance role="Guest">
            <ContactCard labelProperty="FirstName"/>
          </RolInstance>
        </Col>
        <Col sm="4 text-center">
          <ArrowRightIcon alt="ArrowRight" size="large"/>
        </Col>
        <Col sm="4">
          <RoleInstance role={props.specialisedRole}>
            <PSRol.Consumer>{ psrol =>
              <RoleDropZone
                ariaLabel="To accept the invitation, drag your own contact card over here and drop it."
                bind={psrol.bind /* As we have a role, just bind the role we drop.*/}
                checkbinding={psrol.checkbinding}
              >
                <Card>
                  <Card.Body>
                    <p>To accept the invitation, drag your own contact card over here and drop it. create</p>
                  </Card.Body>
                </Card>
              </RoleDropZone>
            }</PSRol.Consumer>
            <PSRol.Consumer>{ psrol =>
              <RoleDropZone
                ariaLabel="To accept the invitation, drag your own contact card over here and drop it."
                bind={psrol.bind_ /* As we have a role, just bind the role we drop.*/}
                checkbinding={psrol.checkbinding}
              >
                <Card>
                  <Card.Body>
                    <p>To accept the invitation, drag your own contact card over here and drop it. create</p>
                  </Card.Body>
                </Card>
              </RoleDropZone>
            }</PSRol.Consumer>
          </RoleInstance>
        </Col>
      </Form.Group>
    </section>
  </>);
}

ViewIncomingInvitation.propTypes = {specialisedRole: PropTypes.string};

// OBSOLETE

import React, { forwardRef } from "react";
import {deconstructLocalName} from "../urifunctions.js";
import RoleInstance from "../roleinstance.js";
import View from "../view.js";
import {PSView, PSRol} from "../reactcontexts.js";
import ExternalRole from "../externalrole.js";
import RoleDropZone from "../roleDropzone.js";
import {makeSingleRolePresentation} from "../cards.js";
import {addBehaviour} from "../behaviourcomponent.js";
import {addFillARole, addOpenContextOrRoleForm} from "../cardbehaviour.js";

import
  { Row
  , Col
  , Form
  , Card
  } from "react-bootstrap";
import PropTypes from "prop-types";

////////////////////////////////////////////////////////////////////////////////
// ROLEFORM
////////////////////////////////////////////////////////////////////////////////

export function RoleForm (props)
{

  if (props.rolename == "External")
  {
    return  <ExternalRole>
              <View viewname={props.viewname}>
                <RoleFormInView cardprop={props.cardprop}/>
              </View>
            </ExternalRole>;
  }
  else
  {
    return  <RoleInstance role={props.rolename}>
              <View viewname={props.viewname}>
                <RoleFormInView cardprop={props.cardprop}/>
              </View>
            </RoleInstance>;
  }
}

RoleForm.propTypes =
  { viewname: PropTypes.string.isRequired
  , rolename: PropTypes.string.isRequired
  , cardprop: PropTypes.string
};


export function RoleFormInView(props)
{
  function roleForm (psview)
  {
    const cardProp = props.cardprop;

    const DraggableCard = addBehaviour(
      makeSingleRolePresentation(
        // eslint-disable-next-line react/display-name
        forwardRef( function(props, ref)
        {
          // eslint-disable-next-line react/prop-types
          return  <Card ref={ref} tabIndex={props.tabIndex} aria-label={props["aria-label"]}>
                    <Card.Body>
                      <Card.Text>{
                        // eslint-disable-next-line react/prop-types
                        props.propval(cardProp)
                      }</Card.Text>
                    </Card.Body>
                  </Card>;
        })),
      [addFillARole, addOpenContextOrRoleForm]
      );

    return  <PSRol.Consumer>{
              function (psrol)
              {
                function TheDropZone()
                {
                  return <RoleDropZone
                    bind={psrol.bind_}
                    checkbinding={psrol.checkbinding}
                    ariaLabel="To set this form to a particular user, drag his or her card over here and drop it."
                    >
                      {
                        psview.viewproperties.map(
                          function( propName )
                            {
                              const localName = deconstructLocalName (propName);
                              return  <Form.Group as={Row} key={propName}>
                                        <Form.Label column sm="3">{ localName }</Form.Label>
                                        <Col sm="9">
                                          <Form.Control aria-label={ localName } defaultValue={psview.propval(propName)} onBlur={e => psview.propset(propName, e.target.value)}/>
                                        </Col>
                                      </Form.Group>;
                            })
                      }
                    </RoleDropZone>;
                }
                if (cardProp)
                {
                  return <>
                          <TheDropZone/>
                          <Row><DraggableCard labelProperty={cardProp}/></Row>
                        </>;
                }
                else
                {
                  return <TheDropZone/>;
                }
          }}</PSRol.Consumer>;
  }

  return  <PSView.Consumer>
          { roleForm }
          </PSView.Consumer>;
}
RoleFormInView.propTypes =
  { cardprop: PropTypes.string
};

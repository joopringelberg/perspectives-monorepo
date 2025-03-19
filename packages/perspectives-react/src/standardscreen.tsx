// BEGIN LICENSE
// Perspectives Distributed Runtime
// Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//
// Full text of this license can be found in the LICENSE file in the projects root.
// END LICENSE

import React from 'react';

import {PDRproxy, CONTINUOUS, Perspective, ContextInstanceT, ContextType, RoleType} from "perspectives-proxy";
import PerspectivesComponent from "./perspectivesComponent";
import {PSContext} from "./reactcontexts.js";
import PerspectiveBasedForm from "./perspectivebasedform.js";
import PerspectiveTable from "./perspectivetable.js";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";
import { Tab, Nav, Navbar, Container, Button, Card, Tabs } from "react-bootstrap";
import { mapRoleVerbsToBehaviourNames } from './maproleverbstobehaviours';

interface StandardScreenProps
{
  contextinstance: ContextInstanceT
  contexttype: ContextType
  myroletype: RoleType
  contextIdToAddRoleInstanceTo: ContextInstanceT
}

interface StandardScreenState
{
  perspectives: Perspective[]
}

export default class StandardScreen extends PerspectivesComponent<StandardScreenProps, StandardScreenState>
{
  constructor( props : StandardScreenProps )
  {
    super(props);
    this.state =
      { perspectives: [] };
  }

  componentDidMount()
  {
    this.getPerspectives();
  }

  componentDidUpdate(prevProps : StandardScreenProps)
  {
    if (this.props.contextinstance != prevProps.contextinstance ||
        this.props.myroletype != prevProps.myroletype )
    {
      this.getPerspectives();
    }
  }

  getPerspectives ()
  {
    const component = this;
    PDRproxy.then(function(pproxy)
      {
        component.addUnsubscriber(
          // (contextInstance, userRoleInstance, userRoleType, receiveValues, fireAndForget)
          pproxy.getPerspectives(
            component.props.contextinstance
            ,component.props.myroletype
            ,function( perspectives : Perspective[] )
            {
              console.log(perspectives);
              component.setState({perspectives: perspectives});
            }
            ,CONTINUOUS
          ));
      });
  }

  mayCreateInstance( perspective : Perspective )
  {
    return !perspective.isCalculated &&
      (perspective.verbs.includes("Create") || perspective.verbs.includes("CreateAndFill"));
  }

  showFormOrTable (perspective : Perspective)
  {
    const component = this;
    return  (<Tab key={perspective.id} eventKey={perspective.id} title={perspective.displayName}>
            <Container>
              { perspective.isFunctional ?
                <PerspectiveBasedForm
                  perspective={perspective}
                  behaviours={mapRoleVerbsToBehaviourNames( perspective )}
                  cardtitle={ perspective.identifyingProperty }
                  showControls={true}
                  />
                : <PerspectiveTable
                    cardcolumn={ perspective.identifyingProperty }
                    //roleRepresentation
                    perspective={perspective}
                    />}
            </Container>
          </Tab>);
  }

  createRoleInstance( perspective : Perspective )
  {
    const component = this;
    PDRproxy.then( function (pproxy)
      {
        // If a ContextRole Kind, create a new context, too.
        if (perspective.roleKind == "ContextRole" && Object.keys(perspective.contextTypesToCreate).length > 0)
        {
          pproxy.createContext (
              {
                //id will be set in the core.
                prototype : undefined,
                ctype: perspective.contextTypesToCreate[0], // Arbitrary choice, for now.
                rollen: {},
                externeProperties: {}
              },
              perspective.roleType,                           // qualified role name.
              component.props.contextIdToAddRoleInstanceTo,   // context instance to add to.
              component.props.myroletype)
            .catch(e => UserMessagingPromise.then( um => 
              um.addMessageForEndUser(
                { title: i18next.t("createContext_title", { ns: 'preact' }) 
                , message: i18next.t("createContext_message", {ns: 'preact', type: component.props.contexttype})
                , error: e.toString()
                })));
          }
        else
        {
          pproxy
            .createRole(
              component.props.contextinstance,
              perspective.roleType,
              component.props.myroletype)
            .catch(e => UserMessagingPromise.then( um => 
              um.addMessageForEndUser(
                { title: i18next.t("createRole_title", { ns: 'preact' }) 
                , message: i18next.t("createRole_message", {ns: 'preact', roletype: perspective.roleType})
                , error: e.toString()
                })))
          }
      });
  }

  handleKeyDown (event : React.KeyboardEvent, perspective : Perspective)
  {
    const component = this;
      switch(event.code){
        case "Enter": // Return
        case "Space": // Space
          component.createRoleInstance( perspective );
          event.preventDefault();
          event.stopPropagation();
          break;
      }
  }

  render()
  {
    const component = this;
    if (component.stateIsComplete())
    {
      // Fetched perspectives from the server, but do we have one?
      if (component.state.perspectives[0])
      {
        return (
          <Container role="application">
            <Tabs defaultActiveKey={component.state.perspectives[0].id} id="perspective-tabs">
            {
              component.state.perspectives.map( perspective =>
                {
                  if (Object.keys( perspective.roleInstances ).length > 0 )
                  {
                    // We have instances
                    return component.showFormOrTable(perspective);
                  }
                  else if (component.mayCreateInstance(perspective))
                  {
                    if (Object.keys( perspective.properties ).length > 0 )
                    {
                      // No instances, but properties anyway:
                      return component.showFormOrTable(perspective);
                    }
                    else
                    {
                      // There may be properties for instances when they are made. Just show a create button.
                      return  <Tab key={perspective.id} eventKey={perspective.id} title={perspective.displayName}>
                                <Button
                                  tabIndex={0}
                                  variant="secondary"
                                  onClick={() => component.createRoleInstance( perspective )}
                                  onKeyDown={ ev => component.handleKeyDown(ev, perspective)}
                                  aria-label="Click or press space or return to add a row"
                                  >Create
                                </Button>
                              </Tab>;
                    }
                  }
                  else
                  {
                    return null;
                  }
              }
              )
            }
            </Tabs>
          </Container>);
      }
      else {
        return    <Container>
                    <Card>
                      <Card.Body>
                        <Card.Title>An error condition occurred</Card.Title>
                        <Card.Text>
                          The role you currently have in this context has no perspectives. Try another role.
                        </Card.Text>
                      </Card.Body>
                      </Card>
                  </Container>;
      }
    }
    else
    {
      return null;
    }
  }
}

StandardScreen.contextType = PSContext;

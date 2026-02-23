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

import React, { PureComponent } from "react";
import {PDRproxy, PropertyType, RoleInstanceT, Perspective, Roleinstancewithprops, RoleVerb, FieldDisplayConstraint} from "perspectives-proxy";
import { mapRoleVerbsToBehaviourNames } from "./maproleverbstobehaviours.js";
import RoleDropZone from "./roleDropzone.js";
import PerspectivesComponent from "./perspectivesComponent";
import RoleInstance from "./roleinstance.js";
import SmartFieldControlGroup from "./smartfieldcontrolgroup.js";
import FormControls from "./formcontrols.js";
import
  { Card, Form
  } from "react-bootstrap";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";
import { CardProperties } from "./cardbehaviour.js";
import { cardWithConfigurableBehaviour } from "./adorningComponentWrapper.js";
import { isEqual } from 'lodash';
import { isInPublicScheme } from "./urifunctions.js";

////////////////////////////////////////////////////////////////////////////////
// PERSPECTIVEBASEDFORM
// This component requires a perspective on its props.
// It may have a prop roleinstance and when present it is used to index the
// instances of the perspective.
////////////////////////////////////////////////////////////////////////////////

interface PerspectiveBasedFormProps {
  perspective: Perspective;
  roleinstance?: RoleInstanceT;
  cardtitle: PropertyType;
  showControls : boolean;
  suppressIdentifyingProperty?: boolean;
  fieldConstraints?: FieldDisplayConstraint[];
}

interface PerspectiveBasedFormState {
  roleInstanceWithProps?: Roleinstancewithprops;
  selectedField: number;
}

export default class PerspectiveBasedForm extends PerspectivesComponent<PerspectiveBasedFormProps, PerspectiveBasedFormState>
{
  constructor(props : PerspectiveBasedFormProps)
  {
    super(props);
    // Note that rolintanceWithprops may be undefined.
    this.state = {roleInstanceWithProps: this.computeRoleInstanceWithProps(), selectedField: 0};
    this.checkBinding = this.checkBinding.bind(this);
    this.bind_ = this.bind_.bind(this);
    this.createRoleInstance = this.createRoleInstance.bind(this);
  }

  // Possibly returns an object with this shape: roleinstancewithprops (see perspectivesshape.js).
  // It is either the roleinstance passed in the props, or the first roleinstance
  computeRoleInstanceWithProps() : Roleinstancewithprops | undefined
  {
    const component = this;
    if (component.props.roleinstance)
    {
      return Object.values( this.props.perspective.roleInstances )
        .filter( inst => inst.roleId == component.props.roleinstance)[0];
    }
    else
    {
      return Object.values( component.props.perspective.roleInstances )[0];
    }

  }

  // Always returns an array of strings (possibly empty).
  findValue( propId : string )
  {
    const component = this;
    if (component.state.roleInstanceWithProps && component.state.roleInstanceWithProps.propertyValues[propId])
    {
      return component.state.roleInstanceWithProps.propertyValues[propId].values;
    }
    else
    {
      return [];
    }
  }

  // Always returns an object with this shape:
  // PropTypes.shape(
  //   { values: PropTypes.arrayOf( PropTypes.string ).isRequired
  //   , propertyVerbs: PropTypes.arrayOf( PropTypes.string).isRequired
  //   })
  findValues( propId : string )
  {
    const component = this;
    if (component.state.roleInstanceWithProps)
    {
      return component.state.roleInstanceWithProps.propertyValues[propId];
    }
    else
    {
      return {values:[], propertyVerbs:[]};
    }
  }

  componentDidMount()
  {
    const component = this;
    if (!component.state.roleInstanceWithProps && !component.props.perspective.isCalculated && component.props.perspective.isMandatory)
    {
      component.createRoleInstance();
    }
  }

  componentDidUpdate() {
    const component = this;
    const roleInstanceWithProps = component.computeRoleInstanceWithProps();
    
    if (!isEqual(roleInstanceWithProps, component.state.roleInstanceWithProps)) {
      component.setState({ roleInstanceWithProps });
    }
  }

  createRoleInstance()
  {
    const component = this;
    if (component.props.perspective.roleType)
    { 
      PDRproxy.then(
        function (pproxy)
        {
          return pproxy
            .createRole(
              component.props.perspective.contextInstance,
              component.props.perspective.roleType!,
              component.props.perspective.userRoleType)
            .catch(e => UserMessagingPromise.then( um => 
              um.addMessageForEndUser(
                { title: i18next.t("createRole_title", { ns: 'preact' }) 
                , message: i18next.t("createRole_message", {ns: 'preact', roletype: component.props.perspective.roleType})
                , error: e.toString()
                })));
        }
    )}
  }

  // returns a promise for a boolean value.
  checkBinding(rolinstance : RoleInstanceT)
  {
    const component = this;
    return PDRproxy.then( pproxy =>
      // checkBinding( <contexttype>, <localRolName>, <binding>, [() -> undefined] )
      pproxy
        .checkBindingP(
          component.props.perspective.roleType!,
          rolinstance) )
  }

  bind_(rolInstance : RoleInstanceT)
  {
    const component = this;
    return PDRproxy.then(
      function (pproxy)
      {
        pproxy.bind_(
          component.state.roleInstanceWithProps!.roleId,
          rolInstance, // binding
          component.props.perspective.userRoleType)
        });
  }

  render ()
  {
    const component = this;
    const perspective = this.props.perspective;
    // Compute behaviour names including any object-state-based role verbs for the current role instance.
    const behaviourNames = mapRoleVerbsToBehaviourNames(perspective, component.state.roleInstanceWithProps);

    function Controls()
    {
      const isPublic = component.state.roleInstanceWithProps ? isInPublicScheme(component.state.roleInstanceWithProps!.roleId) : false;
      return  <RoleInstance
                roleinstance={component.state.roleInstanceWithProps ? component.state.roleInstanceWithProps.roleId : undefined}
                roletype={component.props.perspective.roleType!}
                rolekind={component.props.perspective.roleKind}
                contextinstance={component.props.perspective.contextInstance}
                contexttype={component.props.perspective.contextType}
                myroletype={component.props.perspective.userRoleType}
                allowedtoremovecontext={component.props.perspective.verbs.includes("RemoveContext" as RoleVerb) || component.props.perspective.verbs.includes("DeleteContext")}
              >
                {/* The default element for the RoleInstance. The controls will have a create button. 
                When there is no role instance, this will be rendered by the RoleInstance component.
                */}
                <FormControls
                  perspective={ component.props.perspective}
                  contextinstance={ component.props.perspective.contextInstance }
                  // No roleinstance.
                  myroletype={component.props.perspective.userRoleType}
                  card={ <DraggableCard 
                      className={isPublic ? 'public-role' : ''}
                      behaviourNames={behaviourNames}
                      aria-label={component.props.cardtitle} 
                      title={component.props.perspective.displayName}/> }
                  />

                {/* This will be displayed by RoleInstance if there is a role instance. */}
                <RoleDropZone
                  bind={component.bind_}
                  checkbinding={component.checkBinding}
                  ariaLabel="To fill the role whose properties are displayed here, drag another role onto it."
                >
                  <FormControls
                    perspective={ component.props.perspective}
                    contextinstance={ component.props.perspective.contextInstance }
                    roleinstance={component.state.roleInstanceWithProps ? component.state.roleInstanceWithProps.roleId : undefined}
                    myroletype={component.props.perspective.userRoleType}
                    card={ <DraggableCard 
                      tabIndex={0}
                      className={isPublic ? 'public-role' : ''}
                      behaviourNames={behaviourNames}
                      aria-label={component.props.cardtitle} 
                      title={component.state.roleInstanceWithProps?.readableName || component.props.cardtitle}/> }
                  />
                </RoleDropZone>
              </RoleInstance>;
    }

    
    function handleTab(e: React.KeyboardEvent<HTMLFormElement>): void
    {
      const propertyCount: number = Object.values(component.props.perspective.properties)
      .filter(property => !component.props.suppressIdentifyingProperty || property.id !== perspective.identifyingProperty)
      .length;
      if (component.state.selectedField < propertyCount && e.key === "Tab" && !e.shiftKey)
      {
        e.stopPropagation();
        e.preventDefault();
        component.setState({ selectedField: (component.state.selectedField + 1)});
      }
      else if (component.state.selectedField > 0 && e.key === "Tab" && e.shiftKey)
      {
        e.stopPropagation();
        e.preventDefault();
        component.setState({ selectedField: (component.state.selectedField - 1)});
      }
      else if (e.key === "Enter")
      {
        e.stopPropagation();
        e.preventDefault();
      }
      else if (e.key !== "Tab")
      {
        e.stopPropagation();
        // e.preventDefault();
      }
    }

    // If there are no properties defined on this role type,
    // Just show the form controls.
    if (Object.keys( component.props.perspective.properties).length === 0 && component.props.perspective.roleType)
    {
      return <Controls/>;
    }
    else
    {
      // Build a lookup map from property id to field constraint for O(1) access.
      const constraintMap = new Map(
        (component.props.fieldConstraints ?? []).map(c => [c.propertyType.value as string, c])
      );
      return (
        <Form onKeyDown={handleTab}>
          {
            Object.values(perspective.properties)
              .filter( property => !component.props.suppressIdentifyingProperty || property.id !== perspective.identifyingProperty)
              .map((serialisedProperty, index) =>
              {
                const constraint = constraintMap.get(serialisedProperty.id as string);
                return (
                  <div 
                    onClick= {() => component.setState({selectedField: index})}
                    key={serialisedProperty.id}
                  >
                    <SmartFieldControlGroup
                      hasFocus={index === component.state.selectedField}
                      serialisedProperty={serialisedProperty}
                      propertyValues={component.findValues( serialisedProperty.id )}
                      roleId={component.state.roleInstanceWithProps ? component.state.roleInstanceWithProps.roleId : undefined}
                      myroletype={component.props.perspective.userRoleType}
                      contextinstance={component.props.perspective.contextInstance}
                      minLines={constraint?.minLines}
                      maxLines={constraint?.maxLines}
                    />
                  </div>
                );
              })
          }
          { component.props.showControls ? <Controls/> : null}
        </Form>);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
// ROLECARD
// This component is used to display a role in a card.
////////////////////////////////////////////////////////////////////////////////

const RoleCard: React.FC<CardProperties> = (props) => {
  // The rest will be aria-label and className.
  return (<Card aria-label={props["aria-label"]}>
    <Card.Body className={'navbarCard' + (props.className ? ' ' + props.className : '')}>
      <Card.Text>
        {props.title}
      </Card.Text>
    </Card.Body>
  </Card>);
}
// This is now a component with the props behaviourNames, title and labelProperty.
const DraggableCard = cardWithConfigurableBehaviour(RoleCard);
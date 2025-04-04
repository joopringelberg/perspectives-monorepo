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
import {string} from "prop-types";

import {PDRproxy, ContextInstanceT, ContextType, RoleType, Unsubscriber, PropertyType, EnumeratedOrCalculatedProperty, ScreenDefinition, ChatElementDef, ColumnElementDef, FormElementDef, MarkDownElementDef, Perspective, Roleinstancewithprops, RowElementDef, ScreenElementDefTagged, TabDef, TableElementDef, WidgetCommonFields, MainScreenElements, TableFormDef, RoleInstanceT} from "perspectives-proxy";
import PerspectivesComponent from "./perspectivesComponent";
import {PSContext, PSContextType} from "./reactcontexts.js";
import PerspectiveBasedForm from "./perspectivebasedform.js";
import PerspectiveTable from "./perspectivetable.js";
import PerspectivesTabs from "./perspectivestabs.js";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";

import {Tab, Container, Row, Col} from "react-bootstrap";
import {MarkDownWidget} from './markdownWidget.js';
import SmartFieldControl from './smartfieldcontrol.js';
import {ChatComponent} from './chatcomponent.js';
import { externalRole } from './urifunctions.js';
import { mapRoleVerbsToBehaviourNames } from './maproleverbstobehaviours';

interface FreeFormProps
{
  screen: MainScreenElements
  myroletype: RoleType
  contextinstance: ContextInstanceT
  contexttype: ContextType
}
// MainScreenElements
interface FreeFormState
{
  screen: ScreenDefinition | "Reload" | "TryAnotherRole" | undefined
}

export class FreeFormScreen extends PerspectivesComponent<FreeFormProps, FreeFormState>
{
  declare context: PSContextType
  static contextType = PSContext
  activeTabKey: number;
  unsubscriber: Unsubscriber | undefined;
  
  constructor( props : FreeFormProps)
  {
    super(props);
    this.state =
      { screen: undefined };
    this.screenElement = this.screenElement.bind(this);
    this.buildRow = this.buildRow.bind(this);
    this.buildColumn = this.buildColumn.bind(this);
    this.activeTabKey = 0;
    this.unsubscriber = undefined;
  }
  mayCreateInstance( perspective : Perspective )
  {
    return !perspective.isCalculated &&
      (perspective.verbs.includes("Create") || perspective.verbs.includes("CreateAndFill"));
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
              perspective.roleType,                       // qualified role name
              perspective.contextIdToAddRoleInstanceTo,   // context instance to add to.
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
        case "Enter":
        case "Space":
          component.createRoleInstance( perspective );
          event.preventDefault();
          event.stopPropagation();
          break;
      }
  }
  buildScreen(screen : ScreenDefinition)
  {
    const component = this;
    let contents;
    if (screen.tabs)
    {
      contents = component.buildTabs(screen.tabs);
    }
    else if (screen.rows)
      {
        contents = screen.rows.map( component.screenElement );
      }
    else if (screen.columns)
      {
        contents = screen.columns.map( component.screenElement );
      }
    return  (<Container role="application">
            { screen.title ? <h3>{screen.title}</h3> : null }
            {
              contents
            }
            </Container>);
  }

  buildTabs(tabs : TabDef[])
  {
    const component = this;
    let defaultActiveKey = 0;
    tabs.forEach( function (tab, index){
      if (tab.isDefault)
      {
        defaultActiveKey = index;
      }
    } );
    return  <PerspectivesTabs tabs={tabs} defaultActiveKey={defaultActiveKey} activeTabKey={defaultActiveKey}>
            {
              tabs.map((tab, index) =>
                <Tab.Pane key={index} eventKey={index} title={tab.title}>
                  <Container>
                  { tab.elements.map( component.screenElement) }
                  </Container>
                </Tab.Pane>)
            }
            </PerspectivesTabs>;
  }
  screenElement(taggedElement : ScreenElementDefTagged, index : number)
  {
    const component = this;
    switch (taggedElement.elementType){
      case "RowElementD":
        return component.buildRow( taggedElement.element as RowElementDef, index );
      case "ColumnElementD":
        return component.buildColumn( taggedElement.element as ColumnElementDef, index );    
      case "TableElementD":
        const tableDef = taggedElement.element as TableElementDef;
        return (
          <div
            className="border-bottom pb-4 pt-4 widget"
            key={index}
            >
          { tableDef.fields.title ? <h4>{tableDef.fields.title}</h4> : null }
          { buildTable( tableDef ) }
          </div>);
      case "FormElementD":
        const formDef = taggedElement.element as FormElementDef;
        return (
          <div
            className="border-bottom pb-4 pt-4 widget"
            key={index}
            >
          { formDef.fields.title ? <h4>{formDef.fields.title}</h4> : null }
          { buildForm( formDef ) }
          </div>);
      case "MarkDownElementD":
        const markDownDef = taggedElement.element as MarkDownElementDef;
        return (
          <div 
            key={index}
          >{ component.buildMarkDown( markDownDef )}</div>
        )
      case "ChatElementD":
        const chatDef = taggedElement.element as ChatElementDef;
        return (
          <div
            key={index}
          >{ component.buildChat( chatDef )}</div>
        )
    }
  }
  buildRow({elements} : RowElementDef, index : number)
  {
    const component = this;
    return (
      <Row key={index}>
      {
        elements.map(component.screenElement)
      }
      </Row>
    );
  } 
  buildColumn({elements} : ColumnElementDef, index : number)
  {
    const component = this;
    return (
      <Col key={index}>
      {
        elements.map(component.screenElement)
      }
      </Col>
    );
  }
  buildChat({fields} : ChatElementDef)
  {
    const component = this;
    const {chatRole, chatInstance, messageProperty, mediaProperty} = fields;
    if (chatInstance)
    {
      return <ChatComponent 
              externalrole={externalRole( component.props.contextinstance) }
              roleinstance={chatInstance}
              roletype={chatRole}
              messagesproperty={messageProperty}
              mediaproperty={mediaProperty}
              myroletype={component.props.myroletype}
            >
            </ChatComponent>;
    }
    else
    {
      return <div/>;
    }
  }
  buildMarkDown({tag, element} : MarkDownElementDef)
  {
    const component = this;
      let perspective : Perspective, markDownProperty : PropertyType | undefined, conditionProperty : PropertyType | undefined;
      switch (tag) {
        // MarkDownConstant is by construction functional.
        case "MarkDownConstantDef":
        return <MarkDownWidget markdown={element.text} contextid={component.props.contextinstance} myroletype={component.props.myroletype}/>;
        // MarkDownExpression is required to be functional.
        case "MarkDownExpressionDef":
        // text is wrapped in Maybe
        if (element.text)
          {
            return <MarkDownWidget markdown={element.text} contextid={component.props.contextinstance} myroletype={component.props.myroletype}/>;
          }
        else
        {
          return null;
        }
      // MarkDownPerspective may be on a relational role.
      //   MarkDownPerspectiveDef { widgetFields :: WidgetCommonFieldsDef, conditionProperty :: Maybe PropertyType} |
      case "MarkDownPerspectiveDef":
        return component.buildMarkDownPerspective(element);
    }
  }

  buildMarkDownPerspective({ widgetFields, conditionProperty: cprop } : { widgetFields : WidgetCommonFields, conditionProperty : EnumeratedOrCalculatedProperty | null})
  {
      // The property is only consultable when it just has the verb Consult,
      // or when it is calculated. It will be shown disabled as a consequence.
      function propertyOnlyConsultable(roleInstance : Roleinstancewithprops)
      {
        if (roleInstance.propertyValues)
        {
          const propertyVerbs = roleInstance.propertyValues[ markDownProperty ].propertyVerbs;
          return (propertyVerbs.indexOf("Consult") > -1 
            && propertyVerbs.length == 1)
            || perspective.properties[markDownProperty].isCalculated;
        }
        else
        {
          return false;
        }
      }
    const component = this;
    const perspective = widgetFields.perspective;
    const conditionProperty = cprop ? cprop.value : undefined;
    const markDownProperty = Object.keys( perspective.properties ).filter( prop => prop != conditionProperty )[0] as PropertyType;  
    return  <Container>{
              Object.values( perspective.roleInstances )
                .filter( roleInstance => !!conditionProperty ? roleInstance.propertyValues[ conditionProperty ].values[0] == "true" : true)
                .map( roleInstance => 
                <Row key= { roleInstance.roleId }>
                  <Col>
                    <SmartFieldControl
                      // By construction, a single property is allowed and it must be the property with the MarkDown Range.
                      serialisedProperty = { perspective.properties[ markDownProperty ] }
                      propertyValues = { roleInstance.propertyValues[ markDownProperty ] }
                      roleId = { roleInstance.roleId }
                      myroletype = { perspective.userRoleType }
                      disabled={ propertyOnlyConsultable(roleInstance) || !roleInstance.roleId }
                      isselected={true}
                      contextinstance={component.props.contextinstance}
                    />
                  </Col>
                </Row>
              )
            }</Container>

  }

  render()
  {
    return this.buildScreen(this.props.screen as ScreenDefinition);
  }
}

export function buildTable(table : TableElementDef, showControls : boolean = true, showAsAccordionItem : boolean = false)
{
  const perspective = table.fields.perspective;
  // const title = widgetCommonFields.title;
  return (
    <PerspectiveTable
      key={table.fields.perspective.id}
      cardcolumn={ perspective.identifyingProperty }
      //roleRepresentation
      perspective={perspective}
      showcontrolsandcaption={showControls}
      showAsAccordionItem={showAsAccordionItem}
      />);
}
export function buildForm(form : FormElementDef, showControls : boolean = true, roleInstance? : RoleInstanceT)
{
  const perspective = form.fields.perspective;
  return (
    <PerspectiveBasedForm
      perspective={perspective}
      behaviours={mapRoleVerbsToBehaviourNames( perspective )}
      cardtitle={ perspective.identifyingProperty }
      showControls={showControls}
      roleinstance={roleInstance}
      />);
}

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

import React, { createRef } from "react"; // 2
import PerspectivesComponent from "./perspectivesComponent";
import {deconstructLocalName} from "./urifunctions.js";
import SmartFieldControl from "./smartfieldcontrol.js";
import RoleInstance from "./roleinstance.js";

import "././styles/components.css";
import { PropertyType, RoleInstanceT, RoleType, Perspective, PropertyValues, SerialisedProperty, mapRange, InputType } from "perspectives-proxy";
import { WithOutBehavioursProps } from "./adorningComponentWrapper";
import ModelDependencies from "./modelDependencies";
import { t } from "i18next";
import { formatPropertyValue } from "./utilities";

////////////////////////////////////////////////////////////////////////////////
// NAVIGATING IN A GRID AND A CELL
////////////////////////////////////////////////////////////////////////////////
// https://www.w3.org/TR/wai-aria-practices-1.1/#keyboard-interaction-for-data-grids
// https://www.w3.org/TR/wai-aria-practices-1.1/#gridNav_inside

////////////////////////////////////////////////////////////////////////////////
// TABINDEX VALUES
////////////////////////////////////////////////////////////////////////////////
// A negative value means that the element should be focusable,
// but should not be reachable via sequential keyboard navigation;
// Presumably this means clicking on it or setting it from Javascript.
/**
 * A constant representing the focusable state. Element is focusable, but not reachable via sequential keyboard navigation.
 * 
 * @constant {number} focusable
 * @default -1
 */
const focusable = -1;

// 0 means that the element should be focusable and reachable via sequential keyboard navigation,
// but its relative order is defined by the platform convention;
/**
 * Element is focusable and reachable via sequential keyboard navigation, but its relative order is defined by the platform convention.
 * @constant {number} receiveFocusByKeyboard
 * @default 0
 */
const receiveFocusByKeyboard = 0;


////////////////////////////////////////////////////////////////////////////////
// TABLECELL
////////////////////////////////////////////////////////////////////////////////
interface TableCellProps
{
  propertyname: PropertyType;
  myroletype: RoleType;
  isselected: boolean;
  iscard: boolean;
  roleinstance: RoleInstanceT;
  roleRepresentation:  React.ComponentType<WithOutBehavioursProps>;
  serialisedProperty: SerialisedProperty;
  propertyValues?: PropertyValues;
  perspective: Perspective;
  readableName: string;
}

interface TableCellState
{
  editable: boolean;
}

export default class TableCell extends PerspectivesComponent<TableCellProps, TableCellState>
{
  inputRef: React.RefObject<HTMLElement | null>;
  private inputType: InputType;
  private selectionByKeyboard: boolean;
  constructor (props : TableCellProps)
  {
    super(props);
    // Being editable is not determined by props, but entirely by interaction with the cell
    // through the keyboard.
    this.state = { editable: false };
    this.handleClick = this.handleClick.bind(this);
    this.handleKeyDown = this.handleKeyDown.bind(this);
    // A reference to the Form.Control that handles input.
    // It is used to dispatch the custom SetRow and SetColumn events.
    // It also receives focus.
    this.inputRef = createRef();
  this.inputType = mapRange( this.props.serialisedProperty.range )
  this.selectionByKeyboard = false;
  }

  componentDidMount()
  {
    // Only claim focus on initial mount if nothing else currently has focus (avoid stealing from another table/input).
    if (this.props.isselected &&
        typeof document !== 'undefined' &&
        (document.activeElement === document.body || document.activeElement == null))
    {
      this.setFocus();
    }
  }

  // State is determined by being editable.
  // Being selected is determined by the props.
  // When selected, the cell should have focus.
  // When the value changes, we stop editing.
  // When we start editing, we should re-establish the focus.
  componentDidUpdate(prevProps : TableCellProps)
  {
    const component = this;

    // Always move focus to the newly selected cell when selection status changes to true, so arrow navigation continues to work.
    if (component.props.isselected && !prevProps.isselected)
    {
      component.setFocus();
    }

    if (prevProps.isselected && !component.props.isselected && component.state.editable)
    {
      component.setState({editable: false});
    }
  }

  setFocus()
  {
    const component = this;
    const focusInnerControl = (root: HTMLElement | null) => {
      if (!root) return false;
      // If the ref points to an adornment wrapper div, try to locate an actual form control inside.
      let target: HTMLElement | null = root;
      if (root instanceof HTMLDivElement) {
        const inner = root.querySelector('input,textarea,select,.form-control,[contenteditable="true"]') as HTMLElement | null;
        if (inner) target = inner;
      }
      if (target && document.activeElement !== target) {
        // console.log("TableCell.setFocus: focusing element for column " + component.props.propertyname + " roleinstance " + component.props.roleinstance + (target !== root ? " (inner control)" : " (wrapper)"));
        try { target.focus(); } catch (_) { /* ignore */ }
        return true;
      }
      return false;
    };

    if (!focusInnerControl(component.inputRef.current as HTMLElement | null)) {
      // Ref not ready yet: retry shortly.
      setTimeout(() => focusInnerControl(component.inputRef.current as HTMLElement | null), 50);
    }
  }


  // Set the column in the RoleTable_.
  // React will then re-render, giving TableCell the value true for the isselected prop.
  handleClick ()
  {
    this.inputRef.current?.dispatchEvent( new CustomEvent('SetColumn', { detail: this.props.propertyname, bubbles: true }) );
    if (this.props.isselected && !this.state.editable && !this.props.iscard) {
      this.setState({editable: true});
    }
  }

  handleKeyDown(event : React.KeyboardEvent)
  {
    const component = this;
    if (component.props.isselected)
    {
      if (component.state.editable)
      {
        switch( event.code )
        {
          // Stop editing, allow event to bubble.
          // Content has been restored in the SmartFieldControl.
          // We stop editing with value change from the PDR!
          case "Enter":
          case "Escape":
            component.setState({editable: false});
            break;
        }
      }
      else if (!event.shiftKey
                && !component.props.serialisedProperty.isCalculated
                && !component.propertyOnlyConsultable()
              )
      {
        switch(event.code){
          case "Enter":
            component.selectionByKeyboard = true;
            component.setState({editable:true});
            event.preventDefault();
            event.stopPropagation();
            break;
          }
      }
    }
  }

  propertyOnlyConsultable()
  {
    if (this.props.propertyValues)
    {
      const propertyVerbs = this.props.propertyValues.propertyVerbs;
      return propertyVerbs.indexOf("Consult") > -1 && propertyVerbs.length == 1;
    }
    else
    {
      return false;
    }
  }

  values() : string[]
  {
    if (this.props.propertyname == ModelDependencies.roleWithIdProp)
    {
      if (this.props.readableName)
        {
          return [this.props.readableName];
        }
    }
    if (this.props.propertyValues)
    {
      return this.props.propertyValues.values;
    }
    else
    {
      return [];
    }
  }

  render ()
  {
    const component = this;
    const title = component.values().length > 0 ? formatPropertyValue(component.values(), component.inputType) : deconstructLocalName(component.props.propertyname);

    if (component.props.iscard)
    {
      if (component.props.isselected)
      {
        if (component.state.editable)
        {
          return (
            <td 
              onKeyDown={component.handleKeyDown}
              onClick={ component.handleClick }
            >
              <SmartFieldControl
                inputRef={component.inputRef as React.RefObject<HTMLElement>}
                aria-label={title}
                serialisedProperty={component.props.serialisedProperty}
                propertyValues={component.props.propertyValues}
                roleId={component.props.roleinstance}
                myroletype={component.props.myroletype}
                disabled={false}
                isselected={component.props.isselected}
                contextinstance={component.props.perspective.contextInstance}
              />
            </td>);
        }
        else
        {
          return (
            <td onKeyDown={component.handleKeyDown} role="gridcell" aria-selected={true}>
              <RoleInstance
                roleinstance={component.props.roleinstance}
                contextinstance={component.props.perspective.contextInstance}
                contexttype={component.props.perspective.contextType}
                roletype={component.props.perspective.roleType}
                rolekind={component.props.perspective.roleKind}
                myroletype={component.props.myroletype}
                allowedtoremovecontext={component.props.perspective.verbs.includes("RemoveContext") || component.props.perspective.verbs.includes("DeleteContext")}
              >
                {[
                  <component.props.roleRepresentation
                    externalRef={component.inputRef as React.RefObject<HTMLElement>}
                    key={component.props.roleinstance}
                    // The title is the value of the cell. It is picked up automatically.
                    title={title}
                    aria-label={title}
                    // Other properties to pass on.
                    tabIndex={receiveFocusByKeyboard}
                    className={`shadow ${component.props.isselected ? 'card-selected' : ''}`}
                    onClick={component.handleClick}
                    type={component.inputType == 'date' ? 'text' : component.inputType || 'text'}
                  />
                ]}
              </RoleInstance>
            </td>);
        }
      }
      else
      {
        return (
      <td role="gridcell" aria-selected={component.props.isselected}>
              <component.props.roleRepresentation
                externalRef={component.inputRef as React.RefObject<HTMLElement>}
                key={component.props.roleinstance}
                tabIndex={focusable}
                title={title}
        className={`shadow ${component.props.isselected ? 'card-selected' : ''}`}
                onClick={component.handleClick}
                aria-label={title} // deconstructLocalName ( component.props.propertyname )
                type={component.inputType == 'date' ? 'text' : component.inputType || 'text'}
              />
          </td>);
      }
    }
    else
    {
      return (
        <td
          onClick={component.handleClick}
         onKeyDown={component.handleKeyDown}
        >
          <SmartFieldControl
            inputRef={component.inputRef  as React.RefObject<HTMLElement>}
            aria-label={title}
            serialisedProperty={component.props.serialisedProperty}
            propertyValues={component.props.propertyValues}
            roleId={component.props.roleinstance}
            myroletype={component.props.myroletype}
            disabled={!component.state.editable}
            isselected={component.props.isselected}
            contextinstance={component.props.perspective.contextInstance}
          />
        </td>);
    }
  }
}

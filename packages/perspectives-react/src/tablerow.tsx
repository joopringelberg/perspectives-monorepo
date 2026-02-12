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
import TableCell from "./tablecell.js";
import "././styles/components.css";
import { Perspective, Roleinstancewithprops, SerialisedProperty, RoleInstanceT, RoleType } from "perspectives-proxy";
import { WithOutBehavioursProps } from "./adorningComponentWrapper";

////////////////////////////////////////////////////////////////////////////////
// TABLEROW
////////////////////////////////////////////////////////////////////////////////
interface TableRowProps
  { myroletype: RoleType
  , roleinstance: RoleInstanceT
  , column: string
  , isselected: boolean
  , cardcolumn: string
  , roleRepresentation:  React.ComponentType<WithOutBehavioursProps>
  , roleinstancewithprops: Roleinstancewithprops
  , perspective: Perspective
  , orderedProperties: SerialisedProperty[]
  }

export default class TableRow extends PerspectivesComponent<TableRowProps>
{
  private ref : React.RefObject<HTMLTableRowElement>;
  private longPressTimeout: number | null;
  private touchStartPoint?: { x: number; y: number };
  constructor (props : TableRowProps)
  {
    super(props);
    this.handleClick = this.handleClick.bind(this);
    this.handleKeyDown = this.handleKeyDown.bind(this);
    this.handleDoubleClick = this.handleDoubleClick.bind(this);
    this.handleContextMenu = this.handleContextMenu.bind(this);
    this.handleTouchStart = this.handleTouchStart.bind(this);
    this.handleTouchEnd = this.handleTouchEnd.bind(this);
    this.handleTouchCancel = this.handleTouchCancel.bind(this);
    this.ref = createRef() as React.RefObject<HTMLTableRowElement>;
    this.longPressTimeout = null;
  }

  handleClick (event : React.MouseEvent)
  {
    // event.preventDefault();
    event.stopPropagation();
    // Signal to Table that this row is selected.
    this.ref.current?.dispatchEvent( new CustomEvent('SetRow', 
      { detail: 
        { roleInstance: this.props.roleinstance }
      ,  bubbles: true }
      ) );
    // When shift is held, the card column becomes selected.
    if ( event.shiftKey )
    {
      this.ref.current?.dispatchEvent( new CustomEvent('SelectCardColumn', { bubbles: true }) );
    }
  }

  handleDoubleClick (event : React.MouseEvent)
  {
    event.stopPropagation();
    // Signal to Table that this row is selected.
    this.ref.current?.dispatchEvent( new CustomEvent('OpenDetails', 
      { detail: 
        { roleInstance: this.props.roleinstance
        , roleType: this.props.perspective.roleType }
      ,  bubbles: true }) );
  }

  handleKeyDown (event : React.KeyboardEvent)
  {
    switch(event.code){
      case "Space": // Space
        if (event.shiftKey)
        {
          event.preventDefault();
          event.stopPropagation();
          // Signal to Table that this row is selected
          this.ref.current.dispatchEvent( new CustomEvent('SetRow', 
            { detail: 
              { roleInstance: this.props.roleinstance }
            ,  bubbles: true }) );
          this.ref.current?.dispatchEvent( new CustomEvent('SelectCardColumn', { bubbles: true } ) );
          }
        break;
    }
  }

  handleContextMenu(event: React.MouseEvent)
  {
    event.preventDefault();
    event.stopPropagation();
    const x = event.clientX;
    const y = event.clientY;
    this.ref.current?.dispatchEvent(new CustomEvent('RowContextMenu', {
      detail: { roleInstance: this.props.roleinstance, x, y },
      bubbles: true
    }));
  }

  handleTouchStart(event: React.TouchEvent)
  {
    if (event.touches.length === 1) {
      const touch = event.touches[0];
      this.touchStartPoint = { x: touch.clientX, y: touch.clientY };
      this.longPressTimeout = window.setTimeout(() => {
        this.ref.current?.dispatchEvent(new CustomEvent('RowContextMenu', {
          detail: { roleInstance: this.props.roleinstance, x: touch.clientX, y: touch.clientY },
          bubbles: true
        }));
      }, 600) as unknown as number;
    }
  }

  handleTouchEnd(event: React.TouchEvent)
  {
    if (this.longPressTimeout !== null) {
      window.clearTimeout(this.longPressTimeout);
      this.longPressTimeout = null;
    }
  }

  handleTouchCancel(event: React.TouchEvent)
  {
    if (this.longPressTimeout !== null) {
      window.clearTimeout(this.longPressTimeout);
      this.longPressTimeout = null;
    }
  }

  render()
  {
    const component = this;
    const roleInstanceWithProps = component.props.roleinstancewithprops;
    return  <tr
              onClick={component.handleClick}
              onDoubleClick={component.handleDoubleClick}
              onKeyDown={component.handleKeyDown}
              onContextMenu={component.handleContextMenu}
              onTouchStart={component.handleTouchStart}
              onTouchEnd={component.handleTouchEnd}
              onTouchCancel={component.handleTouchCancel}
              ref={component.ref}
              key={ roleInstanceWithProps.roleId }
            >{
              component.props.orderedProperties.map( serialisedProperty =>
                <TableCell
                  key={serialisedProperty.id}
                  propertyname = {serialisedProperty.id}
                  serialisedProperty={serialisedProperty}
                  roleinstance={component.props.roleinstance}
                  propertyValues={ roleInstanceWithProps.propertyValues[serialisedProperty.id] }
                  iscard = {serialisedProperty.id == component.props.cardcolumn}
                  myroletype = {component.props.myroletype}
                  isselected = { component.props.isselected && (component.props.column == serialisedProperty.id) }
                  roleRepresentation={component.props.roleRepresentation}
                  perspective={component.props.perspective}
                  readableName={roleInstanceWithProps.readableName}
                /> )
            }</tr>;
  }
}

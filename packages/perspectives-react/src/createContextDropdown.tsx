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

import React, { Component, forwardRef } from "react";
import {Dropdown} from 'react-bootstrap';
import {PlusIcon} from '@primer/octicons-react';
import { func, objectOf, string } from "prop-types";
import i18next from "i18next";
import { ContextType } from "perspectives-proxy";

interface CreateContextDropDownProps {
  create: (eventKey: ContextType ) => void;
  contexts: { [key: string]: ContextType };
}

interface CreateContextDropDownState {
  actions: string[];
}

export default class CreateContextDropDown extends Component<CreateContextDropDownProps, CreateContextDropDownState>
{
  constructor( props : CreateContextDropDownProps )
  {
    super(props);
    this.state = {actions: []};
  }

  render()
  {
    const component = this;
    const items = Object.keys(component.props.contexts).map(
      function(contextName)
      {
        return    <Dropdown.Item
                    key={contextName}
                    eventKey={contextName}
                  >{
                    component.props.contexts[contextName]
                  }</Dropdown.Item>;
      });
    items.unshift(    <Dropdown.Item
                        key="JustTheRole"
                        eventKey="JustTheRole"
                      >{
                        i18next.t("contextDropdown_title", { ns: 'preact' }) 
                      }</Dropdown.Item> );
      
    if (Object.keys( component.props.contexts ).length == 0)
    {
      return  <div
                className="btn btn-link p-0 me-2"
                tabIndex={0}
                // Note that the parameter passed to create is ignored. In this case, this component was handed a create function that only creates a role.
                onClick={ e => {
                  e.stopPropagation();
                  component.props.create("JustTheRole" as ContextType)
                } }
              >
                <i 
                  className="bi bi-plus-circle" 
                  style={{ 
                    fontSize: '1.1rem',
                    color: 'var(--bs-primary)'
                  }}
                ></i>
              </div>
    }
    else
    {
      return  <Dropdown
                id="dropdown-createContext"
                title="Contexts to create"
                focusFirstItemOnShow={false}
                onClick={ e => {
                  // Keep a click on an item within the Dropdown.
                  // The action is handled by the onSelect handler of the Dropdown.
                  e.stopPropagation();
                }}
                onSelect={ contextType => {
                  component.props.create( contextType as ContextType)
                  } }>
                <Dropdown.Toggle as={CustomToggle} id="CreateContext_Toggle" disabled={Object.keys(component.props.contexts).length == 0}>
                  <i 
                    className="bi bi-plus-circle" 
                    style={{ 
                      fontSize: '1.1rem',
                      color: 'var(--bs-primary)'
                    }}
                  ></i>
                </Dropdown.Toggle>
                  <Dropdown.Menu>{ items }</Dropdown.Menu>
              </Dropdown>;
    }
  }
}

CreateContextDropDown.propTypes =
  { create: func
  , contexts: objectOf(string)
  };

// eslint-disable-next-line react/display-name, react/prop-types
interface CustomToggleProps {
  children: React.ReactNode;
  onClick: (e: React.MouseEvent<HTMLAnchorElement, MouseEvent>) => void;
  disabled: boolean;
}

const CustomToggle = forwardRef<HTMLAnchorElement, CustomToggleProps>(({ children, onClick, disabled }, ref) => (
  <a
    href=""
    ref={ref}
    className={disabled ? "disabledIcon" : "iconStyle"}
    onClick={(e) => {
      e.preventDefault();
      e.stopPropagation()
      if (!disabled)
      {
        onClick(e);
      }
    }}
  >
    {children}
    &#x25bc;
  </a>
));

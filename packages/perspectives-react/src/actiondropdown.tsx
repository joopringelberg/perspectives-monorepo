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
import {ZapIcon} from '@primer/octicons-react';

interface ActionDropDownProps {
  runaction: (eventKey: string | null) => void;
  actions: { [key: string]: string };
}

export default class ActionDropDown extends Component<ActionDropDownProps>
{
  constructor(props: ActionDropDownProps)
  {
    super(props);
    this.state = {actions: []};
  }

  render()
  {
    const component = this;
    return  <Dropdown
              id="dropdown-action"
              title="Actions"
              focusFirstItemOnShow={false}
              onSelect={ component.props.runaction }>
              <Dropdown.Toggle as={CustomToggle} id="Actions_Toggle" disabled={Object.keys(component.props.actions).length === 0}>
                <ZapIcon aria-label="Actions to execute" size="medium"/>
              </Dropdown.Toggle>
                <Dropdown.Menu>
                  {
                    Object.keys(component.props.actions).map(
                      function(actionName)
                      {
                        return    <Dropdown.Item
                                    key={actionName}
                                    eventKey={actionName}
                                  >{
                                    component.props.actions[actionName]
                                  }</Dropdown.Item>;
                      }
                    )
                  }
                </Dropdown.Menu>
            </Dropdown>;
  }
}

// eslint-disable-next-line react/display-name
interface CustomToggleProps {
  children: React.ReactNode;
  onClick: (e: React.MouseEvent<HTMLAnchorElement, MouseEvent>) => void;
  disabled: boolean;
}

const CustomToggle = forwardRef<HTMLAnchorElement, CustomToggleProps>(({ children, onClick, disabled }, ref) => (
  <a
    href=""
    ref={ref}
    className={disabled ? "text-muted" : "text-secondary"}
    onClick={(e) => {
      e.preventDefault();
      if (!disabled) {
        onClick(e);
      }
    }}
  >
    {children}
    &#x25bc;
  </a>
));

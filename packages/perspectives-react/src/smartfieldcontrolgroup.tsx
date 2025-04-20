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

// This Component is built upon the data sent from the PDR for a single property.
//
// type RoleInstanceWithProperties =
// { roleId :: String
// , objectStateBasedRoleVerbs :: Array String
// , objectStateBasedSerialisedProperties :: Object SerialisedProperty
//      Notice that the keys of this object are property types!
// , propertyValues :: Object ValuesWithVerbs
// , actions :: Array String
// }
//
// The verbs in this type contain both those based on context- and subject state,
// and those based on object state.
//
// type ValuesWithVerbs =
//   { values :: Array String
//   , propertyVerbs :: Array String
//   }
//
// type SerialisedProperty =
//   { id :: String
//   , displayName :: String
//   , isFunctional :: Boolean
//   , isMandatory :: Boolean
//   , isCalculated :: Boolean
//   , range :: String
//   , verbs :: Array String
//   }

import React, { PureComponent } from "react";
const Component = PureComponent;
import
  { Row
  , Col
  , Form
  } from "react-bootstrap";
import { bool, string, shape, arrayOf } from "prop-types";
import SmartFieldControl from "./smartfieldcontrol.js";
import { ContextInstanceT, RoleInstanceT, RoleType, PropertyValues, SerialisedProperty } from "perspectives-proxy";

interface SmartFieldControlGroupProps
{
  serialisedProperty: SerialisedProperty;
  propertyValues?: PropertyValues;
  roleId?: RoleInstanceT;
  myroletype: RoleType;
  contextinstance: ContextInstanceT;
  hasFocus?: boolean;
}

interface SmartFieldControlGroupState
{
  containerWidth: number;
}

export default class SmartFieldControlGroup extends Component<SmartFieldControlGroupProps, SmartFieldControlGroupState>
{
  private containerRef = React.createRef<HTMLDivElement>();
  state = { containerWidth: 0 };
  private resizeObserver: ResizeObserver | null = null;

  constructor(props : SmartFieldControlGroupProps)
  {
    super(props);
  }

  componentDidMount(): void {
    if (this.containerRef.current) {
      this.resizeObserver = new ResizeObserver(entries => {
        if (entries[0]) {
          this.setState({
            containerWidth: entries[0].contentRect.width
          });
        }
      });
      this.resizeObserver.observe(this.containerRef.current);
    }
  }

  componentWillUnmount(): void {
    if (this.resizeObserver ) {
      this.resizeObserver.disconnect();
    }
  }

  // The property is only consultable when it just has the verb Consult,
  // or when it is calculated. It will be shown disabled as a consequence.
  propertyOnlyConsultable()
  {
    if (this.props.propertyValues)
    {
      const propertyVerbs = this.props.propertyValues.propertyVerbs;
      const property = this.props.serialisedProperty;
      return (propertyVerbs.indexOf("Consult") > -1 
        && propertyVerbs.length == 1)
        || property.isCalculated;
    }
    else
    {
      return false;
    }
  }

  render()
  {
    const component = this;
    const { containerWidth } = this.state;
    const isHorizontal = containerWidth > 400;
    return (
      <div ref={this.containerRef}>
        <Form.Group as={ isHorizontal ? Row : 'div'} className="mb-2">
          <Form.Label
            column={isHorizontal}
            className={isHorizontal ? "col-4" : ""}
            >
            { component.props.serialisedProperty.displayName }
          </Form.Label>
          <div className={isHorizontal ? "col-8" : ""}>
            <SmartFieldControl
              serialisedProperty = { component.props.serialisedProperty }
              propertyValues = { component.props.propertyValues }
              roleId = { component.props.roleId }
              myroletype = { component.props.myroletype }
              disabled={ component.propertyOnlyConsultable() || !component.props.roleId }
              isselected={!!component.props.hasFocus}
              contextinstance={component.props.contextinstance}
            />
          </div>
        </Form.Group>
      </div>);
    }
}


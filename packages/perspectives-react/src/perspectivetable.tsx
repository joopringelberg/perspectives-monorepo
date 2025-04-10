import React, { createRef } from "react";
import PerspectivesComponent from "./perspectivesComponent";
import {mapRoleVerbsToBehaviourNames} from "./maproleverbstobehaviours.js";
import TableRow from "./tablerow.js";
import TableControls from "./tablecontrols.js";
import i18next from "i18next";

import
  { Table
  , Form,
  Accordion
  } from "react-bootstrap";
import "./components.css";
import { CardProperties } from "./cardbehaviour";
import { CardWithFixedBehaviour, WithOutBehavioursProps } from "./adorningComponentWrapper";
import { RoleInstanceT, Perspective, SerialisedProperty, PDRproxy, ContextType } from "perspectives-proxy";
import { AccordionHeaderWithAddButton } from "./accordionHeaderWithAddButton";

////////////////////////////////////////////////////////////////////////////////
// CARD
////////////////////////////////////////////////////////////////////////////////
// The default component to display in the card column. Shows a plain text control.
// The behaviours granted to the table will be established on this Card component.
// Displays the value of prop cardcolumn of RoleTable (a property of the role).


const RoleCard: React.FC<CardProperties> = ({title, tabIndex, onClick, className, ...rest}) => {

  return  <Form.Control
            readOnly
            plaintext
            className={`ps-2 ${className || ''}`}
            value={title}
            {...rest}
            />;
}

////////////////////////////////////////////////////////////////////////////////
// PERSPECTIVESTABLE
////////////////////////////////////////////////////////////////////////////////
// Context type of PerspectiveTable is PSContext


/**
 * Props for the PerspectiveTable component.
 *
 * @interface PerspectiveTableProps
 * @property {Perspective} perspective - The perspective data to be displayed in the table.
 * @property {string} cardcolumn? - The name of the column that contains card data. If omitted, the identifying property of the perspective is used.
 * @property {boolean}   , showcontrolsandcaption?: boolean? - Whether to show the table controls and caption. Default is true.
 * @property {boolean}   , showAsAccordion?: boolean? - Whether to show the table as an accordion. In that case, no controls, nor caption. Default is false.
 */
interface PerspectiveTableProps
  { perspective: Perspective
  , cardcolumn?: string
  , showcontrolsandcaption?: boolean
  , showAsAccordionItem?: boolean
  }

interface PerspectiveTableState
  { column: string
  , row: RoleInstanceT
  }

export default class PerspectiveTable extends PerspectivesComponent<PerspectiveTableProps, PerspectiveTableState>
{
  private roleRepresentation : React.ComponentType<WithOutBehavioursProps>;
  private eventDiv: React.RefObject<HTMLTableSectionElement>;
  private propertyNames: string[];
  private orderedProperties: SerialisedProperty[];

  constructor (props : PerspectiveTableProps)
  {
    super(props);
    const component = this;
    this.orderedProperties = [];
    this.propertyNames = [];

    component.orderProperties();
    // this.propertyNames = Object.keys( component.props.perspective.properties );
    // Add behaviours to the card component.
    this.roleRepresentation = CardWithFixedBehaviour(RoleCard, mapRoleVerbsToBehaviourNames( component.props.perspective ));
    this.eventDiv = createRef() as React.RefObject<HTMLTableSectionElement>;
    this.handleKeyDown = this.handleKeyDown.bind( this );

    component.state =
      { column: this.propertyNames[0]
      , row: Object.keys( component.props.perspective.roleInstances )[0] as RoleInstanceT
      };
  }

  orderProperties()
  {
    const perspective = this.props.perspective;
    const identifyingProperty = perspective.properties[perspective.identifyingProperty];
    this.orderedProperties = Object.values(perspective.properties);
    this.orderedProperties.splice( this.orderedProperties.indexOf( identifyingProperty), 1);
    if (identifyingProperty)
    {
      this.orderedProperties.unshift(identifyingProperty);
    }
    this.propertyNames = this.orderedProperties.map( p => p.id);
  }

  componentDidMount ()
  {
    const component = this;
    if (component.eventDiv.current)
    {
      component.eventDiv.current.addEventListener('SetRow',
        function (e: Event)
        {
          const customEvent = e as CustomEvent;
          component.setrow(customEvent.detail.roleInstance);
          // e.stopPropagation();
        },
        false);
      component.eventDiv.current.addEventListener('SetColumn',
        function (e)
        {
          const customEvent = e as CustomEvent;
          if (customEvent.detail !== component.state.column)
          {
            component.setState({column: customEvent.detail});
          }
          e.stopPropagation();
        },
        false);
      component.eventDiv.current.addEventListener('SetSelectRow',
        function (e)
        {
          // By definition of row selection, the card column now becomes the current column.
          component.setState( { column: component.props.cardcolumn || component.props.perspective.identifyingProperty } );
          e.stopPropagation();
        },
        false);
      }
  }

  componentDidUpdate(prevProps : PerspectiveTableProps)
  {
    // True iff the arrays are not equal.
    function unequalArrays (arr1 : string[], arr2 : string[])
    {
      let found = false;
      let i = -1;
      let j;
      // Is there an element in arr2 that is not in arr1?
      while (!found && i < arr2.length - 1)
      {
        i++;
        j = arr1.findIndex( n => n == arr2[i] );
        if ( j > -1 )
        {
          arr1.splice(j, 1);
        }
        else
        {
          found = true;
        }
      }
      return found || arr1.length > 0;
    }
    // If the selected row has been deleted, set `row` to the first row.
    if (this.state.row && !this.props.perspective.roleInstances[this.state.row])
    {
      this.setState({row: Object.keys( this.props.perspective.roleInstances )[0] as RoleInstanceT});
    }
    // If we have a different set of properties, recompute the ordered properties.
    if ( unequalArrays( Object.keys( prevProps.perspective.properties ), Object.keys( this.props.perspective.properties ) ) )
    {
      this.orderProperties();
    }
  }

  setrow (cr : RoleInstanceT)
  {
    if (cr !== this.state.row && cr)
    {
      this.setState( {row: cr} );
    }
  }

  handleKeyDown (event : React.KeyboardEvent)
  {
    const component = this;
    const columnIndex = component.propertyNames.indexOf( component.state.column );
    const roleIds = Object.keys( component.props.perspective.roleInstances ) as RoleInstanceT[];
    const rowIndex = roleIds.indexOf( component.state.row );

    switch(event.code){
      case "ArrowDown": // Down arrow
        if ( rowIndex < roleIds.length - 1 )
        {
          component.setrow( roleIds[rowIndex + 1] );
        }
        event.preventDefault();
        break;
      case "ArrowUp": // Up arrow
        if (rowIndex > 0)
        {
          component.setrow( roleIds[rowIndex - 1] );
        }
        event.preventDefault();
        break;
      case "ArrowRight": // Right arrow
        if (columnIndex < component.propertyNames.length - 1)
        {
          event.preventDefault();
          component.setState({column: component.propertyNames[columnIndex + 1]});
        }
        event.stopPropagation();
        break;
      case "ArrowLeft": // Left arrow
        if (columnIndex > 0)
        {
          event.preventDefault();
          component.setState({column: component.propertyNames[columnIndex - 1]});
        }
        event.stopPropagation();
        break;
      }
  }

  constructTable ()
  {
    const component = this,
      perspective = component.props.perspective;
    return <Table
            key={perspective.id}
            responsive
            striped
            bordered
            hover
            size="sm"
            className="mb-0">
            {component.props.showcontrolsandcaption !== false ? <caption>{ i18next.t("table_subscriptionLeader", {ns: 'preact'}) }{ perspective.displayName }</caption> : null}
            <thead>
              <tr>
              { component.propertyNames.map( pn =>
                <th key={pn}>
                  { (perspective.properties[pn]) ? perspective.properties[pn].displayName : null }
                </th>) }
              </tr>
            </thead>
            <tbody
              ref={component.eventDiv}
              onKeyDown={ component.handleKeyDown }
            >
              {
                Object.keys(perspective.roleInstances).map( (roleId) =>
                  <TableRow
                    key={roleId}
                    roleinstance={roleId as RoleInstanceT}
                    isselected={ roleId == component.state.row }
                    myroletype={component.props.perspective.userRoleType}
                    column={component.state.column}
                    cardcolumn = {perspective.identifyingProperty}
                    roleRepresentation={component.roleRepresentation}
                    roleinstancewithprops={perspective.roleInstances[roleId]}
                    perspective={component.props.perspective}
                    orderedProperties={component.orderedProperties}
                    />)
              }
            </tbody>
          </Table>
  }

  render()
  {
    const component = this,
      perspective = component.props.perspective;
    if (component.stateIsComplete(["row"])){    
      return (
          component.props.showAsAccordionItem ?
            <Accordion.Item eventKey={perspective.id} key={perspective.id}>
              <Accordion.Header>
                <AccordionHeaderWithAddButton perspective={perspective}/>
              </Accordion.Header>
              <Accordion.Body>{
              component.constructTable()}
              </Accordion.Body>
            </Accordion.Item>
            :
            (<>
              {component.constructTable()}
              { component.props.showcontrolsandcaption !== false ? 
                <TableControls
                  perspective={ perspective}
                  selectedroleinstance={component.state.row}
                /> 
                : 
                null}
            </>))}
        else {
          return null;
        }
  }
}


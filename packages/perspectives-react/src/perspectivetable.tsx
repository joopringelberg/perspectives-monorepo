import React, { createRef, useContext, useEffect, useRef } from "react";
import PerspectivesComponent from "./perspectivesComponent";
import {mapRoleVerbsToBehaviourNames} from "./maproleverbstobehaviours.js";
import TableRow from "./tablerow.js";
import TableControls from "./tablecontrols.js";
import i18next from "i18next";

import
  { Table
  , Form,
  Accordion,
  AccordionContext
  } from "react-bootstrap";
import "././styles/components.css";
import { CardProperties } from "./cardbehaviour";
import { CardWithFixedBehaviour, WithOutBehavioursProps } from "./adorningComponentWrapper";
import { RoleInstanceT, Perspective, SerialisedProperty, PropertyType, Roleinstancewithprops } from "perspectives-proxy";
import { AppContext } from "./reactcontexts";
import TableItemContextMenu from "./tableItemContextMenu";

// Wrapper functional component to derive `isOpen` from Accordion context and pass to the menu.
const TableItemContextMenuWithOpen: React.FC<{
  eventKey: string;
  perspective: Perspective;
  roleinstance: RoleInstanceT;
  roleOnClipboard?: any;
  systemExternalRole: RoleInstanceT;
  showDetails?: boolean;
  mode?: "all" | "addOnly" | "rowOnly";
}> = ({ eventKey, ...rest }) => {
  const ctx = useContext(AccordionContext);
  const active = ctx?.activeEventKey;
  const isOpen = Array.isArray(active) ? active.includes(eventKey) : active === eventKey;
  return <TableItemContextMenu {...rest} isOpen={isOpen} />;
};

// Floating row context menu, shown at a screen position (right-click / long-press).
const RowContextMenu: React.FC<{
  visible?: boolean;
  position?: { x: number; y: number };
  perspective: Perspective;
  roleId?: RoleInstanceT;
  showDetails?: boolean;
  onRequestClose?: () => void;
}> = ({ visible, position, perspective, roleId, showDetails, onRequestClose }) => {
  const { roleOnClipboard, systemExternalRole } = useContext(AppContext);

  const menuRef = useRef<HTMLDivElement | null>(null);
  const isActive = !!(visible && position && roleId);

  const style: React.CSSProperties = isActive && position
    ? {
        position: "fixed",
        top: position.y,
        left: position.x,
        zIndex: 2000
      }
    : {
        position: "fixed",
        top: 0,
        left: 0,
        zIndex: 2000,
        display: "none"
      };

  useEffect(() => {
    if (!visible) {
      return;
    }

    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.key === "Escape") {
        if (onRequestClose) onRequestClose();
      }
    };

    const handleMouseDown = (event: MouseEvent) => {
      if (!menuRef.current) {
        return;
      }
      if (!menuRef.current.contains(event.target as Node)) {
        if (onRequestClose) onRequestClose();
      }
    };

    document.addEventListener("keydown", handleKeyDown);
    document.addEventListener("mousedown", handleMouseDown);

    return () => {
      document.removeEventListener("keydown", handleKeyDown);
      document.removeEventListener("mousedown", handleMouseDown);
    };
  }, [visible, onRequestClose]);

  if (!isActive) {
    return null;
  }

  return (
    <div
      ref={menuRef}
      className="dropdown-menu show"
      style={style}
    >
      <TableItemContextMenu
        perspective={perspective}
        roleinstance={roleId}
        roleOnClipboard={roleOnClipboard}
        systemExternalRole={systemExternalRole}
        showDetails={showDetails}
        isOpen={true}
        mode="rowOnly"
        renderAsInlineMenu={true}
      />
    </div>
  );
};

////////////////////////////////////////////////////////////////////////////////
// CARD
////////////////////////////////////////////////////////////////////////////////
// The default component to display in the card column. Shows a plain text control.
// The behaviours granted to the table will be established on this Card component.
// Displays the value of prop cardcolumn of RoleTable (a property of the role).


const RoleCard: React.FC<CardProperties> = (props) => {

  return  <Form.Control
            readOnly
            value={props.title}
            placeholder={props["aria-label"]}
            
            className={`ps-2 ${props.className || ''}`}
            aria-label={props["aria-label"] || props.title}
            type={props.type || 'text'}
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
  , showDetails? : boolean
  , sortOnHiddenProperty?: PropertyType
  , sortAscending?: boolean
  }

interface PerspectiveTableState
  { column: string
  , row: RoleInstanceT
  , contextMenuRole?: RoleInstanceT
  , contextMenuPosition?: { x: number; y: number }
  , contextMenuVisible?: boolean
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
      , contextMenuRole: undefined
      , contextMenuPosition: undefined
      , contextMenuVisible: false
      };
  }

  orderProperties()
  {
    const perspective = this.props.perspective;
    // Here we put the identifying property in the front of the list, so it will be the first column.
    const identifyingProperty = perspective.properties[perspective.identifyingProperty];
    this.orderedProperties = Object.values(perspective.properties);
    this.orderedProperties.splice( this.orderedProperties.indexOf( identifyingProperty), 1);
    if (identifyingProperty)
    {
      this.orderedProperties.unshift(identifyingProperty);
    }
    this.propertyNames = this.orderedProperties.map( p => p.id);
    // Finally, remove an eventual sort property from the list of properties.
    if (this.props.sortOnHiddenProperty && this.propertyNames.indexOf(this.props.sortOnHiddenProperty) > -1)
    {
      this.propertyNames.splice(this.propertyNames.indexOf(this.props.sortOnHiddenProperty), 1);
    }
    // Also remove an eventual sort property from the ordered properties.
    if (this.props.sortOnHiddenProperty && this.orderedProperties.map( p => p.id).indexOf(this.props.sortOnHiddenProperty) > -1)
    {
      this.orderedProperties.splice(this.orderedProperties.map( p => p.id).indexOf(this.props.sortOnHiddenProperty), 1);
    }
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
          e.stopPropagation();
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
      component.eventDiv.current.addEventListener('SelectCardColumn',
        function (e)
        {
          // By definition of row selection, the card column now becomes the current column.
          component.setState( { column: component.props.cardcolumn || component.props.perspective.identifyingProperty } );
          e.stopPropagation();
        },
        false);
      component.eventDiv.current.addEventListener('RowContextMenu',
        function (e: Event)
        {
          const customEvent = e as CustomEvent;
          const detail = customEvent.detail as { roleInstance: RoleInstanceT; x: number; y: number };
          component.setState({
            contextMenuRole: detail.roleInstance,
            contextMenuPosition: { x: detail.x, y: detail.y },
            contextMenuVisible: true
          });
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

    // When focus is inside a dropdown (e.g. the fillers dropdown in a card
    // cell), let react-bootstrap handle arrow keys and ESC, and do not
    // perform table-level row/column navigation.
    const target = event.target as HTMLElement | null;
    if (target && target.closest('.dropdown'))
    {
      return;
    }

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
    const LESS = -1;
    const GREATER = 1;
    const EQUAL = 0;
    function orderRoleInstances( r1: Roleinstancewithprops, r2: Roleinstancewithprops ) : number
    {
      if (component.props.sortOnHiddenProperty !== undefined)
      {
        const value1 = r1.propertyValues[component.props.sortOnHiddenProperty]?.values[0];
        const value2 = r2.propertyValues[component.props.sortOnHiddenProperty]?.values[0];
        if (parseInt( value1 ) < parseInt( value2 ))
        {
          return component.props.sortAscending ? LESS : GREATER;
        }
        else if (parseInt( value1 ) > parseInt( value2 ))
        {
          return component.props.sortAscending ? GREATER : LESS;
        }
        else
        {
          return EQUAL;
        }
      }
      return EQUAL;
    }
    const component = this,
      perspective = component.props.perspective;
    return <Table
            key={perspective.id}
            responsive={!component.props.showAsAccordionItem}
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
                Object.values(perspective.roleInstances).sort(orderRoleInstances).map( ({roleId}) =>
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
    if (component.stateIsComplete(["row", "contextMenuRole", "contextMenuPosition", "contextMenuVisible"])) {    
      const canAdd = !perspective.isCalculated &&
        (perspective.verbs.includes("Create") ||
         (perspective.roleKind == "ContextRole" && perspective.verbs.includes("CreateAndFill")));

      return (
        component.props.showAsAccordionItem ?
          <Accordion.Item eventKey={perspective.roleType} key={perspective.id}>
            <Accordion.Header
              className="py-1"
              onClick={() => component.eventDiv.current?.dispatchEvent(
                new CustomEvent('OpenAccordionItem', { detail: perspective.roleType, bubbles: true })
              )}
            >
              <div className="d-flex align-items-center w-100" style={{ minWidth: 0 }}>
                <span
                  className="flex-grow-1 text-truncate"
                  style={{ minWidth: 0 }}
                  title={perspective.displayName}
                >
                  {perspective.displayName}
                </span>
                {canAdd && (
                  <div
                    className="d-flex align-items-center ms-2 accordion-actions" /* reuse class for potential styling */
                    onClick={(e) => e.stopPropagation()}
                    onMouseDown={(e) => e.stopPropagation()}
                  >
                    <AppContext.Consumer>
                      {({ roleOnClipboard, systemExternalRole }) =>
                        <TableItemContextMenuWithOpen
                          eventKey={perspective.roleType}
                          perspective={perspective}
                          roleinstance={component.state.row}
                          roleOnClipboard={roleOnClipboard}
                          systemExternalRole={systemExternalRole}
                          showDetails={component.props.showDetails}
                          mode="addOnly"
                        />
                      }
                    </AppContext.Consumer>
                  </div>
                )}
              </div>
            </Accordion.Header>

            <Accordion.Body>
              {component.constructTable()}
            </Accordion.Body>
            <RowContextMenu
              visible={component.state.contextMenuVisible}
              position={component.state.contextMenuPosition}
              perspective={perspective}
              roleId={component.state.contextMenuRole}
              showDetails={component.props.showDetails}
                onRequestClose={() => {
                  component.setState({
                    contextMenuVisible: false,
                    contextMenuRole: undefined,
                    contextMenuPosition: undefined
                  });
                }}
            />
          </Accordion.Item>
          :
          // Rest of your existing code for non-accordion view
          (<>
            {component.constructTable()}
            {component.props.showcontrolsandcaption !== false ? 
              <TableControls
                perspective={perspective}
                selectedroleinstance={component.state.row}
              /> 
              : 
              null}
            <RowContextMenu
              visible={component.state.contextMenuVisible}
              position={component.state.contextMenuPosition}
              perspective={perspective}
              roleId={component.state.contextMenuRole}
              showDetails={component.props.showDetails}
              onRequestClose={() => {
                component.setState({
                  contextMenuVisible: false,
                  contextMenuRole: undefined,
                  contextMenuPosition: undefined
                });
              }}
            />
          </>))
      } else {
        return null;
      }
  }
}


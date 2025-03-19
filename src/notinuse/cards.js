import React, { Component, createRef, forwardRef } from "react";
import { string, func } from "prop-types";

import PerspectivesComponent from "./perspectivescomponent.js";

import View from "./view.js";

import {PSView, PSRol} from "./reactcontexts.js";

import {Card} from "react-bootstrap";

///////////////////////////////////////////////////////////////////////////////
//// MAKEROLEINLISTREPRESENTATION
///////////////////////////////////////////////////////////////////////////////
/*
From some content (e.g. a Card, the `inner component`), construct a component X that expects a PSRol context.
Provide that context with a RoleInstanceIterator.
The inner component is passed a ref. If it is a function component, define it with React forwardRef
(see example below; for more information, see https://reactjs.org/docs/forwarding-refs.html).
X is constructed so that one can add behaviour to it with addBehaviour.
X represents a role instance, ie. one of the items in the role list. It
  * becomes the selected item in the list (the `cursor`) when the user clicks on it;
  * becomes the selected item in the list when the user navigates to it using up and down arrow keys;
  * shows focus when it becomes the selected item
  * show focus when it is the cursor and the entire list is tabbed into.
The inner component receives these props:
  * aria-label
  * tabIndex
  * propval: this is a function that can be used in the inner component to select property values from the role instance.
The following props should be provided to X:
  * labelProperty (required: it is used for aria-label);
  * viewname (optional). The view should provide all properties used in the inner component). "allProperties" by default;
  * setSelf (required), but this is set automatically when one adds behaviour to the result component
    using addBehaviour.
Note that even though RoleInstance provides PSRol as wel, the components constructed with
`makeRoleInListPresentation` do not tie in well with RoleInstance as it provides no background for
cursor movement like RoleInstances does.
*/
export function makeRoleInListPresentation (RoleRep)
{
  class RoleInListPresentation extends PerspectivesComponent
  {
    constructor(props)
    {
      super(props);
      this.ref = createRef();
    }

    componentDidMount()
    {
      if (this.props.setSelf)
      {
        this.props.setSelf(this);
      }
    }

    componentDidUpdate ()
    {
      const component = this;
      if (component.context.isselected)
      {
        this.ref.current?.focus();
      }
    }
    render()
    {
      const component = this;
      return <View viewname={component.props.viewname ? component.props.viewname : "allProperties"}>
                 <PSView.Consumer>
                   {psview =>
                     <div onClick={() => component.ref.current?.dispatchEvent(
                         new CustomEvent('SetCursor', { detail: component.context.rolinstance, bubbles: true }) )}>
                       <RoleRep
                        ref={component.ref}
                        aria-label={psview.propval(component.props.labelProperty)}
                        tabIndex={component.context.isselected ? "0" : "-1"}
                        propval={psview.propval}
                        >
                       </RoleRep>
                    </div>}
                 </PSView.Consumer>
               </View>;
    }
  }
  RoleInListPresentation.contextType = PSRol;

  RoleInListPresentation.propTypes =
    { viewname: string
    , labelProperty: string.isRequired
    , setSelf: func.isRequired
  };
  return RoleInListPresentation;
}

///////////////////////////////////////////////////////////////////////////////
//// EXAMPLE: NAMEDESCRIPTIONCARD
///////////////////////////////////////////////////////////////////////////////

// A card that displays the Name and Description properties. It can be selected, by
// tabbing into its surrounding list or table and then using the up and down arrow.
// This card must be used in a context that provides PSRol.
export const NameDescriptionCard = makeRoleInListPresentation(
  // eslint-disable-next-line react/display-name
  forwardRef( function(props, ref)
  {
    // eslint-disable-next-line react/prop-types
    return  <Card ref={ref} tabIndex={props.tabIndex} aria-label={props["aria-label"]}>
              <Card.Body>
                <div>
                  <p>{
                    // eslint-disable-next-line react/prop-types
                    props.propval("Name")}</p>
                  <p>{
                    // eslint-disable-next-line react/prop-types
                    props.propval("Description")}</p>
                </div>
              </Card.Body>
            </Card>;
  }));

  ///////////////////////////////////////////////////////////////////////////////
  //// MAKESINGLEROLEREPRESENTATION
  ///////////////////////////////////////////////////////////////////////////////
  /*
  From some content (e.g. a Card, the `inner component`), construct a component X that expects a PSRol context.
  Provide that context with RoleInstance.
  X is constructed so that one can add behaviour to it with addBehaviour.
  X represents a single role instance. If you need a representation of items in a list, use makeRoleInListPresentation. X
    * can be selected with tab and by a click on it
  The inner component receives these props:
    * aria-label
    * tabIndex
    * propval: this is a function that can be used in the inner component to select property values from the role instance.
  The following props should be provided to X:
    * labelProperty (required: it is used for aria-label);
    * viewname (optional). The view should provide all properties used in the inner component). "allProperties" by default;
    * setSelf (required), but this is set automatically when one adds behaviour to the result component
      using addBehaviour.
  */
  export function makeSingleRolePresentation (RoleRep)
  {
    class SingleRolePresentation extends Component
    {
      componentDidMount()
      {
        if (this.props.setSelf)
        {
          this.props.setSelf(this);
        }
      }
      render()
      {
        const component = this;
        const labelProperty = component.props.labelProperty;
        return <View viewname={component.props.viewname ? component.props.viewname : "allProperties"}>
                   <PSView.Consumer>
                     {psview =>
                       <RoleRep
                        aria-label={ labelProperty ? psview.propval(labelProperty) : null}
                        tabIndex="0"
                        propval={psview.propval}
                        >
                       </RoleRep>}
                   </PSView.Consumer>
                 </View>;
      }
    }
    SingleRolePresentation.contextType = PSRol;

    SingleRolePresentation.propTypes =
      { viewname: string
      , labelProperty: string.isRequired
      , setSelf: func.isRequired
    };
    return SingleRolePresentation;
  }

  ///////////////////////////////////////////////////////////////////////////////
  //// EXAMPLE: ARIALABELCARD
  ///////////////////////////////////////////////////////////////////////////////

  // A card that displays the property that is also used as aria-label value.
  // It can be selected by tabbing.
  // This card must be used in a context that provides PSRol.
  export const AriaLabelCard = makeSingleRolePresentation(
    // eslint-disable-next-line react/display-name
    forwardRef( function(props, ref)
    {
      // eslint-disable-next-line react/prop-types
      return  <Card ref={ref} tabIndex={props.tabIndex} aria-label={props["aria-label"]}>
                <Card.Body>
                  <Card.Text>{
                    // eslint-disable-next-line react/prop-types
                    props["aria-label"]}</Card.Text>
                </Card.Body>
              </Card>;
    }));

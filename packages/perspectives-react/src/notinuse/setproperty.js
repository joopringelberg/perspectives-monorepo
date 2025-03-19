import React, { Children, cloneElement } from "react";

import PropTypes from "prop-types";
import {PDRproxy} from "perspectives-proxy";

import PerspectivesComponent from "./perspectivescomponent.js";
import {getQualifiedPropertyName} from "./urifunctions.js";
import {PSView, PSProperty, PSContext} from "./reactcontexts.js";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";

/////// THIS COMPONENT IS NEVER USED.

export default function SetProperty (props)
{
  return <PSContext.Consumer>{ pscontext =>
      <SetProperty_
        id={props.id}
        propertyname={props.propertyname}
        myroletype={pscontext.myroletype}>
          {props.children}
      </SetProperty_>
        }</PSContext.Consumer>
}

SetProperty.propTypes = {
  propertyname: PropTypes.string.isRequired,
  value: PropTypes.array
};

class SetProperty_ extends PerspectivesComponent
{
  constructor (props)
  {
    super(props);
    this.state = { defaultvalue: undefined, setvalue: undefined };
  }

  changeValue (val)
  {
    const component = this;
    const property = getQualifiedPropertyName(component.props.propertyname, component.context.viewproperties);
    const oldValue = component.context[property];
    if (oldValue.length != 1 || oldValue[0] != val)
    {
      PDRproxy.then(
        function(pproxy)
        {
          pproxy.setProperty(
            component.context.rolinstance,
            property,
            val,
            component.props.myroletype )
          .catch(e => UserMessagingPromise.then( um => 
            um.addMessageForEndUser(
              { title: i18next.t("setProperty_title", { ns: 'preact' }) 
              , message: i18next.t("setProperty_message", {ns: 'preact', property})
              , error: e.toString()
              })));
        });
    }
  }

  componentDidMount ()
  {
    const component = this;
    const updater = {
      defaultvalue: component.context[getQualifiedPropertyName(component.props.propertyname, component.context.viewproperties)],
      setvalue: function(val)
      {
        if ( Array.isArray( val ) )
        {
          throw "Perspectives-react, SetProperty: supply a single string value to the function 'setvalue'."
        }
        else
        {
          component.changeValue(val);
        }
      }
    };
    component.setState(updater);
  }

  componentDidUpdate ()
  {
    const component = this;
    const updater = {
      defaultvalue: component.context[getQualifiedPropertyName(component.props.propertyname, component.context.viewproperties)]
    };
    component.setState( updater );
  }

  render ()
  {
    const component = this;
    let children;
    if ( component.stateIsComplete() )
    {
      if (Array.isArray(component.props.children))
      {
        children = component.props.children;
      }
      else
      {
        children = [component.props.children];
      }
      // We provide the children with props, AND we give them a React Context with the same props.
      // Hence for a child we can choose to use a Consumer, or a function with an argument to receive the props.
      return <PSProperty.Provider value={component.state}>
              { Children.map( children, child => cloneElement( child, component.state) ) }
            </PSProperty.Provider>
    }
    else
    {
      return <div/>;
    }
  }
}

SetProperty_.contextType = PSView;

SetProperty_.propTypes = {
  propertyname: PropTypes.string.isRequired,
  value: PropTypes.array
};

// SetProperty passes on:
// defaultvalue
// setvalue

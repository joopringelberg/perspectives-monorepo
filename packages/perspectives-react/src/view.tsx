import React from "react";
import {ContextInstanceT, ContextType, PDRproxy, PropertyType, RoleInstanceT, RoleType, ValueT} from "perspectives-proxy";

import PerspectivesComponent from "./perspectivesComponent";
import {getQualifiedPropertyName} from "./urifunctions.js";
import {PSRol, PSView, PSContext, PSRolType} from "./reactcontexts.js";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";

interface ViewProps
{
  viewname: string;
  children?: React.ReactNode;
}

const View: React.FC<ViewProps> = (props) => {
  return (
    <PSContext.Consumer>
      {pscontext => (
        <View_
          myroletype={pscontext.myroletype}
          viewname={props.viewname}
        >
          {props.children}
        </View_>
      )}
    </PSContext.Consumer>
  );
};

export default View;

interface View_Props extends ViewProps
{
  myroletype: RoleType;
}

interface View_State
{
  contextinstance: ContextInstanceT;
  contexttype: ContextType;
  rolinstance: RoleInstanceT;
  roltype: RoleType;
  viewproperties: PropertyType[];
  propertyValues: {[key: PropertyType]: ValueT[]};
  propval: (ln: string) => ValueT[];
  propset: (ln: string, val: ValueT) => void;
  propdel: (ln: string) => void;
}

// NOTE. If a view contains two properties whose local names are equal (even while their qualified names are unique),
// the state of the View component will have the value of the property that was last fetched for that local name.
// To solve this problem, use the localName rolProperty of the View (however, this has not yet been implemented).
class View_ extends PerspectivesComponent<View_Props, View_State>
{
  declare context: PSRolType


  changeValue (ln : string, val : ValueT)
  {
    const component = this;
    const qualifiedPropertyName = getQualifiedPropertyName(ln, component.state.viewproperties);
    const oldValue = component.state.propertyValues[qualifiedPropertyName];
    if (oldValue.length != 1 || oldValue[0] != val)
    {
      PDRproxy.then(
        function(pproxy)
        {
          pproxy.setProperty(
            component.state.rolinstance,
            qualifiedPropertyName,
            val,
            component.props.myroletype )
          .catch(e => UserMessagingPromise.then( um => 
            um.addMessageForEndUser(
              { title: i18next.t("setProperty_title", { ns: 'preact' }) 
              , message: i18next.t("setProperty_message", {ns: 'preact', property: qualifiedPropertyName})
              , error: e.toString()
              })));
        });
    }
  }

  deleteProperty (ln : string)
  {
    const component = this;
    const qualifiedPropertyName = getQualifiedPropertyName(ln, component.state.viewproperties);
    PDRproxy.then(
      function(pproxy)
      {
        pproxy.deleteProperty(
          component.state.rolinstance,
          qualifiedPropertyName,
          component.props.myroletype)
        .catch(e => UserMessagingPromise.then( um => 
          um.addMessageForEndUser(
            { title: i18next.t("deleteProperty_title", { ns: 'preact' }) 
            , message: i18next.t("deleteProperty_message", {ns: 'preact', property: qualifiedPropertyName})
            , error: e.toString()
            })));
    }
    );
  }

  componentDidMount ()
  {
    const component = this;
    PDRproxy.then(
      function(pproxy)
      {
        if (component.props.viewname == "allproperties")
        {
          console.warn( "Warning: View receives prop viewname=`allproperties`. Do you mean 'allProperties'?");
        }
        pproxy.getViewProperties(
            component.context.roltype,
            component.props.viewname)
          .then(
            function(propertyNames)
            {
              // TODO. Dit is inefficient.
              // Als er een Property bij komt of vanaf gaat, wordt voor elke Property de waarde opnieuw opgehaald.
              // Als er één af gaat is dat al helemaal niet nodig.
              // Komt er één bij, dan hoeven we alleen voor die nieuwe Property de waarde op te halen.

              // We will use the state as the value of PSView.
              // Now add the viewProperties to state.
              component.setState(
                { contextinstance: component.context.contextinstance
                , contexttype: component.context.contexttype
                , rolinstance: component.context.rolinstance!
                , roltype: component.context.roltype
                , viewproperties: propertyNames
                , propval: ln => component.state.propertyValues[ getQualifiedPropertyName(ln, propertyNames) ]
                , propset: function(ln : string, val: ValueT)
                    {
                      if ( Array.isArray( val ) )
                      {
                        throw "Perspectives-react, View: supply a single string value to the function 'setvalue'.";
                      }
                      else
                      {
                        component.changeValue(ln, val);
                      }
                    }
                , propdel: (ln) => component.deleteProperty( ln )
                });
              // Then fetch the values of the properties, to complete the state.
              propertyNames.forEach(
                function(propertyName)
                {
                  component.addUnsubscriber(
                    pproxy.getProperty(
                      component.context.rolinstance!,
                      propertyName,
                      component.context.roltype,
                      function (propertyValues)
                      {
                        const updater = {} as {[key: PropertyType]: ValueT[]};
                        updater[propertyName] = propertyValues;
                        component.setState({propertyValues: updater});
                      }));
                }
              );
            })
          .catch(e => UserMessagingPromise.then( um => 
            um.addMessageForEndUser(
              { title: i18next.t("view_viewproperties_title", { ns: 'preact' }) 
              , message: i18next.t("view_viewproperties_message", {role: component.context.roltype, ns: 'preact'})
              , error: e.toString()
            })));
        }
    );
  }

  render ()
  {
    const component = this;

    if (!component.stateIsEmpty() && component.stateIsComplete())
    {
      return (<PSView.Provider value={component.state}>
        {component.props.children}
        </PSView.Provider>);
    }
    else
    {
      return null;
    }
  }

}

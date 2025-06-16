import "regenerator-runtime";

import { createRef } from 'react';

import {string, func} from "prop-types";
import { PDRproxy, CONTINUOUS, RoleInstanceT, RoleType, ValueT, ContextInstanceT, ContextType, PerspectivesProxy } from 'perspectives-proxy';
import PerspectivesComponent from "./perspectivesComponent";
import {PSContext, AppContext, PSContextType} from "./reactcontexts.js";
import { deconstructModelName, deconstructSegments/*, isExternalRole*/ } from "./urifunctions.js";
import ScreenDefinitionInterpreter from "./screendefinitioninterpreter.js";
import { default as ModelDependencies } from "./modelDependencies.js";
import Pouchdb from "pouchdb-browser";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";

import
  { Col
  , Row
  , Card
  , Container
  , Button
  } from "react-bootstrap";

import lifecycle from 'page-lifecycle';

// TODO. Even though PerspectivesGlobals has been declared external, we cannot import it here.
// Doing so will cause a runtime error if the calling program has not put it on the global scope in time.

// This function fetches an attachment "screens.js" from the model file that is passed in as an argument.
// It expects that model file to be in the models database. This might be a local db (IndexedDB) or a remote Couchdb.
// Pouchdb will handle both.
// Returns a promise for a module.
// WHEN the explicitly stored value for 'me' in the context instance changes, the screen is recomputed.
interface FetchModuleFromPouchdbParams {
  modelName: string;
  systemIdentifier: string;
  couchdbUrl?: string;
}

interface ScreenModule {
  [key: string]: any;
}

function fetchModuleFromPouchdb({ modelName, systemIdentifier, couchdbUrl }: FetchModuleFromPouchdbParams): Promise<ScreenModule | undefined> {
  const modelsDatabaseName = couchdbUrl ? couchdbUrl + systemIdentifier + "_models" : systemIdentifier + "_models";
  const modelsDatabase = new Pouchdb(modelsDatabaseName);

  return modelsDatabase.getAttachment(modelName, "screens.js")
    .then((jstext) => {
      const blob = jstext as Blob;
      return blob.text().then((t: string) => {
        return Function('"use strict";' + t.match(/^(.*)export \{/ms)![1] + "\nreturn perspectivesScreens;")();
      });
    })
    .catch((e: any) => {
      console.error(e);
      return undefined;
    });
}

interface ScreenOutcome {
  roleName: string;
  module: React.ComponentType<any>;
}

// Returns a promise that resolves to an array, possibly empty, of objects of the form {roleName :: String, module :: <A React Component> }.
function importScreens( roleNames : string[], userIdentifier: string, couchdbUrl? : string ): Promise<ScreenOutcome[]> {
  const promises = roleNames.map((roleName) => {
    const modelName = deconstructModelName(roleName);
    return fetchModuleFromPouchdb({ modelName, systemIdentifier: userIdentifier, couchdbUrl }).then((module) => {
      return { module, roleName }; 
    });
  });

  return Promise.allSettled(promises).then((outcomes) => {
    return outcomes.filter((outcome): outcome is PromiseFulfilledResult<{ module: ScreenModule | undefined; roleName: string; }> => outcome.status === "fulfilled" && outcome.value.module![computeScreenName(outcome.value.roleName)]);
  }).then((outcomesWithScreen) => {
    return outcomesWithScreen.map(({ value }) => {
      return { roleName: value.roleName, module: value.module![computeScreenName(value.roleName)] };
    });
  });
}

// > Object { status: "fulfilled", value: 3 }
// > Object { status: "rejected", reason: "foo" }

function computeScreenName( roleName : string ) : string
{
  // Make the identifier start with lowercase and replace '$' with _ (underscore).
  function mapName (s : string) : string
  {
    const regex1 = /\$/gi;
    const regex2 = /^./gi;
    return s.replace(regex1, '_').replace(regex2, s.charAt(0).toLowerCase());
  }

  // screenName = local part of the roleName (all segments).
  return mapName( deconstructSegments(roleName) );

}

interface ScreenProps {
  externalroleinstance: RoleInstanceT;
  setMyRoleType: (roleType: RoleType) => void;
}

export default function Screen(props : ScreenProps)
{
  return  <AppContext.Consumer>
          {
            ({couchdbUrl, systemIdentifier}) => <Screen_ couchdbUrl={couchdbUrl} systemIdentifier={systemIdentifier} {...props}/>
          }
          </AppContext.Consumer>;
}

// Screen_ loads the component in the context of the role `externalroleinstance` that it receives on its props.
interface ScreenProps_
{
  externalroleinstance: RoleInstanceT;
  setMyRoleType: (roleType: RoleType) => void;
  couchdbUrl?: string;
  systemIdentifier: string;
}
interface ScreenState_ {
  myroletype: RoleType | undefined;
  contextinstance: ContextInstanceT | undefined;
  contexttype: ContextType | undefined;
  modules: ScreenOutcome[] | undefined;
  hasError: boolean;
}

class Screen_ extends PerspectivesComponent<ScreenProps_, ScreenState_>
{
  visibilityChangeHandler: () => void;

  constructor (props : ScreenProps_)
  {
    super(props);
    const component = this;
    this.state = { myroletype: undefined, contextinstance: undefined, contexttype: undefined, modules: undefined, hasError: false };
    // component.beforeUnloadListener = function(event)
    //   {
    //     event.preventDefault();
    //     return event.returnValue = "Are you sure you want to exit?";
    //   };
    // window.addEventListener( "beforeUnload", component.beforeUnloadListener, {capture: true} );
    lifecycle.addUnsavedChanges("Context");
    this.visibilityChangeHandler = function()
      {
        // See: https://blog.bitsrc.io/page-lifecycle-api-a-browser-api-every-frontend-developer-should-know-b1c74948bd74
        // See: https://developer.mozilla.org/en-US/docs/Web/API/Document/visibilitychange_event
        if (document.visibilityState == "hidden" || document.visibilityState == "visible")
        {
          // Set to true if page is visible, false when hidden.
          component.setIsOnScreen( component.props.externalroleinstance, (document.visibilityState == "visible").toString() )
          .catch(e => UserMessagingPromise.then( um => 
            um.addMessageForEndUser(
              { title: i18next.t("screen_IsOnScreen_title", { ns: 'preact' }) 
              , message: i18next.t("screen_IsOnScreen_message", {ns: 'preact'})
              , error: e.toString()
              })));
            }
      };

    window.addEventListener( "visibilitychange", this.visibilityChangeHandler);
  }

  // Returns a promise.
  setIsOnScreen (roleInstance : RoleInstanceT, value : string) : Promise<[]>
  {
    const component = this;
    return PDRproxy.then(
      function(pproxy)
      {
        console.log("Setting IsOnScreen of " + roleInstance + " to `" + value +"`.")
        return pproxy
          .setProperty(
            roleInstance
            , ModelDependencies.isOnScreen
            , value as ValueT
            , component.state.myroletype!)
      });
  }

  computeState ()
  {
    const component = this;
    const externalRole = component.props.externalroleinstance;
    const userIdentifier = component.props.systemIdentifier;
    let pproxy : PerspectivesProxy, contextId : ContextInstanceT, contextType : ContextType;
    PDRproxy
      .then( p => pproxy = p)
      .then( pproxy => pproxy.getRolContext( externalRole ) )
      .then( contextId => pproxy.getContextType( contextId ) )
      .then( c => contextType = c )
      .then( () => new Promise( function(resolve, reject)
        {
          return component.addUnsubscriber(
            pproxy.getMeForContext(
              externalRole,
              // userRoles includes roles from aspects.
              function(userRoles)
              {
                // It may happen that there are no user role types.
                if ( userRoles.length == 0)
                {
                  UserMessagingPromise.then( um => 
                    um.addMessageForEndUser(
                      { title: i18next.t("screen_no_usertype_for_context_title", { ns: 'preact' }) 
                      , message: i18next.t("screen_no_usertype_for_context_message", {ns: 'preact'})
                      , error: "No result from GetMeFromContext"
                      }));
                }
                else
                {
                  component
                    .setIsOnScreen( component.props.externalroleinstance, "true") 
                    .then( () => importScreens( userRoles, userIdentifier, component.props.couchdbUrl) )
                    .then( screenModules => 
                      {
                        component.setState(
                          { myroletype: userRoles[0]
                          , contextinstance: contextId!
                          , contexttype: contextType
                          , modules: screenModules
                          });
                        component.props.setMyRoleType( userRoles[0]);
                      })
                    .then( resolve, reject );
                  }
              },
              CONTINUOUS,
              reject))
          }))
      .catch(e => UserMessagingPromise.then( um => 
        um.addMessageForEndUser(
          { title: i18next.t("screen_computestate_title", { ns: 'preact' }) 
          , message: i18next.t("screen_computestate_message", {ns: 'preact'})
          , error: e.toString()
          })));
  }

  componentDidMount ()
  {
    const component = this;
    component.computeState();
  }

  // We ensure this method is called when the user navigates away from the InPlace page. Otherwise, 
  // screen state will persist in Perspectives. 
  componentWillUnmount ()
  {
    // Modify the external role so we know the screen will close.
    const component = this;
    // Need to call the super explicitly, so it will unsubscribe.
    super.componentWillUnmount();
    window.removeEventListener("visibilitychange", this.visibilityChangeHandler);
    // window.removeEventListener( "beforeUnload", component.beforeUnloadListener, {capture: true} );
    lifecycle.removeUnsavedChanges("Context");
    component
      .setIsOnScreen( component.props.externalroleinstance, "false")
      .catch(e => UserMessagingPromise.then( um => 
        um.addMessageForEndUser(
          { title: i18next.t("screen_IsOnScreen_title", { ns: 'preact' }) 
          , message: i18next.t("screen_IsOnScreen_message", {ns: 'preact'})
          , error: e.toString()
          })));
}

  componentDidUpdate (prevProps : ScreenProps_)
  {
    const component = this;
    if ( component.props.externalroleinstance !== prevProps.externalroleinstance)
    {
      component.unsubscribeAll()
        .then( () => this.setState( { myroletype: undefined, contextinstance: undefined, contexttype: undefined, modules: undefined }) )
        .then( () => component.setIsOnScreen( prevProps.externalroleinstance, "false") )
        .then( () => component.computeState());
    }
  }

  static getDerivedStateFromError(/*error*/) {
    // Update state so the next render will show the fallback UI.
    return { hasError: true };
  }

  render ()
  {
    const component = this;
    let TheScreen, pscontext;

    if (component.state.hasError)
    {
      return    <Row>
                  <Col>
                  <Card>
                    <Card.Body>
                      <Card.Title>An error condition occurred</Card.Title>
                      <Card.Text>
                        Due to some error, InPlace could not build the screen that shows the context you requested. More information may be available on the console.
                      </Card.Text>
                      <BackButton buttontext="Back"/>
                    </Card.Body>
                    </Card>
                  </Col>
                </Row>;
    }
    else if (component.stateIsComplete())
    {
      pscontext =
        { contextinstance: component.state.contextinstance!
        , contexttype: component.state.contexttype
        , myroletype: component.state.myroletype!} as PSContextType;
      if (component.state.modules?.length == 1)
      {
        TheScreen = component.state.modules[0]["module"];
        return  <PSContext.Provider value={pscontext}>
                  <TheScreen/>
                </PSContext.Provider>;
      }
      else if (component.state.modules && component.state.modules.length > 0)
      {
        return <p>You must choose a role from {component.state.modules.map( ({roleName}) => roleName ).toString()}</p>;
      }
      else
      {
        return  <PSContext.Provider value={pscontext}>
                  <ScreenDefinitionInterpreter
                    contextinstance={component.state.contextinstance!}
                    contexttype={component.state.contexttype!}
                    myroletype={component.state.myroletype!}
                  />
                </PSContext.Provider>;
      }
    }
    else
      return  <Container>
                <h3 className="text-center pt-5">{ i18next.t( "openContext", { ns: 'preact' }) }</h3>
              </Container>
;
}
}

  // Use like this:
//  <BackButton buttontext="Back to all chats"/>
interface BackButtonProps {
  buttontext: string;
}
export function BackButton(props : BackButtonProps)
{
  const ref = createRef() as React.RefObject<HTMLButtonElement>;
  return (  <Button ref={ref} href="#" onClick={() => history.back()}>
              { props.buttontext }
            </Button>);
}

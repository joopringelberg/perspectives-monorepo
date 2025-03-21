import React, { createRef } from "react";

import {AppContext} from "./reactcontexts.js";

import PerspectivesComponent from "./perspectivesComponent.js";
import { default as ModelDependencies } from "./modelDependencies.js";

// import {PDRproxy} from "perspectives-proxy";
import {PDRproxy, FIREANDFORGET, RoleInstanceT, RoleOnClipboard} from "perspectives-proxy";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";

////////////////////////////////////////////////////////////////////////////////
// ROLEDROPZONE
////////////////////////////////////////////////////////////////////////////////
/*
The user can add an instance to a role with this component by dropping a binding
on it. The new instance will be created and the dropped role fills it.
It can be used both inside a Functional Role and inside an MultiRole: as a descendant
of either a RoleInstance or a RolInstances component, but does not require them
as ReactContext.
Instead, we require two functions on the props:
  * bind: either use PSRol.bind, PSRol.bind_ or PSRoleInstances.bind
  * checkBinding

There is a functional overlap between some methods in this component and the
behaviour we can add to components that represent a role instance (and therefore
require a PSRol context).
The overlap is unfortunate, but unavoidable.
A note to implementers: consider the example of addFillWithRole to be leading for the
implementation choices in RoleDropZone (try to follow that code as much as possible).
*/
interface RoleDropZoneProps
{
  ariaLabel: string;
  bind: (roleData: any) => Promise<void>;
  checkbinding: (roleInstance: RoleInstanceT) => Promise<boolean>;
  children: React.ReactNode;
}

export default function RoleDropZone(props : RoleDropZoneProps)
{
  return  <AppContext.Consumer>
          {
            appcontext => <RoleDropZone_ systemExternalRole={appcontext.systemExternalRole} {...props}>{
              //eslint-disable-next-line react/prop-types
              props.children}</RoleDropZone_>
          }
          </AppContext.Consumer>;
}

interface RoleDropZoneProps_ extends RoleDropZoneProps {
  systemExternalRole: RoleInstanceT;
}

class RoleDropZone_ extends PerspectivesComponent
{
  eventDiv : React.RefObject<HTMLDivElement>;
  constructor (props : RoleDropZoneProps_)
  {
    super(props);
    this.eventDiv = createRef() as React.RefObject<HTMLDivElement>;
  }

  readClipBoard( receiveResults : (roleDataAndBehaviour : RoleOnClipboard | null) => void )
  {
    const component = this;
    PDRproxy.then(
      function(pproxy)
      {
        pproxy.getProperty(
          component.props.systemExternalRole,
          ModelDependencies.cardClipBoard,
          ModelDependencies.systemExternal,
          function (valArr)
          {
            if (valArr[0])
            {
              receiveResults( JSON.parse( valArr[0]) );
            }
            else
            {
              receiveResults( null );
            }
          },
          FIREANDFORGET);
      });
  }

  tryToBind ( {roleData, addedBehaviour} : RoleOnClipboard )
  {
    const component = this;
    if ( addedBehaviour.includes("fillARole"))
    {
      component.props
        .checkbinding( roleData.rolinstance )
        .then(
          function( bindingAllowed : boolean )
          {
            if ( bindingAllowed)
            {
              component.props
                // bind catches its own errors.
                .bind( roleData.rolinstance )
                .then( () =>
                  PDRproxy.then( pproxy =>
                    pproxy.removeRoleFromClipboard(roleData.rolinstance)
                  )
                  .catch(e => UserMessagingPromise.then( um => 
                    um.addMessageForEndUser(
                      { title: i18next.t("clipboardEmpty_title", { ns: 'preact' }) 
                      , message: i18next.t("clipboardEmpty_message", {ns: 'preact'})
                      , error: e.toString()
                      })))
                  );
              }
            else if (component.eventDiv.current){
              component.eventDiv.current.classList.add("failure");
              component.eventDiv.current.focus();
              UserMessagingPromise.then( um => 
                um.addMessageForEndUser(
                  { title: i18next.t("fillerNotAllowed_title", { ns: 'preact' }) 
                  , message: i18next.t("fillerNotAllowed_message", {ns: 'preact'})
                  , error: ""
                  }));
            }
          } );
      }
  }

  handleKeyDown ( event : React.KeyboardEvent<HTMLDivElement> )
  {
    const component = this;
    switch(event.code){
      case "Enter": // Enter
      case "Space": // space
        PDRproxy.then( pproxy => pproxy.getSelectedRoleFromClipboard().then(
          function( roleDataAndBehaviour : RoleOnClipboard | null )
          {
            if (roleDataAndBehaviour)
            {
              component.tryToBind( roleDataAndBehaviour );
            }
          }));
        event.preventDefault();
      }
  }

  render ()
  {
    const component = this;
    return  <div
              ref={component.eventDiv}
              tabIndex={0}

              onDragEnter={ event => {
                event.preventDefault();
                event.stopPropagation();
                component.eventDiv.current?.blur();
                component.eventDiv.current?.classList.add("dropHere");
              } }
              // No drop without this...
              onDragOver ={ event => {
                event.preventDefault();
                event.stopPropagation();
              }}
              onDragLeave={ ev => (ev.target as HTMLElement).classList.remove("dropHere")}
              onBlur={ ev => ev.target.classList.remove("dropHere", "failure")}

              onDrop={ ev => {
                ev.preventDefault();
                ev.stopPropagation()
                component.tryToBind( JSON.parse( ev.dataTransfer.getData("PSRol") ) ) }
                }
              onKeyDown={ ev => component.handleKeyDown( ev )}

              style={ {flexGrow: 1} }
              className="p-2 dropzone"

              aria-dropeffect="execute"
              aria-label={component.props.ariaLabel}
            >
                {component.props.children}
            </div>;
  }
}

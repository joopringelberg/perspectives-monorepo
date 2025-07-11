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

import type {
  RoleInstanceT,
  RoleReceiver,
  ContextInstanceT,
  ValueT,
  PropertyValueReceiver,
  RoleType,
  UserRoleType,
  RoleTypeReceiver,
  PerspectivesReceiver,
  ScreenReceiver,
  TableFormReceiver,
  PropertyType,
  ContextType,
  RoleKind,
  ContextActions,
  FileShareCredentials,
  PSharedFile,
  PerspectivesFile,
  RuntimeOptions,
  PouchdbUser,
  Unsubscriber,
  PRange,
  InputType,
  RoleOnClipboard,
  ContextAndNameReceiver
} from "./perspectivesshape.d.ts";

export type * from "./perspectivesshape.d.ts";
/*
This module is imported both by the core and by clients and bridges the gap between the two. It supports several architectures:
  1 with core and client in the same javascript process;
  2 with core and client in different javascript processes, connected by the Channel Messaging API
    https://developer.mozilla.org/en-US/docs/Web/API/Channel_Messaging_API
  3 with core and client in different processes, connected by TCP. OBSOLETE!! We have commented the code out. It will serve as an example when we develop the Language Server. See the design text "TCP architecture.txt".
The core resolves two promises:
  - one called PDRproxy, resolving to an instance of PerspectivesProxy with an InternalChannel, to be used in the first architecture by direct import;
  - one called InternalChannel, resolving to an instance of InternalChannel, to be used in the second architecture, used by the Service Worker by direct import;
Then there are two functions to be used by clients, that both resolve the PDRproxy promise.
  - createServiceWorkerConnectionToPerspectives, for the second architecture. It resolves the PDRproxy promise with an instance of SharedWorkerChannel, that *uses* the InternalChannel to communicate with the core;
  - createTcpConnectionToPerspectives, for the third architecture. It resolves the PDRproxy promise with an instance of TcpChannel.
The PDRproxy promise is imported by all of the modules in perspectives-react that must connect to the core.
*/

////////////////////////////////////////////////////////////////////////////////
//// CLIENT SIDE PROMISES
////////////////////////////////////////////////////////////////////////////////

let pdrProxyResolver: (value: PerspectivesProxy | PromiseLike<PerspectivesProxy>) => void

// This promise will resolve to an instance of PerspectivesProxy, with a SharedWorkerChannel that holds a port to the SharedWorker execution context,
// or to the page execution context that hosts the PDR.
// The proxy uses the channel to actually send requests to the core. These requests will
// turn up as 'output' of a Producer, ready to be consumed by some process.
// The channel uses the emit function as a callback: when it has a request to send, it calls 'emit'
// after wrapping the request in the appropriate constructor (usually the emitStep).
export const PDRproxy: Promise<PerspectivesProxy> = new Promise(
  function (resolve/*, reject*/)
  {
    pdrProxyResolver = resolve;
    //pdrProxyRejecter = reject;
  });

////////////////////////////////////////////////////////////////////////////////
//// RESOLVE AND CONFIGURE PDRPROXY WITH A CHANNEL
////////////////////////////////////////////////////////////////////////////////
// Creates an instance of PerspectivesProxy with a selected type of channel and
// fullfills the PDRproxy with it.
// Options as described in the module Control.Aff.Sockets:
// type TCPOptions opts = {port :: Port, host :: Host, allowHalfOpen :: Boolean | opts}
// type Port = Int
// type Host = String

type Options = { pageHostingPDRPort?: (pdr: any) => MessagePort };

export function configurePDRproxy(channeltype: "internalChannel" | "sharedWorkerChannel" | "hostPageChannel", options: Options): void 
{
  let sharedWorkerChannel, sharedWorker;
  switch( channeltype )
  {
    case "sharedWorkerChannel":
      sharedWorker =  new SharedWorker(new URL('perspectives-sharedworker', import.meta.url), { type: 'module' })
      //// Its port property is a MessagePort, as documented in https://developer.mozilla.org/en-US/docs/Web/API/MessagePort.
      sharedWorkerChannel = new SharedWorkerChannel( sharedWorker.port );
      sharedWorkerChannelResolver( sharedWorkerChannel );
      pdrProxyResolver( new PerspectivesProxy( sharedWorkerChannel ) );
      break;
    case "hostPageChannel":
        import( "perspectives-core" ).then( pdr => {
        // pageHostingPDRPort returns a MessageChannel as documented here: https://developer.mozilla.org/en-US/docs/Web/API/MessagePort.
        sharedWorkerChannel = new SharedWorkerChannel( options.pageHostingPDRPort!( pdr ) );
        sharedWorkerChannelResolver( sharedWorkerChannel );
        pdrProxyResolver( new PerspectivesProxy( sharedWorkerChannel ) );
        });
       break;
  }
}

////////////////////////////////////////////////////////////////////////////////
//// REQUEST STRUCTURE
////////////////////////////////////////////////////////////////////////////////
interface RequestRecord {
  request?: string;
  subject?: string;
  predicate?: string;
  object?: any;
  contextDescription?: any;
  rolDescription?: any;
  authoringRole?: string;
  onlyOnce?: boolean;
  reactStateSetter?: (response: any) => void;
  corrId?: number;
  trackingNumber?: number;
}

const defaultRequest =
  {
    request: "WrongRequest",
    subject: "The original request did not have a request type!",
    predicate: "",
    object: "",
    reactStateSetter: function(){},
    corrId: 0,
    contextDescription: {},
    onlyOnce: false,
    trackingNumber: 0,
  } as RequestRecord;

/////////////////////////////////////////////////////////////////////////////////////////
// PDRTYPES
/////////////////////////////////////////////////////////////////////////////////////////

type Response = ErrorResponse | ResultResponse | WorkerResponse;

type ErrorResponse = {
  responseType: "APIerror";
  corrId: number;
  error: string;
  warnings: string[];
};

type ResultResponse = {
  responseType: "APIresult";
  corrId: number;
  result: string[];
  warnings: string[];
};

type WorkerResponse = {
  responseType: "WorkerResponse";
  serviceWorkerMessage: "channelId" | "pdrStarted" | "isUserLoggedIn" | "runPDR" | "createAccount" | "resetAccount" | "reCreateInstances" | "recompileLocalModels" | "removeAccount";
  channelId: number;
  pdrStarted: boolean;  // true if the PDR has started.
  isUserLoggedIn: boolean;  // true if the user has logged in before.
  createSuccesful: boolean;  // true if the account has been created.
  resetSuccesful: boolean;  // true if the account has been reset.
  reCreateSuccesful: boolean;  // true if the instances have been recreated.
  recompileSuccesful: boolean;  // true if the local models have been recompiled.
  removeSuccesful: boolean;  // true if the account has been removed.
};

////////////////////////////////////////////////////////////////////////////////
//// SHARED WORKER CHANNEL
//// This code will be executed by the client!
//// The SharedWorkerChannel is a proxy for the ServiceWorker for the client.
////////////////////////////////////////////////////////////////////////////////

class SharedWorkerChannel
{
  port: MessagePort;
  requestId: number;
  valueReceivers: { [key: string]: ((data: any) => void) | undefined };
  channelIdResolver: ((value: number | PromiseLike<number>) => void) | undefined;
  channelId: Promise<number>;

  // port is a MessagePort as documented here: https://developer.mozilla.org/en-US/docs/Web/API/MessagePort.

  // In case of a pageworker, it is the input port of the channel created by the page that wants to use the PDR.
  // The other (output) port is the end of the channel where we call the PDR.
  // This will be the page that hosts the PDR. On this page, we've instantiated the PDR and passed it as an argument in the call to pageHostingPDRPort.
  // This function provides the output end (port) of the channel with a listener that passes requests to the PDR function 'handleClientRequest'.
  // This function is defined in the PDR foreign module 'proxy.js'.
  // The port is bi-directional, so the PDR can send messages back.
  // Such responses from the PDR are handled by the handleWorkerResponse function in this module (perspectives-proxy).
  // Refining the above: some requests are actually handled by the proxy itself.
  // It will call several PDR functions (like createAccount).
  // Most requests will be passed to the PDR API, though.
  constructor( port: MessagePort )
  {
    const serviceWorkerChannel = this;
    this.requestId = -1;
    this.valueReceivers = {};
    this.channelIdResolver = undefined;
    this.channelId = new Promise(
      function (resolve/*, reject*/)
      {
        serviceWorkerChannel.channelIdResolver = resolve;
      });
    this.port = port;

    this.handleWorkerResponse = this.handleWorkerResponse.bind(this);
    this.port.onmessage = this.handleWorkerResponse;
  }

  // The sharedworker or pageworker sends messages of various types.
  // Among them are responses received by the core.
  //
  handleWorkerResponse (e : MessageEvent<Response>)
  {
    if (e.data.responseType === "APIerror")
    {
      // {corrId: i, error: s} where s is is a String, i an int.
      // we just pass errors on.
      this.valueReceivers[ e.data.corrId ]!( e.data );
    }
    else if ( e.data.responseType === "APIresult" )
    {
      // {corrId: i, result: s} where s is an Array of String, i an int.
      // pass the result on
      this.valueReceivers[ e.data.corrId ]!( e.data );
    }
    // Then we have a category of incoming messages that originate in the service worker itself,
    // often in response to a specific request sent by the proxy.
    else if ( e.data.responseType === "WorkerResponse" )
    {
      // {serviceWorkerMessage: m, <field>: <value>} where m is a string. The object may contain any number of other fields, depending on the type of message (i.e. the value of m).
      switch( e.data.serviceWorkerMessage )
      {
        case "channelId":
          // This actually is a response that is not provoked by explicitly asking for it.
          // As soon as the SharedWorker receives a port from this proxy, it will return the channels id.
          // {serviceWorkerMessage: "channelId", channelId: i} where i is a multiple of a million.
          // Handle the port identification message that is sent by the service worker.
          console.log( "SharedWorkerChannel received a channelId: " + e.data.channelId );
          this.channelIdResolver!( e.data.channelId );
          break;
        case "pdrStarted":
          this.valueReceivers.pdrStarted!( e.data.pdrStarted );
          break;
        case "isUserLoggedIn":
          // {serviceWorkerMessage: "isUserLoggedIn", isUserLoggedIn: b} where b is a boolean.
          this.valueReceivers.isUserLoggedIn!( e.data.isUserLoggedIn );
          break;
        case "resetAccount":
          // {serviceWorkerMessage: "resetAccount", resetSuccesful: b} where b is a boolean.
          this.valueReceivers.resetAccount!( e.data.resetSuccesful );
          break;
        case "reCreateInstances":
          // {serviceWorkerMessage: "reCreateInstances", reCreateSuccesful: b} where b is a boolean.
          this.valueReceivers.reCreateInstances!( e.data.reCreateSuccesful );
          break;
        case "recompileLocalModels":
        // {serviceWorkerMessage: "recompileLocalModels", recompileSuccesful: b} where b is a boolean.
        this.valueReceivers.recompileLocalModels!( e.data.recompileSuccesful );
        break;
        case "removeAccount":
          // {serviceWorkerMessage: "removeAccount", removeSuccesful: b} where b is a boolean.
          this.valueReceivers.removeAccount!( e.data.removeSuccesful );
          break;
        case "runPDR":
          // {serviceWorkerMessage: "runPDR", error: e }
          this.valueReceivers.runPDR!( e );
          break;
        case "createAccount":
          // {serviceWorkerMessage: "createAccount", createSuccesful: {success :: Boolean, reason :: Nullable String}}.
          this.valueReceivers.createAccount!( e.data.createSuccesful );
          break;
      }
    }
  }

  // This promise will resolve regardless of whether the PDR has started or not.
  pdrStarted () : Promise<boolean>
  {
    const proxy = this;
    const p = new Promise(
      function(resolver/*, rejecter*/)
      {
        proxy.valueReceivers.pdrStarted = function(hasStarted : boolean)
          {
            proxy.valueReceivers.pdrStarted = undefined;
            resolver( hasStarted );
          };
      }
    ) as Promise<boolean>;
    proxy.channelId.then( channelId => proxy.port.postMessage( {proxyRequest: "pdrStarted", channelId } ) );
    return p;
  }

  // Returns a promise for a boolean value, reflecting whether the end user has logged in before or not.
  isUserLoggedIn () : Promise<boolean>
  {
    const proxy = this;
    const p = new Promise(
      function(resolver/*, rejecter*/)
      {
        proxy.valueReceivers.isUserLoggedIn = function(isLoggedIn : boolean)
          {
            proxy.valueReceivers.isUserLoggedIn = undefined;
            resolver( isLoggedIn );
          };
      }
    ) as Promise<boolean>;
    proxy.channelId.then( channelId => proxy.port.postMessage( {proxyRequest: "isUserLoggedIn", channelId } ) );
    return p;
  }

  // runPDR :: UserName -> PouchdbUser RuntimeOptions -> Promise
  // Runs the PDR, if a value is returned it will be an error message.
  // {serviceWorkerMessage: "runPDR", startSuccesful: success }
  // {serviceWorkerMessage: "runPDR", error: e }
  runPDR (username : string, pouchdbuser : PouchdbUser, options: RuntimeOptions) : Promise<boolean>
  {
    const proxy = this;
    // We do not want to run the pdr twice.
    if (!this.valueReceivers.runPDR)
    {
      const p = new Promise(
        function(resolver, rejecter)
        {
          proxy.valueReceivers.runPDR = function( e )
            {
              proxy.valueReceivers.runPDR = undefined;
              if (e.error)
              {
                rejecter( e.errormessage );
              }
              else
              {
                resolver( e.data.startSuccesful );
              }
            };
        }
      ) as Promise<boolean>;
      proxy.channelId.then( channelId => proxy.port.postMessage({proxyRequest: "runPDR", username, pouchdbuser, options, channelId }));
      return p;
    }
    else {
      return Promise.resolve( false );
    }
  }

  createAccount (perspectivesUser : string, pouchdbuser : PouchdbUser, runtimeOptions : RuntimeOptions, optionalIdentityDocument : any) : Promise<boolean>  
  {
    const proxy = this;
    const p = new Promise(
      function(resolver, rejecter)
      {
        // {success :: Boolean, reason :: Nullable String}
        proxy.valueReceivers.createAccount = function({success, reason})
          {
            proxy.valueReceivers.createAccount = undefined;
            if (success)
            {
              resolver( true );
            }
            else 
            {
              rejecter( {reason} );
            }
          };
      }
    ) as Promise<boolean>;
    proxy.channelId.then( channelId => proxy.port.postMessage( {proxyRequest: "createAccount", perspectivesUser, pouchdbuser, channelId, runtimeOptions, identityDocument: optionalIdentityDocument ? optionalIdentityDocument : null } ) );
    return p;
  }

  resetAccount (username : string, pouchdbuser : PouchdbUser, options : RuntimeOptions) : Promise<boolean>
  {
    const proxy = this;
    const p = new Promise(
      function(resolver/*, rejecter*/)
      {
        proxy.valueReceivers.resetAccount = function(result)
          {
            proxy.valueReceivers.resetAccount = undefined;
            resolver( result );
          };
      }
    ) as Promise<boolean>;
    proxy.channelId.then( channelId => proxy.port.postMessage( {proxyRequest: "resetAccount", username, pouchdbuser, options, channelId } ) );
    return p;
  }

  reCreateInstances (pouchdbuser : PouchdbUser, options : RuntimeOptions) : Promise<boolean>
  {
    const proxy = this;
    const p = new Promise(
      function(resolver/*, rejecter*/)
      {
        proxy.valueReceivers.reCreateInstances = function(result)
          {
            proxy.valueReceivers.reCreateInstances = undefined;
            resolver( result );
          };
      }
    ) as Promise<boolean>;
    proxy.channelId.then( channelId => proxy.port.postMessage( {proxyRequest: "reCreateInstances", pouchdbuser, options, channelId } ) );
    return p;
  }

  recompileLocalModels (pouchdbuser : PouchdbUser) : Promise<boolean>
  {
    const proxy = this;
    const p = new Promise(
      function(resolver/*, rejecter*/)
      {
        proxy.valueReceivers.recompileLocalModels = function(result)
          {
            proxy.valueReceivers.recompileLocalModels = undefined;
            resolver( result );
          };
      }
    ) as Promise<boolean>;
    proxy.channelId.then( channelId => proxy.port.postMessage( {proxyRequest: "recompileLocalModels", pouchdbuser, channelId } ) );
    return p;
  }

  removeAccount (username : string, pouchdbuser : PouchdbUser ) : Promise<boolean>
  {
    const proxy = this;
    const p = new Promise(
      function(resolver/*, rejecter*/)
      {
        proxy.valueReceivers.removeAccount = function(result)
          {
            proxy.valueReceivers.removeAccount = undefined;
            resolver( result );
          };
      }
    ) as Promise<boolean>;
    proxy.channelId.then( channelId => proxy.port.postMessage( {proxyRequest: "removeAccount", username, pouchdbuser, channelId } ) );
    return p;
  }

  // Inform the server that this client shuts down.
  // No other requests may follow this message.
  close()
  {
    // send a message that will make the internal channel in the Service Worker close.
    this.port.postMessage({proxyRequest: "Close"});
  }

  unsubscribe(req : RequestRecord)
  {
    // Send a message that will make the internal channel in the Service Worker close.
    this.port.postMessage( {proxyRequest: "unsubscribe", request: req } );
  }

  nextRequestId () : Promise<number>
  {
    const proxy = this;
    return this.channelId.then(
      function( channelId )
      {
          proxy.requestId = proxy.requestId + 1;
          return proxy.requestId + channelId;
      }
    );
  }

  // Returns a promise for unsuscriber information of the form: {subject: req.subject, corrId: req.corrId}
  send ( req : RequestRecord ) : Promise<Unsubscriber>
  {
    const proxy = this;
    return this.nextRequestId().then(
      function( reqId )
      {
        const setter = req.reactStateSetter;
        // Create a correlation identifier and store it in the request.
        if ( !req.corrId )
        {
          req.corrId = reqId;
        }
        // Store the valueReceiver.
        proxy.valueReceivers[ req.corrId ] = setter;
        // cannot serialise a function, remove it from the request.
        req.reactStateSetter = undefined;
        // console.log( req );
        // send the request through the channel to the service worker.
        proxy.port.postMessage( req );
        // return the elementary data for unsubscribing.
        return {request: "Unsubscribe", subject: req.subject, corrId: req.corrId};
      }
    ) as Promise<{ request: "Unsubscribe", subject: string; corrId: number }>;
  }

}

let sharedWorkerChannelResolver: (value: SharedWorkerChannel | PromiseLike<SharedWorkerChannel>) => void/*, sharedWorkerChannelRejecter*/;

// This promise will resolve to an instance of the the SharedWorkerChannel.
// It is used by InPlace, running in the same javascript process as this proxy.
export const SharedWorkerChannelPromise: Promise<SharedWorkerChannel> = new Promise(
  function (resolve/*, reject*/)
  {
    sharedWorkerChannelResolver = resolve;
    // sharedWorkerChannelRejecter = reject;
  });


////////////////////////////////////////////////////////////////////////////////
//// PERSPECTIVESPROXY
////////////////////////////////////////////////////////////////////////////////
type UserMessageChannel = (message : string) => void;

export class PerspectivesProxy
{
  private static index : number = 0;

  channel: SharedWorkerChannel;
  cursor: Cursor;
  userMessageChannel?: UserMessageChannel;

  constructor (channel : SharedWorkerChannel)
  {
    this.channel = channel;
    this.cursor = new Cursor();
    this.getPDRStatus();
  }

  // Inform the server that this client shuts down.
  // No other requests may follow this message.
  close()
  {
    this.channel.close();
  }

  // Returns a promise for unsuscriber information of the form: {subject: req.subject, corrId: req.corrId}
  // that can be used by the caller to unsubscribe from updates.
  send (req: RequestRecord, receiveValues : valueReceiver, errorHandler? : errorHandler) : Promise<Unsubscriber>
  {
    const cursor = this.cursor;
    const proxy = this;
    req.trackingNumber = PerspectivesProxy.index++;
    // Handle errors here. Use `errorHandler` if provided by the PerspectivesProxy method.
    // Log errors to the console anyway for the developer.
    const handleErrors = function(response : Response) // response = PerspectivesApiTypes.ResponseRecord
    {
      // Restore cursor shape
      cursor.restore(req);
      if (response.responseType === "APIerror")
      {
        console.warn( "This request:\n" + JSON.stringify(req) + "\n results in this error: \n" + response.error );
        if (errorHandler)
        {
          if (response.warnings.length > 0)
          {
            errorHandler( response.error + "\n\n" + response.warnings );
          }
          errorHandler( response.error )
        }
      }
      else if (response.responseType === "APIresult")
      {
        if (response.warnings.length > 0)
        {
          if (proxy.userMessageChannel)
          {
            proxy.userMessageChannel( response.warnings.toString() );
          }
        }
        receiveValues(response.result);
      }
    };
    req.reactStateSetter = handleErrors;
    // Move all properties to the default request to ensure we send a complete request.
    const fullRequest = Object.assign( Object.assign({}, defaultRequest), req);

    // DEVELOPMENT ONLY: warn if any value is undefined
    // if ( Object.values(fullRequest).includes( undefined ) )
    // {
    //   console.warn( "Request misses values: " + JSON.stringify(fullRequest) );
    // }

    // Set cursor shape
    if ( !(req.request == "Unsubscribe") && !(req.request == "GetPDRStatusMessage") )
      {
        cursor.wait(req);
      }
    if (req.request === "Unsubscribe"){
      // If we had been waiting for a response, we can now restore the cursor shape.
      cursor.restore(req);
    }
    return this.channel.send( fullRequest );
  }

  // unsubscribe from the channel.
  unsubscribe (req : RequestRecord)
  {
    this.channel.unsubscribe(req);
  }

  setUserMessageChannel( channel : UserMessageChannel )
  {
    this.userMessageChannel = channel;
  }

  ///////////////////////////////////////////////////////////////////////////////////////
  //// GETTERS.
  //// Getters take a function to receive values in and a function to receive errors in.
  //// They return a value to unsubscribe from updates.
  //// Optionally, by providing the FIREANDFORGET value, one can unsubscribe a call 
  //// immediately.
  ///////////////////////////////////////////////////////////////////////////////////////

  // rolName must be qualified but may use default prefixes.
  getRol (contextID : ContextID, rolName : RolName, receiveValues : RoleReceiver, fireAndForget : SubscriptionType = false, errorHandler? : errorHandler)
  {
    return this.send(
      {request: "GetRol", subject: contextID, predicate: rolName, onlyOnce: fireAndForget},
      receiveValues,
      errorHandler);
  }

  getUnqualifiedRol (contextID : ContextID, localRolName : RolName, receiveValues : RoleReceiver, fireAndForget : SubscriptionType = false, errorHandler? : errorHandler)
  {
    return this.send(
      {request: "GetUnqualifiedRol", subject: contextID, predicate: localRolName, onlyOnce: fireAndForget},
      receiveValues,
      errorHandler);
  }

  getProperty (rolID : RoleInstanceT, propertyName : PropertyType, roleType  : RoleType, receiveValues : PropertyValueReceiver, fireAndForget : SubscriptionType = false, errorHandler? : errorHandler)
  {
    return this.send(
      {request: "GetProperty", subject: rolID, predicate: propertyName, object: roleType, onlyOnce: fireAndForget},
      receiveValues,
      errorHandler);
  }

  getPropertyFromLocalName (rolID : RoleInstanceT, propertyName : PropertyType, roleType  : RoleType, receiveValues : PropertyValueReceiver, fireAndForget : SubscriptionType = false, errorHandler? : errorHandler)
  {
    return this.send(
      {request: "GetPropertyFromLocalName", subject: rolID, predicate: propertyName, object: roleType, onlyOnce: fireAndForget},
      receiveValues,
      errorHandler
    );
  }

  getBinding (rolID : RoleInstanceT, receiveValues : RoleReceiver, fireAndForget : SubscriptionType = false, errorHandler? : errorHandler)
  {
    return this.send(
      {request: "GetBinding", subject: rolID, predicate: "", onlyOnce: fireAndForget},
      receiveValues,
      errorHandler);
  }

  // Note: this function is currently not in use.
  // The lexical context of the roleType can be used by providing the empty string
  // as argument for parameter contextType.
  getRoleBinders (rolID : RoleInstanceT, contextType : ContextType, roleType  : RoleType, receiveValues : RoleReceiver, fireAndForget : SubscriptionType = false, errorHandler? : errorHandler)
  {
    return this.send(
      {request: "GetRoleBinders", subject: rolID, predicate: roleType, object: contextType, onlyOnce: fireAndForget},
      receiveValues,
      errorHandler);
  }

  // getUnqualifiedRoleBinders (rolID : RoleInstance, localRolName, receiveValues)
  // {
  //   return this.send(
  //     {request: "GetUnqualifiedRoleBinders", subject: rolID, predicate: localRolName},
  //     receiveValues);
  // }

  // Returns an array of Role Types.
  getMeForContext (externalRoleInstance : RoleInstanceT, receiveValues : RoleTypeReceiver, fireAndForget : SubscriptionType = false, errorHandler? : errorHandler)
  {
    return this.send(
      {request: "GetMeForContext", subject: externalRoleInstance, onlyOnce: fireAndForget},
      receiveValues,
      errorHandler
    );
  }

  // For the user role type, get his perspectives in the context instance.
  getPerspectives (contextInstance : ContextInstanceT, userRoleType : UserRoleType, receiveValues : PerspectivesReceiver, fireAndForget : SubscriptionType = false, errorHandler? : errorHandler)
  {
    return this.send(
      { request: "GetPerspectives"
      , subject: userRoleType
      , object: contextInstance
      , onlyOnce: fireAndForget
      },
      function (perspectiveStrings)
      {
        return receiveValues(perspectiveStrings.map( JSON.parse ));
      },
      errorHandler
    );
  }

  /**
   * Retrieves a perspective based on the provided role instance or the perspective object role type. Returns the perspective of the role the user currently has in the context.
   *
   * @param roleInstanceOfContext - The role instance of the context.
   * @param perspectiveObjectRoleType - The role type of the perspective object. Defaults to an empty string. If not given, the perspective is on the role instance itself.
   * @param receiveValues - A callback function to receive the perspective values.
   * @param fireAndForget - A boolean indicating whether the request should be fire-and-forget. Defaults to false.
   * @param errorHandler - An optional error handler callback function.
   * @returns The unsubscriber.
   */
  getPerspective (roleInstanceOfContext : RoleInstanceT, perspectiveObjectRoleType : RoleType = "" as RoleType, receiveValues : PerspectivesReceiver, fireAndForget : SubscriptionType = false, errorHandler? : errorHandler)
  {
    return this.send(
      { request: "GetPerspective"
      , subject: perspectiveObjectRoleType
      , predicate: roleInstanceOfContext
      , onlyOnce: fireAndForget
      },
      function (perspectiveStrings)
      {
        return receiveValues(perspectiveStrings.map( JSON.parse ));
      },
      errorHandler
    );
  }

  /**
   * Retrieves a perspective based on the provided role instance or the perspective object role type. Returns the perspective of the provided user role type, iff the user actually has that role in it.
   *
   * @param roleInstanceOfContext - The role instance of the context.
   * @param perspectiveObjectRoleType - The role type of the perspective object. Defaults to an empty string. If not given, the perspective is on the role instance itself.
   * @param userRoleType  - The role type of the user that we want the perspective for. 
   * @param receiveValues - A callback function to receive the perspective values.
   * @param fireAndForget - A boolean indicating whether the request should be fire-and-forget. Defaults to false.
   * @param errorHandler - An optional error handler callback function.
   * @returns The unsubscriber.
   */
  getPerspectiveForUser (roleInstanceOfContext : RoleInstanceT, perspectiveObjectRoleType : RoleType = "" as RoleType, userRoleType: RoleType, receiveValues : PerspectivesReceiver, fireAndForget : SubscriptionType = false, errorHandler? : errorHandler)
  {
    return this.send(
      { request: "GetPerspectiveForUser"
      , subject: perspectiveObjectRoleType
      , predicate: roleInstanceOfContext
      , object: userRoleType
      , onlyOnce: fireAndForget
      },
      function (perspectiveStrings)
      {
        return receiveValues(perspectiveStrings.map( JSON.parse ));
      },
      errorHandler
    );
  }

  // { request: "GetScreen", subject: UserRoleType, predicate: ContextType, object: ContextInstance }
  getScreen(userRoleType : UserRoleType, contextInstance : ContextInstanceT, contextType : ContextType, receiveValues : ScreenReceiver, fireAndForget : SubscriptionType = false, errorHandler? : errorHandler)
  {
    return this.send(
      { request: "GetScreen"
      , subject: userRoleType
      , predicate: contextType
      , object: contextInstance
      , onlyOnce: fireAndForget
      },
      function (screenStrings)
      {
        return receiveValues(screenStrings.map( JSON.parse ));
      },
      errorHandler
    );
  }

  // { request: "GetTableForm", subject: UserRoleType, predicate: ContextInstance, object: RoleType }
  getTableForm( userRoleType : UserRoleType, contextInstance : ContextInstanceT, roleType  : RoleType, receiveValues : TableFormReceiver, fireAndForget : SubscriptionType = false, errorHandler? : errorHandler )
  {
    return this.send( 
      { request: "GetTableForm"
      , subject: userRoleType
      , predicate: contextInstance
      , object: roleType
      , onlyOnce: fireAndForget
      },
      function (tableValueStrings)
      {
        return receiveValues(tableValueStrings.map( JSON.parse ));
      },
      errorHandler
    )
  }

  getLocalRoleSpecialisation( localAspectName : string, contextInstance : ContextInstanceT, receiveValues : RoleTypeReceiver, fireAndForget : SubscriptionType = false, errorHandler? : errorHandler )
  {
    return this.send(
      { request: "GetLocalRoleSpecialisation"
      , subject: contextInstance
      , predicate: localAspectName
      , onlyOnce: fireAndForget},
      receiveValues,
      errorHandler
    );
  }

  getRoleName( rid : RoleInstanceT, receiveValues : valueReceiver, fireAndForget : SubscriptionType = false, errorHandler? : errorHandler )
  {
    this.send(
      { request: "GetRoleName"
      , object: rid
      , onlyOnce: fireAndForget
      }
      , receiveValues
      , errorHandler
    );
  }

  // We haven't made this promisebased because the binding can change, even though its type cannot.
  getBindingType (rolID : RoleInstanceT, receiveValues : RoleTypeReceiver, fireAndForget : SubscriptionType = false, errorHandler? : errorHandler)
  {
    return this.send(
      {request: "GetBindingType", subject: rolID, predicate: "", onlyOnce: fireAndForget},
      receiveValues,
      errorHandler);
  }

  /**
   * Returns a promise for an array having exactly one object, whose keys are indexed Context Names and whose values are the actual context identifiers.
   * @param name - The name to match against indexed Context Names. If the empty string, all indexed Context Names will be returned.
   * @param receiveValues - A function to receive the matched context identifiers.
   * @param fireAndForget - A boolean indicating whether to unsubscribe immediately.
   * @param errorHandler - A function to handle errors.
   * @returns A promise for an array of context identifiers.
   */
  matchContextName(name: string, receiveValues: (value: Record<string, ContextInstanceT>[]) => void, fireAndForget: SubscriptionType = false, errorHandler?: errorHandler) {
    return this.send(
      { request: "MatchContextName", subject: name, onlyOnce: fireAndForget },
      function (values) {
        receiveValues(values.map(JSON.parse));
      },
      errorHandler
    );
  }

  // Returns {roleInstance, firstname, lastname, avatar [OPTIONAL]}
  // rolID is the role that has the Chat properties; 
  // propertyId is one of the Chat properties (messages or media) (by construction it must be Enumerated)
  getChatParticipants( rolID : RoleInstanceT, propertyId : PropertyType, receiveValues : ((participants : ChatParticipantFields[]) => void), fireAndForget : SubscriptionType = false, errorHandler? : errorHandler)
  {
    return this.send(
      {request: "GetChatParticipants", subject: rolID, predicate: propertyId, onlyOnce: fireAndForget},
      (serialisedParticipants) => receiveValues( serialisedParticipants.map( JSON.parse ) ),
      errorHandler);
  }

    /**
   * Retrieves the selected role from the clipboard.
   *
   * This function sends a request to get the selected role from the clipboard
   * and returns a promise that resolves with the role instance.
   *
   * @returns {Promise<RoleInstanceT>} A promise that resolves with the selected role instance.
   */
    subscribeSelectedRoleFromClipboard( receiver: (roleOnClipboard : RoleOnClipboard[]) => void) : Promise<Unsubscriber>
    {
      const proxy = this;
      return proxy.send(
          { request: "GetSelectedRoleFromClipboard", onlyOnce: false },
          values => receiver( values.map( JSON.parse ) )
        );
    }
  

  /**
   * Retrieves the settings from the Perspectives system and processes them.
   *
   * @param receiveValues - A callback function that receives serialised perspectives on roles with settings as an array of objects.
   *                        The settings are provided as strings and are parsed using `JSON.parse`.
   * @param fireAndForget - An optional boolean indicating whether the request should be a "fire and forget" type.
   *                        Defaults to `false`.
   * @param errorHandler - An optional callback function to handle errors that may occur during the request.
   * 
   * @returns The result of the `send` method, which handles the request and response processing.
   */
  getSettings( receiveValues : PerspectivesReceiver, fireAndForget : SubscriptionType = false, errorHandler? : errorHandler)
  {
    return this.send(
      {request: "GetSettings", onlyOnce: fireAndForget},
      function (settingsStrings)
      {
        return receiveValues(settingsStrings.map( JSON.parse ));
      },
      errorHandler
    );
  }

  getWiderContexts( externalRoleInstance : RoleInstanceT, receiveValues : ContextAndNameReceiver, fireAndForget : SubscriptionType = false, errorHandler? : errorHandler)
  {
    return this.send(
      {request: "GetWiderContexts", subject: externalRoleInstance, onlyOnce: fireAndForget},
      function (contextAndNameStrings)
      {
        return receiveValues(contextAndNameStrings.map( JSON.parse ));
      },
      errorHandler
    );
  }
  
  getPDRStatus()
  {
    const component = this;
    // We don't unsubscribe this call.
    this.send(
      {request: "GetPDRStatusMessage", onlyOnce: false},
      status => component.cursor.setPDRStatus( status[0] ? JSON.parse( status[0] ) : {} ) );
  }

  ///////////////////////////////////////////////////////////////////////////////////////
  //// PROMISE RETURNING GETTERS.
  //// These getters, by their nature, return a result only once.
  ///////////////////////////////////////////////////////////////////////////////////////

    // checkBinding( <(QUALIFIED)RolName>, <binding>)
  // Where (local)RolName identifies the role in <contexttype> whose binding specification we want to compare with <binding>.
  // A version that returns a promise for a boolean value. NOTE: the promise can be fulfilled with `false`, meaning the binding cannot be made.
  // This is different then failure, meaning that something went wrong in computing.
  checkBindingP (roleName : RoleType, rolInstance : RoleInstanceT) : Promise<boolean>
  {
    const proxy = this;
    return new Promise(function(resolver, rejecter)
      {
        proxy.send(
          {request: "CheckBinding", predicate: roleName, object: rolInstance, onlyOnce: true}
          , (r => resolver(r[0] === "true"))
          , rejecter
        );
      });
  }


  // matchContextName( name )
  // {
  //   const proxy = this;
  //   return new Promise(function(resolver, rejecter)
  //     {
  //       proxy.send(
  //         {request: "MatchContextName", subject: name, onlyOnce: true},
  //         function(qualifiedNames)
  //         {
  //           resolver( qualifiedNames );
  //         },
  //         function(e){ rejecter( e )}
  //       );
  //     });
  // }

  getCouchdbUrl() : Promise<string>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        proxy.send(
          { request: "GetCouchdbUrl", onlyOnce: true },
          (r => resolver(r[0])),
          function(e){ rejecter( e )}
        );
      });
  }

  // { request: GetContextActions
  // , subject: RoleType // the user role type
  // , object: ContextInstance
  // }
  // Returns a promise for a stringified object with the actions that can be performed in the context.
  // The keys are the action names taken from the model; the values are their translations in the current language.
  getContextActions(myRoleType : UserRoleType, contextInstance : ContextInstanceT): Promise<ContextActions>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
    {
      proxy.send({ request: "GetContextActions", subject: myRoleType, object: contextInstance, onlyOnce: true }, 
        function( actionsArr )
        {
          if (actionsArr.length === 0)
          {
            resolver( {} );
          }
          else 
          {
            // The actions are stored in the first element of the array, as a stringified object.
            resolver( JSON.parse( actionsArr[0] ) );
          }
        },
        rejecter
        );
    })
  }

  
  /**
   * Retrieves the selected role from the clipboard.
   *
   * This function sends a request to get the selected role from the clipboard
   * and returns a promise that resolves with the role instance.
   *
   * @returns {Promise<RoleInstanceT>} A promise that resolves with the selected role instance.
   */
  getSelectedRoleFromClipboard() : Promise<RoleOnClipboard>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
    {
      proxy.send(
        { request: "GetSelectedRoleFromClipboard", onlyOnce: true },
        ((r : ValueT) => {
          if (r.length === 0) {
            rejecter("No role selected on the clipboard.");
          }
          else {
            resolver(JSON.parse( r[0]) );
          }
        }),
        rejecter
      );
    });
  }

  /**
   * Removes a role instance from the clipboard.
   *
   * @param roleInstance - The role instance to be removed from the clipboard.
   * @returns A promise that resolves to a boolean indicating whether the role instance was successfully removed.
   */
  removeRoleFromClipboard(roleInstance : RoleInstanceT) : Promise<boolean>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
    {
      proxy.send(
        { request: "RemoveRoleFromClipboard", subject: roleInstance, onlyOnce: true },
        (r => resolver(r[0] == "true")),
        rejecter
      );
    });
  }

  /**
   * NOTA BENE: WE MIGHT NOT NEED THIS FUNCTION IF WE CAN SWITCH SELECTED IN THE MODEL!!
   * CURRENTLY THERE IS NO API FUNCTION AddRoleToClipboard.
   * THIS FUNCTION IS MODELLED ON BIND
   * Low level interfact to add a role to the clipboard.
   * (Note: rather than introducing modeldependencies here, we use those in perspectives-react.
   * Further: the API function ensures that only this new item is selected.
   *
   * @param contextinstance - The context instance to create the role in (should be the system identifier).
   * @param localRolName - The qualified or local name of the role (should be ItemsOnClipboard).
   * @param contextType - The type of the context (should be PerspectivesSystem).
   * @param rolDescription - The description of the role (a RolSerialization).
   * @param myroletype - The type of the user role.
   * @returns A promise that resolves to the role instance.
   */
  addRoleToClipboard(contextinstance : ContextInstanceT, localRolName : RolName, contextType : ContextType, rolDescription : RolSerialization, myroletype : UserRoleType) : Promise<RoleInstanceT>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "AddRoleToClipboard", subject: contextinstance, predicate: localRolName, object: contextType, rolDescription: rolDescription, authoringRole: myroletype, onlyOnce: true },
          (r => resolver(r[0])),
          rejecter
        );
      });
  }

  getAllMyRoleTypes(externalRoleInstance : RoleInstanceT) : Promise<Record<UserRoleType, string>>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "GetAllMyRoleTypes", subject: externalRoleInstance, onlyOnce: true},
          (r : ValueT) => {
            if (r.length === 0) {
              resolver({});
            }
            else {
              resolver(JSON.parse(r[0]));
            }
          },
          rejecter
        );
      })
  }

  getViewProperties (rolType : RoleType, viewName : string) : Promise<PropertyType[]>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "GetViewProperties", subject: rolType, predicate: viewName, onlyOnce: true},
          resolver,
          rejecter);
        });
  }

  getContextType (contextID : ContextID) : Promise<ContextType>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "GetContextType", subject: contextID, predicate: "", onlyOnce: true},
          (r => resolver(r[0])),
          rejecter);
      });
  }

  getRolContext (rolID : RoleInstanceT) : Promise<ContextInstanceT>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
        {request: "GetRolContext", subject: rolID, predicate: "", onlyOnce: true},
        (r => resolver(r[0])),
        rejecter);
      });
  }

  getRolType (rolID : RoleInstanceT) : Promise<RoleType>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "GetRolType", subject: rolID, predicate: "", onlyOnce: true},
          (r => resolver(r[0])),
          rejecter);
      });
  }

  // RoleInContext | ContextRole | ExternalRole | UserRole | BotRole
  getRoleKind (rolID : RoleInstanceT) : Promise<RoleKind>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "GetRoleKind", subject: rolID, predicate: "", onlyOnce: true},
          (r => resolver(r[0])),
          rejecter);
      });
  }

  getUnqualifiedRolType (contextType : ContextType, localRolName : string) : Promise<[RoleType] | []>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "GetUnqualifiedRolType", subject: contextType, predicate: localRolName, onlyOnce: true},
          resolver,
          rejecter);
      });
  }

  getFile (roleInstance : RoleInstanceT, propertyName : PropertyType) : Promise<File>
  {
    const proxy = this;
    return new Promise( function( resolver, rejecter)
    {
      return proxy.send(
        { request: "GetFile", subject: roleInstance, predicate: propertyName, onlyOnce: true },
        (r => resolver(r[0])),
        rejecter
        );
    });
  }

  // Returns a promise for the pubic address of the context - if any.
  getPublicUrl (contextInstance : ContextInstanceT) : Promise<[string]|[]>
  {
    const proxy = this;
    return new Promise( function( resolver, rejecter )
    {
      return proxy.send(
        { request: "GetPublicUrl", subject: contextInstance, onlyOnce: true },
        resolver,
        rejecter
      );
    });
  }

    // The instance of model:System$PerspectivesSystem that represents the installation.
  getSystemIdentifier () : Promise<ContextInstanceT>
    {
      const proxy = this;
      return new Promise( function( resolver, rejecter )
      {
        return proxy.send(
          {request: "GetSystemIdentifier", onlyOnce: true},
          (r => resolver(r[0])),
          rejecter
        );
      })
    }

  // The instance of model:System$TheWorld$PerspectivesUsers that represents the natural person owning this installation in the Perspectives Universe.
  getPerspectivesUser () : Promise<RoleInstanceT>
    {
      const proxy = this;
      return new Promise( function( resolver, rejecter )
      {
        return proxy.send(
          {request: "GetPerspectivesUser", onlyOnce: true},
          (r => resolver(r[0])),
          rejecter
        );
      })
    }
  
  // The user role instance in the context of the given role instance that is ultimately filled by the PerspectivesUsers instance that represents
  // the natural person owning this installation in the Perspectives Universe.
  getMeInContext ( roleInstance : RoleInstanceT) : Promise<[RoleInstanceT] | []>
    {
      const proxy = this;
      return new Promise( function( resolver, rejecter )
      {
        return proxy.send(
          {request: "GetMeInContext", subject: roleInstance, onlyOnce: true},
          resolver,
          rejecter
        );
      })
    }
  
  isMe ( roleInstance : RoleInstanceT) : Promise<boolean>
    {
      const proxy = this;
      return new Promise( function( resolver, rejecter )
      {
        return proxy.send(
          {request: "IsMe", subject: roleInstance, onlyOnce: true},
          (r => resolver(r[0] === "true")),
          rejecter
        );
      })
    }

  // Returns a Promise for {accountName, password, storageType, sharedStorageId}
  getFileShareCredentials () : Promise<FileShareCredentials>
    {
      const proxy = this;
      return new Promise( function( resolver, rejecter )
      {
        return proxy.send(
          {request: "GetFileShareCredentials", onlyOnce: true},
          credentials => resolver(JSON.parse(credentials[0] ? credentials[0] : "{}")),
          rejecter
        );
      })
    }


///////////////////////////////////////////////////////////////////////////////////////
  //// SETTERS.
  //// Other than Getters, Setters change the Perspectives Universe.
  //// Setters return a promise that can succeed or fail. The return value may be symbolical
  //// of success or may relate to wat was created, for example.
  ///////////////////////////////////////////////////////////////////////////////////////
  
  // Create a context, bound to a new instance of <roleType> in <contextId>. <roleType> may be a local name.
  // The ctype in the contextDescription must be qualified, but it may use a default prefix.
  /// 
  // createContext( 
  //      <contextDescription>
  //    , <roleType>                      the qualified identifier of the role type to create.
  //    , <ContextIdToAddRoleInstanceTo>  the context instance to create the role instance in.
  //    , <myRoleType> 
  //    )
  // Either throws an error, or returns an array with
  //  - just a single string identifiying the external role of a DBQ role;
  //  - that string and a second that identifies the new context role otherwise.
  // So:  [<externalRoleId>(, <contextRoleId>)?]

  // object must be the type of the context to disambiguate the roleType name in.
  // subject must be the context instance to add a role instance to.
  // The first role instance in the array is the external role instance. The second is the context role instance.
  // NOTE: this function is not used in the current implementation.
  createContext (contextDescription : ContextSerializationRecord, roleType  : RoleType, contextIdToAddRoleInstanceTo : ContextInstanceT, myroletype  : UserRoleType) : Promise<[RoleInstanceT, RoleInstanceT] | [RoleInstanceT]>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "CreateContext", subject: contextIdToAddRoleInstanceTo, predicate: roleType, contextDescription: contextDescription, authoringRole: myroletype, onlyOnce: true},
          resolver,
          rejecter
          );
        });
  }

  // Create a context, bound to the given role instance.
  // createContext_( <contextDescription>, <roleinstance>, ...)
  // Either throws an error, or returns an array with a context identifier.
  createContext_ (contextDescription : ContextSerializationRecord, roleInstance: RoleInstanceT, myroletype  : UserRoleType) : Promise<ContextInstanceT>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "CreateContext_", subject: roleInstance, contextDescription: contextDescription, authoringRole: myroletype, onlyOnce: true},
          (r => resolver(r[0])), 
          rejecter
        );
    });
  }

  // Either throws an error, or returns an array of context identifiers.
  importContexts (contextDescription : ContextSerializationRecord) : Promise<ContextInstanceT[]>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "ImportContexts", contextDescription: contextDescription, onlyOnce: true},
          resolver, 
          rejecter
        );
        });
  }

  // Either throws an error, or returns an empty array.
  // Notice we re-use the contextDescription field.
  importTransaction (transaction : any) : Promise<[]>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "ImportTransaction", contextDescription: transaction, onlyOnce: true},
          resolver, 
          rejecter
          );
      });
  }

  // value is just a single string!
  setProperty (rolID : RoleInstanceT, propertyName  : PropertyType, value : ValueT, myroletype  : UserRoleType) : Promise<[]>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "SetProperty", subject: rolID, predicate: propertyName, object: value, authoringRole: myroletype, onlyOnce: true}
          , resolver
          , rejecter
        );
      });
  }

  addProperty (rolID : RoleInstanceT, propertyName  : PropertyType, value : ValueT, myroletype  : UserRoleType) : Promise<[]>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "AddProperty", subject: rolID, predicate: propertyName, object: value, authoringRole: myroletype, onlyOnce: true}
          , resolver
          , rejecter
        );
      });
  }

  // value is just a single string!
  // saveFile (PerspectivesFileShape, File, myroletype  : UserRoleType)
  saveFile( perspectivesFile : PerspectivesFile, file : File, myroletype  : UserRoleType) : Promise<PerspectivesFile>
  {
    const proxy = this;
    return file.arrayBuffer().then(
      function(buf)
      {
        // Because contextDescription is declared as a Foreign, we put the ArrayBuffer there.
        return new Promise(function (resolver, rejecter)
        {
          return proxy.send(
            {request: "SaveFile", subject: JSON.stringify( perspectivesFile ), contextDescription: buf, authoringRole: myroletype, onlyOnce: true}
            , fileInArray => resolver(fileInArray[0])
            , rejecter
            );
        });
      }
    );
  }

  deleteProperty (rolID : RoleInstanceT, propertyName  : PropertyType, myroletype  : UserRoleType) : Promise<[]>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "DeleteProperty", subject: rolID, predicate: propertyName, authoringRole: myroletype, onlyOnce: true},
          resolver, 
          rejecter
        );
      });
  }

  // { request: Action
  //   , predicate: <object of perspective role instance>
  //   , object: <context instance>
  //   , contextDescription:
  //   	  { perspectiveId:
  //   	  , actionName:
  //   	  }
  //   , authoringRole
  //   ...}
  action (objectRoleInstance : RoleInstanceT, contextInstance : ContextID, perspectiveId : string, actionName : string, authoringRole : string) : Promise<[]>
  {
    const proxy = this;
    const req = { request: "Action"
      , predicate: objectRoleInstance
      , object: contextInstance
      , contextDescription: { perspectiveId, actionName }
      , authoringRole
      , onlyOnce: true
      };
    if (objectRoleInstance)
    {
      req.predicate = objectRoleInstance;
    }
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          req,
          resolver,
          rejecter );
      });
  }

  // { request: ContextAction
  // , subject: RoleType // the user role type
  // , predicate: String // action identifier
  // , object: ContextId
  // }
  contextAction( contextid : ContextInstanceT, myRoleType : UserRoleType, actionName : string) : Promise<[]>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
        {request: "ContextAction", subject: myRoleType, predicate: actionName, object: contextid, authoringRole: myRoleType, onlyOnce: true },
        resolver,
        rejecter)
      });
  }

  removeBinding (rolID : RoleInstanceT, myroletype  : UserRoleType) : Promise<[]>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "RemoveBinding", subject: rolID, authoringRole: myroletype, onlyOnce: true},
          resolver,
          rejecter
        );
      });
  }

  
  /**
   * Removes a role instance.
   *
   * @param {RolName} rolName - The type of the role instance to be removed.
   * @param {RoleInstanceT} rolID - The ID of the role instance to be removed.
   * @param {UserRoleType} myroletype - The type of user role performing the removal.
   * @returns {Promise<[]>} A promise that resolves to an empty array upon successful removal.
   */
  removeRole (rolName : RolName, rolID : RoleInstanceT, myroletype  : UserRoleType) : Promise<[]>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "RemoveRole", subject: rolID, predicate: rolName, authoringRole: myroletype, onlyOnce: true},
          resolver,
          rejecter
          );
      });
  }

  //{request: "RemoveContext", subject: rolID, predicate: rolName, authoringRole: myroletype}
  // rolName must be qualified.
  // remove rolID.
  // rolName must be the authorized role type (the ContextRole type that is filled and that will be removed.)
  removeContext (rolID: RoleInstanceT, rolName : RolName, myroletype  : UserRoleType) : Promise<[]>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "RemoveContext", subject: rolID, predicate: rolName, authoringRole: myroletype, onlyOnce: true},
          resolver,
          rejecter
        );
      });
  }

  // Currently not used!
  deleteRole (contextID : ContextID, rolName : RolName, myroletype : UserRoleType) : Promise<[]>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "DeleteRole", subject: rolName, predicate: contextID, authoringRole: myroletype, onlyOnce: true},
          resolver,
          rejecter
        );
      });
  }

  /**
   * Binds a role to a context instance.
   *
   * @param contextinstance - The context instance to create the role in.
   * @param localRolName - The qualified or local name of the role.
   * @param contextType - The type of the context.
   * @param rolDescription - The description of the role (a RolSerialization).
   * @param myroletype - The type of the user role.
   * @returns A promise that resolves to the role instance.
   */
  bind (contextinstance : ContextInstanceT, localRolName : RolName, contextType : ContextType, rolDescription : RolSerialization, myroletype : UserRoleType) : Promise<RoleInstanceT>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "Bind", subject: contextinstance, predicate: localRolName, object: contextType, rolDescription: rolDescription, authoringRole: myroletype, onlyOnce: true },
          (r => resolver(r[0])),
          rejecter
        );
      });
  }

  bind_ (filledRole : RoleInstanceT, filler : RoleInstanceT, myroletype  : UserRoleType) : Promise<boolean>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "Bind_", subject: filledRole, object: filler, authoringRole: myroletype, onlyOnce: true},
          values => resolver( values[0] == "true" ),
          rejecter
        );
      });
  }

  // We have room for checkBinding_( <binder>, <binding>, [() -> undefined] )

  /**
   * Creates a role instance within a given context.
   *
   * @param contextinstance - The context instance in which the role is to be created.
   * @param rolType - The type of role to be created.
   * @param myroletype - The user role type for the authoring role.
   * @returns A promise that resolves to the created role instance.
   */
  createRole (contextinstance : ContextInstanceT, rolType : RoleType, myroletype  : UserRoleType) : Promise<RoleInstanceT>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "CreateRole", subject: contextinstance, predicate: rolType, authoringRole: myroletype, onlyOnce: true },
          (r => resolver(r[0])),
          rejecter
          );
      });
  }

  /**
   * Creates a role in a context from a RoleSerialization.
   *
   * @param contextinstance - The context instance to create the role in.
   * @param localRolName - The qualified or local name of the role.
   * @param contextType - The type of the context.
   * @param rolDescription - The description of the role (a RolSerialization).
   * @param myroletype - The type of the user role.
   * @returns A promise that resolves to the role instance.
   */
  createRole_ = this.bind;
  
  /**
   * Sets the preferred user role type for the context identified by a given external role ID.
   *
   * @param externalRoleId - The external role type identifier for which the preferred user role type is being set.
   * @param userRoleName - The user role type to be set as the preferred role.
   * @returns A promise that resolves to an empty array upon successful completion.
   */
  setPreferredUserRoleType( externalRoleId : RoleInstanceT, userRoleName : UserRoleType ) : Promise<[]>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "SetPreferredUserRoleType", subject: externalRoleId, object: userRoleName, onlyOnce: true},
          resolver,
          rejecter
        );
      });
  }

  save() : Promise<[]>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "Save", onlyOnce: true},
          resolver,
          rejecter
        );
      });
  }

  evaluateRoleState( rolinstance : RoleInstanceT) : Promise<[]>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "EvaluateRoleInstance", subject: rolinstance, onlyOnce: true},
          resolver,
          rejecter
        );
      });
  }


  /**
   * Restores the context for a specific user role within a given context instance.
   *
   * This function sends a request to restore the context for the specified user role
   * in the provided context instance. It returns a promise that resolves when the
   * operation is complete.
   * 
   * As a result, a transaction is built for the given user according to the perspectives of the user role.
   *
   * @param contextInstance - The context instance to restore for the user.
   * @param userRoleId - The role instance of the user for whom the context is being restored.
   * @param userRoleType - The type of the user role.
   * @returns A promise that resolves to an empty array upon successful restoration.
   */
  restoreContextForUser( contextInstance : ContextInstanceT, userRoleId : RoleInstanceT, userRoleType : RoleType) : Promise<[]>
  {
    const proxy = this;
    return new Promise(function (resolver, rejecter)
      {
        return proxy.send(
          {request: "RestoreContextForUser", subject: contextInstance, predicate: userRoleId, object: userRoleType, onlyOnce: true},
          resolver,
          rejecter
        );
      });
  }

}
export const FIREANDFORGET = true;
export const CONTINUOUS = false;


type SubscriptionType = boolean
type valueReceiver = (value: any) => void;
type errorHandler = (error: string) => void;
type ContextID = string;
type RolName = string;
type ExternalRoleType = string;


export type ContextSerializationRecord =
  { id? : string
  , prototype? : ContextID
  , ctype : ContextType
  , rollen: Record<RoleInstanceT, RolSerialization>
  , externeProperties : PropertySerialization
}

export type RolSerialization =
  { id? : string
  , properties : PropertySerialization
  , binding? : string
  }

export type PropertySerialization = { [key: string]: ValueT[] }

type ChatParticipantFields = {roleInstance : RoleInstanceT, firstname? : ValueT, lastname? : ValueT, avatar? : PSharedFile} // avatar will be a PSharedFile.

type ModeledActionName = string
type TranslatedActionName = string


////////////////////////////////////////////////////////////////////////////////
//// CURSOR HANDLING
////////////////////////////////////////////////////////////////////////////////
type Message = { identifier: string, text: string};

class Cursor {
  private static loadingOverlayElement: HTMLDivElement | null = null;
  private PDRStatus: string = "Processing..."
  private messages: Message[] = [];
  private queuePromise = Promise.resolve();
  
  constructor() {
    // Create the overlay element once
    if (!Cursor.loadingOverlayElement) {
      const overlay = document.createElement('div');
      overlay.className = 'pdr-loading-overlay';
      overlay.innerHTML = `
        <div class="pdr-spinner-container">
          <div class="pdr-spinner"></div>
          <div id="pdrstatus">${this.PDRStatus}</div>
        </div>
      `;
      overlay.style.display = 'none';
      document.body.appendChild(overlay);
      Cursor.loadingOverlayElement = overlay;
      
      // Add styles to your CSS or inject them here
      const style = document.createElement('style');
      style.textContent = `
        .pdr-loading-overlay {
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background-color: rgba(0, 0, 0, 0.3);
          display: flex;
          justify-content: center;
          align-items: center;
          z-index: 9999;
          transition: opacity 0.3s;
        }
        .pdr-spinner-container {
          background-color: white;
          padding: 20px;
          border-radius: 8px;
          text-align: center;
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
        }
        .pdr-spinner {
          border: 4px solid #f3f3f3;
          border-top: 4px solid #3498db;
          border-radius: 50%;
          width: 30px;
          height: 30px;
          animation: pdr-spin 1s linear infinite;
          margin: 0 auto 10px auto;
        }
        @keyframes pdr-spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
      `;
      document.head.appendChild(style);
    }
  }

  pushMessage(identifier: string, text: string) {
    this.messages.unshift({ identifier, text });
    this.setOverlayText(text);
    this.setOverlayVisibility(true);
    // For desktop:
    document.body.style.cursor = "wait";
    }
  
  removeMessage(identifier: string) {
    this.enqueue(() => {
      const index = this.messages.findIndex(msg => msg.identifier === identifier);
      if (index == 0)
      {
        this.messages.shift();
        if (this.messages.length > 0)
        {
          // Since we had at least two messages, we can safely assume the overlay is visible.
          this.setOverlayText( this.messages[0].text);
        }
        else
        {
          // No messages left, hide the overlay.
          this.setOverlayVisibility(false);
          // For desktop:
          document.body.style.cursor = "auto";
        }
      }
      if (index > 0)
      {
        this.messages.splice(index, 1);
      }
    });
  }

  // PDR status messages become active immediately.
  setPDRStatus({ action , message } : { action: "push" | "remove", message: string }) {
    if (action == "push")
    {
      this.pushMessage(message, message)
    }
    else if (action == "remove")
    {
      this.removeMessage(message);
    }
  }

  // Show the loading overlay if it is not already visible
  setOverlayVisibility(visible: boolean) {
    if (Cursor.loadingOverlayElement) {
      Cursor.loadingOverlayElement.style.display = visible ? 'flex' : 'none';
    }
  }

  setOverlayText(text: string) {
    this.PDRStatus = text;
    if (Cursor.loadingOverlayElement) {
      const statusElement = Cursor.loadingOverlayElement.querySelector('#pdrstatus');
      if (statusElement) {
        statusElement.textContent = text;
      }
    }
  }

  wait(request: RequestRecord) {
    const component = this;
    // Add the message to the top of the messages array.
    // But do not yet display it.
    const identifier = request.trackingNumber!.toString();
    this.enqueue(() => this.messages.unshift({ identifier, text: "Processing..." })
    );
    
    setTimeout(() => {
      component.enqueue(() => {
        const index = component.messages.findIndex(msg => msg.identifier === identifier);
        if ( index >= 0 )
        {
          // As the message is still waiting in the array, we can display it now.
          // Move it to the top of the array.
          const message = component.messages[index];
          component.messages.splice(index, 1);
          component.messages.unshift(message);
          // Display it.
          component.setOverlayText(message.text);
          component.setOverlayVisibility(true);
        }
      })}, 400);
  }
  
  restore(request: RequestRecord) {
    this.enqueue(() => {
      this.removeMessage(request.trackingNumber!.toString())
    });
  }

// Execute operations sequentially
  private enqueue<T>(operation: () => T | Promise<T>): Promise<T> {
    const result = this.queuePromise.then(() => operation());
    this.queuePromise = result.then(() => undefined, () => undefined) as Promise<void>; // Ensure queue continues even if operation fails
    return result;
  }
  }

// See: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
// We add "markdown"
export function mapRange( range : PRange ) : InputType
{
  switch (range) {
    case "PString":
      return "text";
    case "PBool":
      return "checkbox";
    case "PDateTime":
      return "datetime-local";
    case "PDate":
      return "date";
    case "PTime":
      return "time";
      case "PNumber":
      return "number";
    case "PEmail":
      return "email";
    case "PFile":
      return "file";
    case "PMarkDown":
      return "markdown"
  }
}



////////////////////////////////////////////////////////////////////////////////
//// TCP CHANNEL
////////////////////////////////////////////////////////////////////////////////
// class TcpChannel
// {
//   constructor (options)
//   {
//     let connection;
//     this.requestId = -1;
//     const valueReceivers = {};
//     // This creates a net.Socket (https://nodejs.org/api/net.html#net_net_createconnection).
//     this.connection = require("net").createConnection(
//       options,
//       // message will be in base64. Appending a string to it converts it to a new string.
//       function ()
//       {
//         console.log("Connection made.");
//       });
//     connection = this.connection;
//     this.valueReceivers = valueReceivers;
//
//     // See: https://nodejs.org/api/net.html#net_class_net_socket
//     connection.on('data',
//       // message will be in base64. Appending a string to it converts it to a new string.
//       function (message)
//       {
//         const messages = (message + "").split("\n");
//         messages.forEach( function(m) // m :: PerspectivesApiTypes.ResponseRecord
//         {
//           if (m !== "")
//           {
//             try
//             {
//               const responseRecord = JSON.parse(m);
//               valueReceivers[responseRecord.corrId](responseRecord);
//             }
//             catch(e)
//             {
//               console.log(e);
//             }
//           }
//         });
//       });
//
//     // https://nodejs.org/docs/latest-v6.x/api/net.html#net_event_error
//     // Emitted when an error occurs. The 'close' event will be called
//     // directly following this event.
//     connection.on('error',
//       function(error)
//       {
//         console.log( "Error on the connection: " + error );
//         // Half-closes the socket. i.e., it sends a FIN packet.
//         // It is possible the server will still send some data.
//         connection.end();
//       });
//
//     // https://nodejs.org/docs/latest-v6.x/api/net.html#net_event_close
//     // Emitted once the socket is fully closed. The argument had_error is a boolean
//     // which says if the socket was closed due to a transmission error.
//     connection.on('close',
//       function(had_error)
//       {
//         // No data will come anymore.
//         if ( had_error )
//         {
//           console.log("The Perspectives Core has hung up because of an error.");
//         }
//         else
//         {
//           console.log("The Perspectives Core has hung up.");
//         }
//       });
//
//       // https://nodejs.org/docs/latest-v6.x/api/net.html#net_event_end
//       // Emitted when the other end of the socket sends a FIN packet.
//       // By default (allowHalfOpen == false) the socket will destroy its file
//       // descriptor once it has written out its pending write queue.
//       connection.on('end',
//         function()
//         {
//           // This means the other side will no longer send data.
//           console.log("The Perspectives Core has hung up.");
//         });
//   }
//
//   nextRequestId ()
//   {
//     this.requestId = this.requestId + 1;
//     return this.requestId.toString();
//   }
//
//   // close will lead the messageProducer of the perspectives core to receive (Right unit).
//   close()
//   {
//     // https://nodejs.org/api/net.html#net_socket_end_data_encoding_callback
//     this.connection.end();
//     this.send = function()
//     {
//       throw( "This client has shut down!");
//     };
//   }
//
//   // req has the following format (taken from: module Perspectives.Api)
//   //   { request :: String
//   //   , subject :: String
//   //   , predicate :: String
//   //   , setterId :: ReactStateSetterIdentifier}
//   // type ReactStateSetterIdentifier = String
//   // Returns a structure that can be used by the caller to unsubscribe from the core dependency network.
//   send(req, receiveValues)
//   {
//     req.corrId = this.nextRequestId();
//     this.valueReceivers[ req.corrId ] = receiveValues;
//     // https://nodejs.org/api/net.html#net_socket_write_data_encoding_callback
//     this.connection.write(JSON.stringify(req) + "\n");
//     // return the elementary data for unsubscribing.
//     return {subject: req.subject, predicate: req.corrId};
//   }
//
//   unsubscribe(req)
//   {
//     delete this.valueReceivers[req.setterId];
//     // https://nodejs.org/api/net.html#net_socket_write_data_encoding_callback
//     this.connection.write(
//       {request: "Unsubscribe", subject: req.subject, predicate: req.predicate, setterId: req.setterId}
//     );
//   }
// }

// test2
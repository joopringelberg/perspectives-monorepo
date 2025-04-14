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
let pdrProxyResolver;
// This promise will resolve to an instance of PerspectivesProxy, with a SharedWorkerChannel that holds a port to the SharedWorker execution context,
// or to the page execution context that hosts the PDR.
// The proxy uses the channel to actually send requests to the core. These requests will
// turn up as 'output' of a Producer, ready to be consumed by some process.
// The channel uses the emit function as a callback: when it has a request to send, it calls 'emit'
// after wrapping the request in the appropriate constructor (usually the emitStep).
const PDRproxy = new Promise(function (resolve /*, reject*/) {
    pdrProxyResolver = resolve;
    //pdrProxyRejecter = reject;
});
function configurePDRproxy(channeltype, options) {
    let sharedWorkerChannel;
    switch (channeltype) {
        case "sharedWorkerChannel":
            const sharedWorker = new SharedWorker(new URL('perspectives-sharedworker', import.meta.url), { type: 'module' });
            //// Its port property is a MessagePort, as documented in https://developer.mozilla.org/en-US/docs/Web/API/MessagePort.
            sharedWorkerChannel = new SharedWorkerChannel(sharedWorker.port);
            sharedWorkerChannelResolver(sharedWorkerChannel);
            pdrProxyResolver(new PerspectivesProxy(sharedWorkerChannel));
            break;
        case "hostPageChannel":
            import('perspectives-core').then(pdr => {
                // pageHostingPDRPort returns a MessageChannel as documented here: https://developer.mozilla.org/en-US/docs/Web/API/MessagePort.
                sharedWorkerChannel = new SharedWorkerChannel(options.pageHostingPDRPort(pdr));
                sharedWorkerChannelResolver(sharedWorkerChannel);
                pdrProxyResolver(new PerspectivesProxy(sharedWorkerChannel));
            });
            break;
    }
}
const defaultRequest = {
    request: "WrongRequest",
    subject: "The original request did not have a request type!",
    predicate: "",
    object: "",
    reactStateSetter: function () { },
    corrId: 0,
    contextDescription: {},
    onlyOnce: false
};
////////////////////////////////////////////////////////////////////////////////
//// SHARED WORKER CHANNEL
//// This code will be executed by the client!
//// The SharedWorkerChannel is a proxy for the ServiceWorker for the client.
////////////////////////////////////////////////////////////////////////////////
class SharedWorkerChannel {
    port;
    requestId;
    valueReceivers;
    channelIdResolver;
    channelId;
    // port is a MessagePort as documented here: https://developer.mozilla.org/en-US/docs/Web/API/MessagePort.
    constructor(port) {
        const serviceWorkerChannel = this;
        this.requestId = -1;
        this.valueReceivers = {};
        this.channelIdResolver = undefined;
        this.channelId = new Promise(function (resolve /*, reject*/) {
            serviceWorkerChannel.channelIdResolver = resolve;
        });
        this.port = port;
        this.handleWorkerResponse = this.handleWorkerResponse.bind(this);
        this.port.onmessage = this.handleWorkerResponse;
    }
    // The sharedworker or pageworker sends messages of various types.
    // Among them are responses received by the core.
    //
    handleWorkerResponse(e) {
        if (e.data.responseType === "APIerror") {
            // {corrId: i, error: s} where s is is a String, i an int.
            // we just pass errors on.
            this.valueReceivers[e.data.corrId](e.data);
        }
        else if (e.data.responseType === "APIresult") {
            // {corrId: i, result: s} where s is an Array of String, i an int.
            // pass the result on
            this.valueReceivers[e.data.corrId](e.data);
        }
        // Then we have a category of incoming messages that originate in the service worker itself,
        // often in response to a specific request sent by the proxy.
        else if (e.data.responseType === "WorkerResponse") {
            // {serviceWorkerMessage: m, <field>: <value>} where m is a string. The object may contain any number of other fields, depending on the type of message (i.e. the value of m).
            switch (e.data.serviceWorkerMessage) {
                case "channelId":
                    // This actually is a response that is not provoked by explicitly asking for it.
                    // As soon as the SharedWorker receives a port from this proxy, it will return the channels id.
                    // {serviceWorkerMessage: "channelId", channelId: i} where i is a multiple of a million.
                    // Handle the port identification message that is sent by the service worker.
                    this.channelIdResolver(e.data.channelId);
                    break;
                case "pdrStarted":
                    this.valueReceivers.pdrStarted(e.data.pdrStarted);
                    break;
                case "isUserLoggedIn":
                    // {serviceWorkerMessage: "isUserLoggedIn", isUserLoggedIn: b} where b is a boolean.
                    this.valueReceivers.isUserLoggedIn(e.data.isUserLoggedIn);
                    break;
                case "resetAccount":
                    // {serviceWorkerMessage: "resetAccount", resetSuccesful: b} where b is a boolean.
                    this.valueReceivers.resetAccount(e.data.resetSuccesful);
                    break;
                case "reCreateInstances":
                    // {serviceWorkerMessage: "reCreateInstances", reCreateSuccesful: b} where b is a boolean.
                    this.valueReceivers.reCreateInstances(e.data.reCreateSuccesful);
                    break;
                case "recompileLocalModels":
                    // {serviceWorkerMessage: "recompileLocalModels", recompileSuccesful: b} where b is a boolean.
                    this.valueReceivers.recompileLocalModels(e.data.recompileSuccesful);
                    break;
                case "removeAccount":
                    // {serviceWorkerMessage: "removeAccount", removeSuccesful: b} where b is a boolean.
                    this.valueReceivers.removeAccount(e.data.removeSuccesful);
                    break;
                case "runPDR":
                    // {serviceWorkerMessage: "runPDR", error: e }
                    this.valueReceivers.runPDR(e);
                    break;
                case "createAccount":
                    // {serviceWorkerMessage: "createAccount", createSuccesful: {success :: Boolean, reason :: Nullable String}}.
                    this.valueReceivers.createAccount(e.data.createSuccesful);
                    break;
            }
        }
    }
    // This promise will resolve regardless of whether the PDR has started or not.
    pdrStarted() {
        const proxy = this;
        const p = new Promise(function (resolver /*, rejecter*/) {
            proxy.valueReceivers.pdrStarted = function (hasStarted) {
                proxy.valueReceivers.pdrStarted = undefined;
                resolver(hasStarted);
            };
        });
        proxy.channelId.then(channelId => proxy.port.postMessage({ proxyRequest: "pdrStarted", channelId }));
        return p;
    }
    // Returns a promise for a boolean value, reflecting whether the end user has logged in before or not.
    isUserLoggedIn() {
        const proxy = this;
        const p = new Promise(function (resolver /*, rejecter*/) {
            proxy.valueReceivers.isUserLoggedIn = function (isLoggedIn) {
                proxy.valueReceivers.isUserLoggedIn = undefined;
                resolver(isLoggedIn);
            };
        });
        proxy.channelId.then(channelId => proxy.port.postMessage({ proxyRequest: "isUserLoggedIn", channelId }));
        return p;
    }
    // runPDR :: UserName -> PouchdbUser RuntimeOptions -> Promise
    // Runs the PDR, if a value is returned it will be an error message.
    // {serviceWorkerMessage: "runPDR", startSuccesful: success }
    // {serviceWorkerMessage: "runPDR", error: e }
    runPDR(username, pouchdbuser, options) {
        const proxy = this;
        // We do not want to run the pdr twice.
        if (!this.valueReceivers.runPDR) {
            const p = new Promise(function (resolver, rejecter) {
                proxy.valueReceivers.runPDR = function (e) {
                    proxy.valueReceivers.runPDR = undefined;
                    if (e.error) {
                        rejecter(e.errormessage);
                    }
                    else {
                        resolver(e.data.startSuccesful);
                    }
                };
            });
            proxy.channelId.then(channelId => this.port.postMessage({ proxyRequest: "runPDR", username, pouchdbuser, options, channelId }));
            return p;
        }
        else {
            return Promise.resolve(false);
        }
    }
    createAccount(perspectivesUser, pouchdbuser, runtimeOptions, optionalIdentityDocument) {
        const proxy = this;
        const p = new Promise(function (resolver, rejecter) {
            // {success :: Boolean, reason :: Nullable String}
            proxy.valueReceivers.createAccount = function ({ success, reason }) {
                proxy.valueReceivers.createAccount = undefined;
                if (success) {
                    resolver(true);
                }
                else {
                    rejecter({ reason });
                }
            };
        });
        proxy.channelId.then(channelId => this.port.postMessage({ proxyRequest: "createAccount", perspectivesUser, pouchdbuser, channelId, runtimeOptions, identityDocument: optionalIdentityDocument ? optionalIdentityDocument : null }));
        return p;
    }
    resetAccount(username, pouchdbuser, options) {
        const proxy = this;
        const p = new Promise(function (resolver /*, rejecter*/) {
            proxy.valueReceivers.resetAccount = function (result) {
                proxy.valueReceivers.resetAccount = undefined;
                resolver(result);
            };
        });
        proxy.channelId.then(channelId => this.port.postMessage({ proxyRequest: "resetAccount", username, pouchdbuser, options, channelId }));
        return p;
    }
    reCreateInstances(pouchdbuser, options) {
        const proxy = this;
        const p = new Promise(function (resolver /*, rejecter*/) {
            proxy.valueReceivers.reCreateInstances = function (result) {
                proxy.valueReceivers.reCreateInstances = undefined;
                resolver(result);
            };
        });
        proxy.channelId.then(channelId => this.port.postMessage({ proxyRequest: "reCreateInstances", pouchdbuser, options, channelId }));
        return p;
    }
    recompileLocalModels(pouchdbuser) {
        const proxy = this;
        const p = new Promise(function (resolver /*, rejecter*/) {
            proxy.valueReceivers.recompileLocalModels = function (result) {
                proxy.valueReceivers.recompileLocalModels = undefined;
                resolver(result);
            };
        });
        proxy.channelId.then(channelId => this.port.postMessage({ proxyRequest: "recompileLocalModels", pouchdbuser, channelId }));
        return p;
    }
    removeAccount(username, pouchdbuser) {
        const proxy = this;
        const p = new Promise(function (resolver /*, rejecter*/) {
            proxy.valueReceivers.removeAccount = function (result) {
                proxy.valueReceivers.removeAccount = undefined;
                resolver(result);
            };
        });
        proxy.channelId.then(channelId => this.port.postMessage({ proxyRequest: "removeAccount", username, pouchdbuser, channelId }));
        return p;
    }
    // Inform the server that this client shuts down.
    // No other requests may follow this message.
    close() {
        // send a message that will make the internal channel in the Service Worker close.
        this.port.postMessage({ proxyRequest: "Close" });
    }
    unsubscribe(req) {
        // Send a message that will make the internal channel in the Service Worker close.
        this.port.postMessage({ proxyRequest: "unsubscribe", request: req });
    }
    nextRequestId() {
        const proxy = this;
        return this.channelId.then(function (channelId) {
            proxy.requestId = proxy.requestId + 1;
            return proxy.requestId + channelId;
        });
    }
    // Returns a promise for unsuscriber information of the form: {subject: req.subject, corrId: req.corrId}
    send(req) {
        const proxy = this;
        return this.nextRequestId().then(function (reqId) {
            const setter = req.reactStateSetter;
            // Create a correlation identifier and store it in the request.
            if (!req.corrId) {
                req.corrId = reqId;
            }
            // Store the valueReceiver.
            proxy.valueReceivers[req.corrId] = setter;
            // cannot serialise a function, remove it from the request.
            req.reactStateSetter = undefined;
            // console.log( req );
            // send the request through the channel to the service worker.
            proxy.port.postMessage(req);
            // return the elementary data for unsubscribing.
            return { request: "Unsubscribe", subject: req.subject, corrId: req.corrId };
        });
    }
}
let sharedWorkerChannelResolver /*, sharedWorkerChannelRejecter*/;
// This promise will resolve to an instance of the the SharedWorkerChannel.
// It is used by InPlace, running in the same javascript process as this proxy.
const SharedWorkerChannelPromise = new Promise(function (resolve /*, reject*/) {
    sharedWorkerChannelResolver = resolve;
    // sharedWorkerChannelRejecter = reject;
});
class PerspectivesProxy {
    channel;
    cursor;
    userMessageChannel;
    constructor(channel) {
        this.channel = channel;
        this.cursor = new Cursor();
    }
    // Inform the server that this client shuts down.
    // No other requests may follow this message.
    close() {
        this.channel.close();
    }
    // Returns a promise for unsuscriber information of the form: {subject: req.subject, corrId: req.corrId}
    // that can be used by the caller to unsubscribe from the core dependency network.
    send(req, receiveValues, errorHandler) {
        const cursor = this.cursor;
        const proxy = this;
        // Handle errors here. Use `errorHandler` if provided by the PerspectivesProxy method.
        // Log errors to the console anyway for the developer.
        const handleErrors = function (response) {
            // Restore cursor shape
            cursor.restore();
            if (response.responseType === "APIerror") {
                console.warn("This request:\n" + JSON.stringify(req) + "\n results in this error: \n" + response.error);
                if (errorHandler) {
                    if (response.warnings.length > 0) {
                        errorHandler(response.error + "\n\n" + response.warnings);
                    }
                    errorHandler(response.error);
                }
            }
            else if (response.responseType === "APIresult") {
                if (response.warnings.length > 0) {
                    if (proxy.userMessageChannel) {
                        proxy.userMessageChannel(response.warnings.toString());
                    }
                }
                receiveValues(response.result);
            }
        };
        req.reactStateSetter = handleErrors;
        // Move all properties to the default request to ensure we send a complete request.
        const fullRequest = Object.assign(Object.assign({}, defaultRequest), req);
        // DEVELOPMENT ONLY: warn if any value is undefined
        // if ( Object.values(fullRequest).includes( undefined ) )
        // {
        //   console.warn( "Request misses values: " + JSON.stringify(fullRequest) );
        // }
        // Set cursor shape
        cursor.wait();
        return this.channel.send(fullRequest);
    }
    // unsubscribe from the channel.
    unsubscribe(req) {
        this.channel.unsubscribe(req);
    }
    setUserMessageChannel(channel) {
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
    getRol(contextID, rolName, receiveValues, fireAndForget = false, errorHandler) {
        return this.send({ request: "GetRol", subject: contextID, predicate: rolName, onlyOnce: fireAndForget }, receiveValues, errorHandler);
    }
    getUnqualifiedRol(contextID, localRolName, receiveValues, fireAndForget = false, errorHandler) {
        return this.send({ request: "GetUnqualifiedRol", subject: contextID, predicate: localRolName, onlyOnce: fireAndForget }, receiveValues, errorHandler);
    }
    getProperty(rolID, propertyName, roleType, receiveValues, fireAndForget = false, errorHandler) {
        return this.send({ request: "GetProperty", subject: rolID, predicate: propertyName, object: roleType, onlyOnce: fireAndForget }, receiveValues, errorHandler);
    }
    getPropertyFromLocalName(rolID, propertyName, roleType, receiveValues, fireAndForget = false, errorHandler) {
        return this.send({ request: "GetPropertyFromLocalName", subject: rolID, predicate: propertyName, object: roleType, onlyOnce: fireAndForget }, receiveValues, errorHandler);
    }
    getBinding(rolID, receiveValues, fireAndForget = false, errorHandler) {
        return this.send({ request: "GetBinding", subject: rolID, predicate: "", onlyOnce: fireAndForget }, receiveValues, errorHandler);
    }
    // Note: this function is currently not in use.
    // The lexical context of the roleType can be used by providing the empty string
    // as argument for parameter contextType.
    getRoleBinders(rolID, contextType, roleType, receiveValues, fireAndForget = false, errorHandler) {
        return this.send({ request: "GetRoleBinders", subject: rolID, predicate: roleType, object: contextType, onlyOnce: fireAndForget }, receiveValues, errorHandler);
    }
    // getUnqualifiedRoleBinders (rolID : RoleInstance, localRolName, receiveValues)
    // {
    //   return this.send(
    //     {request: "GetUnqualifiedRoleBinders", subject: rolID, predicate: localRolName},
    //     receiveValues);
    // }
    // Returns an array of Role Types.
    getMeForContext(externalRoleInstance, receiveValues, fireAndForget = false, errorHandler) {
        return this.send({ request: "GetMeForContext", subject: externalRoleInstance, onlyOnce: fireAndForget }, receiveValues, errorHandler);
    }
    // For the user role type, get his perspectives in the context instance.
    getPerspectives(contextInstance, userRoleType, receiveValues, fireAndForget = false, errorHandler) {
        return this.send({ request: "GetPerspectives",
            subject: userRoleType,
            object: contextInstance,
            onlyOnce: fireAndForget
        }, function (perspectiveStrings) {
            return receiveValues(perspectiveStrings.map(JSON.parse));
        }, errorHandler);
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
    getPerspective(roleInstanceOfContext, perspectiveObjectRoleType = "", receiveValues, fireAndForget = false, errorHandler) {
        return this.send({ request: "GetPerspective",
            subject: perspectiveObjectRoleType,
            predicate: roleInstanceOfContext,
            onlyOnce: fireAndForget
        }, function (perspectiveStrings) {
            return receiveValues(perspectiveStrings.map(JSON.parse));
        }, errorHandler);
    }
    /**
     * Retrieves a perspective based on the provided role instance or the perspective object role type. Returns the perspective of the provided user role type, iff the user actually has that role in it.
     *
     * @param roleInstanceOfContext - The role instance of the context.
     * @param perspectiveObjectRoleType - The role type of the perspective object. Defaults to an empty string. If not given, the perspective is on the role instance itself.
     * @param receiveValues - A callback function to receive the perspective values.
     * @param fireAndForget - A boolean indicating whether the request should be fire-and-forget. Defaults to false.
     * @param errorHandler - An optional error handler callback function.
     * @returns The unsubscriber.
     */
    getPerspectiveForUser(roleInstanceOfContext, perspectiveObjectRoleType = "", userRoleType, receiveValues, fireAndForget = false, errorHandler) {
        return this.send({ request: "GetPerspectiveForUser",
            subject: perspectiveObjectRoleType,
            predicate: roleInstanceOfContext,
            object: userRoleType,
            onlyOnce: fireAndForget
        }, function (perspectiveStrings) {
            return receiveValues(perspectiveStrings.map(JSON.parse));
        }, errorHandler);
    }
    // { request: "GetScreen", subject: UserRoleType, predicate: ContextType, object: ContextInstance }
    getScreen(userRoleType, contextInstance, contextType, receiveValues, fireAndForget = false, errorHandler) {
        return this.send({ request: "GetScreen",
            subject: userRoleType,
            predicate: contextType,
            object: contextInstance,
            onlyOnce: fireAndForget
        }, function (screenStrings) {
            return receiveValues(screenStrings.map(JSON.parse));
        }, errorHandler);
    }
    // { request: "GetTableForm", subject: UserRoleType, predicate: ContextInstance, object: RoleType }
    getTableForm(userRoleType, contextInstance, roleType, receiveValues, fireAndForget = false, errorHandler) {
        return this.send({ request: "GetTableForm",
            subject: userRoleType,
            predicate: contextInstance,
            object: roleType,
            onlyOnce: fireAndForget
        }, function (tableValueStrings) {
            return receiveValues(tableValueStrings.map(JSON.parse));
        }, errorHandler);
    }
    getLocalRoleSpecialisation(localAspectName, contextInstance, receiveValues, fireAndForget = false, errorHandler) {
        return this.send({ request: "GetLocalRoleSpecialisation",
            subject: contextInstance,
            predicate: localAspectName,
            onlyOnce: fireAndForget }, receiveValues, errorHandler);
    }
    getRoleName(rid, receiveValues, fireAndForget = false, errorHandler) {
        this.send({ request: "GetRoleName",
            object: rid,
            onlyOnce: fireAndForget
        }, receiveValues, errorHandler);
    }
    // We haven't made this promisebased because the binding can change, even though its type cannot.
    getBindingType(rolID, receiveValues, fireAndForget = false, errorHandler) {
        return this.send({ request: "GetBindingType", subject: rolID, predicate: "", onlyOnce: fireAndForget }, receiveValues, errorHandler);
    }
    /**
     * Returns a promise for an array having exactly one object, whose keys are indexed Context Names and whose values are the actual context identifiers.
     * @param name - The name to match against indexed Context Names. If the empty string, all indexed Context Names will be returned.
     * @param receiveValues - A function to receive the matched context identifiers.
     * @param fireAndForget - A boolean indicating whether to unsubscribe immediately.
     * @param errorHandler - A function to handle errors.
     * @returns A promise for an array of context identifiers.
     */
    matchContextName(name, receiveValues, fireAndForget = false, errorHandler) {
        return this.send({ request: "MatchContextName", subject: name, onlyOnce: fireAndForget }, function (values) {
            receiveValues(values.map(JSON.parse));
        }, errorHandler);
    }
    // Returns {roleInstance, firstname, lastname, avatar [OPTIONAL]}
    // rolID is the role that has the Chat properties; 
    // propertyId is one of the Chat properties (messages or media) (by construction it must be Enumerated)
    getChatParticipants(rolID, propertyId, receiveValues, fireAndForget = false, errorHandler) {
        return this.send({ request: "GetChatParticipants", subject: rolID, predicate: propertyId, onlyOnce: fireAndForget }, (serialisedParticipants) => receiveValues(serialisedParticipants.map(JSON.parse)), errorHandler);
    }
    /**
   * Retrieves the selected role from the clipboard.
   *
   * This function sends a request to get the selected role from the clipboard
   * and returns a promise that resolves with the role instance.
   *
   * @returns {Promise<RoleInstanceT>} A promise that resolves with the selected role instance.
   */
    subscribeSelectedRoleFromClipboard(receiver) {
        const proxy = this;
        return proxy.send({ request: "GetSelectedRoleFromClipboard", onlyOnce: false }, values => receiver(values.map(JSON.parse)));
    }
    ///////////////////////////////////////////////////////////////////////////////////////
    //// PROMISE RETURNING GETTERS.
    //// These getters, by their nature, return a result only once.
    ///////////////////////////////////////////////////////////////////////////////////////
    // checkBinding( <(QUALIFIED)RolName>, <binding>)
    // Where (local)RolName identifies the role in <contexttype> whose binding specification we want to compare with <binding>.
    // A version that returns a promise for a boolean value. NOTE: the promise can be fulfilled with `false`, meaning the binding cannot be made.
    // This is different then failure, meaning that something went wrong in computing.
    checkBindingP(roleName, rolInstance) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            proxy.send({ request: "CheckBinding", predicate: roleName, object: rolInstance, onlyOnce: true }, (r => resolver(r[0] === "true")), rejecter);
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
    getCouchdbUrl() {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            proxy.send({ request: "GetCouchdbUrl", onlyOnce: true }, (r => resolver(r[0])), function (e) { rejecter(e); });
        });
    }
    // { request: GetContextActions
    // , subject: RoleType // the user role type
    // , object: ContextInstance
    // }
    // Returns a promise for a stringified object with the actions that can be performed in the context.
    // The keys are the action names taken from the model; the values are their translations in the current language.
    getContextActions(myRoleType, contextInstance) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            proxy.send({ request: "GetContextActions", subject: myRoleType, object: contextInstance, onlyOnce: true }, function (actionsArr) {
                if (actionsArr.length === 0) {
                    resolver({});
                }
                else {
                    // The actions are stored in the first element of the array, as a stringified object.
                    resolver(JSON.parse(actionsArr[0]));
                }
            }, rejecter);
        });
    }
    /**
     * Retrieves the selected role from the clipboard.
     *
     * This function sends a request to get the selected role from the clipboard
     * and returns a promise that resolves with the role instance.
     *
     * @returns {Promise<RoleInstanceT>} A promise that resolves with the selected role instance.
     */
    getSelectedRoleFromClipboard() {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            proxy.send({ request: "GetSelectedRoleFromClipboard", onlyOnce: true }, ((r) => {
                if (r.length === 0) {
                    rejecter("No role selected on the clipboard.");
                }
                else {
                    resolver(JSON.parse(r[0]));
                }
            }), rejecter);
        });
    }
    /**
     * Removes a role instance from the clipboard.
     *
     * @param roleInstance - The role instance to be removed from the clipboard.
     * @returns A promise that resolves to a boolean indicating whether the role instance was successfully removed.
     */
    removeRoleFromClipboard(roleInstance) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            proxy.send({ request: "RemoveRoleFromClipboard", subject: roleInstance, onlyOnce: true }, (r => resolver(r[0] == "true")), rejecter);
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
    addRoleToClipboard(contextinstance, localRolName, contextType, rolDescription, myroletype) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "AddRoleToClipboard", subject: contextinstance, predicate: localRolName, object: contextType, rolDescription: rolDescription, authoringRole: myroletype, onlyOnce: true }, (r => resolver(r[0])), rejecter);
        });
    }
    getAllMyRoleTypes(externalRoleInstance) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "GetAllMyRoleTypes", subject: externalRoleInstance, onlyOnce: true }, resolver, rejecter);
        });
    }
    getViewProperties(rolType, viewName) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "GetViewProperties", subject: rolType, predicate: viewName, onlyOnce: true }, resolver, rejecter);
        });
    }
    getContextType(contextID) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "GetContextType", subject: contextID, predicate: "", onlyOnce: true }, (r => resolver(r[0])), rejecter);
        });
    }
    getRolContext(rolID) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "GetRolContext", subject: rolID, predicate: "", onlyOnce: true }, (r => resolver(r[0])), rejecter);
        });
    }
    getRolType(rolID) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "GetRolType", subject: rolID, predicate: "", onlyOnce: true }, (r => resolver(r[0])), rejecter);
        });
    }
    // RoleInContext | ContextRole | ExternalRole | UserRole | BotRole
    getRoleKind(rolID) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "GetRoleKind", subject: rolID, predicate: "", onlyOnce: true }, (r => resolver(r[0])), rejecter);
        });
    }
    getUnqualifiedRolType(contextType, localRolName) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "GetUnqualifiedRolType", subject: contextType, predicate: localRolName, onlyOnce: true }, resolver, rejecter);
        });
    }
    getFile(roleInstance, propertyName) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "GetFile", subject: roleInstance, predicate: propertyName, onlyOnce: true }, (r => resolver(r[0])), rejecter);
        });
    }
    // Returns a promise for the pubic address of the context - if any.
    getPublicUrl(contextInstance) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "GetPublicUrl", subject: contextInstance, onlyOnce: true }, resolver, rejecter);
        });
    }
    // The instance of model:System$PerspectivesSystem that represents the installation.
    getSystemIdentifier() {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "GetSystemIdentifier", onlyOnce: true }, (r => resolver(r[0])), rejecter);
        });
    }
    // The instance of model:System$TheWorld$PerspectivesUsers that represents the natural person owning this installation in the Perspectives Universe.
    getPerspectivesUser() {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "GetPerspectivesUser", onlyOnce: true }, (r => resolver(r[0])), rejecter);
        });
    }
    // The user role instance in the context of the given role instance that is ultimately filled by the PerspectivesUsers instance that represents
    // the natural person owning this installation in the Perspectives Universe.
    getMeInContext(roleInstance) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "GetMeInContext", subject: roleInstance, onlyOnce: true }, resolver, rejecter);
        });
    }
    isMe(roleInstance) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "IsMe", subject: roleInstance, onlyOnce: true }, (r => resolver(r[0] === "true")), rejecter);
        });
    }
    // Returns a Promise for {accountName, password, storageType, sharedStorageId}
    getFileShareCredentials() {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "GetFileShareCredentials", onlyOnce: true }, credentials => resolver(JSON.parse(credentials[0] ? credentials[0] : "{}")), rejecter);
        });
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
    createContext(contextDescription, roleType, contextIdToAddRoleInstanceTo, myroletype) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "CreateContext", subject: contextIdToAddRoleInstanceTo, predicate: roleType, contextDescription: contextDescription, authoringRole: myroletype, onlyOnce: true }, resolver, rejecter);
        });
    }
    // Create a context, bound to the given role instance.
    // createContext_( <contextDescription>, <roleinstance>, ...)
    // Either throws an error, or returns an array with a context identifier.
    createContext_(contextDescription, roleInstance, myroletype) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "CreateContext_", subject: roleInstance, contextDescription: contextDescription, authoringRole: myroletype, onlyOnce: true }, (r => resolver(r[0])), rejecter);
        });
    }
    // Either throws an error, or returns an array of context identifiers.
    importContexts(contextDescription) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "ImportContexts", contextDescription: contextDescription, onlyOnce: true }, resolver, rejecter);
        });
    }
    // Either throws an error, or returns an empty array.
    // Notice we re-use the contextDescription field.
    importTransaction(transaction) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "ImportTransaction", contextDescription: transaction, onlyOnce: true }, resolver, rejecter);
        });
    }
    // value is just a single string!
    setProperty(rolID, propertyName, value, myroletype) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "SetProperty", subject: rolID, predicate: propertyName, object: value, authoringRole: myroletype, onlyOnce: true }, resolver, rejecter);
        });
    }
    addProperty(rolID, propertyName, value, myroletype) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "AddProperty", subject: rolID, predicate: propertyName, object: value, authoringRole: myroletype, onlyOnce: true }, resolver, rejecter);
        });
    }
    // value is just a single string!
    // saveFile (PerspectivesFileShape, File, myroletype  : UserRoleType)
    saveFile(perspectivesFile, file, myroletype) {
        const proxy = this;
        return file.arrayBuffer().then(function (buf) {
            // Because contextDescription is declared as a Foreign, we put the ArrayBuffer there.
            return new Promise(function (resolver, rejecter) {
                return proxy.send({ request: "SaveFile", subject: JSON.stringify(perspectivesFile), contextDescription: buf, authoringRole: myroletype, onlyOnce: true }, fileInArray => resolver(fileInArray[0]), rejecter);
            });
        });
    }
    deleteProperty(rolID, propertyName, myroletype) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "DeleteProperty", subject: rolID, predicate: propertyName, authoringRole: myroletype, onlyOnce: true }, resolver, rejecter);
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
    action(objectRoleInstance, contextInstance, perspectiveId, actionName, authoringRole) {
        const proxy = this;
        const req = { request: "Action",
            predicate: objectRoleInstance,
            object: contextInstance,
            contextDescription: { perspectiveId, actionName },
            authoringRole,
            onlyOnce: true
        };
        if (objectRoleInstance) {
            req.predicate = objectRoleInstance;
        }
        return new Promise(function (resolver, rejecter) {
            return proxy.send(req, resolver, rejecter);
        });
    }
    // { request: ContextAction
    // , subject: RoleType // the user role type
    // , predicate: String // action identifier
    // , object: ContextId
    // }
    contextAction(contextid, myRoleType, actionName) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "ContextAction", subject: myRoleType, predicate: actionName, object: contextid, authoringRole: myRoleType, onlyOnce: true }, resolver, rejecter);
        });
    }
    removeBinding(rolID, myroletype) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "RemoveBinding", subject: rolID, authoringRole: myroletype, onlyOnce: true }, resolver, rejecter);
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
    removeRole(rolName, rolID, myroletype) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "RemoveRole", subject: rolID, predicate: rolName, authoringRole: myroletype, onlyOnce: true }, resolver, rejecter);
        });
    }
    //{request: "RemoveContext", subject: rolID, predicate: rolName, authoringRole: myroletype}
    // rolName must be qualified.
    // remove rolID.
    // rolName must be the authorized role type (the ContextRole type that is filled and that will be removed.)
    removeContext(rolID, rolName, myroletype) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "RemoveContext", subject: rolID, predicate: rolName, authoringRole: myroletype, onlyOnce: true }, resolver, rejecter);
        });
    }
    // Currently not used!
    deleteRole(contextID, rolName, myroletype) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "DeleteRole", subject: rolName, predicate: contextID, authoringRole: myroletype, onlyOnce: true }, resolver, rejecter);
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
    bind(contextinstance, localRolName, contextType, rolDescription, myroletype) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "Bind", subject: contextinstance, predicate: localRolName, object: contextType, rolDescription: rolDescription, authoringRole: myroletype, onlyOnce: true }, (r => resolver(r[0])), rejecter);
        });
    }
    bind_(filledRole, filler, myroletype) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "Bind_", subject: filledRole, object: filler, authoringRole: myroletype, onlyOnce: true }, resolver, rejecter);
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
    createRole(contextinstance, rolType, myroletype) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "CreateRole", subject: contextinstance, predicate: rolType, authoringRole: myroletype, onlyOnce: true }, (r => resolver(r[0])), rejecter);
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
    setPreferredUserRoleType(externalRoleId, userRoleName) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "SetPreferredUserRoleType", subject: externalRoleId, object: userRoleName, onlyOnce: true }, resolver, rejecter);
        });
    }
    save() {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "Save", onlyOnce: true }, resolver, rejecter);
        });
    }
    evaluateRoleState(rolinstance) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "EvaluateRoleInstance", subject: rolinstance, onlyOnce: true }, resolver, rejecter);
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
    restoreContextForUser(contextInstance, userRoleId, userRoleType) {
        const proxy = this;
        return new Promise(function (resolver, rejecter) {
            return proxy.send({ request: "RestoreContextForUser", subject: contextInstance, predicate: userRoleId, object: userRoleType, onlyOnce: true }, resolver, rejecter);
        });
    }
}
const FIREANDFORGET = true;
const CONTINUOUS = false;
////////////////////////////////////////////////////////////////////////////////
//// CURSOR HANDLING
////////////////////////////////////////////////////////////////////////////////
class Cursor {
    wait() {
        document.body.style.cursor = "wait";
    }
    restore() {
        document.body.style.cursor = "auto";
    }
}
// See: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
// We add "markdown"
function mapRange(range) {
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
            return "markdown";
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

export { CONTINUOUS, FIREANDFORGET, PDRproxy, PerspectivesProxy, SharedWorkerChannelPromise, configurePDRproxy, mapRange };
//# sourceMappingURL=perspectives-proxy.js.map

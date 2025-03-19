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

// Notice that even though the method name "postMessage" equals that of Window.postMessage, here we deal
// with MessagePort.postMessage and ServiceWorker.postMessage. These methods have a different interface.
// See:
// https://developer.mozilla.org/en-US/docs/Web/API/Window/postMessage
// https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorker/postMessage
// https://developer.mozilla.org/en-US/docs/Web/API/MessagePort/postMessage

////////////////////////////////////////////////////////////////////////////////
//// STORING PORTS SENT BY CLIENT PAGES
////////////////////////////////////////////////////////////////////////////////
// An array of MessageChannel ports.
const channels = {};
let channelIndex = 1;

////////////////////////////////////////////////////////////////////////////////
//// PORT TO PAGE THAT HOSTS PDR
//// RECEIVE PORTS FROM CLIENTS WHEN RUN IN THE MAIN PAGE, RELAYED THROUGH A SERVICE WORKER
//// This function is passed on by the client in the call configurePDRProxy({pageHostingPDRPort: pageHostingPDRPort})
//// This function returns a MessagePort as documented here: https://developer.mozilla.org/en-US/docs/Web/API/MessagePort.
////////////////////////////////////////////////////////////////////////////////
export default function pageHostingPDRPort(pdr) {
  // Create a channel.
  const channel = new MessageChannel();
  let weHost = false;

  if ('serviceWorker' in navigator) {
    navigator.serviceWorker.register(
      'perspectives-pagedispatcher.js',
      {
        scope: './'
      }).then(function (registration) {
        var serviceWorker;
        if (registration.installing) {
          serviceWorker = registration.installing;
        } else if (registration.waiting) {
          serviceWorker = registration.waiting;
        } else if (registration.active) {
          serviceWorker = registration.active;
        }
        if (serviceWorker) {
          // Listen to messages coming in from the serviceWorker.
          // Notice that all pages that are not the first will never handle a message.
          // Notice that this handler is on the ServiceWorkerContainer of the page; NOT IN THE SERVICEWORKER ITSELF!
          navigator.serviceWorker.addEventListener('message', function (event) {
            switch (event.data.messageType) {
              case "youHost":
                // This message only arrives to the very first page visiting InPlace.
                weHost = true;
                // We've sent ourselves a port.
                channels[channelIndex] = event.data.port;
                // Return the channelIndex.
                channels[channelIndex].postMessage({ serviceWorkerMessage: "channelId", channelId: 1000000 * channelIndex });
                // start listening to the new channel, handle requests.
                // This page must host the PDR.
                channels[channelIndex].onmessage = request => pdr.handleClientRequest(pdr, channels, request);
                channelIndex = channelIndex + 1;
                break;
              case "relayPort":
                // If we are the host, save the port; otherwise ignore.
                if (weHost) {
                  // the new client (page) sends a port. This is a MessagePort.
                  channels[channelIndex] = event.data.port;
                  // Return the channelIndex.
                  channels[channelIndex].postMessage({ serviceWorkerMessage: "channelId", channelId: 1000000 * channelIndex });
                  // start listening to the new channel, handle requests.
                  channels[channelIndex].onmessage = request => pdr.handleClientRequest(pdr, channels, request);
                  channelIndex = channelIndex + 1;
                }
                break;
            }
          });
          if (navigator.serviceWorker.controller) {
            console.log("navigator heeft controler direct na registreren - stuur relayport nu.")
            navigator.serviceWorker.controller.postMessage({ messageType: "relayPort", port: channel.port2 }, [channel.port2]);
          }
        }
        else {
          console.log("Could not get serviceWorker from registration for an unknown reason.");
        }
        navigator.serviceWorker.addEventListener("controllerchange", () => {
          // Send the port to the serviceWorker, to relay it to the page hosting the PDR.
          // Only the serviceworker knows how many clients it has. If there is but one, it will immediately
          // return a "youhost" message to this listener, which will set 'wehost' to true. 
          // See: https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorker/postMessage
          console.log("Controller change. Stuur relayport nu.")
          navigator.serviceWorker.controller.postMessage({ messageType: "relayPort", port: channel.port2 }, [channel.port2]);
        });

      }).catch(function (error) {
        // Something went wrong during registration. The service-worker.js file
        // might be unavailable or contain a syntax error.
        console.log(error);
      });
  }
  else {
    console.log("This browser does not support service workers.");
  }
  // Use port1 in the SharedWorkerChannel.
  return channel.port1;
}

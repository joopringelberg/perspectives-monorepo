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
  let portTransferred = false;

  
  if ('serviceWorker' in navigator) {
    // This assumes perspectives-serviceworker is already registered with scope './'
    navigator.serviceWorker.ready.then(function(registration) {

        function sendPortToController() {
          // Only send if we have a controller
          if (navigator.serviceWorker.controller && !portTransferred) {
            console.log("Pageworker: sending port to controller");
            navigator.serviceWorker.controller.postMessage({ messageType: "relayPort", port: channel.port2 }, [channel.port2]);
            portTransferred = true;
          } else {
            console.log("Pageworker: no controller available yet");
          }
        }
     
      // Always use the active worker from the existing registration
      let serviceWorker = registration.active;
      if (serviceWorker) {
        console.log("Pageworker: using existing service worker");
        sendPortToController();
      } else {
        console.log("Pageworker: waiting for service worker to activate");
        // Wait for the service worker to be ready
        navigator.serviceWorker.addEventListener("controllerchange", () => {
          console.log("Pageworker: controller change detected, sending port");
          sendPortToController();
        });
      }
      // Listen to messages coming in from the serviceWorker. The serviceWorker is the central hub that passes messages
      // between the page hosting the PDR and the other pages.
      // Notice that all pages that are not the first will never handle a message.
      // Notice that this handler is on the ServiceWorkerContainer of EACH PAGE; NOT IN THE SERVICEWORKER ITSELF!

      // Notice that this listener is just used to shuffle ports between pages.
      // We establish a connection between the page that hosts the PDR and the page that
      // wants to use the PDR. That connection is a 'channel'.
      // It consists of two ports. One port is sent by the page that wants to use de PDR.
      // It is received and used by the page that hosts the PDR.
      // The other port is used by the page that wants to use the PDR.
      // Corrolary: this listener has nothing to do with Perspectives calls that are passed 
      // from client pages to the PDR!
      navigator.serviceWorker.addEventListener('message', function (event) {
        switch (event.data.messageType) {
          case "youHost":
            const hostingPage = event.data.port;
            hostingPage.start();
            // This message only arrives to the very first page visiting InPlace.
            // This page must host the PDR.
            weHost = true;
            // We've sent ourselves a port.
            channels[channelIndex] = hostingPage;
            // Return the channelIndex.
            console.log("Pageworker: we host the PDR. We send channelId to the page that hosts the PDR.")
            channels[channelIndex].postMessage({ responseType: "WorkerResponse", serviceWorkerMessage: "channelId", channelId: 1000000 * channelIndex });
            // start listening to the new channel, handle requests.
            // The page that has sent the port will send WorkerResponse messages and API calls
            hostingPage.onmessage = request => pdr.handleClientRequest(pdr, channels, request, 1000000 * channelIndex);
            // increment the index so we're ready for the next page that connects.
            channelIndex = channelIndex + 1;
            break;
          case "relayPort":
            // If we are the host, save the port; otherwise ignore.
            if (weHost) {
              // Notice how this section is exactly the same as the one in the onconnect handler of the SharedWorker.
              const connectionToAPage = event.data.port;
              connectionToAPage.start();
              // the new client (page) sends a port. This is a MessagePort.
              channels[channelIndex] = connectionToAPage;
              // Return the channelIndex.
              console.log("Pageworker: we are not the host. We send channelId to the page that wants to use the PDR.")
              channels[channelIndex].postMessage({ responseType: "WorkerResponse", serviceWorkerMessage: "channelId", channelId: 1000000 * channelIndex });
              // start listening to the new channel, handle requests.
              connectionToAPage.onmessage = request => pdr.handleClientRequest(pdr, channels, request, 1000000 * channelIndex);
              // increment the index so we're ready for the next page that connects.
              channelIndex = channelIndex + 1;
            }
            break;
        }
      });
    }).catch(function (error) {
        // Something went wrong during registration. The service-worker.js file
        // might be unavailable or contain a syntax error.
        console.log("Error using perspectives-service worker for port relay in page setup", error);
      });
  }
  else {
    console.log("This browser does not support service workers.");
  }
  // Use port1 in the SharedWorkerChannel.
  return channel.port1;
}

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

////////////////////////////////////////////////////////////////////////////////
//// SERVICE WORKER
////////////////////////////////////////////////////////////////////////////////
// Notice that this listener is just used to shuffle ports between pages.
// We establish a connection between the page that hosts the PDR and the page that
// wants to use the PDR. That connection is a 'channel'.
// It consists of two ports. One port is sent by the page that wants to use de PDR.
// It is received and used by the page that hosts the PDR.
// The other port is used by the page that wants to use the PDR.
// Corrolary: this listener has nothing to do with Perspectives calls that are passed 
// from client pages to the PDR!
self.addEventListener('message', function(event)
  {
    self.clients.matchAll()
      .then(function(clientList) {
        switch (event.data.messageType ){

          case "relayPort":
            // If there is but one client, return a message immediately.
            if (clientList.length == 1)
            {
              // Return the port sent by the first page. It will communicate with itself through it.
              clientList[0].postMessage({ "messageType": "youHost", port: event.data.port }, [event.data.port]);
            }
            else
            {
              clientList.forEach(function(client)
              {
                // Send to all pages except for the sender.
                if (client.id === event.source.id)
                {
                  return;
                }
              else
                {
                  client.postMessage( event.data, [event.data.port] );
                }
              });
            }
            break;
          }
      })
      .catch(function( error )
      {
        console.log( "Failing in service worker:" + error );
      });
});

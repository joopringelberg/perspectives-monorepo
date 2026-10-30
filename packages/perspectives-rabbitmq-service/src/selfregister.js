// BEGIN LICENSE
// Perspectives Distributed Runtime
// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later
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
// Full text of this license can be found in the LICENSE directory in the projects root.

// END LICENSE

const http = require("http");

var argv = require('yargs/yargs')(process.argv.slice(2)).parse();
const host = 'localhost';
const port = argv.port;
const rabbitHost = argv.rabbithost || 'localhost'
const rabbitPort = argv.rabbitport;
const admin = argv.admin;
const adminPassword = argv.adminpassword;
const maxUsers = argv.maxusers;
const winstonlevel = argv.level || 'info'
const vhost = "mycontexts";
const usersEndpoint = "api/users/";
const permissionsEndpoint = "api/permissions/";

// const levels = {
//     error: 0,
//     warn: 1,
//     info: 2,
//     http: 3,
//     verbose: 4,
//     debug: 5,
//     silly: 6
//   };
// All messages equal to or lower than the provided level end up in the logfiles.
const logger = require('./winstonlogger.js')(winstonlevel);


// This listener is applied to all requests at the /rbsr endpoint.
const requestListener = function (req, res) {

    // Collect the payload.
    const chunks = [];
    
    req.on('data', function (chunk) {
        chunks.push(chunk);
        })

    req.on('end', function() {
        const result = Buffer.concat(chunks).toString();
        let payload
        try 
        {
            logger.verbose( `requestListener with payload to parse: ${result}` );
            payload = JSON.parse(result);
        } 
        catch (ex) 
        {
            logger.error(`Error in requestListener: bad JSON. Expected { userName, password, queueName }, got: ${result}. Error: ${ex}`);
            res.writeHead(400);
            return res.end(`Bad JSON. Expected { userName, password, queueName }, got: ${result}.`);
        }

        if ( payload && payload.userName && payload.password && payload.queueName)
        {
            res.setHeader("Content-Type", "application/json");
            res.writeHead(200);
            // Now that we have the required data, proceed to create an account and set its permissions, but...
            checkNumberOfAccounts()
                // ... only when the set maximum has not been exceeded.
                .then( () =>
                    createAccount( payload.userName, payload.password )
                        .then( () => setPermissions( payload.userName, payload.queueName ))
                        .then( () => res.end( JSON.stringify({succeeded: true}) ))
                        .catch( e => res.end( JSON.stringify( {succeeded: false, error: e}))) )
        }
        else
        {
            res.writeHead(400);
            return res.end(`Missing elements. Expected { userName, password, queueName }, got userName=${payload.userName}, password=${payload.password}, queueName=${payload.queueName}.`);
        }
    });
}

const server = http.createServer(requestListener);
server.listen(port, host, () => {
    logger.info(`Server is running on http://${host}:${port}`);
});

function checkNumberOfAccounts()
{
    logger.verbose('Entering checkNumberOfAccounts.');
    return getCurrentNumberOfUsers().then( n => {
        return new Promise((resolve, reject) => 
        {
            if (n < maxUsers)
            {
                logger.verbose('Number of accounts is less then maximum: ' + n);
                resolve( true );
            }
            else
            {
                logger.warn('Maximum number of users reached, no new accounts possible.');
                reject( "Maximum number of users reached, no new accounts possible.");
            }})
    })
}

function getCurrentNumberOfUsers()
{
    logger.verbose( `Entering getCurrentNumberOfUsers.`);
    return new Promise((resolve, reject) => {
        const options = {
            hostname: rabbitHost,
            port: rabbitPort,
            path: `/${usersEndpoint}`,
            method: 'GET',
            auth: `${admin}:${adminPassword}`,
            };

        const req = http.request(options, (res) => {
            const chunks = [];
            let users;
        
            res.on("data", (chunk) => {
                chunks.push(chunk);
            });
        
            res.on("end", () => {
                try {
                    users = JSON.parse( Buffer.concat(chunks).toString() );
                }
                catch (e){
                    reject( `Error in getCurrentNumberOfUsers: incorrect JSON returned from RabbitMQ: ${e}.` );
                }
                logger.info( `Currently, we have ${users.length} users.`)
                resolve( users.length );
            });
        });
        
        req.on("error", (error) => {
            logger.error( `Error in getCurrentNumberOfUsers: ${error.message ? error.message : error.toString()}.`);
            reject(error);
            });
        req.end();
    })
}

// Returns a string. Sets the permissions necessary for the user to create and access his queue
function setPermissions( userName, queueName )
{
    const data = JSON.stringify( 
        { configure: queueName
        , write: queueName + "|amq\\.topic"
        , read: queueName + "|amq\\.topic"
        } );
    logger.verbose( `Entering setPermissions with {data}.`);
    return new Promise((resolve, reject) => {
        const options = {
            hostname: rabbitHost,
            port: rabbitPort,
            path: `/${permissionsEndpoint}${vhost}/${userName}`,
            method: 'PUT',
            auth: `${admin}:${adminPassword}`,
            headers: {
                'Content-Length': Buffer.byteLength(data),
                'Content-Type': 'application/json',
            }
          };

          const req = http.request(options, (res) => {
            const chunks = [];
        
            res.on("data", (chunk) => {
              chunks.push(chunk);
            });
        
            res.on("end", () => {
                const result = Buffer.concat(chunks).toString();
                logger.verbose( `setPermissions: result from RabbitMQ call is: ${result}.` );
                resolve( result );
            });
          });
        
          req.on("error", (error) => {
            logger.error( `Error in call from setPermissions to RabbitMQ:  ${error.message ? error.message : error.toString()}` );
            reject(error);
          });
          req.write( data );
          req.end();
    })
}

// Returns a string.
function createAccount( userName, password )
{
    const data = JSON.stringify( {password, tags: ""} );
    logger.verbose( `Entering createAccount with: {data}.`);
    return new Promise((resolve, reject) => {
        const options = {
            hostname: rabbitHost,
            port: rabbitPort,
            path: `/${usersEndpoint}${userName}`,
            method: 'PUT',
            auth: `${admin}:${adminPassword}`,
            headers: {
                'Content-Length': Buffer.byteLength(data),
                'Content-Type': 'application/json',
            }
          };

          const req = http.request(options, (res) => {
            const chunks = [];
        
            res.on("data", (chunk) => {
              chunks.push(chunk);
            });
        
            res.on("end", () => {
                const result = Buffer.concat(chunks).toString();
                logger.verbose( `createAccount: ${result}.`)
                resolve( result );
            });
          });
        
          req.on("error", (error) => {
            logger.error( `Error in createAccount: ${error.message ? error.message : error.toString()}`);
            reject(error);
          });
          req.write( data );
          req.end();
    })
}

# perspectives-sharedfilestorage
A relay service to store media files in a cloud service, that can be called from the MyContexts client.

If the users of two MyContexts installations want to share a media file, they cannot exchange that file through the message broker service they use to synchronise role- and context data. Instead, such files must be shared _by reference_. The reference - also called **claim data** - is synchronised as a property. The media file itself (that can be 'claimed') must be stored in a location that is accessible to both parties. Here we call that a 'cloud service'. The _distributed architecture_ of the MyContexts collaboration environment, however, precludes a cloud service that functions as a server in the middle. Instead, each user should have write access to a (personal) cloud that allows her peers read access to the files she wants to share. Consequently, each user should add credentials for that personal cloud service. 

The Perspectives Project recommends using the Mega cloud (https://mega.nz). It provides a generous free entry level account. In fact, version 0.24.0 of MyContexts just supports Mega and no other cloud services. In the future, we may add support for other services.

However, to give new users a head start and a feel for what it is to share media files through MyContexts, we provide each new user with a 'courtesy' number of uploads to a cloud service that is maintained by the Perspectives Project. We call that storage PPSharedFileStorage. This, too, is based on a Mega account.

Each installation would require the credentials of that account. This clearly is undesirable. Therefore we maintain this small service that acts as a relay server for new MyContexts installations. They upload their files to this service, which relays them to the Mega cloud. This is the `perspectives-sharedfilestorage` package whose README you are reading right now.

The MyContexts client limits the number of free uploads. However, we also have the relay service limit the number of uploads. Furthermore, we would like to make sure that only MyContexts installations use this service. This is not entirely possible due to the distributed nature of MyContexts. Instead, we base the usage of the service on the principle of the trusted network that is formed by MyContexts peers. Assuming the bona fide nature of any peer within the network, we ensure that a new user can only use perspectives-sharedfilestorage when spoken for by a peer who is already in the network. This is accounted for by some modelling in `model://perspectives.domains#System`. On the side of the service this requires two endpoints that each require "multipart/form-data":

* `/ppsfs/getsharedfileserverkey`, requires a JSON content body with parameter `sharedfileserverkey` (a previously provided key), that returns a key (a CUID2 identifier);
* `/ppsfs/uploadfile`, requires "multipart/form-data" with parameters `sharedfileserverkey` and `file`, that returns a success or failure message in case the maximum number of uploads for the key has been reached before.

Clarification: the key that is sent along with `getsharedfileserverkey` is the key of a peer already in the trusted network; the key that is sent back is a new key that is given to a new peer. Obviously, `uploadfile` requires a peer's own key.

There is another endpoint: 

* `/ppsfs/stop`, requires "multipart/form-data" with parameter `pw` whose value should equal the password of the mega account behind the relay service (the value of the `--password` command line parameter).

## Apache
Apache must be configured to pass any request to these endpoints to the locally listening `perspectives-sharedfilestorage`. Here is a suitable Apache conf section:

```
    <Location "/ppsfs>
      ProxyPass http://localhost:15673/ppsfs
    </Location>
```

Notice that the given port should be the port configured with runtime parameter `--port` (in this example taken to be 15673).

## Parameters of the service
The parameter `--port` gives the port on which the service should listen.

The parameter `--maxfiles` gives the maximum number of files any user can upload.

The parameter `--maxkeys` gives the maximum number of keys any keyholder can request.

The parameter `--userid` gives the name of the Mega account that is used to access the cloud, while `--password` gives the corresponding password. Obviously, these two credentials are never shared with clients!

The parameter `--statefile` gives the path to the .json file that holds the keys that have been given out previously. Every 10 seconds the service checks whether changes have been made and if so, it saves the state in the file.

To access the server, create a POST request and send a payload consisting of this object:

```
{ key :: String, file :: File Object}
```

## Installing
Install using npm:

```
npm install git+https://github.com/joopringelberg/perspectives-sharedfilestorage.git
```

## Starting the server manually
An example of starting the service manually can be found in the shell script `startservice.sh`.

## Making the service a deamon with pm2
To make sure that the service is restarted after system boot, install [pm2](https://www.npmjs.com/package/pm2) on the server. Then start this service through pm2 like this:

```
pm2 start sharedfilestorage.js -- --port=15673 --maxfiles=10 --maxkeys=100 --userid=user@example.com --password=secret --statefile=providedkeys.json
```

## Developing
This package now lives in the `perspectives-monorepo` at `packages/perspectives-sharedfilestorage`.

Copy `startService.example.sh` to `startService.sh` (gitignored) and `providedkeys.example.json` to `providedkeys.json` (gitignored), fill in real Mega account credentials, then start the service from the package directory using

```
./startService.sh
```

To test locally, use curl:
```
curl -X POST http://localhost:15673/ppsfs/getsharedfileserverkey \
-H "Content-Type: application/json" \
-d '{"sharedfileserverkey":"a-previously-provided-key"}'

curl -X POST http://localhost:15673/ppsfs/uploadfile \
-F "sharedfileserverkey=a-previously-provided-key" \
-F "file=@./smallFlower.png"

curl -X POST http://localhost:15673/ppsfs/stop \
-F "pw=password-of-mega-account"
```

To debug in VScode, create a Node debug configuration like this:

```
{
  "version": "0.2.0",
  "configurations": [
    {
      "name": "Attach by Process ID",
      "processId": "${command:PickProcess}",
      "request": "attach",
      "skipFiles": [
        "<node_internals>/**"
      ],
      "type": "node"
    }
  ]
}
```

Start the process and then attach the debugger.

## Install (on mycontexts.com)
Clone using git:

```
git clone https://github.com/joopringelberg/perspectives-sharedfilestorage.git
```

Then start the service on a free port.

When the source has been changed, update it on the server like this:

```
git pull origin
```

## Test on mycontexts.com
Test with curl like this:

```
curl -X POST https://mycontexts.com/ppsfs/getsharedfileserverkey \
-H "Content-Type: application/json" \
-d '{"sharedfileserverkey":"a-previously-provided-key"}'
```
## How to initialize the service to (re)start it
On requesting a key, an installation should provide its own key. This key must be known to the service in the sense that it is in the state file that serves as persistent storage of all requested keys. Hence, to start the service, initialize it to a value like this:

```
{
	"rootKey": {
		"nrOfUploadedFiles": 0,
		"nrOfRequestedKeys": 0
	}
}

```

where `rootKey` obviously should be some unique identifier. This identifier should be the value of property `SharedFileServerKey` of the instance of `PerspectivesUsers` that owns the installation in the Universe that is the first to hand out keys to other users. A convenient starting installation is the one that manages a BrokerService, as each new user will connect to this installation first - and it will then hand out a key.

There is no 
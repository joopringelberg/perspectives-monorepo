# perspectives-apitypes
Types to use in Purescript programs that communicate with Perspectives-core over TCP. The module also defines a serialization format of contexts, roles and properties that is used to create Contexts and Roles over the API.

This module is used in:
* [perspectives-core](https://github.com/joopringelberg/perspectives-core)
* [perspectives-arc-languageserver](https://github.com/joopringelberg/perspectives-arc-languageserver)

## Installation
Install with npm **(Not yet available)**:

```
$ npm install perspectives-apitypes
```

## Use the types in Purescript
The module `Perspectives.ApiTypes` defines a Reqest and a Response type. These are simple records that are serialized prior to exchanging them over the TCP channel. The type `RequestType` enumerates the allowed type of requests, such as `GetRol`, `GetRolBinding`, etc.

## Context- and Role serialisation
These types are simpler versions of PerspectContext and PerspectRol as defined in the Core program. They cannot be put into Couchdb but are used to transport created contexts and roles through the API to the PDR.

Example:
```
{ "id": "myContext"
, "ctype": "myContextType"
, "rollen": { Role1:  [ { "properties": { "prop1": "1", "prop2": "two" }, "binding": "usr:Me" }
                      , { "properties": {}, "binding": "yetAnotherRole" }  ]}
, "interneProperties": {iprop1: "2"}
, "externeProperties": {}
}
```

1.  Default namespace prefixes are expanded.
2.  Default indexed names are expanded.

|Prefix|Namespace|
|---|---|
|sys|model:System|
|usr|model:User|
|cdb|model:Couchdb|

|Indexed Name|Expansion|
|---|---|
|usr:Me|model:User$MijnSysteem$User_0001|

## Dependency management
See [Publishing a new version](https://github.com/joopringelberg/perspectives-core/blob/master/technical%20readme.md#publishing-a-new-version) in the Perspectives Core (PDR) project.

## Publish new package version:
1. In spago.yaml: update the version of `perspectives-utilities` and `serializablenonemptyarray` at `ref`
2. outcomment their `path` sections
3. incomment their `git` and `ref` sections
4. In package.json: increase the package number
5. Commit
6. Create tag
7. Push tag
8. In spago.yaml: switch back to the local versions.
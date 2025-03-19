# perspectives-react

React components to build a GUI based on the perspectives-protocol (hides connection to perspectives-core).

## Installation
Install with npm:

```
$ npm install perspectives-react
```
## Dependencies
This package depends on:
  * react
  * prop-types
  * react-loadable
  * perspectives-proxy
  * perspectivesGlobals
  * importModule


## Dependency management
See [Publishing a new version](https://github.com/joopringelberg/perspectives-core/blob/master/technical%20readme.md#publishing-a-new-version) in the Perspectives Core (PDR) project.

### Publish new package version:
4. In package.template.json: increase the package number.
4. Update the versions in `bumpversions.sh` and run that script.
5. Commit
6. Create tag
7. Push tag

## Bundled Dependencies
`prop-types` and `react-loadable` are bundled with the distribution.

## External Dependencies
In order to minimize the size of the bundle, and because it will always be used in the context of a program that uses React, we have externalised React.

`perpectives-proxy` is declared external in order to be able to share it with `perspectives-core` (the core also depends on the proxy, but react and core need to share the same instance of proxy).

`perspectivesGlobals` should have a member called `host`. It is declared external, so the calling program has control over its members.

Finally, the `Screen` component should use the unadorned `import` function. However, Webpack treats expressions with this function as a trigger to bundle or split bundles. This is not what we want. Hence, we use a function `importModule` that really is nothing but `import`, to be present on `window` of the calling program.

## Build
Create `dist/perspectives-react.js` by evaluating on the command line:

```
$ npm run build
```
This is equivalent to:
```
$ npx webpack
```
## Watch
Have Webpack watch the sources and update `dist/perspectives-react.js` by evaluating on the command line:

```
$ npm run watch
```
This is equivalent to:
```
$ npx webpack --watch
```

## Components
**NOTICE** THIS SECTION IS OUT OF DATE.

We use the name **prop** to indicate React properties (keys on the props object) and **Property** to indicate Perspectives Properties. Components require some props to be provided as attributes on the Component. Such mandatory attributes are listed below in the tables of Attributes.

### How to build a GUI
Construct a tree of Perspectives React Components. These must be understood as *container components* that provide data to other container components and ultimately to *display components*. Write display components that make use of the props passed on by the container components. These React props correspond to Perspectives Properties. In other words, only the programmers' display components will actually create visible elements.  Display components can only be nested inside components that provide Properties (explained below)!

### Allowed Nesting
Each Component (with the exception of the Context Component) must be nested inside another Component. But not all nestings are valid. Each component needs a number of props. These can be set by using an attribute in JSX, or they can be passed on by the enclosing component. Components differ as to what they pass on, meaning that the attributes a Component needs, depend on where it is used. Consequently, in the descriptions below we show what props a Component needs and what props it passes on. So to find out what attributes you should use in your JSX, subtract the props passed on by the container from the props needed. In rare cases, a Component needs an attribute in each context. Such props are printed in **bold**.

NOTE: the objective of these (data)-containers is that they retrieve values from the PDR. A Component may pass on such data, for example a role-instance. Consequently, a Component may pass on values as props for its children that it does not receive!

Most components need either a contextinstance or a rolinstance. All Components can be categorized as to what they provide to their children: a Context, Role(s) or values for Properties. The matrix below gives a complete overview.


&nbsp;| Needs a rolinstance | Needs a contextinstance
--- | --- | ---
**Passes a rolinstance** |RolBinding, InverseRoleBinding | Rollen, ExternalRole
**Passes a contextinstance** |ContextOfRole, BoundContext | Context
**Provides values of Properties** | View | ViewOnExternalRole, ExternalViewOfBoundContext

*Examples*
  1. `Context` is in the middle row and so provides a Context (it is in the column **Needs a contextinstance**. So if you make it the root of your Component tree, you'll have to provide the fully qualified id of a Context instance). Notice that `Context` merely passes on the props it receives. It is meant to be the root of a Component tree, that you anchor to a Context instance whose fully qualified ID you enter as an attribute.

  2. What can be inside a `Context` Component? Everything in the column **Needs a contextinstance**, so: `Rollen`, `ExternalRole`, `Context` (but this is rather pointless), `ViewOnExternalRole` and `ExternalViewOfBoundContext`.

  3. Where can we use `BoundContext`? It is in the column **Needs a rolinstance**, so we can put it inside anything that provides a Rol. In other words, everything in the row **Passes a rolinstance**: `RolBinding`, `InverseRoleBinding`, `Rollen`, and `ExternalRole`.

*A word on compositions*. The Components `BoundContext` and `ExternalViewOfBoundContext` are *compositions* of other (more elementary) Components. They are here for convenience. However, use them with care as they expect specific conditions. Take `BoundContext` as an example. It is a composition of `RolBinding` followed by `ContextOfRole`. The name suggests that you'll end up with a Context and as such it is put in the row **Provides Context** above. However, this will only be correct if, indeed, the Role is bound to the ExternalRole of a Context! You, as programmer, are responsible for guaranteeing that semantics.

Name | Composition (f <<< g is f after g)
--- | ---
BoundContext | ContextOfRole <<< RolBinding
ExternalViewOfBoundContext | View <<< RolBinding

### Context
The `Context` Component provides a root for a container component hierarchy. This Component need not be nested in any other container.

Prop | Description
--- | ---
**contextinstance** | The identification of the Context. This must be a qualified name.
**contexttype** | The qualified `psp:type` of the Context

`Context` passes the following props down its children tree with the `PSContext` React Context:

Prop | Description
--- | ---
contextinstance | The identification of the Context. This must be a qualified name.
contexttype | The type of the Context.

### Rollen
A `Rollen` Component makes instances of selected roles available. Use the `Rollen` Component to access Roles of the surrounding Context. Select a Role by using its *local name*.

To access the BuitenRol of a Context, use the `ExternalRole` Component rather than trying to include the name of the BuitenRol. To access properties on a BuitenRol, use `ViewOnExternalRole`.

**NOTE**. Any Role that is taken from an Aspect, will not be found by Rollen, because it's current implementation assumes that each Role is in the namespace of the context.


Prop | Description
--- | ---
**rollen** | An array of **local** Role names.
namespace | the *type* of the Context that the roleinstance is taken from (NOTE: this will not be the namespace of a Role that was taken from an Aspect!)
contextinstance | The id of the context that the rolinstances are taken from.

`Rollen` passes the following props to its children:

Prop | Description
--- | ---
key | Used by Reactjs to distinguish between instances (actually is the rolinstance value)
namespace | The namespace as passed into Rollen.
rolinstance | Identifies an instance of a rol.

Roles may be functional or relational, the latter meaning that there can be more than one instance of the Role. A child Component that selects a particular Role will be mapped over the instances of that Role. Each duplicated Component has a prop `rolinstance` on its props (its value is a qualified Role identifier). It will also have a prop `key` with the same value; this prop is used by React to identify the various elements of the resulting sequence.

```
<Rollen rollen={[
          "user",
          "models",
          "trustedCluster"
        ]}>
```

### RolBinding
The `RolBinding` Component navigates to the binding of the Role that is selected by its `rolinstance` prop and passes it on. This will be an ExternalRole or a BuitenRol. Consequently, all elements that need one of these as context can be nested inside a `RolBinding` Component. It is the programmers responsibility to ensure they use appropriate elements for the two kinds of Role!

Prop | Description
--- | ---
namespace | The type of the bound rolinstance.
rolinstance | The id of the bound rolinstance.
rolname | The local name of a Role. Use this when embedding the RolBinding in a `Rollen` component, to select instances of a specific Role.

`RolBinding` passes on:

Prop | Description
--- | ---
rolinstance | The instance of the binding (which is, of course, a rol)
namespace | The type of the binding.


### BoundContext
The `BoundContext` Component navigates to the Context bound to the Role indicated by its `roleinstance` prop. This Component is the composition `ContextOfRole <<< RolBinding`.

Prop | Description
--- | ---
rolinstance | The local name of a Role.
rolname | The local name of a Role. Use this when embedding the BoundContext in a `Rollen` component!

`BoundContext` passes on:

Prop | Description
--- | ---
contextinstance | The identification of the Context. This must be a qualified name.
namespace | The type of the Context.


### InverseRoleBinding
The `InverseRoleBinding` Component navigates to the instances of the Role whose local name is `rolname`, that bind the instance of a Role that is stored in the prop `rolinstance` of the Component `InverseRoleBinding`. Note that `rolname` identifies a Role in *another* Context than the one that holds the rolinstance (we navigate backwards)!

Prop | Description
--- | ---
rolinstance | The id of the instance of the Role to navigate from.
rolname | The local name of a Role to navigate with.

`InverseRoleBinding` passes on:

Prop | Description
--- | ---
key | As rolinstance, used by React.
rolinstance | The id of the instance of the Role.


### ExternalRole
The `ExternalRole` Component navigates from a Context to its ExternalRole.

Prop | Description
--- | ---
contextinstance | The id of the context that we navigate from.
namespace | The type of the context that we navigate from.

`ExternalRole` passes the following props to its children:

Prop | Description
--- | ---
namespace | The **fully qualified** type of the instance of the BuitenRol.
rolinstance | Identifies an instance of a BuitenRol.
rolname | The value "buitenRolBeschrijving"


### ContextOfRole
The `ContextOfRole` Component navigates from a Role to its Context, so it gives access to a Context, just as the `Context` Component does. `ContextOfRole` uses `Context` internally, so passes on exactly the same props as `Context` does.


Prop | Description
--- | ---
rolinstance | The id of the instance of the Rol that we want the Context of.

`ContextOfRole` passes the following props to its children:

Prop | Description
--- | ---
contextinstance | The identification of the Context. This must be a qualified name.
namespace | The type of the Context.


### View
Selects a View of a Role and makes the properties of that View available. A `View` Component can be used inside the `Rollen`, `ExternalRole`, `RolBinding` or `InverseRoleBinding` Component, all of which pass on a `rolinstance` value.

Possible content elements: any user-defined Component that can make good use of the props that are passed on.

Prop | Description
--- | ---
namespace | The **fully qualified** type of the instance of the Rol that we retrieve property values from.
rolinstance | The instance of the Rol that we retrieve property values from.
rolname | The **local name** of a Role. Use this when embedding the View in a `Rollen` component!
viewname | The **local name** of a View.

`View` passes the following props to its children:

Prop | Description
--- | ---
namespace | The namespace that it has received (the qualified name of the Rol type).
rolinstance | The rolinstance that it received.
rolname | the local name of the Rol.
Property | Each Perspectives Property that is part of the View. Note that the **qualified** name of the property is used.

A view provides the function `propval`. This takes a local property name as argument, matches it against the available qualified property names and if it finds a single match, returns the value of that property. If no matches are found or if multiple matches are found, appropriate errors will be thrown so the programmer can use a correct local name or add segments to the local name to make it unique.

```
<View role="user" viewname="FullName">
            <UserName />
          </View>

function UserName (props)
{
  return <p><label>User:</label>{props.firstname + " " + props.lastname}</p>;
}
```

### ExternalViewOfBoundContext
Selects the View `viewname` of an ExternalRole; expects that ExternalRole to be the Binding of its `role` attribute. This Component is the composition `View <<< RolBinding`.

Prop | Description
--- | ---
namespace | The type of the bound rolinstance.
rolinstance | The id of the bound rolinstance.
rolname | The local name of a Role. Use this when embedding the `ExternalViewOfBoundContext` in a `Rollen` component, to select instances of a specific Role.

As this component is composed of `View`, passes the same properties on.

### ViewOnExternalRole
Selects the View `viewname` of the ExternalRole of the Context. Use this Component directly inside a Component that provides a Context, such as `Context` of `BoundContext`.

Attribute | Description
--- | ---
viewname | The **local name** of a View.

As this component is composed of `View`, passes the same properties on.

### SetProperty
Use this Component to change the value of a property. Use it in the context of a Component that provides property values. The attribute `propertyname` is mandatory. It selects the Property whose value will be changed.

Prop | Description
--- | ---
namespace | The type of the rolinstance on which we will change a Property value.
**propertyname** | The **local** name of the Property on which we will change a value.
rolinstance | The id of the rolinstance on which we will change a Property value.
rolname | The local name of a Role.
value | The value of the Property before changing it.

**Note**.
*  If the last segment of the qualified name does not uniquely identify a property, an alert is shown. This is meant for the programmer! Add segments before the last until it uniquely identifies a single property on the view.
*  If the given local name does not identify a property, an alert is shown.

`SetProperty` passes the following props to its children:

Prop | Description
--- | ---
defaultvalue | The value received as prop.
setvalue | A function to change the value. Use this function in html code, e.g. as the action attached to `onblur`.


**Example**
```
<View rolname="gebruiker" viewname="VolledigeNaam">
  <SetProperty propertyname="voornaam">
    <GebruikerVoornaamInput/>
  </SetProperty>
</View>

function GebruikerVoornaamInput (props)
{
  return (<fieldset>
    <legend>Verander de gebruikers' voornaam in:</legend>
    <input defaultValue={props.defaultvalue} onBlur={e => props.setvalue(e.target.value)} />
    </fieldset>);
}
```

*A word on passing value*. If SetProperty is embedded directly in a View, View will see SetProperty is interested in a specific Property (it reads the attribute `propertyName`). View will consequently pass on the prop `value`. SetProperty will then pick up this prop and pass it on to its children as the prop `defaultvalue`. However, if SetProperty is not directly embedded into View, this will not work (View then cannot 'see' that SetProperty asks for it). In such a case, make sure to pass on the `value` prop explicitly as an attribute!

```
function Test (props)
{
  return (
    <div>
      <p><label>v2:</label>{props.v2}</p>
      <SetProperty propertyname="trigger" namespace={props.namespace} rolinstance={props.rolinstance} rolname={props.rolname} value={props.trigger}>
        <TriggerInput/>
      </SetProperty>
    </div>
  );
}

```

### CreateContext
Use this Component to embed a control in that governs creating a new Context (like a button). Optionally, add fields that provide, for example, properties of the new Context. Gather them in a ContextDescription JSON object to pass on to the create function that is provided by CreateContext to its children.

This Component behaves in three distinct ways:

  * Create a Context bound to an existing instance of a Rol. CreateContext then **must** receive a `rolinstance` prop.
  * Create a Context and create a Rol to bind it in. CreateContext then **must** receive a `contextinstance` and `rolname` prop.
  * Just Create a Context.

In all three cases, the `contextname` prop that gives the type of the Context to create, is required.

This Component does **not** provide a contextinstance to its children. Instead, to view the new Context, create for example a Rollen Component that selects it and a View inside that shows it.

Prop | Description
--- | ---
contextinstance | An surrounding, existing Context that we will create a new Rol in that will be boundnd to the new Context.
**contextname** | The type of the Context to be created.
rolinstance | The existing rolinstance to bind the new Context to (optional)
rolname | The local name of the Rol in the surrounding, existing Context that we will create.

`CreateContext` passes on:

Prop | Description
--- | ---
create | A function accepting a ContextDescription JSON value. Calling the function will create the Context.

#### Context- and Role serialisation
These types are simpler versions of PerspectContext and PerspectRol as defined in the Core program. They cannot be put into Couchdb but are used to transport created contexts and roles through the API to the PDR. This structure is described in perspectives-apitypes.

Example:
```
{ "rollen": { Role1:  [ { "properties": { "prop1": "1", "prop2": "two" }, "binding": "someOtherRole" }
                      , { "properties": {}, "binding": "yetAnotherRole" }  ]}
, "interneProperties": {iprop1: "2"}
, "externeProperties": {}
}
```

Notice that the properties `id` and `ctype` (as given in perspectives-apitypes) miss from the example above. This is because a value for `id` is computed (by the PDR) and a value for `ctype` must be provided by passing in the attribute `contextname`.

## Screen
The `Screen` component encapsulates the dynamic loading of a module that defines a component that displays a screen for a User Role in a Context.

From the point of view of the Screen programmer, navigation is in terms of Contexts rather than user roles. We accommodate that by allowing the programmer to provide the external role of a Context Instance. The Screen component asks the PDR for the type of the role that the user plays in that Context Instance. It then uses that Role Type to retrieve a screen.

Consequently, the programmer should store screens in a screen library under the varying User Role names! But Perspectives identifiers start with an upper case letter and use `$` to separate segments, while javascript function names are advised to start with a lowercase letter and cannot contain `$`. Hence the programmer should map the Role Type name as follows:
  * let it start with a lowercase letter;
  * replace each occurrence of `$` with `_` (underscore).

A Screen component needs, on its `props`, a value for `rolinstance`. A typical way of embedding `Screen` is this:

```
<PSRol.Consumer>
  {value => <Screen rolinstance={value.rolinstance}/>}
</PSRol.Consumer>
```

Prop | Description
--- | ---
rolinstance | The external Role of a Context Instance. This is used to retrieve the `screens.js` library document that should be attached to the model the Context type is defined in, and a component in that library that bears the name of the Role type played by the user in the given instance.

**NOTE** Actually, a User Role can have many perspectives in a Context and as many or fewer screens. In fact, a screen can visualise multiple perspectives. However, at this moment of development, we've chosen to provide just a single screen for each User Role.

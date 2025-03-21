import React, {createContext} from "react";
import { RoleDataProper, RoleOnClipboard } from "perspectives-proxy";
import { ContextInstanceT, ContextType, PropertyType, RoleInstanceT, RoleKind, RoleType, ValueT } from "perspectives-proxy";

// Two contexts, PSRol and PSRoleInstances, have the functions bind, bind_ and checkBinding. 
// All return promises.
// Their implementation is built along these decisions:
//    * each calls a proxy function and handles its errors by itself, displaying a message for the end user;
//    * bind and bind_ do NOT check whether the binding is allowed.
// Hence, in all situations where the bind and bind_ functions from these contexts are applied, 
// the programmer should make sure that the binding is actually allowed. 
// He can use checkBinding for that purpose. It returns a promise for a Boolean value.
// If the result is `false`, the situation might require that a message for the end user is displayed.

////////////////////////////////////////////
//// PSCONTEXT
////////////////////////////////////////////
export type PSContextType = 
  { contextinstance: ContextInstanceT
  , contexttype: ContextType
  // use this as value for authoringRole in calls to the PDR.
  , myroletype: RoleType
  };

export const DefaultPSContext =
  { contextinstance: "" as ContextInstanceT
  , contexttype: "" as ContextType
  , myroletype: "" as RoleType
  };

export const PSContext = createContext<PSContextType>( DefaultPSContext);

////////////////////////////////////////////
//// APPCONTEXT
////////////////////////////////////////////
export type AppContextType = 
  { systemExternalRole: RoleInstanceT   // The external role of MySystem.
  , systemIdentifier: ContextInstanceT  // The identifier of MySystem.
  , systemUser: RoleInstanceT           // The user role of MySystem.
  
  , externalRoleId?: RoleInstanceT       // The external role of the selected context.
  , roleId?: RoleInstanceT               // The role identifier of the selected role (OBSOLETE?)
  , myRoleType?: RoleType                // The role type of the users' role in the selected context.
  , couchdbUrl?: string
  };

export const AppContext = createContext<AppContextType>(
  { systemExternalRole: "" as RoleInstanceT
  , systemIdentifier: "" as ContextInstanceT
  , systemUser: "" as RoleInstanceT
  });
                      
////////////////////////////////////////////
//// PSROLEINSTANCES
////////////////////////////////////////////
export type PSRoleInstances = 
  { contextinstance: ContextInstanceT
  , contexttype: ContextType
  , rol: RoleType         // This is possibly a non-qualified role name. Probably obsolete.
  , roltype: RoleType     // This is a qualified role name.
  , roleKind: RoleKind
  , instances: RoleInstanceT[]
  , cursor: string
  // Handles its own errors and returns a promise.
  , createRole: (roleData: RoleDataProper) => Promise<void>
  , bind: (roleData: RoleDataProper) => Promise<RoleInstanceT>
  , checkbinding: (roleData: RoleDataProper) => Promise<boolean>
}

export const PSRoleInstances = createContext<PSRoleInstances>(
  { contextinstance: "" as ContextInstanceT
  , contexttype: "" as ContextType
  , rol: "" as RoleType
  , roltype: "" as RoleType
  , roleKind: ""  as RoleKind
  , instances: [] as RoleInstanceT[]
  , cursor: ""  // The cursor for the next page of instances.
  // Handles its own errors and returns a promise.
  , createRole: function(ignore){ console.warn("Default (no-op) createRole called. You likely miss a PSRoleInstances.Provider!");} as (roleData: RoleDataProper) => Promise<void>
  , bind: function(ignore){console.warn("Default (no-op) bind called. You likely miss a PSRoleInstances.Provider!");} as (roleData: RoleDataProper) => Promise<RoleInstanceT>
  , checkbinding: function(ignore){console.warn("Default (no-op) checkbinding called. You likely miss a PSRol.Provider or PSRoleInstances.Provider!");} as (roleData: RoleDataProper) => Promise<boolean>
  });

////////////////////////////////////////////
//// PSROL
////////////////////////////////////////////
export type PSRolType = 
  { contextinstance: ContextInstanceT
  , contexttype: ContextType
  , roltype: RoleType
  , roleKind: RoleKind
  , bind_: (roleData: RoleDataProper) => Promise<[]|void>
  , bind: (roleInstance: RoleInstanceT) => Promise<RoleInstanceT|void>
  , checkbinding: (roleData: RoleDataProper) => Promise<boolean>
  , removerol: () => Promise<void>
  , removecontext: () => Promise<void>
  , rolinstance?: RoleInstanceT
  , isselected: boolean
  };
  
  export const DefaultPSRol = 
    { contextinstance: "" as ContextInstanceT
    , contexttype: "" as ContextType
    , roltype: "" as RoleType
    , roleKind: "" as RoleKind
    , bind_: (ignore:RoleDataProper) => Promise.reject("Specialise the bind_ function in the PSRol.Provider!")  
    , bind: (ignore:RoleInstanceT) => Promise.reject("Specialise the bind function in the PSRol.Provider!")
    , checkbinding: (roleData : RoleDataProper) => Promise.reject("Specialise the checkbinding function in the PSRol.Provider!")
    , removerol: () => Promise.reject("Specialise the removerol function in the PSRol.Provider!")
    , removecontext: () => Promise.reject("Specialise the removecontext function in the PSRol.Provider!")
    , rolinstance: undefined
    , isselected: false
    }

export const PSRol = createContext<PSRolType>( DefaultPSRol );
    
    ////////////////////////////////////////////
//// PSVIEW
////////////////////////////////////////////
export type PSViewType = 
  { contextinstance: ContextInstanceT
  , contexttype: ContextType
  , rolinstance: RoleInstanceT
  , roltype: RoleType
  , viewproperties: PropertyType[]
  , propertyValues: {[key: PropertyType]: ValueT[]}
  , propval: (propname: string) => ValueT[]
  , propset: (propname: string, propvalue: ValueT) => void
  , propdel: (propname: string) => void
  // And there will be a member for each property, holding its values.
}
export const PSView = createContext(
  { contextinstance: "" as ContextInstanceT
  , contexttype: "" as ContextType
  , rolinstance: "" as RoleInstanceT
  , roltype: "" as RoleType
  , viewproperties: [] as PropertyType[]
  , propertyValues: {} as {[key: PropertyType]: ValueT[]}
  , propval: function( ln: string){ return [] as ValueT[]}
  , propset: function(ln: string, val:ValueT){}
  , propdel: function(ln : string){}
  // And there will be a member for each property, holding its values.
  });

////////////////////////////////////////////
//// PSPROPERTY
////////////////////////////////////////////
export type PSPropertyType = 
  { defaultValue: ValueT
  , setvalue: (value: ValueT) => void
  };

export const PSProperty = createContext(
  { defaultValue: ""
  , setvalue: function(){}
  });

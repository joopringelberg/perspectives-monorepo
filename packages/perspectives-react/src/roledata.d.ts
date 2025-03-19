
////////////////////////////////////////////
//// SERIALISED PERSPECTIVES

import { ContextInstanceT, EnumeratedOrCalculatedProperty, PropertyType, RoleInstance, RoleInstanceT, RoleType, ValueT } from "perspectives-proxy";

////////////////////////////////////////////
//// ROLES ETC
////////////////////////////////////////////

export type RoleDataProper = {
  rolinstance?: RoleInstance;
  cardTitle?: string;
  roleType?: string;
  contextType?: string;
};

////////////////////////////////////////////
//// CLIPBOARD
////////////////////////////////////////////
export type RoleOnClipboard = 
  {
    roleData: {
      rolinstance: RoleInstanceT,
      cardTitle: string
      roleType: RoleType,
      contextType: ContextType
    },
    addedBehaviour: string[],
    myroletype: RoleType
  }
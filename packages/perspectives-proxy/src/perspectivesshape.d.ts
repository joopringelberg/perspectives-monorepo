
////////////////////////////////////////////
//// PERSPECTIVES TYPE- AND INSTANCE TYPES
////////////////////////////////////////////

// NOTE: RoleInstanceT is the typescript type of role instances. Don't confuse it with RoleType, which is the typescript type of role types.
export type RoleInstanceT = string & { readonly brand: unique symbol };
export type RoleReceiver = (roleInstance: RoleInstanceT[]) => void;
export type ContextInstanceT = string & { readonly brand: unique symbol };
export type ValueT = string & { readonly brand: unique symbol };
export type PropertyValueReceiver = (value: ValueT[]) => void;
export type RoleType = string & { readonly brand: unique symbol };
export type UserRoleType = RoleType
export type RoleTypeReceiver = (roleType: RoleType[]) => void;
export type PerspectivesReceiver = (perspectives: Perspective[]) => void;
export type ContextAndNameReceiver = (contextAndName: ContextAndName[]) => void;
export type ScreenReceiver = (screen: ScreenDefinition[]) => void;
export type TableFormReceiver = (tableForm: TableFormDef[]) => void;
export type PropertyType = string & { readonly brand: unique symbol };
export type ContextType = string & { readonly brand: unique symbol };
export type RoleKind = "RoleInContext" | "ContextRole" | "ExternalRole" | "UserRole" | "Public" | "PublicProxy"
export type EnumeratedOrCalculatedProperty = {type: "ENP" | "CP", value: PropertyType}

export type ContextActions = Record<ModeledActionName, TranslatedActionName>;

export type FileShareCredentials = { accountName : string, password : string, storageType: PStorageType, sharedStorageId : RoleInstanceT };

export type PStorageType = "mega" | "ppstorage";

////////////////////////////////////////////
//// PSHAREDFILE
////////////////////////////////////////////
export interface PSharedFile {
  name: string;
  size: number;
  type: string;
  sharedStorageId: string;
  storageType: string;
  url: string;
}

////////////////////////////////////////////
//// PERSPECTIVESFILE
////////////////////////////////////////////
export type PerspectivesFile = {
  // The name associated with the file on creating or uploading it. Use only client side.
  fileName: string;
  // The identifier of the attachment of the role instance.
  propertyType: PropertyType;
  mimeType: string;
  // The database where the role instance is stored. (is Nothing for IndexedDB)
  database?: string;
  // The name of the role instance document
  roleFileName: string;
};

////////////////////////////////////////////
//// SERIALISED PERSPECTIVES
////////////////////////////////////////////
export type Perspective = {
  id: string;
  displayName: string;
  isFunctional: boolean;
  isMandatory: boolean;
  isCalculated: boolean;
  userRoleType: RoleType;
  roleType: RoleType;
  roleKind: RoleKind;
  contextType: ContextType;
  contextIdToAddRoleInstanceTo: ContextInstanceT;
  contextTypesToCreate: Record<string, ContextType>;
  identifyingProperty: PropertyType;
  contextInstance: ContextInstanceT;
  roleInstances: Record<string, Roleinstancewithprops>;
  verbs: RoleVerb[];
  properties: Record<string, SerialisedProperty>;
  actions: Record<string, string>;
  possibleFillers: {readableName : string, instance: RoleInstanceT}[];
};

export type RoleVerb = 
  "Remove"            // Remove a single instance
  | "RemoveContext"     // Remove a contextrole instance together with its context.
  | "Delete"            // Remove all instances
  | "DeleteContext"     // Delete all contextrole instances together with their contexts.
  | "Create"            // Create an instance
  | "CreateAndFill"     // CreateAndFill <RoleType> with <roleExpr>
  | "Fill"              // <functionalRoleExpr> with <functionalRoleExpr>
  | "Unbind"            // <roleExpr> from <RoleType>, i.e. remove all binders of type <RoleType>
  | "RemoveFiller"      // <functionalRoleExpr> from <functionalRoleExpr>
  | "Move"              // Move an instance from one context to another.
  

export type Roleinstancewithprops = {
  roleId: RoleInstanceT;
  objectStateBasedRoleVerbs: string[];
  propertyValues: Record<string, PropertyValues>;
  actions: Record<string, string>;
  objectStateBasedProperties: { type: string; value: PropertyType }[];
  // NOTA BENE: publicUrl is OBSOLETE.
  publicUrl?: string;
  filler?: RoleInstanceT;
  isMe: boolean;
  publicUrl?: string;
  readableName: string
};

export type PropertyValues = {
  values: ValueT[];
  propertyVerbs: string[];
};

export type SerialisedProperty = {
  id: PropertyType;
  displayName: string;
  isFunctional: boolean;
  isMandatory: boolean;
  isCalculated: boolean;
  range: PRange;
  constrainingFacets: {
    minLength?: number;
    maxLength?: number;
    pattern?: {
      regex: string;
      label: string;
    };
    whiteSpace?: string;
    enumeration?: string[];
    maxInclusive?: string;
    maxExclusive?: string;
    minInclusive?: string;
    minExclusive?: string;
    totalDigits?: number;
    fractionDigits?: number;
  };
};

export type PRange = "PString" | "PBool" | "PDateTime" | "PDate" | "PTime" | "PNumber" | "PEmail" | "PFile" | "PMarkDown";

export type InputType = "text" | "checkbox" | "datetime-local" | "date" | "time" | "number" | "email" | "file" | "markdown"



////////////////////////////////////////////
//// SERIALISED SCREEN
////////////////////////////////////////////
export type ScreenDefinition = MainScreenElements & {
  title?: string;
  userRole: string; // The translated user role type, e.g. "My Role" or "Mijn Rol"
  whoWhatWhereScreen?: WhoWhatWhereScreenDef;
};

export type MainScreenElements = {
  tabs?: TabDef[];
  rows?: ScreenElementDefTagged[];
  columns?: ScreenElementDefTagged[];
};

export type TabDef = {
  title: string;
  isDefault: boolean;
  elements: ScreenElementDefTagged[];
};

export type ScreenElementDefTagged = {
  elementType: "RowElementD" | "ColumnElementD" | "TableElementD" | "FormElementD" | "MarkDownElementD" | "ChatElementD";
  element: ScreenElementDef;
}

export type ScreenElementDef = RowElementDef | ColumnElementDef | TableElementDef | FormElementDef | MarkDownElementDef | ChatElementDef;

export type RowElementDef = {
  tag: "RowDef";
  elements: ScreenElementDefTagged[];
}

export type ColumnElementDef = {
  tag: "ColumnDef";
  elements: ScreenElementDefTagged[];
}

export type TableElementDef = {
  tag: "TableDef";
  markdown: MarkDownElementDef[];
  widgetCommonFields: WidgetCommonFields;
}

export type FormElementDef = {
  tag: "FormDef";
  markdown: MarkDownElementDef[];
  widgetCommonFields: WidgetCommonFields;
}

export type MarkDownElementDef = MarkDownConstant | MarkDownPerspective | MarkDownExpression;

export type MarkDownConstant = {
  tag: "MarkDownConstantDef";
  element: { text : string, condition?: string, domain: string };
}

export type MarkDownPerspective = {
  tag: "MarkDownPerspectiveDef";
  element: { widgetFields: WidgetCommonFields, conditionProperty: EnumeratedOrCalculatedProperty? };
}

export type MarkDownExpression = {
  tag: "MarkDownExpressionDef";
  element: {textQuery: string, condition? : string, text?: string};
}

export type ChatElementDef = {
  tag: "ChatDef";
  fields: {
    chatRole: RoleType;
    title: string;
    chatInstance?: RoleInstanceT;
    messageProperty: PropertyType;
    mediaProperty: PropertyType;
  };
}

export type WidgetCommonFields = {
  title: string;
  perspective: Perspective;
};

export type WhoWhatWhereScreenDef = {
  who: Who;
  what: What;
  whereto: WhereTo;
};

export type Who = {
  markdown: MarkDownElementDef[];
  chats: ChatElementDef[];
  userRoles: TableFormDef[];
  }

export type TableFormDef = {
  markdown: MarkDownElementDef[];
  table: TableElementDef;
  form: FormElementDef;
};

export type What = 
  {tag: "TableForms", elements: {markdown: MarkDownElementDef[], tableForms: TableFormDef[]}} 
  | 
  {tag: "FreeFormScreen", elements: MainScreenElements}; 

export type WhereTo = {
  markdown: MarkDownElementDef[];
  contextRoles: TableFormDef[];
  }

/////////////////////////////////////////////////////////////////////////////////////////
// PDRTYPES
/////////////////////////////////////////////////////////////////////////////////////////
export type RuntimeOptions = {
  // Default: true. Should be false when someone installs MyContexts on a second device.
  isFirstInstallation: boolean;
  // Default: null. Provide a value to test setup of an experimental new System version.
  useSystemVersion: string | null;
  // Default: the CryptoKey object that has been created on setting up the installation. This is not extractable.
  privateKey?: CryptoKey;
  // Default: the CryptoKey object that has been created on setting up the installation. This is extractable.
  publicKey?: CryptoKey;
  // Default: the package number taken from package.json
  myContextsVersion: string;
};

export type PouchdbUser = {
  systemIdentifier: string;  // the schemaless string
  perspectivesUser: string;  // the schemaless string
  userName: string;          // this MAY be equal to perspectivesUser but it is not required.
  password?: string;         // Optional field
  couchdbUrl?: string;       // Optional field
};

export type Unsubscriber = { request: string, subject: string; corrId: number };

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

 ////////////////////////////////////////////
//// CLIPBOARD
////////////////////////////////////////////
export type ContextAndName =
	{ externalRole : RoleInstanceT
	  readableName : string }

////////////////////////////////////////////
//// INSPECTABLE CONTEXT
////////////////////////////////////////////
// An InspectableContext shows up on screen as a React Bootstrap form with just readonly fields.
export type InspectableContext =
  { // The identifier of the context instance. It will not be shown on screen.
    id: ContextInstanceT
    // The title shows up as a title on screen. It identifies the instance in a human readable way.
  , title: ReadableContextInstance
  // The fully qualified context type identifier. It is a readable identifier. It is shown when we hover the title of the screen.
  , ctype: ReadableContextFQN
  // A table of properties. The first column gives the translated property name (TranslatedPropertyTypeName), the second column the value.
  // Hovering over the first column gives the fully qualified readable property identifier (ReadablePropertyFQN).
  // keys are readable property identifiers, fully qualified.
  // translatedProperty is the translated name of the property.
  // We will display translatedProperty as label and value as value.
  // Property Readable will be available in hover.
  , properties: Record<ReadablePropertyFQN, { translatedProperty: TranslatedPropertyTypeName, value: string }>
  // Next we have a separator, followed by a number of accordions, one for each role type (ReadableRoleFQN).
  // The Accordion title is the translatedRole. If possible, hovering over the title shows the readable fully qualified role type identifier (ReadableRoleFQN).
  // Inside the accordion we have a list of role instances (RoleInstance) of that type.
  // Each role instance shows its title (the value of the readable property of the instance: ReadableRoleInstance).
  // The role instance is a link or button and clicking it calls a function showRole with argument the RoleInstanceT. 
  , roles: Record<ReadableRoleFQN, { translatedRole: TranslatedRoleTypeName, instances: Array<RoleInstance> }>
  // Each role instance shows its title (the value of the readable property of the instance: ReadableRoleInstance).
  // The role instance is a link or button and clicking it calls a function showRole with argument the RoleInstanceT. 
  , unlinkedRoles: Record<ReadableRoleFQN, { translatedRole: TranslatedRoleTypeName, instances: Array<RoleInstance> }>
  // Then follows a field (readonly, as are all other fields) with label "Me". The value will be the ReadableRoleInstance.
  // Right below we have a field with label "My type". The value will be the ReadableRoleFQN.
  // title is the value of the readable property of the instance.
  // roleType is the readable fully qualified role type identifier.
  , me?:  { _id: RoleInstanceT, title: ReadableRoleInstance, roleType: ReadableRoleFQN }
  // Then we have a simple list of types.
  // Each entry shows the translated name of the context type.
  // Hovering it displays the ReadableContextFQN.
  // keys are fully qualified context type identifiers.
  // values are the translated names of the context types.
  , types: Record<ReadableContextFQN, TranslatedContextTypeName>
  // Following item is a button called ExternalRole. Clicking it calls a function showRole with argument the RoleInstanceT.
  // The identifier of the external role.
  , externalRole: RoleInstanceT
  // Finally, we have a simple list of states.
  // Each entry shows the TranslatedStateTypeName; hovering it shows the ReadableStateFQN.
  // keys are fully qualified state identifiers.
  // values are the translated names of the states.
  , states: Record<ReadableStateFQN, TranslatedStateTypeName>
  }

export type ReadableContextInstance = string & { readonly brand: unique symbol };
export type ReadableContextFQN = string & { readonly brand: unique symbol };
export type ReadablePropertyFQN = string & { readonly brand: unique symbol };
export type TranslatedPropertyTypeName = string & { readonly brand: unique symbol };
export type ReadableRoleFQN = string & { readonly brand: unique symbol };
export type TranslatedRoleTypeName = string & { readonly brand: unique symbol };
export type ReadableRoleInstance = string & { readonly brand: unique symbol };
export type ReadableStateFQN = string & { readonly brand: unique symbol };
export type TranslatedContextTypeName = string & { readonly brand: unique symbol };
export type TranslatedStateTypeName = string & { readonly brand: unique symbol };

// An InspectableRole shows up on screen as a React Bootstrap form with just readonly fields.
export type InspectableRole =
  { _id: RoleInstanceT
  // The title shows up as a title on screen. It identifies the instance in a human readable way.
  // title is the value of the readable property of the instance.
  , title: ReadableRoleInstance
  // The ReadableRoleFQN is the readable fully qualified role type identifier. It is shown when we hover the title of the screen.
  // rtype is the readable fully qualified role type identifier.
  , rtype: ReadableRoleFQN
  // occurrence is the occurrence number of the role instance in the context.
  // A simple label-field combination where label is "Position in list of instances" and field is the occurrence number.
  , index: number
  // A simple label-field combination where label is "Is me" and field is a boolean value
  , isMe: boolean
  // A button with ReadableContextInstance as title. Clicking it calls a function showContext with argument the ContextInstanceT.
  , context: { _id: ContextInstance, title: ReadableContextInstance }
  // A table of properties. The first column gives the translated property name (TranslatedPropertyTypeName), the second column the value.
  // Hovering over the first column gives the fully qualified readable property identifier (ReadablePropertyFQN).
  // keys are readable property identifiers, fully qualified.
  // translatedProperty is the translated name of the property.
  // We will display translatedProperty as label and value as value.
  // Property Readable will be available in hover.
  , properties: Record<ReadablePropertyFQN, { translatedProperty: TranslatedPropertyTypeName, value: string }>
  // A label - button combination. The label says "Filler", the button has as text the title (ReadableRoleInstance). On clicking it, calls function showRole with 
  // the _id (RoleInstanceT) argument.
  , filler?: RoleInstance
  // Next follows a separator.
  // We then have an accordion for each ContextInstance. The title of the accordion is the ReadableContextInstance.
  // Inside the accordion we have a list of role instances (RoleInstance) that this role instance fills in that context.
  // For each RoleInstance we show two columns: 
  // * the TranslatedRoleTypeName of the role type (the value of the translatedRole property)
  // * its title (the value of the readable property of the instance: ReadableRoleInstance, that is: instances[i].title).
  // * The latter column contains buttons; clicking it calls function showRole with argument the RoleInstanceT.
  // keys of the outer object are the identifiers of contexts.
  // contextTitle is the readable external role property value of the context.
  // Keys of the nested object are readable role type identifiers, fully qualified.
  // translatedRole is the translated name of the role type.
  , filledRoles:
      Record<ReadableContextFQN, 
        { contextTitle: TranslatedContextTypeName
        , roleInstances: Record<ReadableRoleFQN, { translatedRole: TranslatedRoleTypeName, instances: Array<RoleInstance> }>
        }>
  // Then we have a simple list of types.
  // Each entry shows the translated name of the context type.
  // Hovering it displays the ReadableRoleFQN.
  // keys are fully qualified role type identifiers.
  // values are the translated names of the role types.
  , types: Record<ReadableRoleFQN, TranslatedRoleTypeName>
  // Finally, we have a simple list of states.
  // Each entry shows the TranslatedStateTypeName; hovering it shows the ReadableStateFQN.
  // values are the translated names of the states.
  , states: Record<ReadableStateFQN, TranslatedStateTypeName>
  }

  type RoleInstance =
  { _id: RoleInstanceT
  // title is the value of the readable property of the instance.
  , title: ReadableRoleInstance
  }

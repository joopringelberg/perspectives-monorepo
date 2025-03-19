import type { RoleInstanceT, RoleReceiver, ContextInstanceT, ValueT, PropertyValueReceiver, RoleType, UserRoleType, RoleTypeReceiver, PerspectivesReceiver, ScreenReceiver, TableFormReceiver, PropertyType, ContextType, RoleKind, ContextActions, FileShareCredentials, PSharedFile, PerspectivesFile, RuntimeOptions, PouchdbUser, Unsubscriber, PRange, InputType } from "./perspectivesshape.d.ts";
export type * from "./perspectivesshape.d.ts";
export declare const PDRproxy: Promise<PerspectivesProxy>;
type Options = {
    pageHostingPDRPort?: (pdr: any) => MessagePort;
};
export declare function configurePDRproxy(channeltype: "internalChannel" | "sharedWorkerChannel" | "hostPageChannel", options: Options): void;
interface RequestRecord {
    request?: string;
    subject?: string;
    predicate?: string;
    object?: any;
    contextDescription?: any;
    rolDescription?: any;
    authoringRole?: string;
    onlyOnce?: boolean;
    reactStateSetter?: (response: any) => void;
    corrId?: number;
}
type Response = ErrorResponse | ResultResponse | WorkerResponse;
type ErrorResponse = {
    responseType: "APIerror";
    corrId: number;
    error: string;
};
type ResultResponse = {
    responseType: "APIresult";
    corrId: number;
    result: string[];
};
type WorkerResponse = {
    responseType: "WorkerResponse";
    serviceWorkerMessage: "channelId" | "pdrStarted" | "isUserLoggedIn" | "runPDR" | "createAccount" | "resetAccount" | "reCreateInstances" | "recompileLocalModels" | "removeAccount";
    channelId: number;
    pdrStarted: boolean;
    isUserLoggedIn: boolean;
    createSuccesful: boolean;
    resetSuccesful: boolean;
    reCreateSuccesful: boolean;
    recompileSuccesful: boolean;
    removeSuccesful: boolean;
};
declare class SharedWorkerChannel {
    port: MessagePort;
    requestId: number;
    valueReceivers: {
        [key: string]: ((data: any) => void) | undefined;
    };
    channelIdResolver: ((value: number | PromiseLike<number>) => void) | undefined;
    channelId: Promise<number>;
    constructor(port: MessagePort);
    handleWorkerResponse(e: MessageEvent<Response>): void;
    pdrStarted(): Promise<boolean>;
    isUserLoggedIn(): Promise<boolean>;
    runPDR(username: string, pouchdbuser: PouchdbUser, options: RuntimeOptions): Promise<boolean>;
    createAccount(perspectivesUser: string, pouchdbuser: PouchdbUser, runtimeOptions: RuntimeOptions, optionalIdentityDocument: any): Promise<boolean>;
    resetAccount(username: string, pouchdbuser: PouchdbUser, options: RuntimeOptions): Promise<boolean>;
    reCreateInstances(pouchdbuser: PouchdbUser, options: RuntimeOptions): Promise<boolean>;
    recompileLocalModels(pouchdbuser: PouchdbUser): Promise<boolean>;
    removeAccount(username: string, pouchdbuser: PouchdbUser): Promise<boolean>;
    close(): void;
    unsubscribe(req: RequestRecord): void;
    nextRequestId(): Promise<number>;
    send(req: RequestRecord): Promise<Unsubscriber>;
}
export declare const SharedWorkerChannelPromise: Promise<SharedWorkerChannel>;
export declare class PerspectivesProxy {
    channel: SharedWorkerChannel;
    cursor: Cursor;
    constructor(channel: SharedWorkerChannel);
    close(): void;
    send(req: RequestRecord, receiveValues: valueReceiver, errorHandler?: errorHandler): Promise<Unsubscriber>;
    unsubscribe(req: RequestRecord): void;
    getRol(contextID: ContextID, rolName: RolName, receiveValues: RoleReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getUnqualifiedRol(contextID: ContextID, localRolName: RolName, receiveValues: RoleReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getProperty(rolID: RoleInstanceT, propertyName: PropertyType, roleType: RoleType, receiveValues: PropertyValueReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getPropertyFromLocalName(rolID: RoleInstanceT, propertyName: PropertyType, roleType: RoleType, receiveValues: PropertyValueReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getBinding(rolID: RoleInstanceT, receiveValues: RoleReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getRoleBinders(rolID: RoleInstanceT, contextType: ContextType, roleType: RoleType, receiveValues: RoleReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getMeForContext(externalRoleInstance: RoleInstanceT, receiveValues: RoleTypeReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getPerspectives(contextInstance: ContextInstanceT, userRoleType: UserRoleType, receiveValues: PerspectivesReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getPerspective(roleInstanceOfContext: RoleInstanceT, perspectiveObjectRoleType: RoleType | undefined, receiveValues: PerspectivesReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getScreen(userRoleType: UserRoleType, contextInstance: ContextInstanceT, contextType: ContextType, receiveValues: ScreenReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getTableForm(userRoleType: UserRoleType, contextInstance: ContextInstanceT, roleType: RoleType, receiveValues: TableFormReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getLocalRoleSpecialisation(localAspectName: string, contextInstance: ContextInstanceT, receiveValues: RoleTypeReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getRoleName(rid: RoleInstanceT, receiveValues: valueReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): void;
    getBindingType(rolID: RoleInstanceT, receiveValues: RoleTypeReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    /**
     * Returns a promise for an array having exactly one object, whose keys are indexed Context Names and whose values are the actual context identifiers.
     * @param name - The name to match against indexed Context Names. If the empty string, all indexed Context Names will be returned.
     * @param receiveValues - A function to receive the matched context identifiers.
     * @param fireAndForget - A boolean indicating whether to unsubscribe immediately.
     * @param errorHandler - A function to handle errors.
     * @returns A promise for an array of context identifiers.
     */
    matchContextName(name: string, receiveValues: (value: {
        [key: string]: string;
    }[]) => void, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getChatParticipants(rolID: RoleInstanceT, propertyId: PropertyType, receiveValues: ((participants: ChatParticipantFields[]) => void), fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    checkBindingP(roleName: RoleType, rolInstance: RoleInstanceT): Promise<boolean>;
    getCouchdbUrl(): Promise<string>;
    getContextActions(myRoleType: UserRoleType, contextInstance: ContextInstanceT): Promise<ContextActions>;
    getAllMyRoleTypes(externalRoleInstance: RoleInstanceT): Promise<RoleType[]>;
    getViewProperties(rolType: RoleType, viewName: string): Promise<PropertyType[]>;
    getContextType(contextID: ContextID): Promise<ContextType>;
    getRolContext(rolID: RoleInstanceT): Promise<ContextInstanceT>;
    getRolType(rolID: RoleInstanceT): Promise<RoleType>;
    getRoleKind(rolID: RoleInstanceT): Promise<RoleKind>;
    getUnqualifiedRolType(contextType: ContextType, localRolName: string): Promise<[RoleType] | []>;
    getFile(roleInstance: RoleInstanceT, propertyName: PropertyType): Promise<File>;
    getPublicUrl(contextInstance: ContextInstanceT): Promise<[string] | []>;
    getSystemIdentifier(): Promise<ContextInstanceT>;
    getPerspectivesUser(): Promise<RoleInstanceT>;
    getMeInContext(roleInstance: RoleInstanceT): Promise<[RoleInstanceT] | []>;
    getFileShareCredentials(): Promise<FileShareCredentials>;
    createContext(contextDescription: ContextSerializationRecord, roleType: RoleType, contextIdToAddRoleInstanceTo: ContextInstanceT, myroletype: UserRoleType): Promise<[RoleInstanceT, RoleInstanceT] | [RoleInstanceT]>;
    createContext_(contextDescription: ContextSerializationRecord, roleInstance: RoleInstanceT, myroletype: UserRoleType): Promise<ContextInstanceT>;
    importContexts(contextDescription: ContextSerializationRecord): Promise<ContextInstanceT[]>;
    importTransaction(transaction: any): Promise<[]>;
    setProperty(rolID: RoleInstanceT, propertyName: PropertyType, value: ValueT, myroletype: UserRoleType): Promise<[]>;
    addProperty(rolID: RoleInstanceT, propertyName: PropertyType, value: ValueT, myroletype: UserRoleType): Promise<[]>;
    saveFile(perspectivesFile: PerspectivesFile, file: File, myroletype: UserRoleType): Promise<PerspectivesFile>;
    deleteProperty(rolID: RoleInstanceT, propertyName: PropertyType, myroletype: UserRoleType): Promise<[]>;
    action(objectRoleInstance: RoleInstanceT, contextInstance: ContextID, perspectiveId: string, actionName: string, authoringRole: string): Promise<[]>;
    contextAction(contextid: ContextInstanceT, myRoleType: UserRoleType, actionName: string): Promise<[]>;
    removeBinding(rolID: RoleInstanceT, myroletype: UserRoleType): Promise<[]>;
    removeRol(rolName: RolName, rolID: RoleInstanceT, myroletype: UserRoleType): Promise<[]>;
    removeContext(rolID: RoleInstanceT, rolName: RolName, myroletype: UserRoleType): Promise<[]>;
    deleteRole(contextID: ContextID, rolName: RolName, myroletype: UserRoleType): Promise<[]>;
    bind(contextinstance: ContextInstanceT, localRolName: RolName, contextType: ContextType, rolDescription: RolSerialization, myroletype: UserRoleType): Promise<RoleInstanceT>;
    bind_(filledRole: RoleInstanceT, filler: RoleInstanceT, myroletype: UserRoleType): Promise<[]>;
    createRole(contextinstance: ContextInstanceT, rolType: RoleType, myroletype: UserRoleType): Promise<RoleInstanceT>;
    setPreferredUserRoleType(externalRoleId: ExternalRoleType, userRoleName: UserRoleType): Promise<[]>;
    save(): Promise<[]>;
    evaluateRoleState(rolinstance: RoleInstanceT): Promise<[]>;
}
export declare const FIREANDFORGET = true;
export declare const CONTINUOUS = false;
type SubscriptionType = boolean;
type valueReceiver = (value: any) => void;
type errorHandler = (error: string) => void;
type ContextID = string;
type RolName = string;
type ExternalRoleType = string;
type ContextSerializationRecord = {
    id?: string;
    prototype?: ContextID;
    ctype: ContextType;
    rollen: Record<RoleInstanceT, RolSerialization>;
    externeProperties: PropertySerialization;
};
type RolSerialization = {
    id?: string;
    properties: PropertySerialization;
    binding?: string;
};
type PropertySerialization = {
    [key: string]: ValueT[];
};
type ChatParticipantFields = {
    roleInstance: RoleInstanceT;
    firstname?: ValueT;
    lastname?: ValueT;
    avatar?: PSharedFile;
};
declare class Cursor {
    wait(): void;
    restore(): void;
}
export declare function mapRange(range: PRange): InputType;

import type { RoleInstanceT, RoleReceiver, ContextInstanceT, ValueT, PropertyValueReceiver, RoleType, UserRoleType, RoleTypeReceiver, PerspectivesReceiver, ScreenReceiver, TableFormReceiver, PropertyType, ContextType, RoleKind, ContextActions, FileShareCredentials, PSharedFile, PerspectivesFile, RuntimeOptions, PouchdbUser, Unsubscriber, PRange, InputType, RoleOnClipboard, ContextAndNameReceiver } from "./perspectivesshape.d.ts";
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
    trackingNumber?: number;
}
type Response = ErrorResponse | ResultResponse | WorkerResponse;
type ErrorResponse = {
    responseType: "APIerror";
    corrId: number;
    error: string;
    warnings: string[];
};
type ResultResponse = {
    responseType: "APIresult";
    corrId: number;
    result: string[];
    warnings: string[];
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
type UserMessageChannel = (message: string) => void;
export declare class PerspectivesProxy {
    private static index;
    channel: SharedWorkerChannel;
    cursor: Cursor;
    userMessageChannel?: UserMessageChannel;
    constructor(channel: SharedWorkerChannel);
    close(): void;
    send(req: RequestRecord, receiveValues: valueReceiver, errorHandler?: errorHandler): Promise<Unsubscriber>;
    unsubscribe(req: RequestRecord): void;
    setUserMessageChannel(channel: UserMessageChannel): void;
    getRol(contextID: ContextID, rolName: RolName, receiveValues: RoleReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getUnqualifiedRol(contextID: ContextID, localRolName: RolName, receiveValues: RoleReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getProperty(rolID: RoleInstanceT, propertyName: PropertyType, roleType: RoleType, receiveValues: PropertyValueReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getPropertyFromLocalName(rolID: RoleInstanceT, propertyName: PropertyType, roleType: RoleType, receiveValues: PropertyValueReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getBinding(rolID: RoleInstanceT, receiveValues: RoleReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getRoleBinders(rolID: RoleInstanceT, contextType: ContextType, roleType: RoleType, receiveValues: RoleReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getMeForContext(externalRoleInstance: RoleInstanceT, receiveValues: RoleTypeReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getPerspectives(contextInstance: ContextInstanceT, userRoleType: UserRoleType, receiveValues: PerspectivesReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    /**
     * Retrieves a perspective based on the provided role instance or the perspective object role type. Returns the perspective of the role the user currently has in the context.
     *
     * @param roleInstanceOfContext - The role instance of the context.
     * @param perspectiveObjectRoleType - The role type of the perspective object. Defaults to an empty string. If not given, the perspective is on the role instance itself.
     * @param receiveValues - A callback function to receive the perspective values.
     * @param fireAndForget - A boolean indicating whether the request should be fire-and-forget. Defaults to false.
     * @param errorHandler - An optional error handler callback function.
     * @returns The unsubscriber.
     */
    getPerspective(roleInstanceOfContext: RoleInstanceT, perspectiveObjectRoleType: RoleType | undefined, receiveValues: PerspectivesReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    /**
     * Retrieves a perspective based on the provided role instance or the perspective object role type. Returns the perspective of the provided user role type, iff the user actually has that role in it.
     *
     * @param roleInstanceOfContext - The role instance of the context.
     * @param perspectiveObjectRoleType - The role type of the perspective object. Defaults to an empty string. If not given, the perspective is on the role instance itself.
     * @param userRoleType  - The role type of the user that we want the perspective for.
     * @param receiveValues - A callback function to receive the perspective values.
     * @param fireAndForget - A boolean indicating whether the request should be fire-and-forget. Defaults to false.
     * @param errorHandler - An optional error handler callback function.
     * @returns The unsubscriber.
     */
    getPerspectiveForUser(roleInstanceOfContext: RoleInstanceT, perspectiveObjectRoleType: RoleType | undefined, userRoleType: RoleType, receiveValues: PerspectivesReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
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
    matchContextName(name: string, receiveValues: (value: Record<string, ContextInstanceT>[]) => void, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getChatParticipants(rolID: RoleInstanceT, propertyId: PropertyType, receiveValues: ((participants: ChatParticipantFields[]) => void), fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    /**
   * Retrieves the selected role from the clipboard.
   *
   * This function sends a request to get the selected role from the clipboard
   * and returns a promise that resolves with the role instance.
   *
   * @returns {Promise<RoleInstanceT>} A promise that resolves with the selected role instance.
   */
    subscribeSelectedRoleFromClipboard(receiver: (roleOnClipboard: RoleOnClipboard[]) => void): Promise<Unsubscriber>;
    /**
     * Retrieves the settings from the Perspectives system and processes them.
     *
     * @param receiveValues - A callback function that receives serialised perspectives on roles with settings as an array of objects.
     *                        The settings are provided as strings and are parsed using `JSON.parse`.
     * @param fireAndForget - An optional boolean indicating whether the request should be a "fire and forget" type.
     *                        Defaults to `false`.
     * @param errorHandler - An optional callback function to handle errors that may occur during the request.
     *
     * @returns The result of the `send` method, which handles the request and response processing.
     */
    getSettings(receiveValues: PerspectivesReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getWiderContexts(externalRoleInstance: RoleInstanceT, receiveValues: ContextAndNameReceiver, fireAndForget?: SubscriptionType, errorHandler?: errorHandler): Promise<Unsubscriber>;
    getPDRStatus(): void;
    checkBindingP(roleName: RoleType, rolInstance: RoleInstanceT): Promise<boolean>;
    getCouchdbUrl(): Promise<string>;
    getContextActions(myRoleType: UserRoleType, contextInstance: ContextInstanceT): Promise<ContextActions>;
    /**
     * Retrieves the selected role from the clipboard.
     *
     * This function sends a request to get the selected role from the clipboard
     * and returns a promise that resolves with the role instance.
     *
     * @returns {Promise<RoleInstanceT>} A promise that resolves with the selected role instance.
     */
    getSelectedRoleFromClipboard(): Promise<RoleOnClipboard>;
    /**
     * Removes a role instance from the clipboard.
     *
     * @param roleInstance - The role instance to be removed from the clipboard.
     * @returns A promise that resolves to a boolean indicating whether the role instance was successfully removed.
     */
    removeRoleFromClipboard(roleInstance: RoleInstanceT): Promise<boolean>;
    /**
     * NOTA BENE: WE MIGHT NOT NEED THIS FUNCTION IF WE CAN SWITCH SELECTED IN THE MODEL!!
     * CURRENTLY THERE IS NO API FUNCTION AddRoleToClipboard.
     * THIS FUNCTION IS MODELLED ON BIND
     * Low level interfact to add a role to the clipboard.
     * (Note: rather than introducing modeldependencies here, we use those in perspectives-react.
     * Further: the API function ensures that only this new item is selected.
     *
     * @param contextinstance - The context instance to create the role in (should be the system identifier).
     * @param localRolName - The qualified or local name of the role (should be ItemsOnClipboard).
     * @param contextType - The type of the context (should be PerspectivesSystem).
     * @param rolDescription - The description of the role (a RolSerialization).
     * @param myroletype - The type of the user role.
     * @returns A promise that resolves to the role instance.
     */
    addRoleToClipboard(contextinstance: ContextInstanceT, localRolName: RolName, contextType: ContextType, rolDescription: RolSerialization, myroletype: UserRoleType): Promise<RoleInstanceT>;
    getAllMyRoleTypes(externalRoleInstance: RoleInstanceT): Promise<Record<UserRoleType, string>>;
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
    isMe(roleInstance: RoleInstanceT): Promise<boolean>;
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
    /**
     * Removes a role instance.
     *
     * @param {RolName} rolName - The type of the role instance to be removed.
     * @param {RoleInstanceT} rolID - The ID of the role instance to be removed.
     * @param {UserRoleType} myroletype - The type of user role performing the removal.
     * @returns {Promise<[]>} A promise that resolves to an empty array upon successful removal.
     */
    removeRole(rolName: RolName, rolID: RoleInstanceT, myroletype: UserRoleType): Promise<[]>;
    removeContext(rolID: RoleInstanceT, rolName: RolName, myroletype: UserRoleType): Promise<[]>;
    deleteRole(contextID: ContextID, rolName: RolName, myroletype: UserRoleType): Promise<[]>;
    /**
     * Binds a role to a context instance.
     *
     * @param contextinstance - The context instance to create the role in.
     * @param localRolName - The qualified or local name of the role.
     * @param contextType - The type of the context.
     * @param rolDescription - The description of the role (a RolSerialization).
     * @param myroletype - The type of the user role.
     * @returns A promise that resolves to the role instance.
     */
    bind(contextinstance: ContextInstanceT, localRolName: RolName, contextType: ContextType, rolDescription: RolSerialization, myroletype: UserRoleType): Promise<RoleInstanceT>;
    bind_(filledRole: RoleInstanceT, filler: RoleInstanceT, myroletype: UserRoleType): Promise<boolean>;
    /**
     * Creates a role instance within a given context.
     *
     * @param contextinstance - The context instance in which the role is to be created.
     * @param rolType - The type of role to be created.
     * @param myroletype - The user role type for the authoring role.
     * @returns A promise that resolves to the created role instance.
     */
    createRole(contextinstance: ContextInstanceT, rolType: RoleType, myroletype: UserRoleType): Promise<RoleInstanceT>;
    /**
     * Creates a role in a context from a RoleSerialization.
     *
     * @param contextinstance - The context instance to create the role in.
     * @param localRolName - The qualified or local name of the role.
     * @param contextType - The type of the context.
     * @param rolDescription - The description of the role (a RolSerialization).
     * @param myroletype - The type of the user role.
     * @returns A promise that resolves to the role instance.
     */
    createRole_: (contextinstance: ContextInstanceT, localRolName: RolName, contextType: ContextType, rolDescription: RolSerialization, myroletype: UserRoleType) => Promise<RoleInstanceT>;
    /**
     * Sets the preferred user role type for the context identified by a given external role ID.
     *
     * @param externalRoleId - The external role type identifier for which the preferred user role type is being set.
     * @param userRoleName - The user role type to be set as the preferred role.
     * @returns A promise that resolves to an empty array upon successful completion.
     */
    setPreferredUserRoleType(externalRoleId: RoleInstanceT, userRoleName: UserRoleType): Promise<[]>;
    save(): Promise<[]>;
    evaluateRoleState(rolinstance: RoleInstanceT): Promise<[]>;
    /**
     * Restores the context for a specific user role within a given context instance.
     *
     * This function sends a request to restore the context for the specified user role
     * in the provided context instance. It returns a promise that resolves when the
     * operation is complete.
     *
     * As a result, a transaction is built for the given user according to the perspectives of the user role.
     *
     * @param contextInstance - The context instance to restore for the user.
     * @param userRoleId - The role instance of the user for whom the context is being restored.
     * @param userRoleType - The type of the user role.
     * @returns A promise that resolves to an empty array upon successful restoration.
     */
    restoreContextForUser(contextInstance: ContextInstanceT, userRoleId: RoleInstanceT, userRoleType: RoleType): Promise<[]>;
}
export declare const FIREANDFORGET = true;
export declare const CONTINUOUS = false;
type SubscriptionType = boolean;
type valueReceiver = (value: any) => void;
type errorHandler = (error: string) => void;
type ContextID = string;
type RolName = string;
export type ContextSerializationRecord = {
    id?: string;
    prototype?: ContextID;
    ctype: ContextType;
    rollen: Record<RoleInstanceT, RolSerialization>;
    externeProperties: PropertySerialization;
};
export type RolSerialization = {
    id?: string;
    properties: PropertySerialization;
    binding?: string;
};
export type PropertySerialization = {
    [key: string]: ValueT[];
};
type ChatParticipantFields = {
    roleInstance: RoleInstanceT;
    firstname?: ValueT;
    lastname?: ValueT;
    avatar?: PSharedFile;
};
declare class Cursor {
    private static loadingOverlayElement;
    private PDRStatus;
    private messages;
    private queuePromise;
    constructor();
    pushMessage(identifier: string, text: string): void;
    removeMessage(identifier: string): void;
    setPDRStatus({ action, message }: {
        action: "push" | "remove";
        message: string;
    }): void;
    setOverlayVisibility(visible: boolean): void;
    setOverlayText(text: string): void;
    wait(request: RequestRecord): void;
    restore(request: RequestRecord): void;
    private enqueue;
}
export declare function mapRange(range: PRange): InputType;

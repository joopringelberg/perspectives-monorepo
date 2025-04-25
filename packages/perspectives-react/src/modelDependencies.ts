import { ContextType, PropertyType, RoleType } from "perspectives-proxy";

interface ModelDependenciesT
{
  cardClipBoard: PropertyType;
  itemsOnClipboard: RoleType;
  itemOnClipboardClipboardData: PropertyType;
  itemOnClipboardSelected: PropertyType;
  currentLanguage: PropertyType;
  sysUser: RoleType;
  WWWUser: RoleType;
  startContexts: RoleType;
  system: ContextType;
  systemExternal: RoleType;
  notifications: RoleType;
  allNotifications: RoleType;
  notificationMessage: PropertyType;
  isOnScreen: PropertyType;
  nrOfUploadedFiles: PropertyType;
  disabled: PropertyType;
  pinnedContexts: RoleType;
  recentContexts: RoleType;
  actualRecentContexts: RoleType;
  roleWithIdProp: PropertyType;
  lastShownOnScreen: PropertyType;
  connectedToAMQPBroker: PropertyType
}

const ModelDependencies =
{ cardClipBoard: "model://perspectives.domains#System$PerspectivesSystem$External$CardClipBoard" as PropertyType
, itemsOnClipboard: "model://perspectives.domains#System$PerspectivesSystem$ItemsOnClipboard"
, itemOnClipboardSelected: "model://perspectives.domains#System$PerspectivesSystem$ItemsOnClipboard$Selected"
, itemOnClipboardClipboardData: "model://perspectives.domains#System$PerspectivesSystem$ItemsOnClipboard$ClipboardData"
, currentLanguage: "model://perspectives.domains#System$PerspectivesSystem$External$CurrentLanguage" as PropertyType
, sysUser: "model://perspectives.domains#System$PerspectivesSystem$User" as RoleType
, WWWUser: "model://perspectives.domains#System$PerspectivesSystem$WWWUser" as RoleType
, startContexts: "model://perspectives.domains#System$PerspectivesSystem$StartContexts" as RoleType
, system: "model://perspectives.domains#System$PerspectivesSystem" as ContextType
, systemExternal: "model://perspectives.domains#System$PerspectivesSystem$External" as RoleType
, notifications: "model://perspectives.domains#System$ContextWithNotification$Notifications" as RoleType
, allNotifications: "model://perspectives.domains#System$PerspectivesSystem$AllNotifications" as RoleType
, notificationMessage: "model://perspectives.domains#System$ContextWithNotification$Notifications$Message" as PropertyType
, isOnScreen: "model://perspectives.domains#System$ContextWithScreenState$External$IsOnScreen" as PropertyType
, nrOfUploadedFiles: "model://perspectives.domains#SharedFileServices$SharedFileServices$DefaultFileServer$NrOfUploadedFiles" as PropertyType
, disabled: "model://perspectives.domains#SharedFileServices$SharedFileServices$DefaultFileServer$Disabled" as PropertyType
, pinnedContexts: "model://perspectives.domains#System$PerspectivesSystem$PinnedContexts" as RoleType
, actualRecentContexts: "model://perspectives.domains#System$PerspectivesSystem$ActualRecentContexts" as RoleType
, recentContexts: "model://perspectives.domains#System$PerspectivesSystem$RecentContexts" as RoleType
, roleWithIdProp: "model://perspectives.domains#System$RoleWithId$Id" as PropertyType
, lastShownOnScreen: "model://perspectives.domains#System$PerspectivesSystem$RecentContexts$LastShownOnScreen" as PropertyType
, connectedToAMQPBroker: "model://perspectives.domains#System$PerspectivesSystem$External$ConnectedToAMQPBroker" as PropertyType
} as ModelDependenciesT;

export default ModelDependencies;
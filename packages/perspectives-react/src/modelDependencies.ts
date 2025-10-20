import { ContextType, PropertyType, RoleType } from "perspectives-proxy";

interface ModelDependenciesT
{
  cardClipBoard: PropertyType;
  itemsOnClipboard: RoleType;
  itemOnClipboardClipboardData: PropertyType;
  itemOnClipboardSelected: PropertyType;
  currentLanguage: PropertyType;
  maxHistoryItems: PropertyType;
  sysUser: RoleType;
  WWWUser: RoleType;
  notifiedUser: RoleType;
  apps: RoleType;
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
  connectedToAMQPBroker: PropertyType;
  restart: PropertyType;
}

const ModelDependencies =
{ cardClipBoard: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$External$g4032kuj7v" as PropertyType
, itemsOnClipboard: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$ekp6twrmdc"
, itemOnClipboardSelected: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$ekp6twrmdc$d5vwugvat8"
, itemOnClipboardClipboardData: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$ekp6twrmdc$d5ddja2trj"
, currentLanguage: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$External$gwccl51y21" as PropertyType
, maxHistoryItems: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$External$xdqznzh64i" as PropertyType
, sysUser: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$auftu9ldl2" as RoleType
, WWWUser: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$dfznphx78w" as RoleType
, notifiedUser: "model://perspectives.domains#tiodn6tcyc$w5l42i0gk4$wc1jaepo61" as RoleType
, apps: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$lmo838ttwi" as RoleType
, system: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c" as ContextType
, systemExternal: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$External" as RoleType
, notifications: "model://perspectives.domains#tiodn6tcyc$w5l42i0gk4$qj53ocobq1" as RoleType
, allNotifications: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$lcjg3w5vla" as RoleType
, notificationMessage: "model://perspectives.domains#tiodn6tcyc$w5l42i0gk4$qj53ocobq1$gnbvtwfm3d" as PropertyType
, isOnScreen: "model://perspectives.domains#tiodn6tcyc$rmg5g1jus3$External$r9unsp62y1" as PropertyType
, nrOfUploadedFiles: "model://perspectives.domains#xjrfkxrzyt$ul1y1ukzye$h5m3plntew$if6u0tqgxn" as PropertyType
, disabled: "model://perspectives.domains#xjrfkxrzyt$ul1y1ukzye$h5m3plntew$r420hr69s0" as PropertyType
, pinnedContexts: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$x52ziv2d1i" as RoleType
, actualRecentContexts: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$g9ulp86zo8" as RoleType
, recentContexts: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$chuol755yg" as RoleType
, roleWithIdProp: "model://perspectives.domains#tiodn6tcyc$py3zsx3ml2$yh0db2coqa" as PropertyType
, lastShownOnScreen: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$chuol755yg$rwqvzugh3a" as PropertyType
, connectedToAMQPBroker: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$External$fa87gc7pib" as PropertyType
, restart: "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$External$f7lead49tt" as PropertyType
} as ModelDependenciesT;

export default ModelDependencies;
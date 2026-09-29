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
{ cardClipBoard: "model://perspectives.domains#tiodn6tcyc$aajeqq7111$External$jltbp3n4gn" as PropertyType
, itemsOnClipboard: "model://perspectives.domains#tiodn6tcyc$aajeqq7111$epbhnzzjk6"
, itemOnClipboardSelected: "model://perspectives.domains#tiodn6tcyc$aajeqq7111$epbhnzzjk6$i60oqq3ixz"
, itemOnClipboardClipboardData: "model://perspectives.domains#tiodn6tcyc$aajeqq7111$epbhnzzjk6$yrbedt72mn"
, currentLanguage: "model://perspectives.domains#tiodn6tcyc$aajeqq7111$External$typl7287bz" as PropertyType
, maxHistoryItems: "model://perspectives.domains#tiodn6tcyc$aajeqq7111$External$dih9swynob" as PropertyType
, sysUser: "model://perspectives.domains#tiodn6tcyc$aajeqq7111$d6l1oipw9b" as RoleType
, WWWUser: "model://perspectives.domains#tiodn6tcyc$aajeqq7111$a7n5wciko3" as RoleType
, notifiedUser: "model://perspectives.domains#tiodn6tcyc$zjmhhcduow$nlef0m7gxq" as RoleType
, apps: "model://perspectives.domains#tiodn6tcyc$aajeqq7111$f05po3gcs8" as RoleType
, system: "model://perspectives.domains#tiodn6tcyc$aajeqq7111" as ContextType
, systemExternal: "model://perspectives.domains#tiodn6tcyc$aajeqq7111$External" as RoleType
, notifications: "model://perspectives.domains#tiodn6tcyc$zjmhhcduow$dianp42lry" as RoleType
, allNotifications: "model://perspectives.domains#tiodn6tcyc$aajeqq7111$an1zc1ue40" as RoleType
, notificationMessage: "model://perspectives.domains#tiodn6tcyc$zjmhhcduow$dianp42lry$zbopwr245s" as PropertyType
, isOnScreen: "model://perspectives.domains#tiodn6tcyc$hbvwlxb1oz$External$gao42krd23" as PropertyType
, nrOfUploadedFiles: "model://perspectives.domains#xjrfkxrzyt$r2qxtx17zd$l3c4gcuijg$qk28h6i1bq" as PropertyType
, disabled: "model://perspectives.domains#xjrfkxrzyt$r2qxtx17zd$l3c4gcuijg$m1hqetcbwy" as PropertyType
, pinnedContexts: "model://perspectives.domains#tiodn6tcyc$aajeqq7111$xkv9ibzl4h" as RoleType
, actualRecentContexts: "model://perspectives.domains#tiodn6tcyc$aajeqq7111$uruol9pofu" as RoleType
, recentContexts: "model://perspectives.domains#tiodn6tcyc$aajeqq7111$cgt5y5jtwt" as RoleType
, roleWithIdProp: "model://perspectives.domains#tiodn6tcyc$g1k99jla0o$uqoy4lh4tn" as PropertyType
, lastShownOnScreen: "model://perspectives.domains#tiodn6tcyc$aajeqq7111$cgt5y5jtwt$dpcvml9mqc" as PropertyType
, connectedToAMQPBroker: "model://perspectives.domains#tiodn6tcyc$aajeqq7111$External$hhyzfyt854" as PropertyType
, restart: "model://perspectives.domains#tiodn6tcyc$aajeqq7111$External$me2iiy90oq" as PropertyType
} as ModelDependenciesT;

export default ModelDependencies;
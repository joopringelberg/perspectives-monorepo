module Perspectives.ModelDependencies where

-- | This module contains all Arc identifiers that are used in the PDR source code.
-- | None of these identifiers can be changed in their models without breaking the PDR.
-- | Relevant models are:
-- |    * model://perspectives.domains#System
-- |    * model://perspectives.domains#CouchdbManagement
-- |    * model://perspectives.domains#BrokerServices

------------------------------------------------------------------------------------
-- SYSTEM
------------------------------------------------------------------------------------

perspectivesUsers :: String
perspectivesUsers = "model://perspectives.domains#tiodn6tcyc$xzummxis57$v8palo8w28"

identifiableLastName :: String
identifiableLastName = "model://perspectives.domains#tiodn6tcyc$xku8d2dtb7$xdi1iix1mm"

identifiableFirstName :: String
identifiableFirstName = "model://perspectives.domains#tiodn6tcyc$xku8d2dtb7$iyg9tyrxkt"

theWorld :: String
theWorld = "model://perspectives.domains#tiodn6tcyc$xzummxis57"

theWorldInitializer :: String
theWorldInitializer = "model://perspectives.domains#tiodn6tcyc$xzummxis57$orys2fz4uu"

perspectivesUsersCancelled :: String
perspectivesUsersCancelled = "model://perspectives.domains#tiodn6tcyc$xku8d2dtb7$zde6sowhf0"

perspectivesUsersPublicKey :: String
perspectivesUsersPublicKey = "model://perspectives.domains#tiodn6tcyc$xku8d2dtb7$atijsr67hl"

socialEnvironment :: String
socialEnvironment = "model://perspectives.domains#tiodn6tcyc$v39ynwzgqa"

mySocialEnvironment :: String
mySocialEnvironment = "model://perspectives.domains#System$MySocialEnvironment"

socialEnvironmentPersons :: String
socialEnvironmentPersons = "model://perspectives.domains#tiodn6tcyc$v39ynwzgqa$i1vm2kn9bh"

socialEnvironmentMe :: String
socialEnvironmentMe = "model://perspectives.domains#tiodn6tcyc$v39ynwzgqa$robf7hy8p0"

indexedSocialMe :: String
indexedSocialMe = "model://perspectives.domains#System$SocialMe"

systemIdentityValues :: String
systemIdentityValues = "model://perspectives.domains#System$TheWorld$SystemIdentities"

systemModelName :: String
systemModelName = "model://perspectives.domains#tiodn6tcyc" -- System

sysMe :: String
sysMe = "model://perspectives.domains#System$Me"

mySystem :: String
mySystem = "model://perspectives.domains#System$MySystem"

theSystem :: String
theSystem = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c"

sysUser :: String
sysUser = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$auftu9ldl2"

idProperty :: String
idProperty = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$auftu9ldl2$hjk0vdu7pc"

modelsInUse :: String
modelsInUse = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$ikjc543of4"

modelToRemove :: String
modelToRemove = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$ikjc543of4$s5mfnho8f6"

connectedToAMQPBroker :: String
connectedToAMQPBroker = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$External$fa87gc7pib"

currentSystemHour :: String
currentSystemHour = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$External$w1ssj5gb64"

currentSystemDate :: String
currentSystemDate = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$External$rxi6fo4opx"

cardClipBoard :: String
cardClipBoard = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$External$g4032kuj7v"

currentLanguage :: String
currentLanguage = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$External$gwccl51y21"

previousLanguage :: String
previousLanguage = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$External$oezfz7hglm"

onStartUp :: String
onStartUp = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$External$yjnjkuq1fc"

userChannel :: String
userChannel = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$auftu9ldl2$p2sny66efo"

roleWithId :: String
roleWithId = "model://perspectives.domains#tiodn6tcyc$py3zsx3ml2$yh0db2coqa"

installer :: String
installer = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$nieke8u051"

baseRepository :: String
baseRepository = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$xrydjxm0m6"

itemsOnClipboard :: String
itemsOnClipboard = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$ekp6twrmdc"

itemOnClipboardSelected :: String
itemOnClipboardSelected = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$ekp6twrmdc$d5vwugvat8"

itemOnClipboardClipboardData :: String
itemOnClipboardClipboardData = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$ekp6twrmdc$d5ddja2trj"

itemOnClipboardName :: String
itemOnClipboardName = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$ekp6twrmdc$hfbdvmb9r5"

selectedClipboardItem :: String
selectedClipboardItem = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$ji08fj0bye"

chatAspect :: String
chatAspect = "model://perspectives.domains#tiodn6tcyc$s6roz183wh"

recentContexts :: String
recentContexts = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$chuol755yg"

pinnedContexts :: String
pinnedContexts = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$x52ziv2d1i"

startContexts :: String
startContexts = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$n5t6vtg4xx"

------------------------------------------------------------------------------------
-- ROOTCONTEXT
------------------------------------------------------------------------------------
rootContext :: String
rootContext = "model://perspectives.domains#tiodn6tcyc$z7jzcnhx0j$External"

rootUser :: String
rootUser = "model://perspectives.domains#tiodn6tcyc$z7jzcnhx0j$xa7m95e3at"

isSystemModel :: String
isSystemModel = "model://perspectives.domains#tiodn6tcyc$z7jzcnhx0j$External$kznunb7kge"

rootName :: String
rootName = "model://perspectives.domains#tiodn6tcyc$z7jzcnhx0j$External$psjt7pp46v"

------------------------------------------------------------------------------------
-- INVITATION
------------------------------------------------------------------------------------
invitation :: String
invitation = "model://perspectives.domains#tiodn6tcyc$nkllrx0n2x$External"

------------------------------------------------------------------------------------
-- NOTIFICATION
------------------------------------------------------------------------------------
contextWithNotification :: String
contextWithNotification = "model://perspectives.domains#tiodn6tcyc$w5l42i0gk4"

notifications :: String
notifications = "model://perspectives.domains#tiodn6tcyc$w5l42i0gk4$qj53ocobq1"

notificationMessage :: String
notificationMessage = "model://perspectives.domains#tiodn6tcyc$w5l42i0gk4$qj53ocobq1$gnbvtwfm3d"

------------------------------------------------------------------------------------
-- SETTINGS
------------------------------------------------------------------------------------
settings :: String
settings = "model://perspectives.domains#tiodn6tcyc$ld353u5zzp$External"

allSettings :: String
allSettings = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$o9htt4std6"

------------------------------------------------------------------------------------
-- MODEL
------------------------------------------------------------------------------------
indexedRole :: String
indexedRole = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$av4pglf4vc"

indexedRoleName :: String
indexedRoleName = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$av4pglf4vc$oh30a93epb"

indexedContext :: String
indexedContext = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$oxmvuvcfjv"

indexedContextFuzzies :: String
indexedContextFuzzies = "model://perspectives.domains#System$PerspectivesSystem$IndexedContextsFuzzies"

indexedContextName :: String
indexedContextName = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$oxmvuvcfjv$iwoplexno7"

modelManifest :: String
modelManifest = "model://perspectives.domains#tiodn6tcyc$huw4rt0uj8$External"

domeinFileName :: String
domeinFileName = "model://perspectives.domains#tiodn6tcyc$huw4rt0uj8$External$s8g5zgdnzg"

domeinFileNameOnVersionedModelManifest :: String
domeinFileNameOnVersionedModelManifest = "model://perspectives.domains#tiodn6tcyc$mo66to1n9c$External$kpz5vhpz0n"

domeinFileNameWithVersion :: String
domeinFileNameWithVersion = "model://perspectives.domains#tiodn6tcyc$mo66to1n9c$External$vm82xl40mj"

versionedModelManifestModelCuid :: String
versionedModelManifestModelCuid = "model://perspectives.domains#xyfxpg3lzq$j4md0196ew$External$k8068785f1"

patch :: String
patch = "model://perspectives.domains#tiodn6tcyc$mo66to1n9c$External$ijlb1ixfjo"

build :: String
build = "model://perspectives.domains#tiodn6tcyc$mo66to1n9c$External$bxlapsi9qh"

installedPatch :: String
installedPatch = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$ikjc543of4$rwq7tydj0u"

installedBuild :: String
installedBuild = "model://perspectives.domains#tiodn6tcyc$gu4otpfq9c$ikjc543of4$g3nqpdejmj"

------------------------------------------------------------------------------------
-- COUCHDBMANAGEMENT
------------------------------------------------------------------------------------
couchdbManagementModelName :: String
couchdbManagementModelName = "model://perspectives.domains#xyfxpg3lzq" -- CouchdbManagement

versionToInstall :: String
versionToInstall = "model://perspectives.domains#xyfxpg3lzq$purp0vollf$External$elbyhcbqpi"

modelURI :: String
modelURI = "model://perspectives.domains#xyfxpg3lzq$j4md0196ew$External$lv7ca4f7ux"

versionedModelURI :: String
versionedModelURI = "model://perspectives.domains#xyfxpg3lzq$j4md0196ew$External$hp8fq71nqg"

modelURIReadable :: String
modelURIReadable = "model://perspectives.domains#xyfxpg3lzq$j4md0196ew$External$b8y8q4gqqz"

------------------------------------------------------------------------------------
-- INVITATION
------------------------------------------------------------------------------------

-- TODO, IMPORTANT! this type is no longer implemented in model:System, but it is used in code.
privateChannel :: String
privateChannel = "model://perspectives.domains#System$Invitation$PrivateChannel"

------------------------------------------------------------------------------------
-- CHANNEL
------------------------------------------------------------------------------------
channelInitiator :: String
channelInitiator = "model://perspectives.domains#tiodn6tcyc$evnddhrzdw$e3p1yjfllk"

channel :: String
channel = "model://perspectives.domains#tiodn6tcyc$evnddhrzdw"

channelDatabase :: String
channelDatabase = "model://perspectives.domains#tiodn6tcyc$evnddhrzdw$External$nq0ddmruil"

channelPartner :: String
channelPartner = "model://perspectives.domains#tiodn6tcyc$evnddhrzdw$co68ppsx5m"

------------------------------------------------------------------------------------
-- PHYSICALCONTEXT
------------------------------------------------------------------------------------
addressHost :: String
addressHost = "model://perspectives.domains#tiodn6tcyc$nt9e6zaovz$b0zwl7rvko$fw8jv41clu"

addressPort :: String
addressPort = "model://perspectives.domains#tiodn6tcyc$nt9e6zaovz$b0zwl7rvko$ni50dq6nkc"

addressRelayHost :: String
addressRelayHost = "model://perspectives.domains#tiodn6tcyc$nt9e6zaovz$b0zwl7rvko$qqlmpyn2bb"

addressRelayPort :: String
addressRelayPort = "model://perspectives.domains#tiodn6tcyc$nt9e6zaovz$b0zwl7rvko$xvqqus94vj"

------------------------------------------------------------------------------------
-- BROKERSERVICES
------------------------------------------------------------------------------------
brokerService :: String
brokerService = "model://perspectives.domains#zjuzxbqpgc$nami0qer9d"

myBrokers :: String
myBrokers = "model://perspectives.domains#BrokerServices$MyBrokers"

brokerServiceAccounts :: String
brokerServiceAccounts = "model://perspectives.domains#zjuzxbqpgc$nami0qer9d$vv80iskte1"

brokerServiceContractInUse :: String
brokerServiceContractInUse = "model://perspectives.domains#zjuzxbqpgc$fmx94rj1sn$tuo5fmjocd"

brokerEndpoint :: String
brokerEndpoint = "model://perspectives.domains#zjuzxbqpgc$viqjnzqjvv$External$w39b22ossh"

brokerServiceExchange :: String
brokerServiceExchange = "model://perspectives.domains#zjuzxbqpgc$viqjnzqjvv$External$csguz2yqnl"

brokerContract :: String
brokerContract = "model://perspectives.domains#zjuzxbqpgc$viqjnzqjvv"

accountHolder :: String
accountHolder = "model://perspectives.domains#zjuzxbqpgc$viqjnzqjvv$ox3efdbz2j"

accountHolderName :: String
accountHolderName = "model://perspectives.domains#zjuzxbqpgc$viqjnzqjvv$ox3efdbz2j$ezjf5fr6ea"

accountHolderPassword :: String
accountHolderPassword = "model://perspectives.domains#zjuzxbqpgc$viqjnzqjvv$ox3efdbz2j$y5ag1miyv1"

accountHolderQueueName :: String
accountHolderQueueName = "model://perspectives.domains#zjuzxbqpgc$viqjnzqjvv$External$nje6vo9y2u"

-- "model:perspectives.domains#Couchdb$ContextInstances"

------------------------------------------------------------------------------------
-- AUTHENTICATION
------------------------------------------------------------------------------------
userWithCredentials :: String
userWithCredentials = "model://perspectives.domains#tiodn6tcyc$pl2u8t8nih"

userWithCredentialsUsername :: String
userWithCredentialsUsername = "model://perspectives.domains#tiodn6tcyc$pl2u8t8nih$ht63onkwto"

userWithCredentialsPassword :: String
userWithCredentialsPassword = "model://perspectives.domains#tiodn6tcyc$pl2u8t8nih$xt48hxu7dy"

userWithCredentialsAuthorizedDomain :: String
userWithCredentialsAuthorizedDomain = "model://perspectives.domains#tiodn6tcyc$pl2u8t8nih$ym5zfr0ghe"

------------------------------------------------------------------------------------
-- BODIESWITHACCOUNTS
------------------------------------------------------------------------------------
bodiesWithAccountsModelName :: String
bodiesWithAccountsModelName = "model://perspectives.domains#bxxptg50jp" -- BodiesWithAccounts

------------------------------------------------------------------------------------
-- SHAREDFILESERVICES
------------------------------------------------------------------------------------
sharedFileServices :: String
sharedFileServices = "model://perspectives.domains#SharedFileServices"

mySharedFileServices :: String
mySharedFileServices = "model://perspectives.domains#SharedFileServices$MySharedFileServices"

actualSharedFileServer :: String
actualSharedFileServer = "model://perspectives.domains#xjrfkxrzyt$ul1y1ukzye$jx2xqh4f1w"

fileShareCredentials :: String
fileShareCredentials = "model://perspectives.domains#xjrfkxrzyt$fvq0e98r87$mnfe8z3j7a"


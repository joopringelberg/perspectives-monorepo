module Perspectives.ApiTypes where

import Prelude

import Control.Monad.Except (except)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object (Object, empty) as F
import Partial.Unsafe (unsafePartial)
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Simple.JSON (class ReadForeign, class WriteForeign) 
import Unsafe.Coerce (unsafeCoerce)

-- | Identifies Requests with Responses.
type CorrelationIdentifier = Int

type ID = String
type Value = String
type ContextID = String

-----------------------------------------------------------
-- APIEFFECT 
-----------------------------------------------------------
-- | The type of functions that are passed on as callbacks through the API.
type ApiEffect = ResponseWithWarnings -> Effect Unit

mkApiEffect :: Maybe Foreign -> ApiEffect
mkApiEffect f = (unsafeCoerce $ unsafePartial $ fromJust f) <<< convertResponse
-----------------------------------------------------------
-- REQUEST
-----------------------------------------------------------

-- | The Perspectives Core accepts only a limited set of request types.
data RequestType =
  -- Consulting
    GetBinding
  | GetRoleBinders
  | GetRolContext
  | GetContextType
  | GetRolType
  | GetRoleKind
  | GetUnqualifiedRolType
  | GetRol
  | GetUnqualifiedRol
  | GetProperty
  | AddProperty
  | GetPropertyFromLocalName
  | GetViewProperties
  | GetMeForContext
  | GetAllMyRoleTypes
  | IsMe
  | GetSystemIdentifier
  | GetPerspectivesUser 
  | GetMeInContext
  | GetFileShareCredentials
  | GetChatParticipants
  | GetPerspectives
  | GetPerspective
  | GetPerspectiveForUser
  | GetScreen
  | GetTableForm
  | GetContextActions
  | GetRolesWithProperties
  | GetLocalRoleSpecialisation 
  | MatchContextName
  | GetCouchdbUrl
  | GetRoleName
  | GetFile
  | GetPublicUrl
  | GetSelectedRoleFromClipboard
  | GetSettings
  | GetWiderContexts
  | RemoveRoleFromClipboard
  | AddRoleToClipboard

  -- Pure Deltas
  | CreateContext
  | CreateContext_
  | CreateRole
  | CreateRole_
  | RemoveRole
  | RemoveContext
  | DeleteRole
  | Bind_ -- Formerly SetBinding
  | RemoveBinding
  | SetProperty
  | DeleteProperty

  | Action
  | ContextAction

  | SetPreferredUserRoleType

  | ImportContexts
  | ImportTransaction

  | SaveFile

  | Save

  -- Conveniences
  | Bind -- Formerly BindInNewRol

  -- Meta
  | Unsubscribe
  | ShutDown
  | CheckBinding
  | WrongRequest
  | EvaluateRoleInstance
  | RestoreContextForUser

derive instance genericRequestType :: Generic RequestType _

instance decodeRequestType :: ReadForeign RequestType where
  readImpl s = except $ Right $ case unsafeCoerce s of
    "GetBinding" -> GetBinding
    "GetRoleBinders" -> GetRoleBinders
    "GetRol" -> GetRol
    "GetUnqualifiedRol" -> GetUnqualifiedRol
    "GetRolContext" -> GetRolContext
    "GetContextType" -> GetContextType
    "GetProperty" -> GetProperty
    "AddProperty" -> AddProperty
    "GetPropertyFromLocalName" -> GetPropertyFromLocalName
    "GetViewProperties" -> GetViewProperties
    "GetMeForContext" -> GetMeForContext
    "GetAllMyRoleTypes" -> GetAllMyRoleTypes
    "IsMe" -> IsMe
    "GetFile" -> GetFile
    "GetPublicUrl" -> GetPublicUrl
    "GetSelectedRoleFromClipboard" -> GetSelectedRoleFromClipboard
    "GetSettings" -> GetSettings
    "GetWiderContexts" -> GetWiderContexts
    "RemoveRoleFromClipboard" -> RemoveRoleFromClipboard
    "AddRoleToClipboard" -> AddRoleToClipboard


    "GetSystemIdentifier" -> GetSystemIdentifier
    "GetPerspectivesUser" -> GetPerspectivesUser
    "GetMeInContext" -> GetMeInContext
    "GetFileShareCredentials" -> GetFileShareCredentials
    "GetChatParticipants" -> GetChatParticipants
    "GetPerspectives" -> GetPerspectives
    "GetPerspective" -> GetPerspective
    "GetPerspectiveForUser" -> GetPerspectiveForUser
    "GetScreen" -> GetScreen
    "GetTableForm" -> GetTableForm
    "GetContextActions" -> GetContextActions
    "GetRolesWithProperties" -> GetRolesWithProperties
    "GetLocalRoleSpecialisation" -> GetLocalRoleSpecialisation
    "MatchContextName" -> MatchContextName
    "GetCouchdbUrl" -> GetCouchdbUrl
    "GetRoleName" -> GetRoleName
    "Unsubscribe" -> Unsubscribe
    "ShutDown" -> ShutDown
    "GetRolType" -> GetRolType
    "GetRoleKind" -> GetRoleKind
    "GetUnqualifiedRolType" -> GetUnqualifiedRolType
    "CreateContext" -> CreateContext
    "CreateContext_" -> CreateContext_
    "CreateRole" -> CreateRole
    "CreateRole_" -> CreateRole_
    "RemoveRole" -> RemoveRole
    "RemoveContext" -> RemoveContext
    "DeleteRole" -> DeleteRole
    "Bind_" -> Bind_
    "RemoveBinding" -> RemoveBinding
    "Bind" -> Bind
    "CheckBinding" -> CheckBinding
    "SetProperty" -> SetProperty
    "DeleteProperty" -> DeleteProperty
    "Action" -> Action
    "ContextAction" -> ContextAction
    "SetPreferredUserRoleType" -> SetPreferredUserRoleType
    "ImportContexts" -> ImportContexts
    "ImportTransaction" -> ImportTransaction
    "SaveFile" -> SaveFile
    "Save" -> Save
    "EvaluateRoleInstance" -> EvaluateRoleInstance
    "RestoreContextForUser" -> RestoreContextForUser
    _ -> WrongRequest

instance showRequestType :: Show RequestType where
  show = genericShow

instance eqRequestType :: Eq RequestType where
  eq r1 r2 = show r1 == show r2

-- | A request as can be sent to the core.
newtype Request = Request RequestRecord

type RequestRecord =
  { request :: RequestType
  , subject :: String
  , predicate :: String
  , object :: String
  , reactStateSetter :: Maybe Foreign
  , corrId :: CorrelationIdentifier
  , contextDescription :: Foreign
  , rolDescription :: Maybe RolSerialization
  , authoringRole :: Maybe String
  , onlyOnce :: Boolean}

derive instance genericRequest :: Generic Request _

derive instance newTypeRequest :: Newtype Request _

showRequestRecord :: RequestRecord -> String
showRequestRecord {request, subject, predicate} = "{" <> show request <> ", " <> subject <> ", " <> predicate <> "}"

instance showRequest :: Show Request where
  show (Request r) = showRequestRecord r

derive newtype instance ReadForeign Request

newtype RecordWithCorrelationidentifier = RecordWithCorrelationidentifier {corrId :: CorrelationIdentifier, reactStateSetter :: Maybe Foreign}

derive instance Generic RecordWithCorrelationidentifier _

derive newtype instance ReadForeign RecordWithCorrelationidentifier

-----------------------------------------------------------
-- RESPONSE
-----------------------------------------------------------

-- | The Perspectives Core responds with query results, where a query is the request for the
-- | 'objects' in the basic fact <subject, predicate, object>.
type Object = String

data Response = Result CorrelationIdentifier (Array String) | Error CorrelationIdentifier String

type Warnings = Array String
data ResponseWithWarnings = ResultWithWarnings CorrelationIdentifier (Array String) Warnings | ErrorWithWarnings CorrelationIdentifier String Warnings

convertResponse :: ResponseWithWarnings -> Foreign
convertResponse (ResultWithWarnings i s warnings) = unsafeToForeign {responseType: "APIresult", corrId: i, result: s, warnings}
convertResponse (ErrorWithWarnings i s warnings) = unsafeToForeign {responseType: "APIerror", corrId: i, error: s, warnings}

-----------------------------------------------------------
-- SERIALIZATION OF CONTEXTS AND ROLES ON THE API
-- These types are simpler versions of PerspectContext and PerspectRol.
-- Not meant to put into couchdb, but to use as transport format over the API (whether the TCP or internal channel).
-----------------------------------------------------------

-----------------------------------------------------------
---- CONTEXTSSERIALIZATION
-----------------------------------------------------------
newtype ContextsSerialisation = ContextsSerialisation (Array ContextSerialization)

derive instance genericContextsSerialisation :: Generic ContextsSerialisation _

derive newtype instance WriteForeign ContextsSerialisation
derive newtype instance ReadForeign ContextsSerialisation

-----------------------------------------------------------
---- CONTEXTSERIALIZATION
-----------------------------------------------------------
newtype ContextSerialization = ContextSerialization ContextSerializationRecord

type ContextSerializationRecord =
  { id :: Maybe String
  , prototype :: Maybe ContextID
  , ctype :: ContextID
  , rollen :: F.Object (SerializableNonEmptyArray RolSerialization)
  , externeProperties :: PropertySerialization
}

derive newtype instance eqContextSerialization :: Eq ContextSerialization

derive instance genericContextSerialization :: Generic ContextSerialization _

instance showContextSerialization :: Show ContextSerialization where
  show (ContextSerialization {id, ctype, rollen, externeProperties}) =
    "{ id=" <> show id <> ", ctype=" <> ctype <> ", rollen=" <> show rollen <> ", externeProperties=" <> show externeProperties

derive newtype instance WriteForeign ContextSerialization
derive newtype instance ReadForeign ContextSerialization

defaultContextSerializationRecord :: ContextSerializationRecord
defaultContextSerializationRecord = {id: Nothing, prototype: Nothing, ctype: "", rollen: F.empty, externeProperties: PropertySerialization F.empty}

instance prettyPrintContextSerialization :: PrettyPrint ContextSerialization where
  prettyPrint' tab (ContextSerialization r) = "ContextSerialization " <> prettyPrint' (tab <> "  ") r

-----------------------------------------------------------
---- ROLSERIALIZATION
-----------------------------------------------------------
newtype RolSerialization = RolSerialization
  { id :: Maybe ID
  , properties :: PropertySerialization
  , binding :: Maybe ID
}

derive instance genericRolSerialization :: Generic RolSerialization _

derive newtype instance eqRolSerialization :: Eq RolSerialization

instance showRolSerialization :: Show RolSerialization where
  show (RolSerialization {properties, binding}) = "{ " <> show properties <> ", " <> show binding <> " }"

derive newtype instance WriteForeign RolSerialization
derive newtype instance ReadForeign RolSerialization

instance prettyPrintRolSerialization :: PrettyPrint RolSerialization where
  prettyPrint' tab (RolSerialization r) = "RolSerialization " <> prettyPrint' (tab <> "  ") r

-----------------------------------------------------------
---- PROPERTYSERIALIZATION
-----------------------------------------------------------
newtype PropertySerialization = PropertySerialization (F.Object (Array Value))

derive instance genericPropertySerialization :: Generic PropertySerialization _

derive newtype instance eqPropertySerialization :: Eq PropertySerialization

instance showPropertySerialization :: Show PropertySerialization where
  show (PropertySerialization s) =  show s

derive newtype instance WriteForeign PropertySerialization
derive newtype instance ReadForeign PropertySerialization

instance prettyPrintPropertySerialization :: PrettyPrint PropertySerialization where
  prettyPrint' tab (PropertySerialization r) = "PropertySerialization " <> prettyPrint' (tab <> "  ") r

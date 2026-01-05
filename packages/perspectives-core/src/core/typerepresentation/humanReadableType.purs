module Perspectives.HumanReadableType where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Identifiers (typeUri2LocalName)
import Perspectives.ModelTranslation (translateTypeString)
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.State (State(..))
import Perspectives.Representation.TypeIdentifiers (ActionIdentifier(..), CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), StateIdentifier, ViewType)
import Perspectives.Representation.View (View(..))
import Perspectives.Sidecar.ToReadable (toReadable)

class HumanReadableType i where
  translateType :: i -> MonadPerspectives String

instance HumanReadableType EnumeratedRoleType where
  translateType r@(EnumeratedRoleType er) = do
    translation <- translateTypeString er
    if translation == er then do
      EnumeratedRoleType readable <- toReadable r
      case typeUri2LocalName readable of
        Just ln -> pure ln
        Nothing -> case typeUri2LocalName er of
          Just ln2 -> pure ln2
          Nothing -> pure er
    else
      pure translation

instance HumanReadableType EnumeratedPropertyType where
  translateType p@(EnumeratedPropertyType ep) = do
    translation <- translateTypeString ep
    if translation == ep then do
      EnumeratedPropertyType readable <- toReadable p
      case typeUri2LocalName readable of
        Just ln -> pure ln
        Nothing -> case typeUri2LocalName ep of
          Just ln2 -> pure ln2
          Nothing -> pure ep
    else
      pure translation

instance HumanReadableType CalculatedRoleType where
  translateType r@(CalculatedRoleType cr) = do
    translation <- translateTypeString cr
    if translation == cr then do
      CalculatedRoleType readable <- toReadable r
      case typeUri2LocalName readable of
        Just ln -> pure ln
        Nothing -> case typeUri2LocalName cr of
          Just ln2 -> pure ln2
          Nothing -> pure cr
    else
      pure translation

instance HumanReadableType CalculatedPropertyType where
  translateType p@(CalculatedPropertyType cp) = do
    translation <- translateTypeString cp
    if translation == cp then do
      CalculatedPropertyType readable <- toReadable p
      case typeUri2LocalName readable of
        Just ln -> pure ln
        Nothing -> case typeUri2LocalName cp of
          Just ln2 -> pure ln2
          Nothing -> pure cp
    else
      pure translation

instance HumanReadableType ContextType where
  translateType ct@(ContextType c) = do
    translation <- translateTypeString c
    if translation == c then do
      ContextType readable <- toReadable ct
      case typeUri2LocalName readable of
        Just ln -> pure ln
        Nothing -> case typeUri2LocalName c of
          Just ln2 -> pure ln2
          Nothing -> pure c
    else
      pure translation

-- As we have no views in the translation yaml file yet, we directly use the readableName property.
instance HumanReadableType ViewType where
  translateType vt = getPerspectType vt >>= \(View { readableName }) -> pure
    ( case typeUri2LocalName (unwrap readableName) of
        Just dn -> dn
        Nothing -> unwrap vt
    )

-- As we have no states in the translation yaml file yet, we directly use the readableName property.
instance HumanReadableType StateIdentifier where
  translateType st = getPerspectType st >>= \(State r) -> pure
    ( case typeUri2LocalName (unwrap r.readableName) of
        Just dn -> dn
        Nothing -> unwrap st
    )

instance HumanReadableType ActionIdentifier where
  translateType (ActionIdentifier a) = do
    translation <- translateTypeString a
    if translation == a then do
      case typeUri2LocalName a of
        Just ln -> pure ln
        Nothing -> case typeUri2LocalName a of
          Just ln2 -> pure ln2
          Nothing -> pure a
    else
      pure translation

instance HumanReadableType PropertyType where
  translateType (ENP p) = translateType p
  translateType (CP p) = translateType p

instance HumanReadableType RoleType where
  translateType (ENR r) = translateType r
  translateType (CR r) = translateType r
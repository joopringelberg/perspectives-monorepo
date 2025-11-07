-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

module Perspectives.Representation.Class.PersistentType
  ( module Perspectives.Couchdb.Revision
  , module Perspectives.Representation.Class.PersistentType
  , module Perspectives.Representation.TypeIdentifiers
  , readable2stable
  ) where

import Perspectives.Couchdb.Revision

import Control.Monad.Except (catchError, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Time.Duration (Milliseconds(..))
import Effect.AVar (AVar)
import Effect.Aff (delay)
import Effect.Aff.AVar (empty, put, read)
import Effect.Exception (error)
import Foreign.Object (insert, lookup) as FO
import Foreign.Object (lookup)
import Perspectives.CoreTypes (MP, MonadPerspectives, TypeFix(..), TypeKind(..), TypeToBeMapped(..))
import Perspectives.DomeinCache (modifyDomeinFileInCache, retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Identifiers (typeUri2ModelUri)
import Perspectives.ModelDependencies.ReadableStableMappings (modelStableToReadable)
import Perspectives.PerspectivesState (getModelUnderCompilation, getTypeToBeFixed)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Identifiable (class Identifiable, identifier)
import Perspectives.Representation.Context (Context)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.State (State)
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PerspectiveType(..), StateIdentifier(..), ViewType(..))
import Perspectives.Representation.View (View)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..), Stable)
import Prelude (class Eq, class Show, Unit, bind, const, pure, show, unit, ($), (<<<), (<>), (>>=), (<$>), discard, (==))

type Namespace = String

class (Show i, Identifiable v i, Revision v, Newtype i String, Eq v) <= PersistentType v i | v -> i, i -> v where
  retrieveFromDomein :: i -> ModelUri Stable -> MonadPerspectives v
  cacheInDomeinFile :: i -> v -> MonadPerspectives Unit
  toTypeToBeMapped :: i -> TypeToBeMapped
  fromTypeToBeMapped :: Partial => TypeToBeMapped -> i

-- | Get any type representation for Perspectives, either from cache or from a model file in
-- | couchdb.
getPerspectType :: forall v i. PersistentType v i => i -> MonadPerspectives v
getPerspectType id = do
  id' <- case (typeUri2ModelUri (unwrap id)) of
    Nothing -> throwError (error $ "getPerspectType cannot retrieve type with incorrectly formed id: '" <> show id <> "'.")
    Just modelUri -> case lookup modelUri modelStableToReadable of
      -- This is a proxy for having a Stable type (it works for the system models).
      Just _ -> pure id
      -- No Stable type, so we need to switch, unless this model is under compilation.
      Nothing -> readable2stable id
  case typeUri2ModelUri (unwrap id') of
    Nothing -> throwError (error $ "getPerspectType cannot retrieve type with incorrectly formed id: '" <> show id <> "'.")
    (Just ns) -> retrieveFromDomein id' (ModelUri ns)

-- | Convert a Readable type identifier to a Stable one.
-- | Ensures a type is Stable, unless the model it belongs to is currently under compilation.
readable2stable :: forall v i. PersistentType v i => i -> MonadPerspectives i
readable2stable i0 = do
  case (typeUri2ModelUri (unwrap i0)) of
    Nothing -> throwError (error $ "getPerspectType cannot retrieve type with incorrectly formed id: '" <> show i0 <> "'.")
    Just modelUri -> do
      mu <- getModelUnderCompilation
      case mu of
        Just compilingModelUri | compilingModelUri == ModelUri modelUri -> pure i0
        _ -> do
          -- Create a hotline for this specific request.
          -- Fill it with a Fix message.
          hotline <- lift $ empty
          lift $ put (Fix (toTypeToBeMapped i0)) hotline
          typeToBeFixed <- getTypeToBeFixed
          -- Push the hotline into the coordination AVar so it will be picked up by the fixer.
          lift $ put (TypeFixingHotline hotline) typeToBeFixed
          -- Wait for the fixer to pick up the Fix and post the result or an error.
          hotlineOrError <- waitForHotline hotline
          case hotlineOrError of
            Right (Fixed (TypeToBeMapped { kind, fqn })) -> do
              -- lift $ log ("getPerspectType: received Fixed; fqn=" <> fqn)
              pure (wrap fqn)
            Left e -> do
              -- lift $ log ("getPerspectType: TypeFixingError (hotline) for " <> show i0 <> ": " <> e)
              throwError (error ("getPerspectType: error during type fixing: " <> e))
            _ -> throwError (error "getPerspectType: unexpected hotline state.")

  where
  waitForHotline :: AVar TypeFix -> MonadPerspectives (Either String TypeFix)
  waitForHotline hotline = loop false
    where
    loop :: Boolean -> MonadPerspectives (Either String TypeFix)
    loop logged = do
      tf <- lift $ read hotline
      case tf of
        Fixed _ -> pure (Right tf)
        TypeFixingError e -> pure (Left e)
        Fix _ -> do
          -- Log once on the first wait and add a small delay to avoid busy-waiting.
          -- if logged then pure unit else lift $ log "getPerspectType: waiting for hotline..."
          lift $ delay (Milliseconds 1.0)
          loop true
        _ -> pure $ Left "getPerspectType: unexpected coordination state."

tryGetPerspectType :: forall v i. PersistentType v i => i -> MonadPerspectives (Maybe v)
tryGetPerspectType id = catchError ((getPerspectType id) >>= (pure <<< Just))
  \_ -> pure Nothing

getEnumeratedRole :: EnumeratedRoleType -> MP EnumeratedRole
getEnumeratedRole = getPerspectType

getCalculatedRole :: CalculatedRoleType -> MP CalculatedRole
getCalculatedRole = getPerspectType

getEnumeratedProperty :: EnumeratedPropertyType -> MP EnumeratedProperty
getEnumeratedProperty = getPerspectType

getCalculatedProperty :: CalculatedPropertyType -> MP CalculatedProperty
getCalculatedProperty = getPerspectType

getContext :: ContextType -> MP Context
getContext = getPerspectType

getView :: ViewType -> MP View
getView = getPerspectType

getState :: StateIdentifier -> MP State
getState = getPerspectType

tryGetState :: StateIdentifier -> MP (Maybe State)
tryGetState = tryGetPerspectType

typeExists :: forall v i. PersistentType v i => i -> MP Boolean
typeExists id = catchError (((getPerspectType id) :: MP v) >>= pure <<< const true) \e -> pure false

-----------------------------------------------------------
-- ADD TO A DOMEINFILE
-----------------------------------------------------------
addContextToDomeinFile :: Context -> DomeinFile Stable -> DomeinFile Stable
addContextToDomeinFile c (DomeinFile dff@{ contexts }) = DomeinFile dff { contexts = FO.insert (unwrap $ (identifier c :: ContextType)) c contexts }

addEnumeratedRoleToDomeinFile :: EnumeratedRole -> DomeinFile Stable -> DomeinFile Stable
addEnumeratedRoleToDomeinFile c (DomeinFile dff@{ enumeratedRoles }) = DomeinFile dff { enumeratedRoles = FO.insert (unwrap $ (identifier c :: EnumeratedRoleType)) c enumeratedRoles }

-- addQueryFunctionToDomeinFile :: String -> QueryFunction -> DomeinFile -> DomeinFile
-- addQueryFunctionToDomeinFile id c (DomeinFile dff@{queries}) = DomeinFile dff {queries = FO.insert (unwrap $ (identifier c :: QueryFunctionType)) c queries}

-----------------------------------------------------------
-----------------------------------------------------------
ifNamespace :: forall i. Newtype i String => i -> (DomeinFile Stable -> DomeinFile Stable) -> MP Unit
ifNamespace i modifier = maybe (pure unit) (modifyDomeinFileInCache modifier) (ModelUri <$> typeUri2ModelUri (unwrap i))

-- Put error boundaries around calls to this function.
retrieveFromDomein_
  :: forall v i
   . PersistentType v i
  => i
  -> (DomeinFile Stable -> Maybe v)
  -> ModelUri Stable
  -> (MonadPerspectives v)
retrieveFromDomein_ id lookupFunction ns = do
  df <- retrieveDomeinFile ns
  case lookupFunction df of
    Nothing -> throwError $ error ("retrieveFromDomein_: cannot find definition of " <> (show id) <> " for " <> show ns)
    (Just v) -> pure v

-----------------------------------------------------------
-- INSTANCES
-----------------------------------------------------------
instance persistentContext :: PersistentType Context ContextType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{ contexts }) -> DomeinFile dff { contexts = FO.insert (unwrap i) v contexts })
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile { contexts }) -> FO.lookup (unwrap i) contexts)
  toTypeToBeMapped (ContextType i) = TypeToBeMapped { kind: KContext, fqn: i }
  fromTypeToBeMapped (TypeToBeMapped { kind: KContext, fqn }) = ContextType fqn

instance persistentEnumeratedRole :: PersistentType EnumeratedRole EnumeratedRoleType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{ enumeratedRoles }) -> DomeinFile dff { enumeratedRoles = FO.insert (unwrap i) v enumeratedRoles })
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile { enumeratedRoles }) -> FO.lookup (unwrap i) enumeratedRoles)
  toTypeToBeMapped i = TypeToBeMapped { kind: KEnumeratedRole, fqn: unwrap i }
  fromTypeToBeMapped (TypeToBeMapped { kind: KEnumeratedRole, fqn }) = EnumeratedRoleType fqn

instance persistentCalculatedRole :: PersistentType CalculatedRole CalculatedRoleType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{ calculatedRoles }) -> DomeinFile dff { calculatedRoles = FO.insert (unwrap i) v calculatedRoles })
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile { calculatedRoles }) -> FO.lookup (unwrap i) calculatedRoles)
  toTypeToBeMapped i = TypeToBeMapped { kind: KCalculatedRole, fqn: unwrap i }
  fromTypeToBeMapped (TypeToBeMapped { kind: KCalculatedRole, fqn }) = CalculatedRoleType fqn

instance persistentEnumeratedProperty :: PersistentType EnumeratedProperty EnumeratedPropertyType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{ enumeratedProperties }) -> DomeinFile dff { enumeratedProperties = FO.insert (unwrap i) v enumeratedProperties })
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile { enumeratedProperties }) -> FO.lookup (unwrap i) enumeratedProperties)
  toTypeToBeMapped i = TypeToBeMapped { kind: KEnumeratedProperty, fqn: unwrap i }
  fromTypeToBeMapped (TypeToBeMapped { kind: KEnumeratedProperty, fqn }) = EnumeratedPropertyType fqn

instance persistentCalculatedProperty :: PersistentType CalculatedProperty CalculatedPropertyType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{ calculatedProperties }) -> DomeinFile dff { calculatedProperties = FO.insert (unwrap i) v calculatedProperties })
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile { calculatedProperties }) -> FO.lookup (unwrap i) calculatedProperties)
  toTypeToBeMapped i = TypeToBeMapped { kind: KCalculatedProperty, fqn: unwrap i }
  fromTypeToBeMapped (TypeToBeMapped { kind: KCalculatedProperty, fqn }) = CalculatedPropertyType fqn

instance persistentView :: PersistentType View ViewType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{ views }) -> DomeinFile dff { views = FO.insert (unwrap i) v views })
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile { views }) -> FO.lookup (unwrap i) views)
  toTypeToBeMapped i = TypeToBeMapped { kind: KView, fqn: unwrap i }
  fromTypeToBeMapped (TypeToBeMapped { kind: KView, fqn }) = ViewType fqn

instance persistentState :: PersistentType State StateIdentifier where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{ states }) -> DomeinFile dff { states = FO.insert (unwrap i) v states })
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile { states }) -> FO.lookup (unwrap i) states)
  toTypeToBeMapped i = TypeToBeMapped { kind: KState, fqn: unwrap i }
  fromTypeToBeMapped (TypeToBeMapped { kind: KState, fqn }) = StateIdentifier fqn

-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.PublicationRecovery where

import Prelude

import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.AvarMonadAsk (modify)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Decacheable (decache)
import Effect (Effect)
import Effect.Aff.AVar (new)
import Effect.Aff.Class (liftAff)
import Perspectives.Persistence.API (deleteDatabase, getDocument, withDatabase)
import Foreign.Object (insert)
import Perspectives.PerspectivesState (defaultRuntimeOptions)
import Perspectives.Persistent (saveMarkedResources)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), PerspectivesUser(..))
import Perspectives.Sync.HandleTransaction (Delta(..), executeDeltas)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (createTransaction)
import Perspectives.Sync.VersionedDelta (DeltaEnvelope(..), parseIncomingDelta)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.TypesForDeltas (UniverseContextDelta(..), UniverseRoleDelta(..))
import Test.PDRInstance (noBus, testPouchdbUser, withPDRCached)
import Test.PDRInstance.Types (runInPDR)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "Public versioned manifest creation" do
  test "recreates a previously cached public context after its database is deleted" do
    withPDRCached (testPouchdbUser "alice") defaultRuntimeOptions Nothing noBus "test/pdr-snapshot/universe/alice" \pdr ->
      runInPDR pdr do
        let
          database = "https://publication.invalid/cw_publication_regression/"
          contextId = ContextInstance "pub:https://publication.invalid/cw_publication_regression/#publication@2.0"
          roleId = RoleInstance "pub:https://publication.invalid/cw_publication_regression/#publication@2.0$External"
          subject = ENR $ EnumeratedRoleType "model://perspectives.domains#xyfxpg3lzq$purp0vollf$btvwbqimks"
          applyCreation = case parseIncomingDelta externalDelta, parseIncomingDelta contextDelta of
            Right (UniverseRoleEnvelope (UniverseRoleDelta role)), Right (UniverseContextEnvelope (UniverseContextDelta context)) ->
              executeDeltas
                [ URD signed (UniverseRoleDelta $ role { id = contextId, roleInstance = roleId })
                , UCD signed (UniverseContextDelta $ context { id = contextId })
                ]
            _, _ -> lift $ liftAff $ assert "Both creation deltas must decode to their own envelope types" false
        withDatabase "cw_publication_regression" \db ->
          modify \s -> s { databases = insert database db s.databases }
        transaction <- liftAff $ createTransaction subject false >>= new
        runReaderT applyCreation transaction
        saveMarkedResources
        PerspectContext context <- getDocument database "publication@2.0"
        PerspectRol external <- getDocument database "publication@2.0$External"
        liftAff $ equal contextId context.id
        liftAff $ equal roleId external.id
        deleteDatabase "cw_publication_regression"
        withDatabase "cw_publication_regression" \db ->
          modify \s -> s { databases = insert database db s.databases }
        decache contextId
        decache roleId
        replay <- liftAff $ createTransaction subject false >>= new
        runReaderT applyCreation replay
        saveMarkedResources
        PerspectContext recreated <- getDocument database "publication@2.0"
        PerspectRol recreatedExternal <- getDocument database "publication@2.0$External"
        liftAff $ equal contextId recreated.id
        liftAff $ equal roleId recreatedExternal.id
        deleteDatabase "cw_publication_regression"
  where
  signed = SignedDelta { author: PerspectivesUser "alice", encryptedDelta: "", signature: Nothing }
  externalDelta = "{\"authorizedRole\":\"model://perspectives.domains#xyfxpg3lzq$purp0vollf$ghdhfh0c6p@12.4\",\"authorizedRoleKind\":\"ENR\",\"contextType\":\"model://perspectives.domains#xyfxpg3lzq$j4md0196ew@12.4\",\"deltaFormatVersion\":2,\"deltaType\":\"ConstructExternalRole\",\"id\":\"publication@2.0\",\"resourceKey\":\"def:#publication@2.0$External\",\"resourceVersion\":0,\"roleInstance\":\"publication@2.0$External\",\"roleType\":\"model://perspectives.domains#xyfxpg3lzq$j4md0196ew$External@12.4\",\"subject\":\"model://perspectives.domains#xyfxpg3lzq$purp0vollf$btvwbqimks@12.4\",\"subjectKind\":\"ENR\"}"
  contextDelta = "{\"contextType\":\"model://perspectives.domains#xyfxpg3lzq$j4md0196ew@12.4\",\"deltaFormatVersion\":2,\"deltaType\":\"ConstructEmptyContext\",\"id\":\"publication@2.0\",\"resourceKey\":\"def:#publication@2.0\",\"resourceVersion\":0,\"subject\":\"model://perspectives.domains#xyfxpg3lzq$purp0vollf$btvwbqimks@12.4\",\"subjectKind\":\"ENR\"}"

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
-- Full text of this license can be found in the LICENSE directory in the
-- projects root.

-- END LICENSE

module Test.Layer3Scaffold
  ( TopicLogLevelPair
  , LogConfiguration
  , emptyLogConfiguration
  , ModelTest
  , SynchronisationResults
  , SynchronisationModelConfiguration
  , TestModelLoadMethod(..)
  , getSynchronisationResults
  , getSynchronisationResultsOverAMQP
  , executeModelTest
  , runSynchronisationSuite
  , runSynchronisationSuiteOverAMQP
  ) where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (runExceptT)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, bracket, error, launchAff_, message)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (Ref, read, write)
import Foreign.Object (empty) as OBJ
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.RunAction (runContextAction)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), RuntimeOptions, (##=), (##>))
import Perspectives.Extern.Couchdb (addModelToLocalStore_)
import Perspectives.Identifiers (buitenRol, modelUri2LocalName, unversionedModelUri)
import Perspectives.Instances.Builders (createAndAddRoleInstance, constructContext)
import Perspectives.Instances.ObjectGetters (binding, getEnumeratedRoleInstances)
import Perspectives.Logging (ansiMagenta, ansiRed, infoTest)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Names (lookupIndexedContext)
import Perspectives.Persistence.API (PouchdbUser)
import Perspectives.Persistent (tryGetPerspectContext)
import Perspectives.PerspectivesState (defaultRuntimeOptions, getLogConfig, getPerspectivesUser, setLogConfig, setTopicLogLevel)
import Perspectives.Query.UnsafeCompiler (getPropertyValues)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, PerspectivesUser, RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), IndexedContext(..), PropertyType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction', shareWithPeers)
import Perspectives.Sidecar.StableIdMapping (ModelUri(..), Stable)
import Perspectives.Sidecar.ToStable (toStable)
import Perspectives.TypePersistence.LoadArc (loadCompileAndStoreArcFile_)
import Test.PDRInstance (SynchronisationResult, connectPDRs, pollUntil, pollUntilTestFinishes, snapshotPDR, testPouchdbUser, withTwoPDRsCached, withTwoPDRsCachedNoBus)
import Test.PDRInstance.Types (PDRInstance, runInPDR)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

type TopicLogLevelPair =
  { topic :: LogTopic
  , logLevel :: LogLevel
  }

type LogConfiguration =
  { pdrA :: Array TopicLogLevelPair
  , pdrB :: Array TopicLogLevelPair
  }

emptyLogConfiguration :: LogConfiguration
emptyLogConfiguration =
  { pdrA: []
  , pdrB: []
  }

type ModelTest =
  { testContextTypeName :: String
  , logConfiguration :: Maybe LogConfiguration
  }

type SynchronisationResults = Array SynchronisationResult

-- | LoadModelFromRepository fetches the model as published; CompileModelFromSource compiles
-- | it anew, independently, in both Alice's and Bob's PDR (compilation is not synchronised).
data TestModelLoadMethod
  = LoadModelFromRepository
  | CompileModelFromSource
      { sourcePath :: String
      , modelUriReadable :: String
      , basedOnVersion :: Maybe String
      }

type SynchronisationModelConfiguration =
  { suiteName :: String
  , snapshotDirAlice :: String
  , snapshotDirBob :: String
  , testModel :: String
  , testModelLoadMethod :: TestModelLoadMethod
  , indexedTestContext :: String
  , testAppManager :: String
  , testAppFollowerType :: String
  , testsType :: String
  , testSucceededProperty :: String
  , testNameProperty :: String
  , setupLogConfiguration :: LogConfiguration
  , tests :: Array ModelTest
  }

type WithTwoPDRsCachedLike =
  forall a
   . PouchdbUser
  -> RuntimeOptions
  -> Maybe String
  -> String
  -> PouchdbUser
  -> RuntimeOptions
  -> Maybe String
  -> String
  -> (PDRInstance -> PDRInstance -> Aff a)
  -> Aff a

withSavedPDRLogConfig :: forall a. PDRInstance -> Aff a -> Aff a
withSavedPDRLogConfig pdr action =
  bracket
    (runInPDR pdr getLogConfig)
    (\logConfig -> runInPDR pdr (setLogConfig logConfig))
    (\_ -> action)

withSavedTwoPDRLogConfigs :: forall a. PDRInstance -> PDRInstance -> Aff a -> Aff a
withSavedTwoPDRLogConfigs pdrA pdrB action =
  withSavedPDRLogConfig pdrA $ withSavedPDRLogConfig pdrB action

applyLogConfigurationToPDRs :: PDRInstance -> PDRInstance -> LogConfiguration -> Aff Unit
applyLogConfigurationToPDRs pdrA pdrB logConfiguration = do
  runInPDR pdrA do
    for_ logConfiguration.pdrA \{ topic, logLevel } -> setTopicLogLevel topic logLevel

  runInPDR pdrB do
    for_ logConfiguration.pdrB \{ topic, logLevel } -> setTopicLogLevel topic logLevel

runSynchronisationSuite
  :: Ref (Maybe SynchronisationResults)
  -> TestSuite
  -> SynchronisationModelConfiguration
  -> Effect Unit
runSynchronisationSuite = runSynchronisationSuiteInternal getSynchronisationResults

runSynchronisationSuiteOverAMQP
  :: Ref (Maybe SynchronisationResults)
  -> TestSuite
  -> SynchronisationModelConfiguration
  -> Effect Unit
runSynchronisationSuiteOverAMQP = runSynchronisationSuiteInternal getSynchronisationResultsOverAMQP

runSynchronisationSuiteInternal
  :: (Ref (Maybe SynchronisationResults) -> SynchronisationModelConfiguration -> Aff SynchronisationResults)
  -> Ref (Maybe SynchronisationResults)
  -> TestSuite
  -> SynchronisationModelConfiguration
  -> Effect Unit
runSynchronisationSuiteInternal getResults cacheRef scaffoldSuite cfg =
  launchAff_ do
    results <- getResults cacheRef cfg
    liftEffect $ runTest do
      scaffoldSuite
      suite cfg.suiteName do
        for_ results \result -> case result of
          Right { testName, testSucceeded } ->
            test (testName <> " should succeed in Bob's PDR") do
              assert ("Bob should see that test '" <> testName <> "' succeeded") testSucceeded
          Left { testName, err } ->
            test ("test '" <> testName <> "' failed with error") do
              assert ("Bob should see that the test succeeded, but got error: " <> show err) false

getSynchronisationResults
  :: Ref (Maybe SynchronisationResults)
  -> SynchronisationModelConfiguration
  -> Aff SynchronisationResults
getSynchronisationResults = getSynchronisationResultsInternal withTwoPDRsCached true

getSynchronisationResultsOverAMQP
  :: Ref (Maybe SynchronisationResults)
  -> SynchronisationModelConfiguration
  -> Aff SynchronisationResults
getSynchronisationResultsOverAMQP = getSynchronisationResultsInternal withTwoPDRsCachedNoBus false

getSynchronisationResultsInternal
  :: WithTwoPDRsCachedLike
  -> Boolean
  -> Ref (Maybe SynchronisationResults)
  -> SynchronisationModelConfiguration
  -> Aff SynchronisationResults
getSynchronisationResultsInternal withTwoPDRsFn connectPeers cacheRef cfg = do
  cached <- liftEffect $ read cacheRef
  case cached of
    Just results -> pure results
    Nothing -> do
      results <- withTwoPDRsFn
        (testPouchdbUser "alice")
        defaultRuntimeOptions
        (Just ansiRed)
        cfg.snapshotDirAlice
        (testPouchdbUser "bob")
        defaultRuntimeOptions
        (Just ansiMagenta)
        cfg.snapshotDirBob
        \pdrA pdrB -> do
          withSavedTwoPDRLogConfigs pdrA pdrB do
            applyLogConfigurationToPDRs pdrA pdrB cfg.setupLogConfiguration

            when connectPeers (connectPDRs pdrA pdrB)

            alice <- runInPDR pdrA getPerspectivesUser
            bob <- runInPDR pdrB getPerspectivesUser

            case cfg.testModelLoadMethod of
              LoadModelFromRepository -> runInPDR pdrA do
                infoTest "Alice loads test model in PDRA"
                runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType sysUser)
                  $
                    addModelToLocalStore_ [ cfg.testModel ] (RoleInstance "Ignored")
              CompileModelFromSource { sourcePath, modelUriReadable, basedOnVersion } -> do
                source <- readTextFile UTF8 sourcePath
                -- Alice compiles first and coins the stable ids; Bob reuses Alice's mapping so both
                -- PDRs end up with the same CUIDs for the model's types and individuals.
                aliceMapping <- runInPDR pdrA do
                  infoTest "Alice compiles and stores test model from source"
                  compilationResult <- runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType sysUser)
                    ( loadCompileAndStoreArcFile_
                        (ModelUri cfg.testModel :: ModelUri Stable)
                        source
                        true
                        (unsafePartial modelUri2LocalName $ unversionedModelUri cfg.testModel)
                        modelUriReadable
                        basedOnVersion
                        Nothing
                    )
                  case compilationResult of
                    Left errs -> throwError $ error ("Failed to compile and store test model: " <> show errs)
                    Right (Tuple _ (Tuple _ mapping)) -> pure mapping

                runInPDR pdrB do
                  infoTest "Bob compiles and stores test model from source, reusing Alice's stable-id mapping"
                  compilationResult <- runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType sysUser)
                    ( loadCompileAndStoreArcFile_
                        (ModelUri cfg.testModel :: ModelUri Stable)
                        source
                        true
                        (unsafePartial modelUri2LocalName $ unversionedModelUri cfg.testModel)
                        modelUriReadable
                        basedOnVersion
                        (Just aliceMapping)
                    )
                  case compilationResult of
                    Left errs -> throwError $ error ("Failed to compile and store test model: " <> show errs)
                    Right _ -> pure unit

            testAppContextA <- pollUntil 100 (Milliseconds 100.0)
              "Indexed test context to appear in pdrA after loading test model"
              ( runInPDR pdrA
                  do
                    IndexedContext indexedTestContext' <- toStable (IndexedContext cfg.indexedTestContext)
                    lookupIndexedContext indexedTestContext'
              )

            runInPDR pdrA do
              infoTest "Alice gives Bob the role Follower in the App in PDRA"
              testAppManager' <- toStable (CalculatedRoleType cfg.testAppManager)
              testAppFollowerType' <- toStable (EnumeratedRoleType cfg.testAppFollowerType)
              runMonadPerspectivesTransaction' shareWithPeers (CR testAppManager')
                do
                  void $ createAndAddRoleInstance
                    testAppFollowerType'
                    (unwrap testAppContextA)
                    ( RolSerialization
                        { id: Nothing
                        , properties: PropertySerialization OBJ.empty
                        , binding: Just (unwrap bob)
                        }
                    )

            void $ pollUntil 100 (Milliseconds 100.0)
              "Alice checks that the Follower role has been constructed in pdrA"
              ( runInPDR pdrA do
                  infoTest "Alice checks that the Follower role has been constructed and filled in pdrA"
                  testAppFollowerType' <- toStable (EnumeratedRoleType cfg.testAppFollowerType)
                  roles <- testAppContextA ##= getEnumeratedRoleInstances testAppFollowerType' >=> binding
                  if null roles then pure Nothing
                  else pure (Just roles)
              )

            let runATest = \{ testContextTypeName, logConfiguration } -> executeModelTest cfg pdrA pdrB testAppContextA alice bob testContextTypeName logConfiguration
            traverse runATest cfg.tests

      liftEffect $ write (Just results) cacheRef
      --- Temporary: snapshot Alices' databases.
      let
        alice = testPouchdbUser "alice"
        snapshotDirAlice = cfg.snapshotDirAlice <> "/snapshot-after-tests"
      attempt (snapshotPDR alice.systemIdentifier alice.perspectivesUser snapshotDirAlice) >>= case _ of
        Left err -> log $ "[withPDRCached] Warning: snapshot creation failed: " <> message err
        Right _ -> log $ "[withPDRCached] Snapshot saved to: " <> snapshotDirAlice

      pure results

executeModelTest
  :: SynchronisationModelConfiguration
  -> PDRInstance
  -> PDRInstance
  -> ContextInstance
  -> PerspectivesUser
  -> PerspectivesUser
  -> String
  -> Maybe LogConfiguration
  -> Aff SynchronisationResult
executeModelTest cfg pdrA pdrB testAppContextA _alice _bob testContextTypeR mLogConfiguration =
  withSavedTwoPDRLogConfigs pdrA pdrB do
    case mLogConfiguration of
      Just logConfiguration -> applyLogConfigurationToPDRs pdrA pdrB logConfiguration
      Nothing -> pure unit

    testContextType <- runInPDR pdrA
      (toStable (ContextType testContextTypeR))
    testFollowerType <- runInPDR pdrA
      (toStable (EnumeratedRoleType $ testContextTypeR <> "$Follower"))
    testLeaderType <- runInPDR pdrA
      (toStable (EnumeratedRoleType $ testContextTypeR <> "$Leader"))

    theTest <- runInPDR pdrA do
      infoTest "Alice creates a test in PDRA"
      testAppManager' <- toStable (CalculatedRoleType cfg.testAppManager)
      testsType' <- toStable (EnumeratedRoleType cfg.testsType)
      runMonadPerspectivesTransaction' shareWithPeers (CR testAppManager')
        do
          result <- runExceptT $ constructContext (Just (ENR testsType'))
            $ ContextSerialization
                { id: Nothing
                , ctype: unwrap testContextType
                , prototype: Nothing
                , rollen: OBJ.empty
                , externeProperties: PropertySerialization OBJ.empty
                }
          case result of
            Left err -> throwError $ error ("Failed to create test context: " <> show err)
            Right testCtx -> do
              void $ createAndAddRoleInstance testsType' (unwrap testAppContextA)
                ( RolSerialization
                    { id: Nothing
                    , properties: PropertySerialization OBJ.empty
                    , binding: Just $ buitenRol (unwrap testCtx)
                    }
                )
              pure testCtx

    let testExternalRole = RoleInstance $ buitenRol (unwrap theTest)

    void $ pollUntil 100 (Milliseconds 100.0)
      "Bob to have the test and the Follower role in it in pdrB"
      ( runInPDR pdrB do
          mtheTest <- tryGetPerspectContext theTest
          case mtheTest of
            Nothing -> pure Nothing
            Just _ -> do
              infoTest "Bob has verified that he has the test context in PDRB"
              roles <- theTest ##= getEnumeratedRoleInstances testFollowerType
              if null roles then pure Nothing
              else do
                infoTest "Bob has verified that he has the Follower role in the test in PDRB"
                pure (Just roles)
      )

    runInPDR pdrA
      do
        runMonadPerspectivesTransaction' shareWithPeers (ENR testLeaderType)
          ( do
              lift $ infoTest "Alice executes a test in PDRA"
              runContextAction (unwrap testLeaderType) "RunTest" (unwrap theTest)
          )

    pollUntilTestFinishes 100 (Milliseconds 100.0)
      "Bob to have a value for the test to succeed in pdrB"
      ( runInPDR pdrB do
          testNameProperty' <- toStable (EnumeratedPropertyType cfg.testNameProperty)
          mtestName <- testExternalRole ##> getPropertyValues (ENP testNameProperty')
          case mtestName of
            Just (Value testName) -> do
              infoTest ("Bob sees that the test has a name: " <> show testName)
              testSucceededProperty' <- toStable (EnumeratedPropertyType cfg.testSucceededProperty)
              mtestSucceeded <- testExternalRole ##> getPropertyValues (ENP testSucceededProperty')
              case mtestSucceeded of
                Just (Value testSucceeded) -> pure (Right { testName, testSucceeded: testSucceeded == "true" })
                Nothing -> pure (Left { testName, err: error "TestSucceeded property not found" })
            Nothing -> pure (Left { testName: "unknown testname", err: error "TestName property not found" })
      )

-- allOn :: Array TopicLogLevelPair
-- allOn =
--   [ { topic: RESOURCE, logLevel: Trace }
--   , { topic: DELTA, logLevel: Trace }
--   , { topic: STATE, logLevel: Trace }
--   , { topic: SYNC, logLevel: Trace }
--   , { topic: BROKER, logLevel: Trace }
--   , { topic: INSTALL, logLevel: Trace }
--   , { topic: TEST, logLevel: Debug }
--   , { topic: MODEL, logLevel: Trace }
--   , { topic: PERSISTENCE, logLevel: Trace }
--   , { topic: QUERY, logLevel: Trace }
--   , { topic: AUTH, logLevel: Trace }
--   , { topic: UPGRADE, logLevel: Trace }
--   , { topic: PARSER, logLevel: Trace }
--   , { topic: COMPILER, logLevel: Trace }
--   ]


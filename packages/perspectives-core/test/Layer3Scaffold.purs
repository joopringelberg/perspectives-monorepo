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
  , getSynchronisationResults
  , executeModelTest
  , runSynchronisationSuite
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
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, read, write)
import Foreign.Object (empty) as OBJ
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.RunAction (runContextAction)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), (##=), (##>))
import Perspectives.Extern.Couchdb (addModelToLocalStore_)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.Builders (createAndAddRoleInstance, constructContext)
import Perspectives.Instances.ObjectGetters (binding, getEnumeratedRoleInstances)
import Perspectives.Logging (ansiMagenta, ansiRed, infoTest)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Names (lookupIndexedContext)
import Perspectives.Persistent (tryGetPerspectContext)
import Perspectives.PerspectivesState (defaultRuntimeOptions, disableAllLogging, getPerspectivesUser, setTopicLogLevel)
import Perspectives.Query.UnsafeCompiler (getPropertyValues)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, PerspectivesUser, RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), IndexedContext(..), PropertyType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction', shareWithPeers)
import Perspectives.Sidecar.ToStable (toStable)
import Test.PDRInstance (PDRInstance, SynchronisationResult, connectPDRs, pollUntil, pollUntilTestFinishes, runInPDR, testPouchdbUser, withTwoPDRsCached)
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
  , logConfiguration :: LogConfiguration
  }

type SynchronisationResults = Array SynchronisationResult

type SynchronisationModelConfiguration =
  { suiteName :: String
  , snapshotDirAlice :: String
  , snapshotDirBob :: String
  , testModel :: String
  , indexedTestContext :: String
  , testAppManager :: String
  , testAppFollowerType :: String
  , testsType :: String
  , testSucceededProperty :: String
  , testNameProperty :: String
  , setupLogConfiguration :: LogConfiguration
  , tests :: Array ModelTest
  }

runSynchronisationSuite
  :: Ref (Maybe SynchronisationResults)
  -> TestSuite
  -> SynchronisationModelConfiguration
  -> Effect Unit
runSynchronisationSuite cacheRef scaffoldSuite cfg =
  launchAff_ do
    results <- getSynchronisationResults cacheRef cfg
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
getSynchronisationResults cacheRef cfg = do
  cached <- liftEffect $ read cacheRef
  case cached of
    Just results -> pure results
    Nothing -> do
      results <- withTwoPDRsCached
        (testPouchdbUser "alice")
        defaultRuntimeOptions
        (Just ansiRed)
        cfg.snapshotDirAlice
        (testPouchdbUser "bob")
        defaultRuntimeOptions
        (Just ansiMagenta)
        cfg.snapshotDirBob
        \pdrA pdrB -> do

          runInPDR pdrA do
            for_ cfg.setupLogConfiguration.pdrA \{ topic, logLevel } -> setTopicLogLevel topic logLevel

          runInPDR pdrB do
            for_ cfg.setupLogConfiguration.pdrB \{ topic, logLevel } -> setTopicLogLevel topic logLevel

          connectPDRs pdrA pdrB

          alice <- runInPDR pdrA getPerspectivesUser
          bob <- runInPDR pdrB getPerspectivesUser

          runInPDR pdrA do
            infoTest "Alice loads test model in PDRA"
            runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType sysUser)
              $
                addModelToLocalStore_ [ cfg.testModel ] (RoleInstance "Ignored")

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
      pure results

executeModelTest
  :: SynchronisationModelConfiguration
  -> PDRInstance
  -> PDRInstance
  -> ContextInstance
  -> PerspectivesUser
  -> PerspectivesUser
  -> String
  -> LogConfiguration
  -> Aff SynchronisationResult
executeModelTest cfg pdrA pdrB testAppContextA _alice _bob testContextTypeR logConfiguration = do

  runInPDR pdrA do
    for_ logConfiguration.pdrA \{ topic, logLevel } -> setTopicLogLevel topic logLevel

  runInPDR pdrB do
    for_ logConfiguration.pdrB \{ topic, logLevel } -> setTopicLogLevel topic logLevel

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

  r <-
    ( pollUntilTestFinishes 100 (Milliseconds 100.0)
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
    )

  runInPDR pdrA
    ( do
        disableAllLogging
        setTopicLogLevel TEST Debug
    )
  runInPDR pdrB
    ( do
        disableAllLogging
        setTopicLogLevel TEST Debug
    )
  pure r

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


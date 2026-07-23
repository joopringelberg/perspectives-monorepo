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

module Test.SinglePDRScaffold
  ( TopicLogLevelPair
  , LogConfiguration
  , emptyLogConfiguration
  , ModelTest
  , SinglePDRResults
  , SinglePDRModelConfiguration
  , getSinglePDRResults
  , executeModelTest
  ) where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_, traverse)
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, new, read, write)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (empty) as OBJ
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.RunAction (runContextAction)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), (##>))
import Perspectives.Extern.Couchdb (addModelToLocalStore_)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.Builders (createAndAddRoleInstance, constructContext)
import Perspectives.Logging (ansiRed, infoTest)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Names (lookupIndexedContext)
import Perspectives.PerspectivesState (defaultRuntimeOptions, disableAllLogging, setTopicLogLevel)
import Perspectives.Query.UnsafeCompiler (getPropertyValues)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), IndexedContext(..), PropertyType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction', shareWithPeers)
import Perspectives.Sidecar.ToStable (toStable)
import Test.PDRInstance (PDRInstance, SynchronisationResult, noBus, pollUntil, pollUntilTestFinishes, runInPDR, testPouchdbUser, withPDRCached)

type TopicLogLevelPair =
  { topic :: LogTopic
  , logLevel :: LogLevel
  }

type LogConfiguration =
  { pdr :: Array TopicLogLevelPair
  }

emptyLogConfiguration :: LogConfiguration
emptyLogConfiguration =
  { pdr: []
  }

type ModelTest =
  { testContextTypeName :: String
  , logConfiguration :: LogConfiguration
  }

type SinglePDRResults = Array SynchronisationResult

type SinglePDRModelConfiguration =
  { suiteName :: String
  , snapshotDirectory :: String
  , testModel :: String
  , indexedTestContext :: String
  , testAppManager :: String
  , testsType :: String
  , testSucceededProperty :: String
  , testNameProperty :: String
  , setupLogConfiguration :: LogConfiguration
  , tests :: Array ModelTest
  }

cachedSinglePDRResults :: Ref (Maybe SinglePDRResults)
cachedSinglePDRResults = unsafePerformEffect $ new Nothing

getSinglePDRResults :: SinglePDRModelConfiguration -> Aff SinglePDRResults
getSinglePDRResults cfg = do
  cached <- liftEffect $ read cachedSinglePDRResults
  case cached of
    Just results -> pure results
    Nothing -> do
      results <- withPDRCached
        (testPouchdbUser "alice")
        defaultRuntimeOptions
        (Just ansiRed)
        noBus
        cfg.snapshotDirectory
        \pdr -> do
          runInPDR pdr do
            for_ cfg.setupLogConfiguration.pdr \{ topic, logLevel } -> setTopicLogLevel topic logLevel

          runInPDR pdr do
            infoTest "Loading test model"
            runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType sysUser)
              $
                addModelToLocalStore_ [ cfg.testModel ] (RoleInstance "Ignored")

          testAppContext <- pollUntil 100 (Milliseconds 100.0)
            "Indexed test context to appear after loading test model"
            ( runInPDR pdr
                do
                  IndexedContext indexedTestContext' <- toStable (IndexedContext cfg.indexedTestContext)
                  lookupIndexedContext indexedTestContext'
            )

          traverse (\testCase -> executeModelTest pdr testAppContext testCase.testContextTypeName testCase.logConfiguration cfg) cfg.tests

      liftEffect $ write (Just results) cachedSinglePDRResults
      pure results

executeModelTest
  :: PDRInstance
  -> ContextInstance
  -> String
  -> LogConfiguration
  -> SinglePDRModelConfiguration
  -> Aff SynchronisationResult
executeModelTest pdr testAppContext testContextTypeR logConfiguration cfg = do
  runInPDR pdr do
    for_ logConfiguration.pdr \{ topic, logLevel } -> setTopicLogLevel topic logLevel

  testContextType <- runInPDR pdr
    (toStable (ContextType testContextTypeR))
  testTesterType <- runInPDR pdr
    (toStable (EnumeratedRoleType $ testContextTypeR <> "$Tester"))

  theTest <- runInPDR pdr do
    infoTest "Creating test context"
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
            void $ createAndAddRoleInstance testsType' (unwrap testAppContext)
              ( RolSerialization
                  { id: Nothing
                  , properties: PropertySerialization OBJ.empty
                  , binding: Just $ buitenRol (unwrap testCtx)
                  }
              )
            pure testCtx

  let testExternalRole = RoleInstance $ buitenRol (unwrap theTest)

  runInPDR pdr do
    runMonadPerspectivesTransaction' shareWithPeers (ENR testTesterType)
      do
        lift $ infoTest "Executing RunTest action"
        runContextAction (unwrap testTesterType) "RunTest" (unwrap theTest)

  r <- pollUntilTestFinishes 100 (Milliseconds 100.0)
    "Test to complete with a result"
    ( runInPDR pdr
        do
          testNameProperty' <- toStable (EnumeratedPropertyType cfg.testNameProperty)
          mtestName <- testExternalRole ##> getPropertyValues (ENP testNameProperty')
          case mtestName of
            Just (Value testName) -> do
              infoTest ("Test has a name: " <> testName)
              testSucceededProperty' <- toStable (EnumeratedPropertyType cfg.testSucceededProperty)
              mtestSucceeded <- testExternalRole ##> getPropertyValues (ENP testSucceededProperty')
              case mtestSucceeded of
                Just (Value testSucceeded) -> pure (Right { testName, testSucceeded: testSucceeded == "true" })
                Nothing -> pure (Left { testName, err: error "TestSucceeded property not found" })
            Nothing -> pure (Left { testName: "unknown testname", err: error "TestName property not found" })
    )

  runInPDR pdr do
    disableAllLogging
    setTopicLogLevel TEST Debug

  pure r

-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Test.SidecarUniqueTypeNames where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (null)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Foreign.Object as OBJ
import Perspectives.Sidecar.StableIdMapping (emptyStableIdMapping)
import Perspectives.Sidecar.UniqueTypeNames (planCuidAssignments)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert)

theSuite :: Free TestF Unit
theSuite = suite "Test.SidecarUniqueTypeNames" do
  test "planCuidAssignments does not create action self-aliases for unchanged actions" do
    let
      actionFqn = "model://perspectives.domains#System$SocialEnvironment$Me$Cancel"
      actionSnapshot =
        { fqn: actionFqn
        , declaringRoleFqn: "model://perspectives.domains#System$SocialEnvironment$Me"
        , localName: "Cancel"
        , qfdHash: "qfd-12345678"
        }
      mapping0 = emptyStableIdMapping
        { actionKeys = OBJ.fromFoldable [ Tuple actionFqn actionSnapshot ]
        , actionCuids = OBJ.fromFoldable [ Tuple actionFqn "i0olka52hc" ]
        }
      cur =
        { contexts: OBJ.empty
        , roles: OBJ.empty
        , properties: OBJ.empty
        , views: OBJ.empty
        , states: OBJ.empty
        , actions: OBJ.fromFoldable [ Tuple actionFqn actionSnapshot ]
        , contextIndividuals: []
        , roleIndividuals: []
        , contextIndividualKeys: OBJ.empty
        , roleIndividualKeys: OBJ.empty
        }
      planned = planCuidAssignments cur mapping0
    liftAff $ assert "unchanged canonical actions must not be inserted as aliases" $
      OBJ.lookup actionFqn planned.mappingWithAliases.actions == Nothing
    liftAff $ assert "unchanged actions do not need a fresh CUID" $
      null planned.needCuids.actions

  test "planCuidAssignments keeps the CUID of a state whose query changed and that has an identity alias" do
    let
      stateFqn = "model://perspectives.domains#BrokerServices$BrokerContract$Terminated"
      oldSnapshot = { fqn: stateFqn, queryHash: "qfd-40ab9d05" }
      newSnapshot = { fqn: stateFqn, queryHash: "qfd-11111111" }
      mapping0 = emptyStableIdMapping
        { stateKeys = OBJ.fromFoldable [ Tuple stateFqn oldSnapshot ]
        , stateCuids = OBJ.fromFoldable [ Tuple stateFqn "fz9kcl0kmp" ]
        -- Identity alias as recorded by an earlier recompilation in which only the query changed.
        , states = OBJ.fromFoldable [ Tuple stateFqn stateFqn ]
        }
      cur =
        { contexts: OBJ.empty
        , roles: OBJ.empty
        , properties: OBJ.empty
        , views: OBJ.empty
        , states: OBJ.fromFoldable [ Tuple stateFqn newSnapshot ]
        , actions: OBJ.empty
        , contextIndividuals: []
        , roleIndividuals: []
        , contextIndividualKeys: OBJ.empty
        , roleIndividualKeys: OBJ.empty
        }
      planned = planCuidAssignments cur mapping0
    liftAff $ assert "an identity alias must not evict the CUID of the state" $
      OBJ.lookup stateFqn planned.mappingWithAliases.stateCuids == Just "fz9kcl0kmp"
    liftAff $ assert "the state does not need a fresh CUID" $
      null planned.needCuids.states

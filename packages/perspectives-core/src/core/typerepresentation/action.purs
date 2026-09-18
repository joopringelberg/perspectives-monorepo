-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2021 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
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

module Perspectives.Representation.Action where

import Prelude

import Control.Alt ((<|>))
import Data.Array (intercalate, uncons)
import Data.Eq.Generic (genericEq)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Foreign (F, isNull, isUndefined)
import Partial.Unsafe (unsafePartial)
import Perspectives.Identifiers (typeUri2LocalName_)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..), domain, functional, range)
import Perspectives.Repetition (Duration, Repeater)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic as THREE
import Perspectives.Representation.TypeIdentifiers (ActionIdentifier(..))
import Simple.JSON (class ReadForeign, class WriteForeign, read', writeImpl)

data StartMoment = Immediately | After Duration | OnceSettled

derive instance genericStartMoment :: Generic StartMoment _
instance showStartMoment :: Show StartMoment where
  show = genericShow

instance eqStartMoment :: Eq StartMoment where
  eq = genericEq

instance WriteForeign StartMoment where
  writeImpl Immediately = writeImpl "Immediately"
  writeImpl (After duration) = writeImpl { after: duration }
  writeImpl OnceSettled = writeImpl "OnceSettled"

-- Transitional reader: accepts legacy DomeinFiles compiled with `Maybe Duration`.
-- Remove after the repository has been regenerated and installations reboot from canonical shapes.
instance ReadForeign StartMoment where
  readImpl f
    | isNull f || isUndefined f = pure Immediately
    | otherwise =
        do
          constructor <- read' f :: F String
          unsafePartial case constructor of
            "Immediately" -> pure Immediately
            "OnceSettled" -> pure OnceSettled
          <|> After <<< _.after <$> (read' f :: F { after :: Duration })
          <|> After <$> (read' f :: F Duration)

newtype ActionEffect = ActionEffect
  { bindings :: Array QueryFunctionDescription
  , stages :: Array QueryFunctionDescription
  , capturedBindingNames :: Array String
  }

derive instance genericActionEffect :: Generic ActionEffect _
derive instance newtypeActionEffect :: Newtype ActionEffect _
instance showActionEffect :: Show ActionEffect where
  show = genericShow

instance eqActionEffect :: Eq ActionEffect where
  eq = genericEq

instance WriteForeign ActionEffect where
  writeImpl (ActionEffect r) = writeImpl r

-- Transitional reader: accepts legacy single-QFD action effects and the short-lived `{ stages }` shape.
-- Remove after the repository has been regenerated and installations reboot from canonical shapes.
instance ReadForeign ActionEffect where
  readImpl f =
    ActionEffect <$> (read' f :: F { bindings :: Array QueryFunctionDescription, stages :: Array QueryFunctionDescription, capturedBindingNames :: Array String })
      <|> (\{ stages } -> ActionEffect { bindings: [], stages, capturedBindingNames: [] }) <$> (read' f :: F { stages :: Array QueryFunctionDescription })
      <|> (\qfd -> ActionEffect { bindings: [], stages: [ qfd ], capturedBindingNames: [] }) <$> (read' f :: F QueryFunctionDescription)

traverseActionEffect :: forall m. Applicative m => (QueryFunctionDescription -> m QueryFunctionDescription) -> ActionEffect -> m ActionEffect
traverseActionEffect f (ActionEffect { bindings, stages, capturedBindingNames }) =
  (\bindings' stages' -> ActionEffect { bindings: bindings', stages: stages', capturedBindingNames })
    <$> traverse f bindings
    <*> traverse f stages

actionEffectSignature :: (QueryFunctionDescription -> String) -> ActionEffect -> String
actionEffectSignature f (ActionEffect { bindings, stages }) = intercalate "|" (f <$> (bindings <> stages))

queryFunctionDescriptionOfActionEffect :: ActionEffect -> QueryFunctionDescription
queryFunctionDescriptionOfActionEffect (ActionEffect { bindings, stages }) = unsafePartial case uncons (bindings <> stages) of
  Just { head, tail } -> foldl makeSequence head tail
  where
  makeSequence :: QueryFunctionDescription -> QueryFunctionDescription -> QueryFunctionDescription
  makeSequence left right = BQD (domain left) (BinaryCombinator SequenceF) left right (range right) (THREE.and (functional left) (functional right)) (THREE.or (functional left) (functional right))

data AutomaticAction
  = ContextAction
      ( TimeFacets
          (effect :: ActionEffect)
      )
  | RoleAction
      ( TimeFacets
          ( currentContextCalculation :: QueryFunctionDescription
          , effect :: ActionEffect
          )
      )

type TimeFacets f =
  { startMoment :: StartMoment
  , endMoment :: Maybe Duration
  , repeats :: Repeater
  | f
  }

effectOfAction :: AutomaticAction -> QueryFunctionDescription
effectOfAction (ContextAction { effect }) = queryFunctionDescriptionOfActionEffect effect
effectOfAction (RoleAction action) = queryFunctionDescriptionOfActionEffect action.effect

derive instance genericAutomaticAction :: Generic AutomaticAction _
instance showAutomaticAction :: Show AutomaticAction where
  show = genericShow

instance eqAutomaticAction :: Eq AutomaticAction where
  eq = genericEq

instance WriteForeign AutomaticAction where
  writeImpl (ContextAction r) = writeImpl { constructor: "ContextAction", r }
  writeImpl (RoleAction r) = writeImpl { constructor: "RoleAction", r }

instance ReadForeign AutomaticAction where
  readImpl f =
    -- order matters here!
    do
      { r } :: { r :: TimeFacets (effect :: ActionEffect, currentContextCalculation :: QueryFunctionDescription) } <- read' f
      pure $ RoleAction r
      <|>
        do
          { r } :: { r :: TimeFacets (effect :: ActionEffect) } <- read' f
          pure $ ContextAction r

newtype Action = Action { qfd :: ActionEffect, readable :: String, id :: ActionIdentifier }

derive instance genericAction :: Generic Action _
derive instance newtypeAction :: Newtype Action _
instance showAction :: Show Action where
  show = genericShow

instance eqAction :: Eq Action where
  eq = genericEq

derive newtype instance WriteForeign Action
derive newtype instance ReadForeign Action

instance Semigroup Action where
  append (Action { qfd: qfd1, readable: readable1, id: id1 }) (Action { qfd: qfd2, readable: readable2, id: id2 }) = Action
    { qfd: makeSequence qfd1 qfd2
    , readable: readable1 <> " <> " <> readable2
    -- Arbitrarily append the local name of the second action's id to the first action's id
    , id: ActionIdentifier (unwrap id1 <> "_" <> typeUri2LocalName_ (unwrap id2))
    }
    where
    makeSequence :: ActionEffect -> ActionEffect -> ActionEffect
    makeSequence (ActionEffect left) (ActionEffect right) = ActionEffect (left <> right)

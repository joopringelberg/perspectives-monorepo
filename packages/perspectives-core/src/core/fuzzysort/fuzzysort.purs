-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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

module Perspectives.Fuzzysort where

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Array (uncons)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, fromFoldable, keys, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), ArrayWithoutDoubles(..), InformedAssumption(..))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.ModelDependencies (indexedContextFuzzies)
import Perspectives.Names (getMySystem)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Prelude (bind, map, pure, ($), (<$>), discard, (>=))
import Simple.JSON (writeJSON)

-- const result = fuzzysort.single('query', 'some string that contains my query.')
-- // exact match returns a score of 0. lower is worse
-- result.score // -59
-- result.indexes // [29, 30, 31, 32, 33]
-- result.target // some string that contains my query.
-- result.obj // reference to your original obj when using options.key
-- fuzzysort.highlight(result, '<b>', '</b>') // some string that contains my <b>query</b>.

type Target = String

-- | We represent just the fields of interest for this module.
type FuzzyResultRecord a =
  { score :: Number
  , indexes :: Array Int
  , target :: a
}

foreign import matchStringsImpl :: forall a. Fn2 String (Array a) (Array (FuzzyResultRecord a))

-- fuzzysort.go('mr', ['Monitor.cpp', 'MeshRenderer.cpp'])
matchStrings :: forall a. String -> Array a -> Array (FuzzyResultRecord a)
matchStrings = runFn2 matchStringsImpl

-- | Return an object whose keys are Indexed Context Names and whose values are
-- | the actual context identifiers.
-- | If s is the empty string, all IndexedContextNames will be returned.
matchIndexedContextNames :: String -> ContextInstance ~~> Value
matchIndexedContextNames s _ = ArrayT do
  indexedNames <- lift $ gets _.indexedContexts
  sortedMatches <- pure $ matchStrings s (keys indexedNames)
  (matchingIndexedNames :: Array String) <- pure (_.target <$> sortedMatches)
  mysystem <- lift $ getMySystem
  tell $ ArrayWithoutDoubles [RoleAssumption (ContextInstance mysystem) (EnumeratedRoleType indexedContextFuzzies)]
  pure [Value $ writeJSON $ fromFoldable (map
    (\iname -> Tuple iname (unwrap $ unsafePartial $ fromJust $ lookup iname indexedNames))
    matchingIndexedNames)]

fuzzyLookup :: forall a. String -> Object a -> Maybe a
fuzzyLookup s obj = let
  (sortedMatches :: Array (FuzzyResultRecord String)) = matchStrings s (keys obj)
  in 
  case uncons sortedMatches of
    Nothing -> Nothing
    Just {head, tail} -> if head.score >= 0.9
      then lookup head.target obj
      else Nothing

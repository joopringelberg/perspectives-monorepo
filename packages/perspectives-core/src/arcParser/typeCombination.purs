module Perspectives.Parsing.Arc.PhaseTwo.TypeCombination where

import Prelude

import Data.Array (fromFoldable)
import Data.List.NonEmpty (head)
import Perspectives.Parsing.Arc.Expression.AST (FilledByAttribute(..), TypeCombination(..))
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo')
import Perspectives.Query.ExpandPrefix (expandPrefix)
import Perspectives.Query.QueryTypes (RoleInContext(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Cacheable (EnumeratedRoleType(..))
import Perspectives.Representation.Class.PersistentType (ContextType(..))

compileRoleTypeCombination :: forall m. Monad m => EnumeratedRoleType -> TypeCombination -> PhaseTwo' m (ADT RoleInContext)
compileRoleTypeCombination id tc = do
  expandedTypeCombination <- expandPrefix tc
  case expandedTypeCombination of
    Alternatives attrs -> case head attrs of
      FilledByAttribute "None" context -> pure $ UET $ RoleInContext { role: id, context }
      _ -> pure $ SUM (fromFoldable (toRoleInContext <$> attrs))

    Combination attrs -> pure $ PROD (fromFoldable (toRoleInContext <$> attrs))

    DisjunctionOfConjunctions groups -> pure $ SUM (fromFoldable ((\conj -> PROD (fromFoldable (toRoleInContext <$> conj))) <$> groups))

  where
  toRoleInContext :: FilledByAttribute -> ADT RoleInContext
  toRoleInContext (FilledByAttribute bnd context) = UET $ RoleInContext { role: EnumeratedRoleType bnd, context }

compileContextTypeCombination :: forall m. Monad m => TypeCombination -> PhaseTwo' m (ADT ContextType)
compileContextTypeCombination tc = do
  expandedTypeCombination <- expandPrefix tc
  case expandedTypeCombination of
    Alternatives attrs -> pure $ SUM (fromFoldable (toContext <$> attrs))
    Combination attrs -> pure $ PROD (fromFoldable (toContext <$> attrs))
    DisjunctionOfConjunctions groups -> pure $ SUM (fromFoldable ((\conj -> PROD (fromFoldable (toContext <$> conj))) <$> groups))

  where
  toContext :: FilledByAttribute -> ADT ContextType
  toContext (FilledByAttribute s _) = UET $ ContextType s

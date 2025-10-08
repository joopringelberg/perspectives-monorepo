-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Perspectives.Error.Pretty
  ( renderPerspectivesError
  , humanizePerspectivesError
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (for, traverse)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.HumanReadableType (swapDisplayName)
import Perspectives.Identifiers (typeUri2ModelUri, typeUri2LocalName_)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.TypeIdentifiers (PropertyType(..), RoleType(..))

-- | Convert a PerspectivesError to a human-friendly string by replacing
-- | stable ids in typed fields with readable display names where possible.
-- | Falls back to `show` if a constructor isn't handled specially.
renderPerspectivesError :: PerspectivesError -> MonadPerspectives String
renderPerspectivesError e = humanizePerspectivesError e >>= pure <<< show

-- | Map typed identifiers inside an error to their readable counterparts.
-- | Keep everything else unchanged. This allows using the existing Show instance
-- | while still producing readable names.
humanizePerspectivesError :: PerspectivesError -> MonadPerspectives PerspectivesError
humanizePerspectivesError e = case e of
  UserHasNoPerspective s o start end -> do
    s' <- swapRoleType s
    o' <- swapRoleType o
    pure (UserHasNoPerspective s' o' start end)

  UnauthorizedForRole who s o verbs mstart mend -> do
    s' <- swapRoleType s
    o' <- swapRoleType o
    pure (UnauthorizedForRole who s' o' verbs mstart mend)

  UnauthorizedForProperty who s o p v mstart mend -> do
    s' <- swapRoleType s
    o' <- swapRoleType o
    p' <- swapPropertyType p
    pure (UnauthorizedForProperty who s' o' p' v mstart mend)

  UnknownMarkDownConditionProperty start end prop rt -> do
    rt' <- swapRoleType rt
    pure (UnknownMarkDownConditionProperty start end prop rt')

  StateDoesNotExist sid start end -> do
    sid' <- swapDisplayName sid
    pure (StateDoesNotExist sid' start end)

  -- Constructors that carry only strings: try to prettify strings that look like URIs.
  UnknownProperty pos qname adt -> do
    adt' <- for adt \x -> case x of
      ENR rtype -> ENR <$> swapDisplayName rtype
      CR rtype -> CR <$> swapDisplayName rtype

    qname' <- swapPropertyType qname
    pure (UnknownProperty pos qname' adt')

  UnknownRole pos s -> UnknownRole pos <$> humanizeString s
  UnknownContext pos s -> UnknownContext pos <$> swapDisplayName s
  UnknownView pos s -> UnknownView pos <$> humanizeString s
  NotAViewOfObject pos s -> NotAViewOfObject pos <$> humanizeString s
  NotUniquelyIdentifyingPropertyType pos lname alts -> do
    lname' <- swapPropertyType lname
    alts' <- traverse swapPropertyType alts
    pure (NotUniquelyIdentifyingPropertyType pos lname alts')
  NotUniquelyIdentifyingView pos lname alts -> do
    lname' <- swapDisplayName lname
    alts' <- traverse swapDisplayName alts
    pure (NotUniquelyIdentifyingView pos lname' alts')
  NotUniquelyIdentifyingContext pos lname alts -> do
    lname' <- swapDisplayName lname
    alts' <- traverse swapDisplayName alts
    pure (NotUniquelyIdentifyingContext pos lname' alts')
  NotUniquelyIdentifyingRoleType pos lname alts -> do
    lname' <- swapRoleType lname
    alts' <- traverse swapRoleType alts
    pure (NotUniquelyIdentifyingRoleType pos lname' alts')
  NotUniquelyIdentifyingState pos lname alts -> do
    lname' <- swapDisplayName lname
    alts' <- traverse swapDisplayName alts
    pure (NotUniquelyIdentifyingState pos lname' alts')
  NoCalculatedAspect pos ert -> do
    ert' <- swapDisplayName ert
    pure (NoCalculatedAspect pos ert')
  CannotModifyCalculatedProperty props start end -> do
    props' <- traverse swapPropertyType props
    pure (CannotModifyCalculatedProperty props' start end)
  RoleHasNoProperty adt pt start end -> do
    -- It may be that this error arose during the compilation of a model. In that case
    -- compilation failed and this error will be in terms of Readable ids.
    adt' <- traverse swapDisplayName adt
    pure (RoleHasNoProperty adt' pt start end)

  -- Default: leave unchanged.
  _ -> pure e

-- Helpers

swapRoleType :: RoleType -> MonadPerspectives RoleType
swapRoleType rt = case rt of
  ENR ert -> ENR <$> swapDisplayName ert
  CR crt -> CR <$> swapDisplayName crt

swapPropertyType :: PropertyType -> MonadPerspectives PropertyType
swapPropertyType pt = case pt of
  ENP ept -> ENP <$> swapDisplayName ept
  CP cpt -> CP <$> swapDisplayName cpt

-- If the string looks like a type URI, return the local name; otherwise unchanged.
humanizeString :: String -> MonadPerspectives String
humanizeString s = case typeUri2ModelUri s of
  Just _ -> pure (typeUri2LocalName_ s)
  Nothing -> pure s

-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Perspectives.Error.Pretty
  ( renderPerspectivesError
  , humanizePerspectivesError
  , logPerspectivesErrorPretty
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class.Console (log)
import Data.Traversable (for, traverse)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Identifiers (typeUri2ModelUri, typeUri2LocalName_)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.TypeIdentifiers (PropertyType(..), RoleType(..))
import Perspectives.Sidecar.ToReadable (toReadable)

-- | Convert a PerspectivesError to a human-friendly string by replacing
-- | stable ids in typed fields with readable display names where possible.
-- | Falls back to `show` if a constructor isn't handled specially.
renderPerspectivesError :: PerspectivesError -> MonadPerspectives String
renderPerspectivesError e = humanizePerspectivesError e >>= pure <<< show

-- | Log a PerspectivesError to the console with human-readable type names.
logPerspectivesErrorPretty :: PerspectivesError -> MonadPerspectives Unit
logPerspectivesErrorPretty e = renderPerspectivesError e >>= log

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

  UnauthorizedForContext who s ct -> do
    s' <- swapRoleType s
    ct' <- toReadable ct
    pure (UnauthorizedForContext who s' ct')

  UnknownMarkDownConditionProperty start end prop rt -> do
    rt' <- swapRoleType rt
    pure (UnknownMarkDownConditionProperty start end prop rt')

  UnknownMarkDownAction start end action rt -> do
    rt' <- swapRoleType rt
    pure (UnknownMarkDownAction start end action rt')

  StateDoesNotExist sid start end -> do
    sid' <- toReadable sid
    pure (StateDoesNotExist sid' start end)

  -- Constructors that carry only strings: try to prettify strings that look like URIs.
  UnknownProperty pos qname adt -> do
    adt' <- for adt \x -> case x of
      ENR rtype -> ENR <$> toReadable rtype
      CR rtype -> CR <$> toReadable rtype

    qname' <- swapPropertyType qname
    pure (UnknownProperty pos qname' adt')

  UnknownRole pos s -> UnknownRole pos <$> humanizeString s
  UnknownContext pos s -> UnknownContext pos <$> toReadable s
  UnknownView pos s -> UnknownView pos <$> humanizeString s
  NotAViewOfObject pos s -> NotAViewOfObject pos <$> humanizeString s
  NotUniquelyIdentifyingPropertyType pos lname alts -> do
    lname' <- swapPropertyType lname
    alts' <- traverse swapPropertyType alts
    pure (NotUniquelyIdentifyingPropertyType pos lname alts')
  NotUniquelyIdentifyingView pos lname alts -> do
    lname' <- toReadable lname
    alts' <- traverse toReadable alts
    pure (NotUniquelyIdentifyingView pos lname' alts')
  NotUniquelyIdentifyingContext pos lname alts -> do
    lname' <- toReadable lname
    alts' <- traverse toReadable alts
    pure (NotUniquelyIdentifyingContext pos lname' alts')
  NotUniquelyIdentifyingRoleType pos lname alts -> do
    lname' <- swapRoleType lname
    alts' <- traverse swapRoleType alts
    pure (NotUniquelyIdentifyingRoleType pos lname' alts')
  NotUniquelyIdentifyingState pos lname alts -> do
    lname' <- toReadable lname
    alts' <- traverse toReadable alts
    pure (NotUniquelyIdentifyingState pos lname' alts')
  NoCalculatedAspect pos ert -> do
    ert' <- toReadable ert
    pure (NoCalculatedAspect pos ert')
  CannotModifyCalculatedProperty props start end -> do
    props' <- traverse swapPropertyType props
    pure (CannotModifyCalculatedProperty props' start end)
  RoleHasNoProperty adt pt start end -> do
    -- It may be that this error arose during the compilation of a model. In that case
    -- compilation failed and this error will be in terms of Readable ids.
    adt' <- traverse toReadable adt
    pure (RoleHasNoProperty adt' pt start end)

  -- Default: leave unchanged.
  _ -> pure e

-- Helpers

swapRoleType :: RoleType -> MonadPerspectives RoleType
swapRoleType rt = case rt of
  ENR ert -> ENR <$> toReadable ert
  CR crt -> CR <$> toReadable crt

swapPropertyType :: PropertyType -> MonadPerspectives PropertyType
swapPropertyType pt = case pt of
  ENP ept -> ENP <$> toReadable ept
  CP cpt -> CP <$> toReadable cpt

-- If the string looks like a type URI, return the local name; otherwise unchanged.
humanizeString :: String -> MonadPerspectives String
humanizeString s = case typeUri2ModelUri s of
  Just _ -> pure (typeUri2LocalName_ s)
  Nothing -> pure s

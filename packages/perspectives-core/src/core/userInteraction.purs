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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

-- | This module provides a general mechanism for asking yes/no questions to the end
-- | user from within the PDR. The mechanism sends a blocking `setPDRStatus` message
-- | to all connected frontend clients and waits until one of them supplies an answer.
-- |
-- | The message content is supplied as a `PerspectivesWarning` value, whose `show`
-- | instance provides the human-readable question text. The caller also supplies the
-- | labels for the two response buttons (yes / no).
-- |
-- | See `packages/perspectives-core/docsources/pdr-messaging.md` for a full description
-- | of the underlying mechanism.

module Perspectives.UserInteraction
  ( requestUserChoice
  ) where

import Prelude

import Effect.Aff.AVar (take) as AVar
import Effect.Aff.Class (liftAff)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.PerspectivesState (getPDRStatusSetter, getUserIntegrityChoiceAVar)
import Perspectives.Warning (PerspectivesWarning)
import Simple.JSON (writeJSON)

-- | Send a blocking yes/no dialog to all connected frontend clients and return the
-- | user's choice as a `Boolean`.
-- |
-- | * `warning`   — a `PerspectivesWarning` whose `show` instance supplies the question
-- |                 text displayed in the dialog.
-- | * `yesOption` — label for the button that resolves to `true`.
-- | * `noOption`  — label for the button that resolves to `false`.
-- |
-- | The PDR fiber blocks on `AVar.take userIntegrityChoice` until the frontend puts a
-- | value into the AVar (via the `resolveUserIntegrityChoice` proxy request).
requestUserChoice :: PerspectivesWarning -> String -> String -> MonadPerspectives Boolean
requestUserChoice warning yesOption noOption = do
  setPDRStatus <- getPDRStatusSetter
  let payload = writeJSON { message: show warning, yesOption, noOption }
  _ <- pure $ setPDRStatus "requestUserIntegrityChoice" payload
  choiceAVar <- getUserIntegrityChoiceAVar
  liftAff $ AVar.take choiceAVar

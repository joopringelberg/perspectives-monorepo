-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
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

-------------------------------------------------------------------------------
---- TRANSLATION
-------------------------------------------------------------------------------

module Perspectives.ModelTranslation.Representation where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Object (Object, empty, union)
import Perspectives.CoreTypes (Translations)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, write)

-------------------------------------------------------------------------------
---- MODEL TRANSLATION REPRESENTATION
-------------------------------------------------------------------------------

-- The keys are the original notification texts.
newtype NotificationsTranslation = NotificationsTranslation (Object Translations)

derive newtype instance WriteForeign NotificationsTranslation
derive newtype instance ReadForeign NotificationsTranslation

-- The keys are the original markdown texts.
newtype MarkdownsTranslation = MarkdownsTranslation (Object Translations)

derive newtype instance WriteForeign MarkdownsTranslation
derive newtype instance ReadForeign MarkdownsTranslation

-- The keys are the original titles taken from a screen description in the model.
-- (a screen is part of the perspective on a role).
newtype TitlesTranslation = TitlesTranslation (Object Translations)

derive newtype instance WriteForeign TitlesTranslation
derive newtype instance ReadForeign TitlesTranslation

-- The translations of many properties.
-- The keys are the property names; the values are their translations.
newtype PropertiesTranslation = PropertiesTranslation (Object Translations)

derive newtype instance WriteForeign PropertiesTranslation
derive newtype instance ReadForeign PropertiesTranslation

-- The keys are the qualified Action names. Even though Action names are not qualified in the 
-- DomeinFile, we have qualified them in the Translation.
newtype ActionsTranslation = ActionsTranslation (Object Translations)

derive instance Newtype ActionsTranslation _
derive newtype instance WriteForeign ActionsTranslation
derive newtype instance ReadForeign ActionsTranslation

-- Keys are the string representations of StateSpecs.
-- In the translation table we will generate a pseudo-qualified action name from the stateSpec 
-- and the local action name.
newtype ActionsPerStateTranslation = ActionsPerStateTranslation (Object ActionsTranslation)

derive newtype instance WriteForeign ActionsPerStateTranslation
derive newtype instance ReadForeign ActionsPerStateTranslation
instance Semigroup ActionsPerStateTranslation where
  append (ActionsPerStateTranslation a1) (ActionsPerStateTranslation a2) = ActionsPerStateTranslation (a1 `union` a2)

instance Monoid ActionsPerStateTranslation where
  mempty = ActionsPerStateTranslation empty

newtype RoleTranslation = RoleTranslation
  { translations :: Translations
  , properties :: PropertiesTranslation
  , actions :: ActionsPerStateTranslation
  , notifications :: NotificationsTranslation
  , markdowns :: MarkdownsTranslation
  , titles :: TitlesTranslation
  }

derive newtype instance WriteForeign RoleTranslation
derive newtype instance ReadForeign RoleTranslation

-- The keys are the local role names.
newtype RolesTranslation = RolesTranslation (Object RoleTranslation)

derive newtype instance WriteForeign RolesTranslation
derive newtype instance ReadForeign RolesTranslation

newtype ContextTranslation = ContextTranslation
  { translations :: Translations
  , external :: RoleTranslation
  , users :: RolesTranslation
  , things :: RolesTranslation
  , contextroles :: RolesTranslation
  , notifications :: NotificationsTranslation
  -- The keys are the local context names
  , contexts :: ContextsTranslation
  }

instance WriteForeign ContextTranslation where
  writeImpl (ContextTranslation rec) = write rec

derive newtype instance ReadForeign ContextTranslation

newtype ContextsTranslation = ContextsTranslation (Object ContextTranslation)

instance WriteForeign ContextsTranslation where
  writeImpl (ContextsTranslation obj) = write obj

instance ReadForeign ContextsTranslation where
  readImpl f = do
    (ct :: Object ContextTranslation) <- readImpl f
    pure $ ContextsTranslation ct

-- A singleton object. The key is the model name.
newtype ModelTranslation = ModelTranslation
  { namespace :: String
  , version :: String
  , contexts :: Maybe ContextsTranslation
  , roles :: Maybe RolesTranslation
  }

derive newtype instance WriteForeign ModelTranslation
derive newtype instance ReadForeign ModelTranslation

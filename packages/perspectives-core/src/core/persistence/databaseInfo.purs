-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Perspectives.Persistence.DatabaseInfo where

import Effect.Aff.Compat (EffectFnAff)
import Foreign (Foreign)
import Perspectives.Persistence.Types (PouchdbDatabase)

foreign import databaseInfoImpl :: PouchdbDatabase -> EffectFnAff Foreign

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

module Perspectives.Sync.TransactionForPeer where

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------

import Data.Array (elemIndex, filter, snoc, union)
import Data.Foldable (foldM)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)
import Persistence.Attachment (class Attachment)
import Perspectives.Couchdb.Revision (class Revision)
import Perspectives.Data.EncodableMap as ENCMAP
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, PerspectivesUser)
import Perspectives.ResourceIdentifiers (isInPublicScheme)
import Perspectives.UnschemedIdentifiers (UnschemedResourceIdentifier, unschemePerspectivesUser)
import Perspectives.Sync.CanonicalJson (canonicalizeJsonString)
import Perspectives.Sync.DateTime (SerializableDateTime)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.PropertyHistory (checkpoint)
import Perspectives.Sync.Transaction (PublicKeyInfo)
import Perspectives.Sync.VersionedDelta (isPredefinedContextScaffolding)
import Prelude (class Ord, class Show, class Eq, bind, compare, map, not, pure, show, ($), (<$>), (<>), (&&), (<<<), (==), (>>=), eq)
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
newtype TransactionForPeer = TransactionForPeer
  { author :: PerspectivesUser
  , perspectivesSystem :: ContextInstance
  , timeStamp :: SerializableDateTime
  , deltas :: Array SignedDelta
  , publicKeys :: ENCMAP.EncodableMap PerspectivesUser PublicKeyInfo
  }

derive instance genericRepTransactionForPeer :: Generic TransactionForPeer _

derive instance newtypeTransactionForPeer :: Newtype TransactionForPeer _

instance showTransactionForPeer :: Show TransactionForPeer where
  show = genericShow

derive newtype instance ReadForeign TransactionForPeer
derive newtype instance WriteForeign TransactionForPeer

type WrappedContentKey =
  { recipient :: UnschemedResourceIdentifier
  , wrappedKey :: String
  }

type EncryptedDeltas =
  { ciphertext :: String
  , iv :: String
  , wrappedKeys :: Array WrappedContentKey
  }

newtype EncryptedTransactionForPeer = EncryptedTransactionForPeer
  { author :: PerspectivesUser
  , perspectivesSystem :: ContextInstance
  , timeStamp :: SerializableDateTime
  , encryptedDeltas :: EncryptedDeltas
  , publicKeys :: ENCMAP.EncodableMap PerspectivesUser PublicKeyInfo
  }

derive instance genericRepEncryptedTransactionForPeer :: Generic EncryptedTransactionForPeer _

derive instance newtypeEncryptedTransactionForPeer :: Newtype EncryptedTransactionForPeer _

instance showEncryptedTransactionForPeer :: Show EncryptedTransactionForPeer where
  show = genericShow

derive newtype instance ReadForeign EncryptedTransactionForPeer
derive newtype instance WriteForeign EncryptedTransactionForPeer

instance Attachment EncryptedTransactionForPeer where
  getAttachments _ = Nothing
  setAttachment t _ = t

instance Attachment TransactionForPeer where
  getAttachments _ = Nothing
  setAttachment t _ = t

-- | Add the new delta to the end of the array, in the Transaction.
addToTransactionForPeer :: SignedDelta -> TransactionForPeer -> TransactionForPeer
addToTransactionForPeer d (TransactionForPeer r@{ deltas }) = TransactionForPeer r
  { deltas =
      if isJust $ elemIndex d deltas then deltas
      else snoc deltas d
  }

-- | Keep local bootstrap history out of peer traffic and identity exports.
withoutPredefinedContextScaffolding :: TransactionForPeer -> TransactionForPeer
withoutPredefinedContextScaffolding (TransactionForPeer r) =
  removeUnusedPublicKeys $ TransactionForPeer r
    { deltas = filter (not <<< isPredefinedContextScaffolding <<< _.encryptedDelta <<< unwrap) r.deltas }

removeUnusedPublicKeys :: TransactionForPeer -> TransactionForPeer
removeUnusedPublicKeys (TransactionForPeer r) =
  let
    authors = r.deltas >>= \delta ->
      map unschemePerspectivesUser $
        [ (unwrap delta).author ] <> case checkpoint (unwrap delta).encryptedDelta of
          Nothing -> []
          Just { winner } -> [ (unwrap winner).author ]
  in
    TransactionForPeer r
      { publicKeys = ENCMAP.filterKeys (\author -> isJust $ elemIndex (unschemePerspectivesUser author) authors) r.publicKeys
      }

-- | Group signing identities without losing the public locator of their resources.
deduplicatePublicKeys :: ENCMAP.EncodableMap PerspectivesUser PublicKeyInfo -> Effect (ENCMAP.EncodableMap PerspectivesUser PublicKeyInfo)
deduplicatePublicKeys (ENCMAP.EncodableMap publicKeys) =
  ENCMAP.EncodableMap <<< Map.fromFoldable <<< Map.values <$>
    foldM addBundle Map.empty (Map.toUnfoldable publicKeys :: Array (Tuple PerspectivesUser PublicKeyInfo))
  where
  addBundle bundles (Tuple author info) =
    let
      identity = unschemePerspectivesUser author
    in
      case Map.lookup identity bundles of
        Nothing -> pure $ Map.insert identity (Tuple author info) bundles
        Just (Tuple existingAuthor existing) -> do
          sameSigningKey <- sameKey existing.key info.key
          sameTransportKey <- sameKey existing.transportKey info.transportKey
          if sameSigningKey && sameTransportKey then
            let
              locator = if isInPublicScheme (unwrap existingAuthor) then existingAuthor else author
              merged = existing { deltas = union existing.deltas info.deltas }
            in
              pure $ Map.insert identity (Tuple locator merged) bundles
          else throw $ "Conflicting verification keys for identity " <> show identity

  sameKey left right =
    if left == right then pure true
    else do
      canonicalLeft <- canonicalizeJsonString left
      canonicalRight <- canonicalizeJsonString right
      pure $ canonicalLeft == canonicalRight

transactieID :: TransactionForPeer -> String
transactieID (TransactionForPeer { author, timeStamp }) = show author <> "_" <> show timeStamp

-- | The Revision instance is a stub; we don't really need it (except in tests).
instance revisionTransactionForPeer :: Revision TransactionForPeer where
  rev t = Nothing
  changeRevision _ t = t

instance revisionEncryptedTransactionForPeer :: Revision EncryptedTransactionForPeer where
  rev t = Nothing
  changeRevision _ t = t

instance eqTransactionForPeer :: Eq TransactionForPeer where
  eq (TransactionForPeer { timeStamp: t1, author: a1 }) (TransactionForPeer { timeStamp: t2, author: a2 }) = eq t1 t2 && eq a1 a2

instance eqEncryptedTransactionForPeer :: Eq EncryptedTransactionForPeer where
  eq (EncryptedTransactionForPeer { timeStamp: t1, author: a1 }) (EncryptedTransactionForPeer { timeStamp: t2, author: a2 }) = eq t1 t2 && eq a1 a2

instance ordTransactionForPeer :: Ord TransactionForPeer where
  compare (TransactionForPeer { timeStamp: t1 }) (TransactionForPeer { timeStamp: t2 }) = compare t1 t2

instance ordEncryptedTransactionForPeer :: Ord EncryptedTransactionForPeer where
  compare (EncryptedTransactionForPeer { timeStamp: t1 }) (EncryptedTransactionForPeer { timeStamp: t2 }) = compare t1 t2

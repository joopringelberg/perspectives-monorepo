module Perspectives.Persistent.FromViews where

import Prelude

import Data.Foldable (for_)
import Effect.Aff (catchError)
import Persistence.Attachment (class Attachment)
import Perspectives.CoreTypes (class Persistent, MonadPerspectives)
import Perspectives.Persistence.API (Keys, getViewOnDatabase, resetViewIndex)
import Perspectives.Persistence.Types (DatabaseName, ViewName)
import Perspectives.Persistent (getPerspectEntiteit)
import Simple.JSON (class ReadForeign, class WriteForeign)

--  | This module provides a function to get resources from a view in the database.
--  | It tries to retrieve each resource and puts it in the cache.
--  | If it fails to retrieve a resource, it resets the view and tries again.
--  | Pouchdb views are not reliable.
getSafeViewOnDatabase
  :: forall a i key
   . Attachment a
  => Persistent a i
  => ReadForeign key
  => WriteForeign key
  => ReadForeign i
  => DatabaseName
  -> ViewName
  -> Keys key
  -> MonadPerspectives (Array i)
getSafeViewOnDatabase = getSafeViewOnDatabase_ identity

getSafeViewOnDatabase_
  :: forall a i key result
   . Attachment a
  => Persistent a i
  => ReadForeign key
  => WriteForeign key
  => ReadForeign i
  => ReadForeign result
  => (result -> i)
  -> DatabaseName
  -> ViewName
  -> Keys key
  -> MonadPerspectives (Array result)
getSafeViewOnDatabase_ f dbName viewname keys = do
  (results :: Array result) <- getViewOnDatabase dbName viewname keys
  -- Now try to put each instance in cache. 
  catchError
    ( do
        for_ (f <$> results) (getPerspectEntiteit false)
        pure results
    )
    ( \_ -> do
        -- Reset the view.
        void $ resetViewIndex dbName viewname
        -- Try again.
        getViewOnDatabase dbName viewname keys
    )

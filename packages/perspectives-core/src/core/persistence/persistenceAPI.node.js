// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

// Node.js-compatible version of persistenceAPI.js.
//
// Use this file instead of persistenceAPI.js when running the PDR in a Node.js
// environment (e.g. for automated testing with `spago test`).
//
// Key differences from the browser version:
//   1. Uses pouchdb-core + pouchdb-adapter-memory (no native LevelDB bindings).
//      Databases are in-memory only — sufficient for unit/integration testing.
//      Remote CouchDB replication still works via pouchdb-adapter-http.
//   2. Does NOT wrap the fetch call with a CORS-mode override — Node.js is not
//      subject to browser CORS restrictions.
//   3. `isOffLineImpl` always returns `false` (Node.js has no navigator.onLine).
//   4. `toFileImpl` / `fromBlobImpl` use Buffer because the browser File/Blob
//      constructors are not available in Node.js.

import PouchDB from "pouchdb-core";
import PouchDBMemoryAdapter from "pouchdb-adapter-memory";
import PouchDBHttpAdapter from "pouchdb-adapter-http";
import PouchDBMapReduce from "pouchdb-mapreduce";

PouchDB.plugin(PouchDBMemoryAdapter);
PouchDB.plugin(PouchDBHttpAdapter);
PouchDB.plugin(PouchDBMapReduce);

function convertPouchError( originalE )
{
  // Pouchdb returns an object in case of promise rejection.
  // But sometimes it returns a TypeError. We convert that to the same form.
  // We then stringify the objects and make a Javascript Error out of it.
  // By throwing a Javascript error, we make toAff catch it
  // and coerce it to an error in MonadError.
  // This is caught by catchError in Purescript, e.g. in addDocument.

  if ( originalE instanceof TypeError )
  {
    return new Error( JSON.stringify(
      { status: originalE.status
      , name: originalE.name
      , message: originalE.message
      , error: originalE.stack})
    );
  }
  else
  {
    return new Error( JSON.stringify(
      { status: originalE.status
      , name: originalE.constructor.name
      , message: originalE.message
      , error: originalE.error}));
  }
}

// In the Node.js context there are no CORS restrictions, so we do not override
// the fetch implementation.  PouchDB's default fetch (node-fetch) is used as-is.
//
// When databaseName is a URL (http:// or https://) we create a remote PouchDB
// instance backed by pouchdb-adapter-http.  This happens e.g. when the PDR opens
// a read-only view on a public repository such as
// "https://perspectives.domains/cw_perspectives_domains".
// For all other names (plain identifiers) we use the in-memory adapter, which is
// appropriate for unit/integration tests that should not touch the filesystem.
export function createDatabaseImpl( databaseName )
{
  if (databaseName.startsWith('http://') || databaseName.startsWith('https://'))
  {
    return new PouchDB( databaseName );
  }
  return new PouchDB( databaseName, { adapter: 'memory' } );
}

export function createRemoteDatabaseImpl( databaseName, couchdbUrl )
{
  var P = PouchDB.defaults({ prefix: couchdbUrl });
  return new P(databaseName);
}

export function deleteDatabaseImpl ( database ) {
  return function (onError, onSuccess) {
    database.destroy( function(err, response)
      {
        if (err != null)
        {
          onError( convertPouchError(err) );
        }
        else
        {
          onSuccess(response);
        }
      });

    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
};

export function documentsInDatabaseImpl( database, options ) {
  return function (onError, onSuccess) {
    database.allDocs( options, function(err, response)
      {
        if (err != null)
        {
          onError( convertPouchError(err) );
        }
        else
        {
          onSuccess(response);
        }
      });

    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
};

export function databaseInfoImpl ( database ) {
  return function (onError, onSuccess) {
    database.info( function(err, response)
      {
        if (err != null)
        {
          onError( convertPouchError(err) );
        }
        else
        {
          onSuccess(response);
        }
      });

    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
};

export function compactDatabaseImpl(db) {
  return function(onError, onSuccess) {
    db.compact()
      .then(function(result) {
        onSuccess(result);
      })
      .catch(function(err) {
        onError(convertPouchError(err));
      });
      
    return function(cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
}

export function cleanupDeletedDocsImpl (dbName) {
  const origDb = new PouchDB(dbName);
  const tempDb = new PouchDB(`${dbName}_temp`);
  let docsToKeep = [];
  
  return origDb.allDocs({include_docs: true})
    .then(result => {
      docsToKeep = result.rows
        .filter(row => !row.doc._deleted)
        .map(row => row.doc);
      if (docsToKeep.length) {
        return tempDb.bulkDocs(docsToKeep);
      } else {
        return Promise.resolve();
      }
    })
    .then(() => origDb.destroy())
    .then(() => tempDb.replicate.to(new PouchDB(dbName)))
    .then(() => tempDb.destroy())
    .then(() => {
      console.log(`Cleanup complete. Kept ${docsToKeep.length} non-deleted documents.`);
      return {};
    })
    .catch(err => {
      console.error(`Error during cleanup:`, convertPouchError(err));
      throw convertPouchError(err);
    });
  }

export function viewCleanupImpl(db) {
  return function(onError, onSuccess) {
    db.viewCleanup()
      .then(function(result) {
        onSuccess(result);
      })
      .catch(function(err) {
        onError(convertPouchError(err));
      });
      
    return function(cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
}

export function replicateOnce (origin, recovery, last_seq) {
  return function(onError, onSuccess) {
    const options = last_seq !== "" ? { since: last_seq, live: false } : { live: false };
    recovery.replicate.from(origin, options)
      .on('complete', function(info) {
        console.log(`replicated ${origin.name} to ${recovery.name}. Last sequence: ${info.last_seq}, previous: ${last_seq}`);
        onSuccess(info.last_seq ? info.last_seq.toString() : "");
      })
      .on('error', function(err) {
        console.error(`Error while replicating ${origin.name} to ${recovery.name}:`, err);
        onError(convertPouchError(err));
      });
      
    return function(cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
}

export function addDocumentImpl ( database, doc, force ) {
  return function (onError, onSuccess) {
    database.put(doc, {force: force}, function(err, response)
      {
        if (err != null)
        {
          onError( convertPouchError(err) );
        }
        else
        {
          onSuccess(response);
        }
      });

    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
};

export function purgeDocumentImpl (db, docId, rev) {
  return function (onError, onSuccess) {
    db.purge( docId, rev, function(err, response)
      {
        if (err != null)
        {
          onError( convertPouchError(err) );
        }
        else
        {
          onSuccess(response);
        }
      });

    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
};

export function bulkDocsImpl( database, docs, deleteP )
{
  if (deleteP)
    {
      docs.forEach( doc => doc._deleted = true )
    }
  return function (onError, onSuccess){
    database.bulkDocs( docs, function( err, response)
    {
      if (err !== null)
      {
        onError( convertPouchError(err) );
      }
      else
      {
        onSuccess(response);
      }
    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
      };
    })
  };
}

export function deleteDocumentImpl ( database, docName, rev ) {
  return function (onError, onSuccess) {
    database.remove(docName, rev, function(err, response)
      {
        if (err != null)
        {
          onError( convertPouchError(err) );
        }
        else
        {
          onSuccess(response);
        }
      });

    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
};

export function getDocumentImpl ( database, docName ) {
  return function (onError, onSuccess) {
    database.get( docName, function(err, response)
      {
        if (err != null)
        {
          onError( convertPouchError(err) );
        }
        else
        {
          onSuccess(response);
        }
      });

    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
};

// In Node.js, use the built-in Buffer API for Base64 encoding.
const b2a = function (b)
  {
    return Buffer.from(b).toString('base64');
  };

export function getDocumentWithConflictsImpl(db, docId, conflicts) {
  return function (onError, onSuccess) {
    db.get( docId, {conflicts: conflicts}, function(err, response)
      {
        if (err != null)
        {
          onError( convertPouchError(err) );
        }
        else
        {
          onSuccess(response);
        }
      });

    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
};

export function addAttachmentImpl ( database, docName, attachmentId, mrev, attachment, type) {
  return function (onError, onSuccess) {
    database.putAttachment(docName, attachmentId, mrev, attachment, type, function(err, response)
      {
        if (err != null)
        {
          onError( convertPouchError(err) );
        }
        else
        {
          onSuccess(response);
        }
      });

    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
};

export function getAttachmentImpl( database, docName, attachmentId )
{
  return function (onError, onSucces)
  {
    database.getAttachment(docName, attachmentId, function( err, blob )
      {
        if (err != null)
        {
          onError( convertPouchError( err ))
        }
        else
        {
          onSucces(blob);
        }
      });
    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
}

export function getViewOnDatabaseImpl( database, viewname, key, multipleKeys, includeDocs, forceRefresh )
{
  return function (onError, onSucces)
  {
    function notDeletedP(row)
    {
      return !(row.value && row.value.deleted);
    }

    if (multipleKeys)
    {
      database.query( viewname,
        { keys: key, include_docs: includeDocs, stale: forceRefresh ? 'update_after' : undefined },
        function (err, result)
        {
          if (err != null)
          {
            onError( convertPouchError( err ))
          }
          else
          {
            result.rows = result.rows.filter(notDeletedP);
            onSucces( result );
          }
        });
    }
    else if (key != "")
    {
      database.query( viewname,
        { key: key, include_docs: includeDocs, stale: forceRefresh ? 'update_after' : undefined },
        function (err, result)
        {
          if (err != null)
          {
            onError( convertPouchError( err ))
          }
          else
          {
            result.rows = result.rows.filter(notDeletedP);
            onSucces( result );
          }
        });
    }
    else
    {
      database.query( viewname, 
        {include_docs: includeDocs, stale: forceRefresh ? 'update_after' : undefined},
        function (err, result)
        {
          if (err != null)
          {
            onError( convertPouchError( err ))
          }
          else
          {
            result.rows = result.rows.filter(notDeletedP);
            onSucces( result );
          }
        });
    }
    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
}

// In Node.js there is no File constructor — return a plain Buffer wrapped in an
// object that mimics just enough of the File interface for the PDR to use.
export function toFileImpl( fileName, mimeType, arrayBuffer )
{
  const buf = Buffer.from(arrayBuffer);
  buf.name = fileName;
  buf.type = mimeType;
  return buf;
}

// In Node.js a "blob" may be a Buffer.  Return the contents as a UTF-8 string.
export function fromBlobImpl( blob )
{
  if (Buffer.isBuffer(blob))
  {
    return Promise.resolve(blob.toString('utf8'));
  }
  // Fall back for Node.js 18+ where Blob is available globally.
  if (typeof blob.text === 'function')
  {
    return blob.text();
  }
  return Promise.resolve(blob.toString());
}

export function resetViewIndexImpl(db, viewName) {
  return new Promise((resolve, reject) => {
    const parts = viewName.split('/');
    const designDocId = '_design/' + parts[0];
    
    let originalViewFunction;
    
    db.get(designDocId)
      .then((designDoc) => {
        originalViewFunction = designDoc.views[parts[1]].map;
        delete designDoc.views[parts[1]];
        return db.put(designDoc);
      })
      .then(() => db.viewCleanup())
      .then(() => db.get(designDocId))
      .then((designDoc) => {
        designDoc.views[parts[1]] = { map: originalViewFunction };
        return db.put(designDoc);
      })
      .then(() => db.query(viewName, { limit: 1, update_seq: true }))
      .then(() => resolve(true))
      .catch((err) => reject(err));
  });
}

// Node.js has no `navigator.onLine`; we optimistically report "online".
export function isOffLineImpl()
{
  return false;
}

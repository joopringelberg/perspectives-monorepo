# Structured Logging in the PDR

> **Relevant modules**
> - `packages/perspectives-core/src/core/coreTypes.purs` — `LogLevel`, `LogTopic`, `LogConfig`
> - `packages/perspectives-core/src/core/perspectivesState.purs` — `getLogConfig`, `setLogConfig`, `setTopicLogLevel`, `disableTopicLogging`, `disableAllLogging`
> - `packages/perspectives-core/src/core/errorHandling/logging.purs` — `pdrLog` and all convenience aliases
> - `packages/perspectives-core/src/core/Main.purs` — `setLogLevelForTopic`, `disableLogTopic`, `disableLogging` (callable from JavaScript)
>
> **Related design document**
> - `packages/perspectives-core/design/structured-logging.md`

---

## 1. Motivation

Before this facility was added, logging in the PDR consisted of ad-hoc calls to
`log` / `warn` scattered throughout the codebase. Those calls were either always
on or had to be commented out manually when they became noisy. There was no way
to selectively observe one subsystem without seeing every other subsystem's
output.

The structured logging layer solves this by:

- Assigning every log message a **severity level** and a **topic tag**.
- Storing a **runtime-configurable threshold** per topic (and a global default)
  in `PerspectivesState`.
- Suppressing any message whose level is below the configured threshold.
- Exposing the threshold as a JavaScript-callable API so it can be changed **in
  production without restarting the PDR**.

---

## 2. Core types (`coreTypes.purs`)

### 2.1 `LogLevel`

```purescript
data LogLevel = Trace | Debug | Info | Warn | Error | Silent
```

The constructors are ordered: `Trace < Debug < Info < Warn < Error < Silent`.

| Level    | Use for |
|----------|---------|
| `Trace`  | Function entry/exit, intermediate values, loop iterations |
| `Debug`  | State changes, decision branches, computed results |
| `Info`   | Normal operational milestones: model loaded, transaction sent |
| `Warn`   | Unexpected but recoverable: fallback used, retry, modeller warnings |
| `Error`  | Operation failed; system continues but the result is incorrect or missing |
| `Silent` | Special sentinel — used only as a **threshold** to suppress all output |

`Silent` is never passed as the *level* of a real log call; it is only ever
used as the threshold value that makes the `level >= threshold` guard fail for
every real message.

### 2.2 `LogTopic`

```purescript
data LogTopic
  = SYNC | BROKER | QUERY | PERSISTENCE | STATE | AUTH
  | MODEL | UPGRADE | PARSER | COMPILER | INSTALL | OTHER
```

| Topic         | Subsystem |
|---------------|-----------|
| `SYNC`        | Transaction distribution, delta handling, AMQP messaging |
| `BROKER`      | Broker-service connection and account management |
| `QUERY`       | Query compilation and execution |
| `PERSISTENCE` | CouchDB/PouchDB reads and writes, cache management |
| `STATE`       | State evaluation and automatic actions |
| `AUTH`        | Authentication, signing, public keys |
| `MODEL`       | Model loading, parsing, DomeinFiles |
| `UPGRADE`     | Data upgrades, model recompilation |
| `PARSER`      | ARC parser errors |
| `COMPILER`    | ARC compiler (Phases 1–3) |
| `INSTALL`     | Model installation, Couchdb setup |
| `OTHER`       | Anything that does not fit another topic |

### 2.3 `LogConfig`

```purescript
type LogConfig =
  { defaultLevel :: LogLevel
  , topicLevels  :: Map LogTopic LogLevel
  }
```

`topicLevels` holds per-topic threshold overrides.  A topic that has no entry
in the map falls back to `defaultLevel`.

`LogConfig` is a field of `PerspectivesState` (initialised in
`newPerspectivesState` with `defaultLevel = Warn` and an empty map).

---

## 3. Emitting a log message (`logging.purs`)

### 3.1 The core function

```purescript
pdrLog :: LogTopic -> LogLevel -> String -> MonadPerspectives Unit
pdrLog topic level message = do
  { defaultLevel, topicLevels } <- gets _.logConfig
  let threshold = fromMaybe defaultLevel (Map.lookup topic topicLevels)
  when (level >= threshold) do
    let prefix = "[" <> show level <> "] [" <> show topic <> "] "
    liftEffect $ Console.log (prefix <> message)
```

**Output format:**
```
[DEBUG] [SYNC] Sending transaction to peer fdifuzxgdq
[WARN]  [MODEL] Property type not found: SomeProperty
[ERROR] [PERSISTENCE] CouchDB returned 409 for document xyz
```

### 3.2 Convenience aliases

`logging.purs` exports pre-bound aliases for every topic × relevant level
combination, following the naming pattern `<level><Topic>`:

```purescript
traceSync        debugSync        infoSync         warnSync         errorSync
traceBroker      debugBroker      infoBroker       warnBroker       errorBroker
traceQuery       debugQuery
tracePersistence debugPersistence warnPersistence  errorPersistence
traceState       debugState       warnState
warnAuth
debugModel       infoModel        warnModel        errorModel
debugUpgrade     infoUpgrade      errorUpgrade
debugCompiler    errorCompiler
debugInstall     infoInstall      errorInstall
warnOther        errorOther
errorParser
```

A typical call site looks like:

```purescript
import Perspectives.Logging (warnModel)

warnModel $ "Property type not found: " <> show propType
```

or, inside a `MonadPerspectivesTransaction` context where a `lift` is needed:

```purescript
import Perspectives.Logging (errorSync)
import Control.Monad.Trans.Class (lift)

lift $ errorSync $ "Delta rejected: " <> show delta
```

---

## 4. Changing the configuration at runtime (`perspectivesState.purs`)

All helpers operate inside `MonadPerspectives`.

### 4.1 `setTopicLogLevel`

```purescript
setTopicLogLevel :: LogTopic -> LogLevel -> MonadPerspectives Unit
```

Adds or replaces the per-topic threshold.  Existing overrides for other topics
are not affected.

```purescript
-- Show all SYNC messages (including fine-grained trace output):
setTopicLogLevel SYNC Trace

-- Lower MODEL back to Warn after debugging:
setTopicLogLevel MODEL Warn
```

### 4.2 `disableTopicLogging`

```purescript
disableTopicLogging :: LogTopic -> MonadPerspectives Unit
```

Raises the threshold for the given topic to `Silent`, suppressing all output
from that topic.  Equivalent to `setTopicLogLevel topic Silent`.

### 4.3 `disableAllLogging`

```purescript
disableAllLogging :: MonadPerspectives Unit
```

Sets `defaultLevel = Silent` and clears all per-topic overrides.  No log
message from any topic will be emitted until the configuration is changed again.

### 4.4 `setLogConfig` / `getLogConfig`

For completely replacing or reading the entire configuration:

```purescript
getLogConfig :: MonadPerspectives LogConfig
setLogConfig :: LogConfig -> MonadPerspectives Unit
```

---

## 5. Calling the API from JavaScript (`Main.purs`)

A module-level `globalStateRef` (a `Ref (Maybe (AVar PerspectivesState))`)
stores the active state AVar after `runPDR` initialises it.  Three `Effect Unit`
functions are compiled into the PDR bundle and callable from JavaScript via the
`pdr` object available in the SharedWorker.

### 5.1 `setLogLevelForTopic`

```typescript
pdr.setLogLevelForTopic(topic: string)(level: string)(): void
```

Sets the per-topic log threshold.

- `topic` — one of `"SYNC"`, `"BROKER"`, `"QUERY"`, `"PERSISTENCE"`,
  `"STATE"`, `"AUTH"`, `"MODEL"`, `"UPGRADE"`, `"PARSER"`, `"COMPILER"`,
  `"INSTALL"`, `"OTHER"`.
- `level` — one of `"TRACE"`, `"DEBUG"`, `"INFO"`, `"WARN"`, `"ERROR"`,
  `"SILENT"`.

Unknown strings are ignored silently.

**Example** (browser DevTools console, while the SharedWorker is running):

```javascript
// Show all SYNC messages from this moment on:
pdr.setLogLevelForTopic("SYNC")("TRACE")();
```

### 5.2 `disableLogTopic`

```typescript
pdr.disableLogTopic(topic: string)(): void
```

Suppresses all output for the given topic.

```javascript
pdr.disableLogTopic("QUERY")();
```

### 5.3 `disableLogging`

```typescript
pdr.disableLogging(): void
```

Suppresses all log output from every topic.  Useful when a verbose subsystem is
drowning out the console during production troubleshooting.

```javascript
pdr.disableLogging()();
```

> **Note on PureScript calling convention**
> All three functions are exported as curried PureScript `Effect Unit` values.
> In JavaScript, `Effect Unit` compiles to a zero-argument function, so each
> call must be terminated with `()`.  Curried string arguments each also require
> their own `()`:
> ```javascript
> pdr.setLogLevelForTopic("SYNC")("DEBUG")();
> //                     ^topic  ^level  ^Effect
> ```

---

## 6. Migration guide for call sites

Existing `logPerspectivesError`, `warnModeller`, or raw `log` calls can be
replaced incrementally. The pattern is:

| Before | After |
|--------|-------|
| `log "Some message"` | `debugState "Some message"` (choose topic/level) |
| `logPerspectivesError $ Custom msg` | `errorOther msg` (or more specific topic) |
| `warnModeller msg` | `warnModel msg` (already delegates internally) |
| `warnModellerWithError e msg` | `warnModel (msg <> ": " <> show e)` |

`warnModeller` and `warnModellerWithError` in `errorLogging.purs` already
delegate to `pdrLog MODEL Warn`, so they respect the configured threshold.
Call sites that still use them do not need to be changed immediately; they will
automatically be filtered along with all other `MODEL` messages.

---

## 7. Adding a new topic

1. Add the new constructor to `LogTopic` in `coreTypes.purs`:
   ```purescript
   data LogTopic = ... | MYNEWTOPIC
   ```
2. Add the `Show` instance case in the same file:
   ```purescript
   show MYNEWTOPIC = "MYNEWTOPIC"
   ```
3. Add the `Eq` and `Ord` cases (they use derived instances via `compare`, so
   only `Eq` needs a manual entry in its hand-written instance).
4. Add `parseLogTopic "MYNEWTOPIC" = Just MYNEWTOPIC` in `Main.purs` so that
   the JavaScript API recognises the new string.
5. Add convenience aliases in `logging.purs` as needed and export them.

# Structured Logging for the PDR

## Problem

Logging in the PDR is ad-hoc: `log` and `warn` calls are scattered throughout
the codebase. During development they get commented out or removed, only to be
re-added later. There is no way to selectively enable logging for a subsystem
without editing source files.

## Goals

1. **Never comment out or remove log statements again.** A runtime threshold
   determines what is visible.
2. **Filter by topic** so that, for example, only synchronisation messages are
   shown while debugging sync issues.
3. **Minimal footprint.** The mechanism must be simple enough to adopt
   incrementally — no heavyweight framework.

## Design

### Log levels

Five ordered severity levels:

```
Trace < Debug < Info < Warn < Error
```

| Level | Use for |
|-------|---------|
| Trace | Function entry/exit, intermediate values, loop iterations |
| Debug | State changes, decision branches, computed results |
| Info  | Normal operational milestones: model loaded, transaction sent |
| Warn  | Unexpected but recoverable: fallback used, retry, modeller warnings |
| Error | Operation failed; system continues but result is incorrect or missing |

### Topics

A topic is a simple string tag that identifies a subsystem. Initial set:

| Topic | Subsystem |
|-------|-----------|
| `SYNC` | Transaction distribution, delta handling, AMQP messaging |
| `BROKER` | Broker service connection and account management |
| `QUERY` | Query compilation and execution |
| `PERSISTENCE` | Couchdb reads/writes, cache management |
| `STATE` | State evaluation and automatic actions |
| `AUTH` | Authentication, signing, public keys |
| `MODEL` | Model loading, parsing, domain files |
| `UPGRADE` | Data upgrades |

New topics can be added freely; they are just strings. Unrecognised topics
inherit the global default level.

### Configuration

The log configuration lives in `PerspectivesState`:

```purescript
type LogConfig =
  { defaultLevel :: LogLevel
  , topicLevels  :: Map LogTopic LogLevel
  }
```

A message is emitted when its level is **≥** the configured threshold for its
topic (or the default if the topic has no override).

### API

A single entry point replaces the various `log`/`warn` calls:

```purescript
type LogTopic = String

data LogLevel = Trace | Debug | Info | Warn | Error

derive instance Eq LogLevel
derive instance Ord LogLevel

pdrLog :: LogTopic -> LogLevel -> String -> MonadPerspectives Unit
```

Plus convenience aliases:

```purescript
traceSync   = pdrLog "SYNC"   Trace
debugSync   = pdrLog "SYNC"   Debug
warnBroker  = pdrLog "BROKER" Warn
-- etc.
```

The existing `warnModeller` and `warnModellerWithError` stay but delegate to
`pdrLog "MODEL" Warn` internally, so they also obey the configured level.

### Implementation sketch

```purescript
pdrLog :: LogTopic -> LogLevel -> String -> MonadPerspectives Unit
pdrLog topic level message = do
  { defaultLevel, topicLevels } <- gets _.logConfig
  let threshold = fromMaybe defaultLevel (Map.lookup topic topicLevels)
  when (level >= threshold) do
    let prefix = "[" <> show level <> "] [" <> topic <> "] "
    liftEffect $ log (prefix <> message)
```

### Output format

```
[DEBUG] [SYNC] Sending transaction to peer fdifuzxgdq
[WARN]  [MODEL] Property type not found: SomeProperty
[ERROR] [PERSISTENCE] Couchdb returned 409 for document xyz
```

### Migration path

1. Add `LogConfig` to `PerspectivesState` with a sensible default
   (`defaultLevel = Warn`).
2. Implement `pdrLog`.
3. Convert existing logging call sites **incrementally**, one module or topic
   at a time. No big-bang rewrite needed.
4. Remove commented-out log statements as they are replaced.

### Runtime control

Because the config is in `PerspectivesState`, it can be changed at runtime —
for example through a debug panel in MyContexts or via an API call. This means
a user or developer can turn on `SYNC = Trace` while investigating a problem,
without restarting.

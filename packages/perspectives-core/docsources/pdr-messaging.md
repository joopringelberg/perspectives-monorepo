# PDR → Frontend Messaging

This document describes the two complementary mechanisms that the Perspectives Distributed Runtime (PDR) uses to send messages to the end user's browser, and how a developer can use them.

> **Source modules (PDR side):**
> - `packages/perspectives-core/src/core/coreTypes.purs` — `Warning`, `PerspectivesState`
> - `packages/perspectives-core/src/core/perspectivesState.purs` — `pushMessage`, `removeMessage`, `addWarning`, etc.
> - `packages/perspectives-core/src/core/Main.purs` — startup wiring
> - `packages/perspectives-core/src/core/proxy.js` — JS bridge
> - `packages/perspectives-core/src/core/proxy.purs` — PureScript FFI declarations
>
> **Source modules (transport layer):**
> - `packages/perspectives-sharedworker/src/perspectives-sharedworker.js`
> - `packages/perspectives-pageworker/src/perspectives-pageworker.js`
>
> **Source modules (client side):**
> - `packages/perspectives-proxy/src/perspectives-proxy.ts` — `SharedWorkerChannel.handleWorkerResponse`, `Cursor`, `PerspectivesProxy`
> - `packages/mycontexts/src/www.tsx` — wiring the warning channel

---

## Overview

There are two distinct mechanisms:

| Mechanism | Direction | Timing | Purpose |
|---|---|---|---|
| **`setPDRStatus`** (push / remove) | PDR → all clients | Real-time, out-of-band | Show / hide a loading overlay with a status message |
| **`warnings`** accumulation | PDR → responding client | Piggybacked on API response | Deliver non-fatal diagnostic messages to the modelling user |

A third, specialised extension of `setPDRStatus` — the **`requestUserIntegrityChoice`** dialog — blocks the PDR fiber until the user has made a yes/no choice; it is described separately below.

---

## Mechanism 1 — Real-time Status Messages (`setPDRStatus`)

### Design intent

The PDR runs in a SharedWorker (Chrome, Firefox, Edge) or a page-hosted worker (Safari, which does not support SharedWorkers). Long-running operations (e.g. loading a model, restoring resources) should display a "busy" indicator with a human-readable status text in all connected browser tabs simultaneously. When the operation finishes the message should disappear.

### PDR side (PureScript)

The two helper functions to use are both defined in `Perspectives.PerspectivesState`:

```purescript
-- | Show a status message in the frontend loading overlay.
pushMessage :: String -> MonadPerspectives Unit

-- | Remove a previously pushed status message.
removeMessage :: String -> MonadPerspectives Unit
```

Both functions call `getPDRStatusSetter` to retrieve the `setPDRStatus` function stored inside `PerspectivesState`, then invoke it with the appropriate action string (`"push"` or `"remove"`) and the message text.

```purescript
-- From perspectivesState.purs:
getPDRStatusSetter :: MonadPerspectives (String -> String -> Unit)
getPDRStatusSetter = gets _.setPDRStatus

pushMessage :: String -> MonadPerspectives Unit
pushMessage msg = do
  setPDRStatus <- getPDRStatusSetter
  pure $ setPDRStatus "push" msg

removeMessage :: String -> MonadPerspectives Unit
removeMessage msg = do
  setPDRStatus <- getPDRStatusSetter
  pure $ setPDRStatus "remove" msg
```

#### The `setPDRStatus` field in `PerspectivesState`

`PerspectivesState` (defined in `coreTypes.purs`) carries:

```purescript
, setPDRStatus :: String -> String -> Unit
```

Its default value (set in `perspectivesState.purs`) is a no-op:

```purescript
, setPDRStatus: \_ _ -> unit
```

During startup (`runPDR` in `Main.purs`), the field is replaced with a real implementation obtained from the SharedWorker:

```purescript
-- Main.purs (simplified):
statusMessageChannel :: Fn2 String String Unit <- toAff Proxy.pdrStatusMessageChannel
-- ...
new (state' { setPDRStatus = ((runFn2 statusMessageChannel) :: String -> String -> Unit) })
```

`Proxy.pdrStatusMessageChannel` is a `Promise (Fn2 String String Unit)` declared as a foreign import in `proxy.purs` and resolved in `proxy.js`.

### JS bridge (`proxy.js`)

```javascript
// proxy.js — the Promise that will hold the sendPDRStatusMessage function
// from the SharedWorker.
export const pdrStatusMessageChannel = new Promise(function(resolver) {
  receivePDRStatusMessageChannelResolver = resolver;
});

// Called by the SharedWorker to install the actual broadcast function.
export function receivePDRStatusMessageChannel(channel) {
  receivePDRStatusMessageChannelResolver(channel);
}
```

The Promise is resolved when the SharedWorker calls `receivePDRStatusMessageChannel`.

### SharedWorker layer (`perspectives-sharedworker.js`)

Each time a new browser tab connects, the SharedWorker passes the local `sendPDRStatusMessage` function to `proxy.js`:

```javascript
self.onconnect = function(e) {
  // ...subscribe the new page...
  pdr.receivePDRStatusMessageChannel(sendPDRStatusMessage);
};

// Broadcasts {responseType: "PDRMessage", action, message} to ALL connected tabs.
function sendPDRStatusMessage(action, message) {
  Object.values(subscribingPages).forEach(function(channel) {
    channel.postMessage({ responseType: "PDRMessage", action, message });
  });
}
```

> **Note:** Each new `onconnect` event replaces the previously registered function.  Because all calls to `receivePDRStatusMessageChannel` resolve the same Promise (and a resolved Promise cannot be re-resolved), the final installed function is `sendPDRStatusMessage` from the **first** connected tab.  In practice this is fine: `sendPDRStatusMessage` always iterates over all current `subscribingPages`, so it broadcasts to every tab regardless of when the Promise was resolved.

### PageWorker layer (`perspectives-pageworker.js`)

On Safari (where SharedWorkers are not available), the PDR runs in a hosting page. The page-worker layer (`perspectives-pageworker.js`) implements the same `handleClientRequest`/`receivePDRStatusMessageChannel` interface from `proxy.js` and posts `PDRMessage` frames back to client pages via the same Channel Messaging API — the client side is therefore identical.

### Proxy layer (`perspectives-proxy.ts`)

`SharedWorkerChannel.handleWorkerResponse` receives all messages from the worker. When `responseType === "PDRMessage"` and `action` is `"push"` or `"remove"`, it delegates to `Cursor.setPDRStatus`:

```typescript
// SharedWorkerChannel.handleWorkerResponse (perspectives-proxy.ts):
else if (e.data.responseType === "PDRMessage") {
  if (e.data.action === "requestUserIntegrityChoice") {
    // Special case — see Mechanism 3 below.
  } else {
    proxy.cursor.setPDRStatus(e.data as { action: "push" | "remove"; message: string });
  }
}
```

`Cursor.setPDRStatus` maps the action to the overlay:

```typescript
// Cursor (perspectives-proxy.ts):
setPDRStatus({ action, message }: { action: "push" | "remove"; message: string }) {
  const STATUS_ID = Cursor.STATUS_ID;   // fixed string "PDR_STATUS"
  if (action === "push") {
    this.pushMessage(STATUS_ID, message);
  } else if (action === "remove") {
    this.removeMessage(STATUS_ID);
  }
}
```

`Cursor.pushMessage` shows a DOM overlay (`div.pdr-loading-overlay`) with a spinner and the message text. `Cursor.removeMessage` hides it when all messages have been cleared.

---

## Mechanism 2 — Warnings (Piggybacked on API Responses)

### Design intent

Non-fatal problems detected during the execution of an API call (e.g. model inconsistencies, parse warnings) are collected in the `warnings` array of `PerspectivesState` and returned to the *requesting client* as part of the API response.  This mechanism is intended for diagnostics aimed at **modellers**, not end users.

### PDR side (PureScript)

Warnings accumulate in `PerspectivesState.warnings :: Array Warning` where:

```purescript
-- coreTypes.purs:
type Warning = { message :: String, error :: String }
```

Three helpers write to this array:

```purescript
-- errorLogging.purs:
warnModeller :: PerspectivesWarning -> MonadPerspectives Unit
warnModeller warning =
  modify \(s@{ warnings }) -> s { warnings = cons { message: show warning, error: "" } warnings }

warnModellerWithError :: PerspectivesWarning -> String -> MonadPerspectives Unit
warnModellerWithError warning error =
  modify \(s@{ warnings }) -> s { warnings = cons { message: show warning, error } warnings }

-- perspectivesState.purs:
addWarning :: Warning -> MonadPerspectives Unit
addWarning w = modify \s -> s { warnings = cons w s.warnings }
```

At the end of every API call, `sendResponse` in `perspectivesAPI.purs` attaches the current warnings to the outbound response and resets the accumulator:

```purescript
-- perspectivesAPI.purs:
sendResponse :: Response -> Api.ApiEffect -> MonadPerspectives Unit
sendResponse (Result corrId values) ae = do
  warnings <- getWarnings
  resetWarnings
  liftEffect $ ae (ResultWithWarnings corrId values (writeJSON <$> warnings))
sendResponse (Error corrId message) ae = do
  warnings <- getWarnings
  resetWarnings
  liftEffect $ ae (ErrorWithWarnings corrId message (writeJSON <$> warnings))
```

### Proxy layer (`perspectives-proxy.ts`)

When `PerspectivesProxy.send` receives an `APIresult` response with a non-empty `warnings` array it calls the `userMessageChannel` callback (if one has been registered):

```typescript
// PerspectivesProxy.send (perspectives-proxy.ts):
else if (response.responseType === "APIresult") {
  if (response.warnings.length > 0) {
    if (proxy.userMessageChannel) {
      proxy.userMessageChannel(
        response.warnings.map(s => JSON.parse(s) as Warning)
      );
    }
  }
  receiveValues(response.result);
}
```

The callback type is:

```typescript
type UserMessageChannel = (warnings: Warning[]) => void;
export type Warning = { message: string; error: string };
```

Register the callback via:

```typescript
pdrProxy.setUserMessageChannel((warnings: Warning[]) => {
  // Display warnings to the user however is appropriate.
});
```

### MyContexts layer (`www.tsx`)

The MyContexts application registers the callback during component construction and forwards warnings to the `UserMessaging` subsystem:

```typescript
// www.tsx (App constructor):
PDRproxy.then(pproxy =>
  pproxy.setUserMessageChannel((warnings: Warning[]) =>
    UserMessagingPromise.then(um =>
      warnings.forEach(warning =>
        um.addMessageForEndUser({
          title: i18next.t("userMessagingPanel_title", { ns: "mycontexts" }),
          message: i18next.t("userMessagingPanel_message", {
            ns: "mycontexts",
            warning: warning.message,
          }),
          error: warning.error,
        })
      )
    )
  )
);
```

---

## Mechanism 3 — Blocking Integrity-Choice Dialog (`requestUserIntegrityChoice`)

This is a specialised extension of Mechanism 1, used when the PDR detects a missing resource and must block the current fiber until the user has decided whether to restore or permanently remove it.

### Flow overview

```
PDR fiber
  │
  ├─ calls setPDRStatus("requestUserIntegrityChoice", jsonPayload)
  │      ─────────────────────────────────────────────────────────►  SharedWorker
  │                                                                     │
  │                                                                     ├─ postMessage({responseType:"PDRMessage",
  │                                                                     │               action:"requestUserIntegrityChoice",
  │                                                                     │               message: jsonPayload})
  │                                                                     │         ──────────────────────────────────► Browser tab
  │                                                                                                                      │
  │                                                                                                                      ├─ proxy.cursor.showIntegrityChoiceDialog(payload)
  │                                                                                                                      │         (blocking modal dialog)
  │                                                                                                                      │
  │  ◄──── port.postMessage({proxyRequest:"resolveUserIntegrityChoice", choice: true|false}) ────────────────────────────┤
  │
  ├─ blocks on AVar.take(userIntegrityChoice)
  │         (unblocked when SharedWorker calls putUserIntegrityChoiceFn(choice))
  │
  └─ continues (restore or remove)
```

### PDR side (PureScript) — `requestIntegrityChoice`

Defined in `Perspectives.ReferentialIntegrity` (`fixBrokenLinks.purs`):

```purescript
requestIntegrityChoice :: String -> MonadPerspectives Boolean
requestIntegrityChoice message = do
  setPDRStatus <- getPDRStatusSetter
  _ <- pure $ setPDRStatus "requestUserIntegrityChoice" message
  choiceAVar <- getUserIntegrityChoiceAVar
  liftAff $ AVar.take choiceAVar   -- blocks until the frontend responds
```

The `message` parameter is a JSON-encoded `IntegrityChoicePayload`:

```json
{
  "resourceKind": "rol",
  "message": "Missing role cuid123 of type My Role",
  "restoreOption": "Herstel",
  "removeOption": "Verwijder definitief"
}
```

> **Note:** `restoreOption` and `removeOption` are the button labels displayed to the user.  The current implementation uses hardcoded Dutch strings (`"Herstel"` = "Restore", `"Verwijder definitief"` = "Remove permanently").  See `buildIntegrityChoiceMessage` in `fixBrokenLinks.purs` if multi-language support is needed in the future.

### Proxy layer — receiving the dialog request

In `SharedWorkerChannel.handleWorkerResponse`:

```typescript
else if (e.data.responseType === "PDRMessage") {
  if (e.data.action === "requestUserIntegrityChoice") {
    const payload = JSON.parse(e.data.message) as IntegrityChoicePayload;
    proxy.cursor.showIntegrityChoiceDialog(payload).then((choice: boolean) => {
      proxy.channelId.then(channelId =>
        proxy.port.postMessage({
          proxyRequest: "resolveUserIntegrityChoice",
          choice,
          channelId,
        })
      );
    });
  }
  // ...
}
```

`Cursor.showIntegrityChoiceDialog` renders a modal overlay with the message and two buttons (restore / remove permanently) and resolves the returned `Promise<boolean>` when the user clicks one.

### Proxy layer — delivering the answer back to the PDR

The `resolveUserIntegrityChoice` proxy request is handled in `proxy.js` (SharedWorker context):

```javascript
case "resolveUserIntegrityChoice":
  if (putUserIntegrityChoiceFn !== null) {
    putUserIntegrityChoiceFn(req.choice)();   // fills the AVar in PerspectivesState
  }
  channels[corrId2ChannelId(req.channelId)].postMessage({
    responseType: "WorkerResponse",
    serviceWorkerMessage: "resolveUserIntegrityChoice",
    success: true,
  });
  break;
```

`putUserIntegrityChoiceFn` was registered during PDR startup from `Main.purs`:

```purescript
-- Main.purs:
liftEffect $ Proxy.registerPutUserIntegrityChoice \choice -> launchAff_ do
  s <- read state
  put choice s.userIntegrityChoice
```

`registerPutUserIntegrityChoice` is the PureScript FFI wrapper around `registerPutUserIntegrityChoice_` in `proxy.js`, which stores the callback so `handleClientRequest` can call it when the `resolveUserIntegrityChoice` proxy request arrives.

---

## Startup Wiring (Summary)

The following sequence happens once per PDR startup (`runPDR` in `Main.purs`):

1. `pdrStatusMessageChannel` (a `Promise`) is exported from `proxy.js` and imported as a PureScript foreign import in `proxy.purs`.
2. The SharedWorker calls `pdr.receivePDRStatusMessageChannel(sendPDRStatusMessage)` on each new client connection. This resolves the `pdrStatusMessageChannel` Promise with the `sendPDRStatusMessage` broadcast function.
3. In `Main.purs`, `toAff Proxy.pdrStatusMessageChannel` awaits that Promise and unwraps the JS function as `Fn2 String String Unit`.
4. The function is stored as `setPDRStatus` in the newly created `PerspectivesState` record.
5. From this point on, any MonadPerspectives code can call `pushMessage` / `removeMessage` and the message will be broadcast to all connected browser tabs.

---

## Quick Reference for PDR Developers

### Show a status message from PDR code

```purescript
import Perspectives.PerspectivesState (pushMessage, removeMessage)

-- In MonadPerspectives:
myLongOperation :: MonadPerspectives Unit
myLongOperation = do
  pushMessage "Loading model..."
  -- ... do the actual work ...
  removeMessage "Loading model..."
```

> **Important:** always pair every `pushMessage` with a matching `removeMessage` using the **exact same string**. The client matches messages by their text identifier. An unmatched push will leave the loading overlay visible indefinitely.

### Add a warning to the current API response

```purescript
import Perspectives.ErrorLogging (warnModeller, warnModellerWithError)
import Perspectives.PerspectivesState (addWarning)

-- Simple warning (no associated error detail):
warnModeller SomePerspectivesWarning

-- Warning with error detail:
warnModellerWithError SomePerspectivesWarning (show someError)

-- Or directly with a plain record:
addWarning { message: "Something looks wrong", error: "" }
```

Warnings are automatically appended to the next API response and then cleared. They arrive at the client as `Warning[]` through the `userMessageChannel` callback registered on `PerspectivesProxy`.

### Choosing the right mechanism

| You want to… | Use |
|---|---|
| Show a spinner / status text while a long operation runs | `pushMessage` / `removeMessage` |
| Notify the modeller of a non-fatal inconsistency detected during an API call | `addWarning` / `warnModeller` |
| Block until the end user makes a restore/remove choice | `requestIntegrityChoice` (see `fixBrokenLinks.purs`) |

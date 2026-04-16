# BrokerContract model investigation

This note compares the desired BrokerContract state machine in:

- `/home/runner/work/perspectives-monorepo/perspectives-monorepo/packages/perspectives-core/src/model/BrokerContract.smcat`

with the current model implementation in:

- `/home/runner/work/perspectives-monorepo/perspectives-monorepo/packages/perspectives-core/src/model/BrokerServices@6.1.arc`

## Main mismatches

1. **Initialization is split differently**
   - Diagram: `Initialize` then `CanRegister`.
   - Model: `NoAdministrator` and `NoAccountHolder` before `CanRegister`.
   - Effect: same intent, but no 1:1 state mapping.

2. **Missing `Suspended` state**
   - Diagram: `Active -> ExpiresSoon -> Suspended -> Terminated`.
   - Model: `Active` with nested `ExpiresSoon`, but no `Suspended` state.
   - Effect: there is no explicit grace-period suspension phase.

3. **Missing extension flow**
   - Diagram: `ExtensionPending` plus transitions controlled by `ExtensionRequested`.
   - Model: no `ExtensionRequested` property/state and no `ExtensionPending`.
   - Effect: no modeled renewal request/approval cycle.

4. **No permission toggle behavior for suspension/restore**
   - Diagram references `PreventAMQPaccountFromReading` and `SetPermissions`.
   - Model: no corresponding transition actions.
   - Effect: the access-right lifecycle in the diagram is not represented.

5. **No restore transition from `Terminated` to `Active`**
   - Diagram: `Terminated -> Active` via `RestoreAccount`.
   - Model: no explicit restore transition/action.
   - Effect: termination behaves as final in practice.

6. **Termination timing path differs**
   - Diagram: `Suspended -> Terminated` when `CurrentDate > TerminatesOn`.
   - Model: top-level `Terminated` condition checks `CurrentDate > TerminatesOn` directly.
   - Effect: model can terminate without ever entering a suspension state.

## AccountHolder capabilities per model state

Below, "AccountHolder" means role `BrokerContract$AccountHolder` in `BrokerServices@6.1.arc`.

1. **`NoAdministrator`**
   - No AccountHolder role instance exists yet.
   - Effective capability: none as AccountHolder.

2. **`NoAccountHolder`**
   - Administrator creates the AccountHolder role instance.
   - AccountHolder is an invitee (`sys:Invitation$Invitee`) and can accept/fill invitation-related data through inherited invitation behavior.

3. **`CanRegister`**
   - On entry, registration is automatic for AccountHolder:
     - queue is created,
     - credentials are generated,
     - `rabbit:SelfRegisterWithRabbitMQ` is called,
     - queue is bound and listening starts.
   - User-facing choice is limited; this is mostly system-driven.

4. **`Active` (including nested `ExpiresSoon`)**
   - AccountHolder can:
     - read broker connection data (`Url`, `Exchange`, `CurrentQueueName`),
     - edit contract properties on `extern` (`Registered`, `UseExpiresOn`, `GracePeriodExpiresOn`, `TerminatesOn`, `ContractTerminated`, `IsInUse`),
     - run action `TerminateContract` (only in state `Active`),
     - create/fill/remove `Queues` and edit `QueueName`,
     - use all role verbs on own role and read/edit `AccountName`/`AccountPassword`.

5. **`Terminated`**
   - Account is deleted (`DeleteAMQPaccount`), queues are deleted, `Registered=false`, `ContractTerminated=true`, `IsInUse=false`.
   - The `TerminateContract` action is no longer available (guarded by `in object state Active`).
   - Other broad property permissions remain defined in the model.

## Conclusion

The current model only implements part of the lifecycle described in `BrokerContract.smcat`. The largest functional gap is the missing **suspension + extension/restore flow**.

If full alignment with the diagram is required, the next change should add explicit modeled support for:

- `Suspended`,
- `ExtensionPending`,
- `ExtensionRequested`-driven transitions,
- AMQP permission revoke/restore actions,
- optional restore path from `Terminated` to `Active`.

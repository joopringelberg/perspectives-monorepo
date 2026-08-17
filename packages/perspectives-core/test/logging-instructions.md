# Logging in Layer 3 Test Scaffolds

This note explains how to configure logging in synchronization tests with #sym:setTopicLogLevel and what is applied automatically.

## Hard-coded baseline log levels

Two places set log levels before your suite or test-specific settings run:

1. PDR startup in #sym:startPDRInstance and #sym:startPDRInstanceFromSnapshot sets:
- `RESOURCE` to `Error`

2. #sym:withTwoPDRsCachedNoBus sets for both PDRs:
- `TEST` to `Trace`
- `BROKER` to `Trace`

These are defaults:
* The first set of defaults applies to everything in setting up a PDR.
* The second set of defaults applies to connecting over AMQP.

Suite and test configuration can override them.

## Suite-level logging

Use `setupLogConfiguration` in your `SynchronisationModelConfiguration` as the suite baseline.

This baseline is applied once at suite setup time for both PDRs.

Important: the scaffold saves each PDR log config before suite setup and restores both configs after the suite run ends.

## Per-test logging

In each `ModelTest`, `logConfiguration` is optional:
- `Nothing`: no test-specific override; suite configuration stays in effect.
- `Just config`: temporary override for that test only.

Important: the scaffold saves each PDR log config before running a test and restores both configs immediately after that test finishes (also when a test fails).

## Recommended usage pattern

1. Put stable defaults in `setupLogConfiguration`.
2. Set most tests to `logConfiguration: Nothing`.
3. Use `Just ...` only for noisy or hard-to-debug tests.
4. Remove temporary per-test overrides when debugging is done.

## Minimal examples

Suite baseline:

```purescript
setupLogConfiguration:
  { pdrA:
      [ { topic: TEST, logLevel: Trace }
      , { topic: STATE, logLevel: Trace }
      ]
  , pdrB:
      [ { topic: TEST, logLevel: Trace } ]
  }
```

Test that inherits suite rules:

```purescript
{ testContextTypeName: myTest, logConfiguration: Nothing }
```

Test with temporary override:

```purescript
{ testContextTypeName: myHardTest
, logConfiguration: Just
    { pdrA: [ { topic: SYNC, logLevel: Trace } ]
    , pdrB: [ { topic: BROKER, logLevel: Trace } ]
    }
}
```

## Bracketed logging levels

For temporary log-level escalation inside helper functions, use a bracketed pattern:

1. Save the current log config.
2. Set temporary topic level(s).
3. Run the action.
4. Restore the saved config in the bracket finalizer.

This guarantees cleanup even when the action throws.

The module #sym:Test.Test.PDRInstance.SubscribePDRtoAMQP now uses this pattern in:
- #sym:subscribePDRtoAMQP
- #sym:unsubscribePDRfromAMQP

Minimal shape:

```purescript
withBracketedTopicLogLevel pdr topicLevels action =
    bracket
        (do
            oldConfig <- runInPDR pdr getLogConfig
            runInPDR pdr do
                for_ topicLevels \{ topic, logLevel } ->
                    setTopicLogLevel topic logLevel
            pure oldConfig
        )
        (\oldConfig -> runInPDR pdr $ setLogConfig oldConfig)
        (\_ -> action)

-- Example call:
withBracketedTopicLogLevel pdr
    [ { topic: BROKER, logLevel: Trace }
    , { topic: TEST, logLevel: Debug }
    ]
    someAffAction
```

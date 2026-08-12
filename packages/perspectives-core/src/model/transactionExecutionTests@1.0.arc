domain model://joopringelberg.nl#TransactionExecutionTests@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#TransactionExecutionTests
  use sensor for model://perspectives.domains#Sensor
  use cdb for model://perspectives.domains#Couchdb

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Transaction Execution Tests App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:TransactionExecutionTestsApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:TransactionExecutionTestsApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestApp
    indexed mm:TransactionExecutionTestsApp
    aspect sys:RootContext
    external
    
    user Manager = sys:Me
      perspective on Tests
        only (CreateAndFill, RemoveContext)
      perspective on Tests >> binding >> context >> Tester
        only (Create, Fill)
          
    -- To execute any test, run the action RunTest in the first PDR.
    -- To check if a test has succeeded, retrieve the value of TestSucceeded in the second PDR.
    context Tests (relational) filledBy Test

  case Test
    -- The automatic actions are contextualised in their specialisations,
    -- meaning that specialisation of Tester is created.
    on entry 
      do for Initializer
        bind me to Tester

    external
      property TestName (String)
      property TestSucceeded (Boolean)

    
    user Initializer = me
      perspective on Tester
        only (Create, Fill)

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      perspective on extern
        props (TestName, TestSucceeded) verbs (SetPropertyValue, Consult)

  ------------------------------------------------------------------------------
  ---- T01 - Context creation with no automatic actions
  ------------------------------------------------------------------------------
  case T01
    aspect mm:Test

    external
      state Success = context >> ( (exists Embedded1 >> binding) and Embedded1 >> binding >> context >> isInState EmbeddedContext1 )
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
      perspective on Embedded1
        only (CreateAndFill)
      action RunTest
        TestName = "T01 - Context creation with no automatic actions" for extern
        create context EmbeddedContext1 bound to Embedded1

    context Embedded1 filledBy EmbeddedContext1

    case EmbeddedContext1

  ------------------------------------------------------------------------------
  ---- T02 — Role creation with no automatic actions
  ------------------------------------------------------------------------------
  case T02
    aspect mm:Test

    external
      state Success = context >> ( (exists Role1) and Role1 >> isInState Role1 )
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
      perspective on Role1
        only (Create)
      action RunTest
        TestName = "T02 - Role creation with no automatic actions" for extern
        create role Role1

    thing Role1

  ------------------------------------------------------------------------------
  ---- T03 — Context creation triggers on entry property assignment
  ------------------------------------------------------------------------------
  case T03
    aspect mm:Test

    external
      state Success = context >> EmbeddedT03 >> binding >> P
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on EmbeddedT03
        only (CreateAndFill)
      action RunTest
        TestName = "T03 - Context creation triggers on entry property assignment" for extern
        create context EmbeddedContextT03 bound to EmbeddedT03

    context EmbeddedT03 filledBy EmbeddedContextT03

    case EmbeddedContextT03
      on entry
        do for Tester
          P = true for extern
      external
        property P (Boolean)
      user Tester = me
        perspective on extern
          props (P) verbs (SetPropertyValue, Consult)
  
  ------------------------------------------------------------------------------
  ---- T04 — Role creation triggers on entry property assignment
  ------------------------------------------------------------------------------
  case T04
    aspect mm:Test

    external
      state Success = context >> RoleT04 >> Q
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      
      perspective on RoleT04
        only (Create)
        props (Q) verbs (SetPropertyValue, Consult)
      
      action RunTest
        TestName = "T04 - Role creation triggers on entry property assignment" for extern
        create role RoleT04

    thing RoleT04
      property Q (Boolean)
      on entry
        do for Tester
          Q = true

  ------------------------------------------------------------------------------
  ---- T05 — Role removal triggers on exit property assignment (no cascade)
  ------------------------------------------------------------------------------
  case T05
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          create role RoleT05
          create role Recorder
    external
      property TestFinished (Boolean)
      state TestSucceeded = context >> Recorder >> R
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on RoleT05
        only (Create, Remove)
      perspective on Recorder
        only (Create)
        props (R) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "T05 - Role removal triggers on exit property assignment (no cascade)" for extern
        remove role RoleT05

    thing RoleT05
      on exit
        do for Tester
          R = true for context >> Recorder

    thing Recorder
      property R (Boolean)

  ------------------------------------------------------------------------------
  ---- T06 — Context creation cascades: on entry creates another context
  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
  ---- T07 — Role creation cascades: on entry creates another role
  ------------------------------------------------------------------------------
  case T07
    aspect mm:Test

    external
      state Success = context >> ((exists TriggerT07) and ((exists ResultT07) and ResultT07 >> Q))
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      
      perspective on TriggerT07
        only (Create)
      
      perspective on ResultT07
        only (Create)
        props (Q) verbs (SetPropertyValue, Consult)

      action RunTest
        TestName = "T07 - Role creation cascades: on entry creates another role" for extern
        create role TriggerT07

    thing TriggerT07
      on entry
        do for Tester
          create role ResultT07

    thing ResultT07
      property Q (Boolean)
      on entry
        do for Tester
          Q = true

  ------------------------------------------------------------------------------
  ---- T08 — Role exit cascades: on exit creates a new role
  ------------------------------------------------------------------------------
  case T08
    aspect mm:Test

    external
      state Success = (exists TestName) and (exists context >> Archive)
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester

      perspective on Ephemeral
        only (Create, Remove)

      perspective on Archive
        only (Create)

      action RunTest
        TestName = "T08 - Role exit cascades: on exit creates a new role" for extern
        create role Ephemeral
        remove role Ephemeral

    thing Ephemeral
      on exit
        do for Tester
          create role Archive
          TestSucceeded = true for context >> extern

    thing Archive

  ------------------------------------------------------------------------------
  ---- T08a — Exit-to-entry cascade chain: on exit creates role that on entry creates another role
  ------------------------------------------------------------------------------
  case T08a
    aspect mm:Test

    external
      state Success = (exists TestName) and (exists context >> FinalT09)
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester

      perspective on TriggerT09
        only (Create, Remove)

      perspective on IntermediateT09
        only (Create)

      perspective on FinalT09
        only (Create)

      action RunTest
        TestName = "T08a - Exit-to-entry cascade chain" for extern
        create role TriggerT09
        remove role TriggerT09

    thing TriggerT09
      on exit
        do for Tester
          create role IntermediateT09

    thing IntermediateT09
      on entry
        do for Tester
          create role FinalT09

    thing FinalT09

  ------------------------------------------------------------------------------
  ---- T09 — Context removal cascades through multiple roles
  ------------------------------------------------------------------------------
  case T09
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          create context EmbeddedContextT09 bound to EmbeddedT09

    external
      property ARemoved (Boolean)
      property BRemoved (Boolean)
      -- The test on existence of TestName is to prevent double evaluation of state Success
      -- in the same transaction. To protect us from infinite loops, a state is evaluated only once per transaction.
      state Success = (exists TestName) and ARemoved and BRemoved
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on EmbeddedT09
        only (CreateAndFill, RemoveContext)
      action RunTest
        TestName = "T09 - Context removal cascades through multiple roles" for extern
        remove context EmbeddedT09

    context EmbeddedT09 filledBy EmbeddedContextT09

    case EmbeddedContextT09
      on entry
        do for Tester
          create role A
          create role B

      user Tester = me
        perspective on EnclosingT09
          props (ARemoved, BRemoved) verbs (SetPropertyValue, Consult)
        perspective on A
          only (Create, Remove)
        perspective on B
          only (Create, Remove)
      
      context EnclosingT09 = extern >> binder EmbeddedT09 >> context >> extern
      
      thing A
        on exit
          do for Tester
            ARemoved = true for context >> EnclosingT09
      thing B
        on exit
          do for Tester
            BRemoved = true for context >> EnclosingT09
  
  ------------------------------------------------------------------------------
  ---- T10 — Role removal deferred
  ------------------------------------------------------------------------------
  case T10
    aspect mm:Test
    state TesterAvailable = exists Tester
      on entry
        do for Tester
          create role RoleT10
          create role SecondRoleT10
    
    external
      property SecondRoleT10Fired (Boolean)
      property ThirdRoleRemoved (Boolean)
      state TestSucceeded = SecondRoleT10Fired and ThirdRoleRemoved
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester

      perspective on extern
        props (SecondRoleT10Fired, ThirdRoleRemoved) verbs (SetPropertyValue, Consult)

      perspective on RoleT10
        only (Create)

      perspective on SecondRoleT10
        only (Create, Remove)
      
      perspective on ThirdRoleT10
        only (Create, Remove)
      
      action RunTest
        TestName = "T10 - Role removal deferred" for extern
        create role ThirdRoleT10 

    thing RoleT10
      state TestStarted = (exists context >> ThirdRoleT10)
        on entry
          do for Tester
            remove role context >> ThirdRoleT10
            ThirdRoleRemoved = true for context >> extern

    thing SecondRoleT10
      -- This state can only be entered after state RoleT10$TestStarted has been entered.
      state Role10Fired = context >> extern >> ThirdRoleRemoved
        -- Subsequently, state TestStarted can only be entered if the removal of ThirdRoleT10 has been deferred.
        state TestStarted = (exists context >> ThirdRoleT10)
          on entry
            do for Tester
              SecondRoleT10Fired = true for context >> extern
    
    thing ThirdRoleT10

  ------------------------------------------------------------------------------
  ---- T11 — ExecuteDestructiveEffect deferred until phase-1 base case (SKIPPED)
  ------------------------------------------------------------------------------
  
    ------------------------------------------------------------------------------
  ---- T12 — Full context removal lifecycle
  ------------------------------------------------------------------------------
  case T12
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          create context EmbeddedContextOfT12 bound to RoleOfT12
          Counter = 0 for extern
    external
      property Counter (Number)
      -- This state condition will be checked immediately after creation and found to be true.
      -- It will become false again when EmbeddedContextOfT12 is created.
      --
      -- When the test is run, we start a fresh transaction.
      -- Removing RoleOfT12 touches this state condition again. 
      -- But RoleOfT12 will be marked untouchable on removal.
      -- That causes step 2.1 to add this state to the deferred evaluation queue.
      -- Finally, step 2.5 re-evaluates this state after physical removal 
      -- If the test succeeds, physical removal has occurred (step 2.4), which is the condition for the test to succeed.
      state TestSucceeded = (not exists context >> RoleOfT12) 
        state CounterIncremented = Counter == 1
          on entry
            do for Tester
              TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (Counter) verbs (SetPropertyValue, Consult)
      perspective on RoleOfT12
        only (CreateAndFill, RemoveContext)
      action RunTest
        TestName = "T12 — Full context removal lifecycle" for extern
        remove context RoleOfT12

    context RoleOfT12 filledBy EmbeddedContextOfT12

    case EmbeddedContextOfT12
      on exit
        do for Tester
          -- Marks exit in step 1.6.
          Counter = ExternOfT12 >> Counter + 1 for ExternOfT12

      thing ExternOfT12 (functional) = extern >> binder RoleOfT12 >> context >> extern

      user Tester = me
        perspective on ExternOfT12
          props (Counter) verbs (SetPropertyValue, Consult)

  ------------------------------------------------------------------------------
  ---- T13 — Property change causes state entry in phase 2
  ------------------------------------------------------------------------------
  case T13
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          create role RoleOfT13
          POfT13 = true for extern
    external
      property POfT13 (Boolean)
      state TestState = POfT13
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on RoleOfT13
        only (Create)
      perspective on extern
        props (TestSucceeded, POfT13) verbs (SetPropertyValue, Consult)
      action RunTest
        POfT13 = false for extern
        TestName = "T13 — Property change causes state entry in phase 2" for extern

    thing RoleOfT13

  ------------------------------------------------------------------------------
  ---- T14 — Property change causes state entry in phase 2
  ------------------------------------------------------------------------------
  case T14
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          create role RoleOfT14
          POfT14 = true for extern
    external
      property POfT14 (Boolean)
      state TestState = POfT14
        on exit
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on RoleOfT14
        only (Create)
      perspective on extern
        props (TestSucceeded, POfT14) verbs (SetPropertyValue, Consult)
      action RunTest
        POfT14 = false for extern
        TestName = "T14 — Property change causes state entry in phase 2" for extern

    thing RoleOfT14

  ------------------------------------------------------------------------------
  ---- T15 — Multiple inverted-query results deduplicated in phase 2
  ------------------------------------------------------------------------------
  case T15
    aspect mm:Test
    state TesterAvailable = exists Tester
      on entry
        do for Tester
          create role RecorderOfT15
          EntryCountOfT15 = 0 for RecorderOfT15

    -- R1 and R2 both occur in this single state condition.
    -- Creating both in one transaction should still evaluate this state once.
    state BothRolesPresent = (exists Role1OfT15) and (exists Role2OfT15)
      on entry
        do for Tester
          letA
            recorder <- RecorderOfT15
          in
            EntryCountOfT15 = recorder >> EntryCountOfT15 + 1 for recorder

    external
      state Success = context >> RecorderOfT15 >> EntryCountOfT15 == 1
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on TriggerOfT15
        only (Create)
      perspective on Role1OfT15
        only (Create)
      perspective on Role2OfT15
        only (Create)
      perspective on RecorderOfT15
        only (Create)
        props (EntryCountOfT15) verbs (SetPropertyValue, Consult)
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "T15 — Multiple inverted-query results deduplicated in phase 2" for extern
        create role TriggerOfT15

    thing TriggerOfT15
      on entry
        do for Tester
          create role Role1OfT15
          create role Role2OfT15

    thing Role1OfT15

    thing Role2OfT15

    thing RecorderOfT15
      property EntryCountOfT15 (Number)

  ------------------------------------------------------------------------------
  ---- T16 — State evaluation in phase 2 creates a new context
  ------------------------------------------------------------------------------
  case T16
    aspect mm:Test
    state TesterAvailable = exists Tester
      on entry
        do for Tester
          create role SignalOfT16
          GoOfT16 = false for SignalOfT16

    -- Entering this state happens in phase 2 when GoOfT16 flips to true.
    -- The on entry action creates a context, which should trigger step 2.2
    -- and therefore a new phase-1 pass for the created context.
    state TriggerInnerCreation = SignalOfT16 >> GoOfT16
      on entry
        do for Tester
          create context InnerContextT16 bound to InnerOfT16
    
    external
      state Success = context >> InnerOfT16 >> DoneOfT16
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on SignalOfT16
        only (Create)
        props (GoOfT16) verbs (SetPropertyValue, Consult)
      perspective on InnerOfT16
        only (CreateAndFill)
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "T16 — State evaluation in phase 2 creates a new context" for extern
        GoOfT16 = true for SignalOfT16

    thing SignalOfT16
      property GoOfT16 (Boolean)

    context InnerOfT16 filledBy InnerContextT16

    case InnerContextT16
      on entry
        do for Tester
          -- If we do not lift this into the next transaction, state Success will not be 
          -- entered because it already has been evaluated in the current transaction. The test will fail.
          after 200 Milliseconds
            DoneOfT16 = true for extern

      external
        property DoneOfT16 (Boolean)

      user Tester = me
        perspective on extern
          props (DoneOfT16) verbs (SetPropertyValue, Consult)

  ------------------------------------------------------------------------------
  ---- T17 — State condition on untouchable role is postponed until removal
  ------------------------------------------------------------------------------
  case T17
    aspect mm:Test
    state TesterAvailable = exists Tester
      on entry
        do for Tester
          create role GuardOfT17
          create role WatcherOfT17

      -- Having state GuardAbsent as a substate of TesterAvailable ensures 
      -- that its first evaluation is only AFTER GuardOfT17 has been created.
      -- Otherwise, it would be evaluated immediately after creating T17, which is before GuardOfT17 has been created.
      -- However, because Tester is then not yet available, nothing happens! This is a bit precarious.
      -- Removing GuardOfT17 first marks it untouchable.
      -- This condition should be postponed in phase 2 and re-evaluated
      -- after physical removal in step 2.5.
      state GuardAbsent = not exists GuardOfT17
        on entry
          do for Tester
            letA
              watcher <- WatcherOfT17
            in
              SafeOfT17 = true for watcher

    external
      state Success = context >> WatcherOfT17 >> SafeOfT17
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on GuardOfT17
        only (Create, Remove)
      perspective on WatcherOfT17
        only (Create)
        props (SafeOfT17) verbs (SetPropertyValue, Consult)
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "T17 — State condition on untouchable role is postponed until removal" for extern
        remove role GuardOfT17

    thing GuardOfT17

    thing WatcherOfT17
      property SafeOfT17 (Boolean)

  ------------------------------------------------------------------------------
  ---- T18 — State condition on untouchable context is postponed until removal
  ------------------------------------------------------------------------------
  case T18
    aspect mm:Test
    state TesterAvailable = exists Tester
      on entry
        do for Tester
          create context ChildContextT18 bound to ChildOfT18
          create role ObserverOfT18

      -- Same deferral pattern as T17, but now for a context role.
      -- Removing ChildOfT18 marks its filled context untouchable first;
      -- this condition should be postponed in phase 2 and re-evaluated
      -- after physical removal in step 2.5.
      state ChildAbsent = not exists ChildOfT18
        on entry
          do for Tester
            letA
              observer <- ObserverOfT18
            in
              ChildGoneOfT18 = true for observer

    external
      state Success = context >> ObserverOfT18 >> ChildGoneOfT18
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on ChildOfT18
        only (CreateAndFill, RemoveContext)
      perspective on ObserverOfT18
        only (Create)
        props (ChildGoneOfT18) verbs (SetPropertyValue, Consult)
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "T18 — State condition on untouchable context is postponed until removal" for extern
        remove context ChildOfT18

    context ChildOfT18 filledBy ChildContextT18

    case ChildContextT18

    thing ObserverOfT18
      property ChildGoneOfT18 (Boolean)

  ------------------------------------------------------------------------------
  ---- T19 — Same state is not entered twice in a single transaction
  ------------------------------------------------------------------------------
  case T19
    aspect mm:Test
    state TesterAvailable = exists Tester
      on entry
        do for Tester
          create role RecorderOfT19
          EntryCountOfT19 = 0 for RecorderOfT19

    external
      state Success = context >> RecorderOfT19 >> EntryCountOfT19 == 1
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on COfT19
        only (CreateAndFill, RemoveContext)
      perspective on RecorderOfT19
        only (Create)
        props (EntryCountOfT19) verbs (SetPropertyValue, Consult)
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "T19 — Same state is not entered twice in a single transaction" for extern
        create context CContextT19 bound to COfT19

    thing RecorderOfT19
      property EntryCountOfT19 (Number)

    context COfT19 filledBy CContextT19

    case CContextT19
      
      external
        state ReEnterIncreaseEntryCount = InternalTriggerOfT19
          on entry
            do for Tester
              -- This will again make the condition of IncreaseEntryCount true, but the state IncreaseEntryCount will not be entered again in this transaction.
              InternalTriggerOfT19 = false

        -- Closed world: property doesn't exist, condition evaluates to true.
        -- State is entered immediately on creation.
        state IncreaseEntryCount = not context >> extern >> InternalTriggerOfT19
          on entry
            do for Tester
              -- Increase counter.
              EntryCountOfT19 = context >> RecorderInT19 >> EntryCountOfT19 + 1 for context >> RecorderInT19
              -- Exit state IncreaseEntryCount. This triggers a chain of actions that will eventually 
              -- lead to re-evaluation and would re-enter this state, but that will be blocked.
              InternalTriggerOfT19 = true

        property EntryCountOfT19 (Boolean)
        property InternalTriggerOfT19 (Boolean)

      thing RecorderInT19 (functional) = extern >> binder COfT19 >> context >> RecorderOfT19

      user Tester = me
        perspective on RecorderInT19
          props (EntryCountOfT19) verbs (SetPropertyValue, Consult)
        perspective on extern
          props (InternalTriggerOfT19) verbs (SetPropertyValue, Consult)

  ------------------------------------------------------------------------------
  ---- T20 — State re-entry after role exit and re-create in the same transaction
  ------------------------------------------------------------------------------
  case T20
    aspect mm:Test
    state TesterAvailable = exists Tester
      on entry
        do for Tester
          create role RecorderOfT20
          Cycles = 0 for RecorderOfT20
          ExitedOnce = false for RecorderOfT20
          create role ROfT20

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on RecorderOfT20
        only (Create)
        props (Cycles, ExitedOnce) verbs (SetPropertyValue, Consult)
      perspective on ROfT20
        only (Create, Remove)
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "T20 — State re-entry after role exit and re-create in the same transaction" for extern
        -- The destructive remove will be postponed. 
        -- Subsequently, we cannot recreate because the role is functional.
        remove role ROfT20
        create role ROfT20

    thing RecorderOfT20
      property Cycles (Number)
      property ExitedOnce (Boolean)

    -- We must make this a relational role, otherwise the re-creation of ROfT20 will not be possible.
    -- This is because removing the instance will be postponed and if the role is functional, the re-creation will be blocked.
    -- This does not compromise the validity of the test.
    thing ROfT20 (relational)
      state ReEntered = context >> RecorderOfT20 >> (ExitedOnce and Cycles == 1)
        on entry
          do for Tester
            TestSucceeded = true for context >> extern
      on entry
        do for Tester
          Cycles = context >> RecorderOfT20 >> Cycles + 1 for context >> RecorderOfT20
      on exit
        do for Tester
          Cycles = context >> RecorderOfT20 >> Cycles - 1 for context >> RecorderOfT20
          ExitedOnce = true for context >> RecorderOfT20

  ------------------------------------------------------------------------------
  ---- T21 — CWH: `not exists X` sees same-transaction creation
  ------------------------------------------------------------------------------
  case T21
    aspect mm:Test

    external
      state Success =
        (exists context >> COfT21 >> binding >> context >> WatcherOfT21)
        and
        (not context >> COfT21 >> binding >> context >> WatcherOfT21 >> NoXOfT21)
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on COfT21
        only (CreateAndFill)
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "T21 — CWH: `not exists X` sees same-transaction creation" for extern
        create context CContextT21 bound to COfT21

    context COfT21 filledBy CContextT21

    case CContextT21
      on entry
        do for Tester
          create role WatcherOfT21
          create role XOfT21

      -- This state must be evaluated in phase 2 against the current world,
      -- where XOfT21 already exists because it is created in the same transaction.
      -- We therefore expect this state to be false, and the property NoXOfT21 to be not set (so its negation evaluates to false).
      state NoXStateOfT21 = not exists XOfT21
        on entry
          do for Tester
            NoXOfT21 = true for WatcherOfT21

      thing XOfT21

      thing WatcherOfT21
        property NoXOfT21 (Boolean)

      user Tester = me
        perspective on WatcherOfT21
          only (Create)
          props (NoXOfT21) verbs (SetPropertyValue, Consult)
        perspective on XOfT21
          only (Create)

  ------------------------------------------------------------------------------
  ---- T22 — CWH: `not exists X` enters when X is absent
  ------------------------------------------------------------------------------
  case T22
    aspect mm:Test

    external
      state Success = context >> COfT22 >> binding >> context >> WatcherOfT22 >> NoXOfT22
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on COfT22
        only (CreateAndFill)
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "T22 — CWH: `not exists X` enters when X is absent" for extern
        create context CContextT22 bound to COfT22

    context COfT22 filledBy CContextT22

    case CContextT22
      on entry
        do for Tester
          create role WatcherOfT22

      -- Complement of T21: XOfT22 is absent in this transaction, so this
      -- closed-world condition should evaluate true in phase 2.
      state NoXStateOfT22 = not exists XOfT22
        on entry
          do for Tester
            NoXOfT22 = true for WatcherOfT22

      thing XOfT22

      thing WatcherOfT22
        property NoXOfT22 (Boolean)

      user Tester = me
        perspective on WatcherOfT22
          only (Create)
          props (NoXOfT22) verbs (SetPropertyValue, Consult)
        perspective on XOfT22
          only (Create)

  ------------------------------------------------------------------------------
  ---- Unbind re-ordering in a single automatic action
  ------------------------------------------------------------------------------
  case T23
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          letA
            filler <- create role Filler
            filledrole <- create role FilledRole
            recorder <- create role Recorder
          in
            bind_ filler to filledrole
    external
      property TestFinished (Boolean)
      state TestSucceeded = context >> Recorder >> R == 2
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on Filler
        only (Create, Remove)
        props (StartValue) verbs (SetPropertyValue, Consult)
      perspective on FilledRole
        only (Create, Remove, Fill, RemoveFiller)
        props (StartValue) verbs (Consult)
      perspective on Recorder
        only (Create)
        props (R) verbs (SetPropertyValue, Consult)
      action RunTest
        remove filler of FilledRole
        TestName = "T23 — Monotone simulation: destructive op follows constructive ops" for extern
        -- If actually unbinding happens before the property is read, then R does not have a value. If unbinding is postponed until after the property is read, then R will be 1 + 1 = 2.
        R = 1 + FilledRole >> StartValue for Recorder

    thing FilledRole filledBy Filler

    thing Filler
      property StartValue (Number)
      on entry
        do for Tester
          StartValue = 1

    thing Recorder
      property R (Number)

  ------------------------------------------------------------------------------
  ---- Remove re-ordering in a single automatic action
  ------------------------------------------------------------------------------
  case RoleRemoveReordering
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          create role RoleT05
          create role Recorder
    external
      property TestFinished (Boolean)
      state TestSucceeded = context >> Recorder >> R == 2
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on RoleT05
        only (Create, Remove)
        props (StartValue) verbs (SetPropertyValue, Consult)
      perspective on Recorder
        only (Create)
        props (R) verbs (SetPropertyValue, Consult)
      action RunTest
        remove role RoleT05
        TestName = "Role removal must be postponed to the last" for extern
        -- If actually removing happens before the property is read, then R does not have a value. If removing is postponed until after the property is read, then R will be 1 + 1 = 2.
        R = 1 + RoleT05 >> StartValue for Recorder

    thing RoleT05
      property StartValue (Number)
      on entry
        do for Tester
          StartValue = 1

    thing Recorder
      property R (Number)

  ------------------------------------------------------------------------------
  ---- T24 — Peer transaction: own-user reaction distributed via embedded sharing transaction
  ---- T25 — Peer transaction: own-user reaction triggers further state cascade
  ---- These tests require two PDRs. They have been moved to the model "model://joopringelberg.nl#SynchronisationTestModel@2.0".
  ---- Run Test.ConstructiveSynchronisationTest to test them.
  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
  ---- T26 — Concurrent transactions are serialised
  ------------------------------------------------------------------------------
  case T26
    aspect mm:Test
    external
      property Trigger (Boolean)
      property FirstActionDone (Boolean)
      property SecondActionDone (Boolean)
      property FirstActionCompleted (Boolean)
      property SecondActionCompleted (Boolean)
      -- Both actions are lifted out of this transaction and executed in separate transactions. 
      state RunFirstAction = Trigger
        on entry
          do for Tester after 100 Milliseconds
            -- Unconditionally set FirstActionDone to true.
            FirstActionDone = true
        -- Only when the second action has completed, complete the first action.
        state SecondActionDone = SecondActionDone and not FirstActionCompleted
          on entry
            do for Tester
              FirstActionCompleted = true

      state RunSecondAction = Trigger
        on entry
          do for Tester after 100 Milliseconds
            SecondActionDone = true
        -- Only when the first action has completed, complete the second action.
        state FirstActionDone = FirstActionDone and not SecondActionCompleted
          on entry
            do for Tester
              SecondActionCompleted = true

      state TestSucceeded = (FirstActionCompleted and not SecondActionCompleted) or (SecondActionCompleted and not FirstActionCompleted)
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded, Trigger, FirstActionDone, SecondActionDone, FirstActionCompleted, SecondActionCompleted) verbs (SetPropertyValue, Consult)
      action RunTest
        Trigger = true for extern
        TestName = "T26 — Concurrent transactions are serialised" for extern

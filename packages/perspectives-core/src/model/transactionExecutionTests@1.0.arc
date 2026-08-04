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

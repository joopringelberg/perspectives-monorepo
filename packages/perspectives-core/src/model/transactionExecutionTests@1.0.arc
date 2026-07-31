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
  case T06
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          create context EmbeddedContextT06 bound to EmbeddedT06

    external
      -- The test on existence of TestName is to prevent double evaluation of state Success
      -- in the same transaction. To protect us from infinite loops, a state is evaluated only once per transaction.
      state Success = (exists TestName) and context >> EmbeddedT06 >> binding >> P
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on EmbeddedT06
        only (CreateAndFill)
      action RunTest
        TestName = "T06 - Context creation cascades: on entry creates another context" for extern

    context EmbeddedT06 filledBy EmbeddedContextT06

    case EmbeddedContextT06
      on entry
        do for Tester
          P = true for extern
      external
        property P (Boolean)
      user Tester = me
        perspective on extern
          props (P) verbs (SetPropertyValue, Consult)
  
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
      state Success = exists context >> Archive
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester

      perspective on Ephemeral
        only (Remove)

      perspective on Archive
        only (Create)
        props (Timestamp) verbs (SetPropertyValue, Consult)

      action RunTest
        TestName = "T08 - Role exit cascades: on exit creates a new role" for extern
        remove role Ephemeral

    thing Ephemeral
      on exit
        do for Tester
          letA
            archive <- create role Archive
          in
            Timestamp = callExternal sensor:ReadSensor( "clock", "now" ) returns DateTime for archive

    thing Archive
      property Timestamp (DateTime)

domain model://joopringelberg.nl#SinglePDRDestructiveTests@2.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#SinglePDRDestructiveTests
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
          Name = "Single PDR Destructive Tests App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:SinglePDRDestructiveTestApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:SinglePDRDestructiveTestApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestApp
    indexed mm:SinglePDRDestructiveTestApp
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
  ---- TESTS. All these tests first create something.
  ---- Then they destroy it. This should trigger a state change, which is checked.
  ---- The TestName describes the destructive operation.
  ------------------------------------------------------------------------------
  
  ------------------------------------------------------------------------------
  ---- Remove a role.
  ------------------------------------------------------------------------------
  case Test_RemoveRole
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          create role TestRole1
    external
      property TestFinished (Boolean)
      state TestFinished = TestFinished
        on entry
          -- Moves the role removal out of this transaction.
          -- Because setting TestFinished also triggers state evaluation, the removal of the role 
          -- would not trigger a state change in the same transaction. So we delay the removal of the role.
          do for Tester
            after 200 Milliseconds remove role context >> TestRole1
        state TestSucceeded = not exists context >> TestRole1
          on entry
            do for Tester
              TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      perspective on TestRole1
        only (Create, Remove)
      action RunTest
        TestName = "Remove a role" for extern
        TestFinished = true for extern

    thing TestRole1

  ------------------------------------------------------------------------------
  ---- Remove one role instance from two instances of the same role type.
  ------------------------------------------------------------------------------
  case Test_RemoveOneRoleInstance
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          letA
            tr1 <- create role TestRole2
            tr2 <- create role TestRole2
          in
            -- This is to make sure that the two role instances are different.
            P = 1 for tr1
            P = 2 for tr2
    external
      property TestFinished (Boolean)
      state TestFinished = TestFinished
        on entry
          -- Moves the role removal out of this transaction.
          -- Because setting TestFinished also triggers state evaluation, the removal of the role 
          -- would not trigger a state change in the same transaction. So we delay the removal of the role.
          do for Tester
            after 200 Milliseconds remove role filter context >> TestRole2 with P == 1
        state TestSucceeded = context >> TestRole2 >>= count == 1 and context >> TestRole2 >> P >>= first == 2
          on entry
            do for Tester
              TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      perspective on TestRole2
        only (Create, Remove)
        props (P) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "Remove one role instance from two instances of the same role type" for extern
        TestFinished = true for extern

    thing TestRole2 (relational)
      property P (Number)

  ------------------------------------------------------------------------------
  ---- Delete two role instances at once.
  ------------------------------------------------------------------------------
  case Test_DeleteTwoRoles
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          create role TestRole3
          create role TestRole3
    external
      property TestFinished (Boolean)
      state TestFinished = TestFinished
        on entry
          -- Moves the role removal out of this transaction.
          -- Because setting TestFinished also triggers state evaluation, the removal of the role 
          -- would not trigger a state change in the same transaction. So we delay the removal of the role.
          do for Tester
            after 200 Milliseconds delete role TestRole3 from context
        state TestSucceeded = not exists context >> TestRole3
          on entry
            do for Tester
              TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      perspective on TestRole3
        only (Create, Delete)
      action RunTest
        TestName = "Delete two role instances at once" for extern
        TestFinished = true for extern

    thing TestRole3 (relational)

  ------------------------------------------------------------------------------
  ---- Delete a property value.
  ------------------------------------------------------------------------------
  case Test_DeleteProperty
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          letA
            tr <- create role TestRole4
          in
            P = 1 for tr
            P =+ 2 for tr
    external
      state TestSucceeded = not exists context >> TestRole4 >> P
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on TestRole4
        only (Create)
        props (P) verbs (Consult, SetPropertyValue, DeleteProperty, AddPropertyValue)
      action RunTest
        TestName = "Delete a property value" for extern
        delete property P from TestRole4

    thing TestRole4
      property P (Number)

  ------------------------------------------------------------------------------
  ---- Remove a property value.
  ------------------------------------------------------------------------------
  case Test_RemoveProperty
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          letA
            tr <- create role TestRole5
          in
            P = 1 for tr
    external
      state TestSucceeded = not exists context >> TestRole5 >> P
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on TestRole5
        only (Create)
        props (P) verbs (Consult, SetPropertyValue, RemovePropertyValue)
      action RunTest
        TestName = "Remove a property value" for extern
        P =- 1 for TestRole5

    thing TestRole5
      property P (Number)

  ------------------------------------------------------------------------------
  ---- Remove one property value while one value remains.
  ------------------------------------------------------------------------------
  case Test_RemoveOnePropertyValue
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          letA
            tr <- create role TestRole6
          in
            P = 1 for tr
            P =+ 2 for tr
    external
      state TestSucceeded = context >> TestRole6 >> P >>= count == 1
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on TestRole6
        only (Create)
        props (P) verbs (Consult, SetPropertyValue, RemovePropertyValue, AddPropertyValue)
      action RunTest
        TestName = "Remove one property value while one value remains" for extern
        P =- 1 for TestRole6

    thing TestRole6
      property P (Number)

  ------------------------------------------------------------------------------
  ---- Remove a role filler (starting from the filled role)
  ------------------------------------------------------------------------------
  case Test_RemoveFiller
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          letA
            tr <- create role Filler1
          in
            bind tr to TestRole7

    external
      property TestFinished (Boolean)
      state TestFinished = TestFinished
        on entry
          -- Moves the role removal out of this transaction.
          -- Because setting TestFinished also triggers state evaluation, the removal of the role 
          -- would not trigger a state change in the same transaction. So we delay the removal of the role.
          do for Tester
            after 200 Milliseconds remove filler of context >> TestRole7
        state TestSucceeded = context >> ((exists TestRole7) and (not exists TestRole7 >> binding) and exists Filler1)
          on entry
            do for Tester
              TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      perspective on TestRole7
        only (Create, RemoveFiller, Fill)
      perspective on Filler1
        only (Create)
      action RunTest
        TestName = "Remove the filler from a role" for extern
        TestFinished = true for extern

    thing TestRole7 filledBy Filler1
    thing Filler1

  ------------------------------------------------------------------------------
  ---- Remove a role as filler (starting from the filler)
  ------------------------------------------------------------------------------
  case Test_RemoveRoleFiller
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          letA
            tr <- create role Filler2
          in
            bind tr to TestRole9

    external
      property TestFinished (Boolean)
      state TestFinished = TestFinished
        on entry
          -- Moves the role removal out of this transaction.
          -- Because setting TestFinished also triggers state evaluation, the removal of the role 
          -- would not trigger a state change in the same transaction. So we delay the removal of the role.
          do for Tester
            after 200 Milliseconds remove as filler context >> Filler2
        state TestSucceeded = context >> ((exists TestRole9) and (not exists TestRole9 >> binding) and exists Filler2)
          on entry
            do for Tester
              TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      perspective on TestRole9
        only (Create, RemoveFiller, Fill)
      perspective on Filler2
        only (Create, Remove)
      action RunTest
        TestName = "Remove a role as filler" for extern
        TestFinished = true for extern

    thing TestRole9 filledBy Filler2
    thing Filler2

  ------------------------------------------------------------------------------
  ---- Remove a role as filler (starting from the filler, from specific role types only)
  ------------------------------------------------------------------------------
  case Test_RemoveRoleFiller_SpecificRoleTypes
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          letA
            tr <- create role Filler3
          in
            bind tr to TestRole10
            bind tr to TestRole11

    external
      property TestFinished (Boolean)
      state TestFinished = TestFinished
        on entry
          -- Moves the role removal out of this transaction.
          -- Because setting TestFinished also triggers state evaluation, the removal of the role 
          -- would not trigger a state change in the same transaction. So we delay the removal of the role.
          do for Tester
            after 200 Milliseconds remove as filler of TestRole11 context >> Filler3 
        state TestSucceeded = context >> ((exists TestRole10) and (not exists TestRole11 >> binding) and (exists Filler3) and exists TestRole10 >> binding)
          on entry
            do for Tester
              TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      perspective on TestRole10
        only (Create, RemoveFiller, Fill)
      perspective on TestRole11
        only (Create, RemoveFiller, Fill)
      perspective on Filler3
        only (Create, Remove)
      action RunTest
        TestName = "Remove a role as filler from specific role types" for extern
        TestFinished = true for extern

    thing TestRole10 filledBy Filler3
    thing TestRole11 filledBy Filler3
    thing Filler3

  ------------------------------------------------------------------------------
  ---- Break the fill link between specific instances
  ------------------------------------------------------------------------------
  case Test_UnBindRoleFiller_SpecificRoleTypes
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          letA
            tr <- create role Filler4
            tr2 <- create role Filler5
          in
            bind tr to TestRole12
            bind tr2 to TestRole13

    external
      property TestFinished (Boolean)
      state TestFinished = TestFinished
        on entry
          -- Moves the role removal out of this transaction.
          -- Because setting TestFinished also triggers state evaluation, the removal of the role 
          -- would not trigger a state change in the same transaction. So we delay the removal of the role.
          do for Tester
            after 200 Milliseconds remove filler context >> Filler4 from context >> TestRole12
        state TestSucceeded = context >> ((exists TestRole12) and (not exists TestRole12 >> binding) and (exists Filler4) and exists TestRole13 >> binding)
          on entry
            do for Tester
              TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      perspective on TestRole12
        only (Create, RemoveFiller, Fill)
      perspective on TestRole13
        only (Create, RemoveFiller, Fill)
      perspective on Filler4
        only (Create, Remove)
      perspective on Filler5
        only (Create, Remove)
      action RunTest
        TestName = "Remove a specific role instance from another instance" for extern
        TestFinished = true for extern

    thing TestRole12 filledBy Filler4
    thing TestRole13 filledBy Filler5
    thing Filler4
    thing Filler5

  ------------------------------------------------------------------------------
  ---- Remove a context without roles.
  ------------------------------------------------------------------------------
  case Test_RemoveContextWithoutRoles
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          create context EmbeddedContext bound to TestRole8
    external
      property TestFinished (Boolean)
      state TestFinished = TestFinished -- and exists callExternal cdb:ContextInstances( "model://joopringelberg.nl#SinglePDRDestructiveTests$Test_RemoveContextWithoutRoles$EmbeddedContext" ) returns mm:Test_RemoveContextWithoutRoles$EmbeddedContext
        on entry
          -- Moves the context removal out of this transaction.
          -- Because setting TestFinished also triggers state evaluation, the removal of the context 
          -- would not trigger a state change in the same transaction. So we delay the removal of the context.
          do for Tester
            after 200 Milliseconds remove context (context >> TestRole8)

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      perspective on TestRole8
        only (CreateAndFill, RemoveContext)
      action RunTest
        TestName = "Remove a context without roles" for extern
        TestFinished = true for extern

    context TestRole8 filledBy EmbeddedContext

    case EmbeddedContext
      on exit
        do for Tester
          TestSucceeded = true for ExternOfTest
      user Tester = me
        perspective on ExternOfTest
          props (TestSucceeded) verbs (SetPropertyValue, Consult)

      thing ExternOfTest = extern >> binder TestRole8 >> context >> extern

  ------------------------------------------------------------------------------
  ---- Remove a context with an unfilled role.
  ------------------------------------------------------------------------------
  case Test_RemoveContextWithUnfilledRole
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          create context EmbeddedContext2 bound to TestRole14
    external
      property ContextExited (Boolean)
      property RoleExited (Boolean)
      property TestFinished (Boolean)
      state TestSucceeded = ContextExited and RoleExited
        on entry
          do for Tester
            TestSucceeded = true

      state TestFinished = TestFinished
        on entry
          -- Moves the context removal out of this transaction.
          -- Because setting ContextExited also triggers state evaluation, the removal of the context 
          -- would not trigger a state change in the same transaction. So we delay the removal of the context.
          do for Tester
            after 200 Milliseconds remove context (context >> TestRole14)

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      perspective on TestRole14
        only (CreateAndFill, RemoveContext)
      action RunTest
        TestName = "Remove a context with an unfilled role" for extern
        TestFinished = true for extern

    context TestRole14 filledBy EmbeddedContext2

    case EmbeddedContext2
      on entry
        do for Tester
          create role RoleOfContext1
      on exit
        do for Tester
          ContextExited = true for ExternOfTest2
      user Tester = me
        perspective on ExternOfTest2
          props (ContextExited, RoleExited) verbs (SetPropertyValue, Consult)
        perspective on RoleOfContext1
          only (Create)
      thing RoleOfContext1
        on exit
          do for Tester
            RoleExited = true for context >> ExternOfTest2

      thing ExternOfTest2 = extern >> binder TestRole14 >> context >> extern

  ------------------------------------------------------------------------------
  ---- Remove a context with a filled role.
  ------------------------------------------------------------------------------
  case Test_RemoveContextWithFilledRole
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          create context EmbeddedContext3 bound to TestRole15

    external
      property TestExecuted (Boolean)
      property ContextExited (Boolean)
      property RoleExited (Boolean)
      property FillerExited (Boolean)
      state TestSucceeded = ContextExited and RoleExited and FillerExited
        on entry
          do for Tester
            TestSucceeded = true

      state TestExecuted = TestExecuted
        on entry
          -- Moves the context removal out of this transaction.
          -- Because setting TestExecuted also triggers state evaluation, the removal of the context 
          -- would not trigger a state change in the same transaction. So we delay the removal of the context.
          do for Tester
            after 200 Milliseconds remove context (context >> TestRole15)

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestExecuted) verbs (SetPropertyValue, Consult)
      perspective on TestRole15
        only (CreateAndFill, RemoveContext)
      action RunTest
        TestName = "Remove a context with a filled role" for extern
        TestExecuted = true for extern

    context TestRole15 filledBy EmbeddedContext3

    case EmbeddedContext3
      on entry
        do for Tester
          letA
            filler <- create role FillerOfContext2
          in
            bind filler to RoleOfContext2
      on exit
        do for Tester
          ContextExited = true for ExternOfTest3
      user Tester = me
        perspective on ExternOfTest3
          props (ContextExited, RoleExited, FillerExited) verbs (SetPropertyValue, Consult)
        perspective on RoleOfContext2
          only (Create, Fill)
        perspective on FillerOfContext2
          only (Create)
      
      thing RoleOfContext2 filledBy FillerOfContext2
        on exit
          do for Tester
            RoleExited = true for context >> ExternOfTest3

      thing ExternOfTest3 = extern >> binder TestRole15 >> context >> extern

      thing FillerOfContext2
        on exit
          do for Tester
            FillerExited = true for context >> ExternOfTest3

  ------------------------------------------------------------------------------
  ---- Remove a context with a filled role that should remain.
  ------------------------------------------------------------------------------
  case Test_RemoveContextWithFilledRoleThatShouldRemain
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          create role FillerOfContext3
          create context EmbeddedContext4 bound to TestRole16

    external
      property TestExecuted (Boolean)
      property ContextExited (Boolean)
      property RoleExited (Boolean)
      property FillerExited (Boolean)
      state TestSucceeded = ContextExited and RoleExited and exists context >> FillerOfContext3
        on entry
          do for Tester
            TestSucceeded = true

      state TestExecuted = TestExecuted
        on entry
          -- Moves the context removal out of this transaction.
          -- Because setting TestExecuted also triggers state evaluation, the removal of the context 
          -- would not trigger a state change in the same transaction. So we delay the removal of the context.
          do for Tester
            after 200 Milliseconds remove context (context >> TestRole16)

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestExecuted) verbs (SetPropertyValue, Consult)
      perspective on TestRole16
        only (CreateAndFill, RemoveContext)
      perspective on FillerOfContext3
        only (Create)
      action RunTest
        TestName = "Remove a context with a filled role that should remain" for extern
        TestExecuted = true for extern

    context TestRole16 filledBy EmbeddedContext4

    thing FillerOfContext3

    case EmbeddedContext4
      on entry
        do for Tester
          bind extern >> binder TestRole16 >> context >> FillerOfContext3 >>= first to RoleOfContext3
      on exit
        do for Tester
          ContextExited = true for ExternOfTest3
      user Tester = me
        perspective on ExternOfTest3
          props (ContextExited, RoleExited, FillerExited) verbs (SetPropertyValue, Consult)
        perspective on RoleOfContext3
          only (Create, Fill)
        perspective on ExternOfTest3 >> context >> FillerOfContext3
          all roleverbs

      
      thing RoleOfContext3 filledBy FillerOfContext3
        on exit
          do for Tester
            RoleExited = true for context >> ExternOfTest3

      thing ExternOfTest3 = extern >> binder TestRole16 >> context >> extern


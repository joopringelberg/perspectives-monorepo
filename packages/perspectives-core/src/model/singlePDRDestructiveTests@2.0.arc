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
  ---- Remove a role filler
  ------------------------------------------------------------------------------
  case Test_RemoveRoleFiller
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
            after 2000 Milliseconds unbind context >> TestRole7
        state TestSucceeded = context >> ((exists TestRole7) and (not exists TestRole7 >> binding) and exists Filler1)
          on entry
            do for Tester
              TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      perspective on TestRole7
        only (Create, Remove, Fill, Unbind)
      perspective on Filler1
        only (Create)
      action RunTest
        TestName = "Remove the filler from a role" for extern
        TestFinished = true for extern

    thing TestRole7 filledBy Filler1
    thing Filler1

  ------------------------------------------------------------------------------
  ---- Remove a role as filler
  ------------------------------------------------------------------------------

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
domain model://joopringelberg.nl#TwoPDRDestructiveTests@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#TwoPDRDestructiveTests
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
          Name = "Two synchronizing PDR instances Destructive Tests App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:TwoPDRDestructiveTestsApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:TwoPDRDestructiveTestsApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestApp
    indexed mm:TwoPDRDestructiveTestsApp
    aspect sys:RootContext
    external
    
    user Manager = sys:Me
      perspective on Tests
        only (CreateAndFill, RemoveContext)
      perspective on Tests >> binding >> context >> Leader
        only (Create, Fill)
      perspective on Follower
        only (Create, Fill)
        props (FirstName) verbs (Consult)
          
    -- To execute any test, run the action RunTest in the first PDR.
    -- To check if a test has succeeded, retrieve the value of TestSucceeded in the second PDR.
    context Tests (relational) filledBy Test

    -- We need to synchronise, hence PerspectivesUsers and not Persons.
    user Follower filledBy (sys:TheWorld$PerspectivesUsers)

  case Test

    -- Why not on entry of Test, like we do for the Leader?
    -- The Test instance may not yet be bound to Tests, so AppFollower is not yet reachable.
    state AppfollowerReachable = exists extern >> binder Tests
      on entry
        do for Initializer
          -- The automatic actions are contextualised in their specialisations,
          -- meaning that specialisations of Leader and Follower are created.
          -- Dit gaat fout als we Follower met AppFollower vullen in plaats van met de vuller van AppFollower.
          bind AppFollower >> binding to Follower
          -- Follower will not have Test instances bound in TestApp$Tests, so this is a surefire way 
          -- to execute this action only for Leader.
          bind me to Leader

    external
      property TestName (String)
      property TestSucceeded (Boolean)

    user AppFollower (functional) = extern >> binder Tests >> context >> Follower
    user Initializer = me
      perspective on Leader
        only (Create, Fill)
      perspective on Follower
        only (Create, Fill)

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      perspective on extern
        props (TestName) verbs (SetPropertyValue, Consult)

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      perspective on Leader
        props (FirstName) verbs (Consult)
      perspective on extern
        props (TestName) verbs (Consult)
        props (TestSucceeded) verbs (Consult, SetPropertyValue)

  ------------------------------------------------------------------------------
  ---- TESTS. All these tests first create something.
  ---- Then they destroy it. This should trigger a state change, which is checked in the Follower's PDR.
  ---- The TestName describes the destructive operation.
  ------------------------------------------------------------------------------
  
  ------------------------------------------------------------------------------
  ---- Remove a role.
  ------------------------------------------------------------------------------
  case Test_RemoveRole
    aspect mm:Test
    state TesterAvailable = exists Leader 
      on entry
        do for Leader
          letA 
            tr <- create role TestRole1
          in
            P = 1 for tr
    external
      property TestFinished (Boolean)
      property RoleAvailable (Boolean)
      state TestFinished = TestFinished
        on entry
          -- Moves the role removal out of this transaction.
          -- Because setting TestName also triggers state evaluation, the removal of the role 
          -- would not trigger a state change in the same transaction. So we delay the removal of the role.
          do for Leader
            after 200 Milliseconds remove role context >> TestRole1
      state TestSucceeded = RoleAvailable and not exists context >> TestRole1
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      perspective on TestRole1
        only (Create, Remove)
        props (P) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "Remove a role" for extern
        TestFinished = true for extern

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on extern
        props (RoleAvailable) verbs (SetPropertyValue, Consult)
      perspective on TestRole1
        props (P) verbs (Consult)

    thing TestRole1
      on entry
        do for Follower
          RoleAvailable = true for context >> extern
      property P (Number)

  ------------------------------------------------------------------------------
  ---- Remove one role instance from two instances of the same role type.
  ------------------------------------------------------------------------------
  case Test_RemoveOneRoleInstance
    aspect mm:Test
    state TesterAvailable = exists Leader 
      on entry
        do for Leader
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
          do for Leader
            after 200 Milliseconds remove role filter context >> TestRole2 with P == 1
        state TestSucceeded = context >> TestRole2 >>= count == 1 and context >> TestRole2 >> P >>= first == 2
          on entry
            do for Follower
              TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      perspective on TestRole2
        only (Create, Remove)
        props (P) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "Remove one role instance from two instances of the same role type" for extern
        TestFinished = true for extern

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on extern
        props (TestFinished) verbs (Consult)
      perspective on TestRole2
        props (P) verbs (Consult)

    thing TestRole2 (relational)
      property P (Number)

  ------------------------------------------------------------------------------
  ---- Delete two role instances at once.
  ------------------------------------------------------------------------------
  case Test_DeleteTwoRoles
    aspect mm:Test
    state TesterAvailable = exists Leader 
      on entry
        do for Leader
          letA
            r1 <- create role TestRole3
            r2 <- create role TestRole3
          in
            P = 1 for r1
            P = 2 for r2
    external
      property TestFinished (Boolean)
      property TwoRolesReceived (Boolean)
      state TwoRolesReceived = context >> TestRole3 >>= count == 2
        on entry
          do for Follower
            TwoRolesReceived = true
      state TestFinished = TestFinished
        on entry
          -- Moves the role removal out of this transaction.
          -- Because setting TestFinished also triggers state evaluation, the removal of the role 
          -- would not trigger a state change in the same transaction. So we delay the removal of the role.
          do for Leader
            after 200 Milliseconds delete role TestRole3 from context
        state TestSucceeded = TwoRolesReceived and not exists context >> TestRole3
          on entry
            do for Follower
              TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      perspective on TestRole3
        only (Create, Delete)
        props (P) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "Delete two role instances at once" for extern
        TestFinished = true for extern

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on extern
        props (TestFinished) verbs (Consult)
        props (TwoRolesReceived) verbs (SetPropertyValue, Consult)
      perspective on TestRole3
        props (P) verbs (Consult)

    thing TestRole3 (relational)
      property P (Number)

  ------------------------------------------------------------------------------
  ---- Delete a property value.
  ------------------------------------------------------------------------------
  case Test_DeleteProperty
    aspect mm:Test
    state TesterAvailable = exists Leader 
      on entry
        do for Leader
          letA
            tr <- create role TestRole4
          in
            P = 1 for tr
            P =+ 2 for tr
    external
      state TestSucceeded = not exists context >> TestRole4 >> P
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on TestRole4
        only (Create)
        props (P) verbs (Consult, SetPropertyValue, DeleteProperty, AddPropertyValue)
      action RunTest
        TestName = "Delete a property value" for extern
        delete property P from TestRole4

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on extern
        props (TestSucceeded) verbs (Consult)

    thing TestRole4
      property P (Number)

  ------------------------------------------------------------------------------
  ---- Remove a property value.
  ------------------------------------------------------------------------------
  case Test_RemoveProperty
    aspect mm:Test
    state TesterAvailable = exists Leader 
      on entry
        do for Leader
          letA
            tr <- create role TestRole5
          in
            P = 1 for tr
    external
      state TestSucceeded = not exists context >> TestRole5 >> P
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on TestRole5
        only (Create)
        props (P) verbs (Consult, SetPropertyValue, RemovePropertyValue)
      action RunTest
        TestName = "Remove a property value" for extern
        P =- 1 for TestRole5

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on extern
        props (TestSucceeded) verbs (Consult, SetPropertyValue)
      perspective on TestRole5
        props (P) verbs (Consult)

    thing TestRole5
      property P (Number)

  ------------------------------------------------------------------------------
  ---- Remove one property value while one value remains.
  ------------------------------------------------------------------------------
  case Test_RemoveOnePropertyValue
    aspect mm:Test
    state TesterAvailable = exists Leader 
      on entry
        do for Leader
          letA
            tr <- create role TestRole6
          in
            P = 1 for tr
            P =+ 2 for tr
    external
      property TwoPropertyValuesReceived (Boolean)
      state TwoPropertyValuesReceived = context >> TestRole6 >> P >>= count == 2
        on entry
          do for Follower
            TwoPropertyValuesReceived = true
      state TestSucceeded = TwoPropertyValuesReceived and context >> TestRole6 >> P >>= count == 1
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on TestRole6
        only (Create)
        props (P) verbs (Consult, SetPropertyValue, RemovePropertyValue, AddPropertyValue)
      action RunTest
        TestName = "Remove one property value while one value remains" for extern
        P =- 1 for TestRole6

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on extern
        props (TestSucceeded, TwoPropertyValuesReceived) verbs (Consult, SetPropertyValue)
      perspective on TestRole6
        props (P) verbs (Consult)

    thing TestRole6
      property P (Number)

  ------------------------------------------------------------------------------
  ---- Remove a role filler (starting from the filled role)
  ------------------------------------------------------------------------------
  case Test_RemoveFiller
    aspect mm:Test
    state TesterAvailable = exists Leader 
      on entry
        do for Leader
          letA
            tr <- create role Filler1
          in
            bind tr to TestRole7
            P = 1 for tr

    external
      property TestFinished (Boolean)
      property FillerExists (Boolean)
      state TestFinished = TestFinished
        on entry
          -- Moves the role removal out of this transaction.
          -- Because setting TestFinished also triggers state evaluation, the removal of the role 
          -- would not trigger a state change in the same transaction. So we delay the removal of the role.
          do for Leader
            after 200 Milliseconds remove filler of context >> TestRole7
      state TestSucceeded = FillerExists and context >> ((exists TestRole7) and (not exists TestRole7 >> binding) and exists Filler1)
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      perspective on TestRole7
        only (Create, RemoveFiller, Fill)
      perspective on Filler1
        only (Create)
        props (P) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "Remove the filler from a role" for extern
        TestFinished = true for extern

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on TestRole7
        props (P) verbs (Consult)
      perspective on extern
        props (FillerExists) verbs (SetPropertyValue, Consult)

    thing TestRole7 filledBy Filler1
      state FillerExists = exists binding
        on entry
          do for Follower
            FillerExists = true for context >> extern
    thing Filler1
      property P (Number)

  ------------------------------------------------------------------------------
  ---- Remove a role as filler (starting from the filler)
  ------------------------------------------------------------------------------
  case Test_RemoveRoleFiller
    aspect mm:Test
    state TesterAvailable = exists Leader 
      on entry
        do for Leader
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
          do for Leader
            after 200 Milliseconds remove as filler context >> Filler2
        state TestSucceeded = context >> ((exists TestRole9) and (not exists TestRole9 >> binding) and exists Filler2)
          on entry
            do for Follower
              TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      perspective on TestRole9
        only (Create, RemoveFiller, Fill)
      perspective on Filler2
        only (Create, Remove)
      action RunTest
        TestName = "Remove a role as filler" for extern
        TestFinished = true for extern

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on extern
        props (TestSucceeded) verbs (Consult)
      perspective on TestRole9
      perspective on Filler2

    thing TestRole9 filledBy Filler2
    thing Filler2

  ------------------------------------------------------------------------------
  ---- Remove a role as filler (starting from the filler, from specific role types only)
  ------------------------------------------------------------------------------
  case Test_RemoveRoleFiller_SpecificRoleTypes
    aspect mm:Test
    state TesterAvailable = exists Leader 
      on entry
        do for Leader
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
          do for Leader
            after 200 Milliseconds remove as filler of TestRole11 context >> Filler3 
        state TestSucceeded = context >> ((exists TestRole10) and (not exists TestRole11 >> binding) and (exists Filler3) and exists TestRole10 >> binding)
          on entry
            do for Follower
              TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
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

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on extern
        props (TestSucceeded) verbs (Consult)
      perspective on TestRole10
      perspective on TestRole11
      perspective on Filler3

    thing TestRole10 filledBy Filler3
    thing TestRole11 filledBy Filler3
    thing Filler3

  ------------------------------------------------------------------------------
  ---- Break the fill link between specific instances
  ------------------------------------------------------------------------------
  case Test_UnBindRoleFiller_SpecificRoleTypes
    aspect mm:Test
    state TesterAvailable = exists Leader 
      on entry
        do for Leader
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
          do for Leader
            after 200 Milliseconds remove filler context >> Filler4 from context >> TestRole12
        state TestSucceeded = context >> ((exists TestRole12) and (not exists TestRole12 >> binding) and (exists Filler4) and exists TestRole13 >> binding)
          on entry
            do for Follower
              TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
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

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on extern
        props (TestSucceeded) verbs (Consult)
      perspective on TestRole12
      perspective on TestRole13
      perspective on Filler4
      perspective on Filler5

    thing TestRole12 filledBy Filler4
    thing TestRole13 filledBy Filler5
    thing Filler4
    thing Filler5

  ------------------------------------------------------------------------------
  ---- Remove a context without roles.
  ------------------------------------------------------------------------------
  case Test_RemoveContextWithoutRoles
    aspect mm:Test
    state TesterAvailable = exists Leader 
      on entry
        do for Leader
          create context EmbeddedContext bound to TestRole8
    external
      property TestFinished (Boolean)
      state TestFinished = TestFinished -- and exists callExternal cdb:ContextInstances( "model://joopringelberg.nl#TwoPDRDestructiveTests$Test_RemoveContextWithoutRoles$EmbeddedContext" ) returns mm:Test_RemoveContextWithoutRoles$EmbeddedContext
        on entry
          -- Moves the context removal out of this transaction.
          -- Because setting TestFinished also triggers state evaluation, the removal of the context 
          -- would not trigger a state change in the same transaction. So we delay the removal of the context.
          do for Leader
            after 200 Milliseconds remove context (context >> TestRole8)

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      perspective on TestRole8
        only (CreateAndFill, RemoveContext)
      action RunTest
        TestName = "Remove a context without roles" for extern
        TestFinished = true for extern

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on extern
        props (TestSucceeded) verbs (Consult)
      perspective on TestRole8

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
    state TesterAvailable = exists Leader 
      on entry
        do for Leader
          create context EmbeddedContext2 bound to TestRole14
    external
      property ContextExited (Boolean)
      property RoleExited (Boolean)
      property TestFinished (Boolean)
      state TestSucceeded = ContextExited and RoleExited
        on entry
          do for Follower
            TestSucceeded = true

      state TestFinished = TestFinished
        on entry
          -- Moves the context removal out of this transaction.
          -- Because setting ContextExited also triggers state evaluation, the removal of the context 
          -- would not trigger a state change in the same transaction. So we delay the removal of the context.
          do for Leader
            after 200 Milliseconds remove context (context >> TestRole14)

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      perspective on TestRole14
        only (CreateAndFill, RemoveContext)
      action RunTest
        TestName = "Remove a context with an unfilled role" for extern
        TestFinished = true for extern

    context TestRole14 filledBy EmbeddedContext2

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on extern
        props (TestSucceeded) verbs (Consult)
      perspective on TestRole14

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
      
      user Follower = extern >> binder TestRole14 >> context >> Follower
        perspective on RoleOfContext1
          
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
    state TesterAvailable = exists Leader 
      on entry
        do for Leader
          create context EmbeddedContext3 bound to TestRole15

    external
      property TestExecuted (Boolean)
      property ContextExited (Boolean)
      property RoleExited (Boolean)
      property FillerExited (Boolean)
      state TestSucceeded = ContextExited and RoleExited and FillerExited
        on entry
          do for Follower
            TestSucceeded = true

      state TestExecuted = TestExecuted
        on entry
          -- Moves the context removal out of this transaction.
          -- Because setting TestExecuted also triggers state evaluation, the removal of the context 
          -- would not trigger a state change in the same transaction. So we delay the removal of the context.
          do for Leader
            after 200 Milliseconds remove context (context >> TestRole15)

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on extern
        props (TestExecuted) verbs (SetPropertyValue, Consult)
      perspective on TestRole15
        only (CreateAndFill, RemoveContext)
      action RunTest
        TestName = "Remove a context with a filled role" for extern
        TestExecuted = true for extern

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on extern
        props (TestSucceeded, ContextExited, RoleExited, FillerExited) verbs (Consult)
      perspective on TestRole15

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
      user Tester = extern >> binder TestRole15 >> context >> Follower
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
    state TesterAvailable = exists Leader 
      on entry
        do for Leader
          create role FillerOfContext3
          create context EmbeddedContext4 bound to TestRole16

    external
      property TestExecuted (Boolean)
      property ContextExited (Boolean)
      property RoleExited (Boolean)
      property FillerExited (Boolean)
      state TestSucceeded = ContextExited and RoleExited and exists context >> FillerOfContext3
        on entry
          do for Follower
            TestSucceeded = true

      state TestExecuted = TestExecuted
        on entry
          -- Moves the context removal out of this transaction.
          -- Because setting TestExecuted also triggers state evaluation, the removal of the context 
          -- would not trigger a state change in the same transaction. So we delay the removal of the context.
          do for Leader
            after 200 Milliseconds remove context (context >> TestRole16)

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on extern
        props (TestExecuted) verbs (SetPropertyValue, Consult)
      perspective on TestRole16
        only (CreateAndFill, RemoveContext)
      perspective on FillerOfContext3
        only (Create)
      action RunTest
        TestName = "Remove a context with a filled role that should remain" for extern
        TestExecuted = true for extern

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on extern
        props (TestSucceeded) verbs (Consult)
      perspective on TestRole16
      perspective on FillerOfContext3

    context TestRole16 filledBy EmbeddedContext4

    thing FillerOfContext3

    case EmbeddedContext4
      on entry
        do for Tester
          bind extern >> binder TestRole16 >> context >> FillerOfContext3 >>= first to RoleOfContext3
      on exit
        do for Follower
          ContextExited = true for ExternOfTest3
      user Tester = me
        perspective on RoleOfContext3
          only (Create, Fill)
      
      user Follower = extern >> binder TestRole16 >> context >> Follower
        perspective on ExternOfTest3
          props (ContextExited, RoleExited, FillerExited) verbs (SetPropertyValue, Consult)
        perspective on RoleOfContext3
          only (Create, Fill)
        perspective on ExternOfTest3 >> context >> FillerOfContext3
          all roleverbs

      
      thing RoleOfContext3 filledBy FillerOfContext3
        on exit
          do for Follower
            RoleExited = true for context >> ExternOfTest3

      thing ExternOfTest3 = extern >> binder TestRole16 >> context >> extern


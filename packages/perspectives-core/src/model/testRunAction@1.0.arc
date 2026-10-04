domain model://joopringelberg.nl#TestRunAction@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#TestRunAction

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
          Name = "Test RunAction" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:TestRunAction >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:TestRunAction >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestApp
    indexed mm:TestRunAction
    aspect sys:RootContext
    external
    
    user Manager = sys:Me
      perspective on Tests
        only (CreateAndFill, RemoveContext)
          
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
  ---- TWO CONTEXT ACTIONS FOR THE SAME USER
  ------------------------------------------------------------------------------
  case TwoContextActionsForSameUser
    aspect mm:Test
    external
      property Action1Executed (Boolean)
      property Action2Executed (Boolean)

      state TestSuccess = Action1Executed and Action2Executed
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester

      perspective on extern
        props (Action1Executed, Action2Executed) verbs (SetPropertyValue, Consult)

      action Action1
        Action1Executed = true for extern
      
      action Action2
        Action2Executed = true for extern

      action RunTest
        TestName = "Two context actions for the same user" for extern
        runContextAction Action1 for Tester in origin
        runContextAction Action2 for Tester in origin

  ------------------------------------------------------------------------------
  ---- TWO ROLE ACTIONS FOR THE SAME USER
  ------------------------------------------------------------------------------
  case TwoRoleActionsForSameUser
    aspect mm:Test
    external

      state TestSuccess = context >> AnotherRole >> (Action1Executed and Action2Executed)
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester

      perspective on AnotherRole
        only (Create)
        props (Action1Executed, Action2Executed) verbs (SetPropertyValue, Consult)
        action Action1
          Action1Executed = true
        
        action Action2
          Action2Executed = true

      action RunTest
        TestName = "Two role actions for the same user" for extern
        create role AnotherRole in origin
        runRoleAction Action1 for Tester on AnotherRole in origin
        runRoleAction Action2 for Tester on AnotherRole in origin
    
    thing AnotherRole
      property Action1Executed (Boolean)
      property Action2Executed (Boolean)

  ------------------------------------------------------------------------------
  ---- ROLE ACTION FOR MULTIPLE OBJECTS
  ------------------------------------------------------------------------------
  case RoleActionForMultipleObjects
    aspect mm:Test
    external

      state TestSuccess = context >> AnotherRole >> ActionExecuted >>= count == 2
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester

      perspective on AnotherRole
        only (Create)
        props (ActionExecuted) verbs (SetPropertyValue, Consult)
        action Action1
          ActionExecuted = true

      action RunTest
        letA
          role1 <- create role AnotherRole in origin
          role2 <- create role AnotherRole in origin
        in
          TestName = "Role action for multiple objects" for extern
          runRoleAction Action1 for Tester on AnotherRole in origin

    thing AnotherRole (relational)
      property ActionExecuted (Boolean)

  ------------------------------------------------------------------------------
  ---- TWO CONTEXT ACTIONS FOR DIFFERENT USERS
  ------------------------------------------------------------------------------
  case TwoContextActionsForDifferentUsers
    aspect mm:Test
    external
      property Action1Executed (Boolean)
      property Action2Executed (Boolean)

      state TestSuccess = Action1Executed and Action2Executed
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester

      perspective on extern
        props (Action1Executed) verbs (SetPropertyValue, Consult)

      action Action1
        Action1Executed = true for extern
    
      action RunTest
        TestName = "Two context actions for different users" for extern
        runContextAction Action1 for Tester in origin
        runContextAction Action2 for Tester2 in origin    
    
    user Tester2 = me
      
      perspective on extern
        props (Action2Executed) verbs (SetPropertyValue, Consult)

      action Action2
        Action2Executed = true for extern

  ------------------------------------------------------------------------------
  ---- TWO ACTIONS FOR THE SAME USER WITH ONCE SETTLED CLAUSES
  ------------------------------------------------------------------------------
  case TwoContextActionsWithOnceSettledClausesForSameUser
    aspect mm:Test
    external
      property Text1 (String)

      state TestSuccess = Text1 == "Action1 executed(settled)Action2 executed"
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester

      perspective on extern
        props (Text1) verbs (SetPropertyValue, Consult)

      action Action1
        Text1 = "Action1 executed" for extern

        -- This demonstrates that all phases of a called transaction are settled 
        -- before the next called action runs.
        once settled
          Text1 = extern >> Text1 + "(settled)" for extern
      
      action Action2
        Text1 = extern >> Text1 + "Action2 executed" for extern

      action RunTest
        TestName = "Two context actions with once settled clauses for the same user" for extern
        runContextAction Action1 for Tester in origin
        runContextAction Action2 for Tester in origin

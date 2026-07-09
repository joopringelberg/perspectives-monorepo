domain model://joopringelberg.nl#StateTestModel@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#StateTestModel

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
          Name = "Test State App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:TestStateApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:TestStateApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestApp
    indexed mm:TestStateApp
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
        props (TestName) verbs (SetPropertyValue, Consult)

  ------------------------------------------------------------------------------
  ---- TESTS. All these tests modify something and then check whether a state condition that depends on a particular query step becomes true.
  ---- The test name mentions that tested step in the ORIGINAL query.
  ---- The TestName repeats the condition.
  ---- Notice that the modification step is separate from the crucial query step that is tested. 
  ---- Also notice that a query tests all of its steps at once.
  ---- These tests are not about triggering, but about testing all different types of steps in a query.
  ---- Each test introduces at most one new kind of step.
  ------------------------------------------------------------------------------
  
  ------------------------------------------------------------------------------
  ---- Test a context state query that tests the existence of a role in the context.
  ---- The crucial assignment is to create the role.
  ------------------------------------------------------------------------------
  case Test_ContextState_RoleStep
    aspect mm:Test
    state TestState = exists TestRole1
      on entry
        do for Tester
          TestSucceeded = true for extern

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on TestRole1
        only (Create)
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        create role TestRole1
        TestName = "(exists TestRole1) - role step" for extern

    thing TestRole1

  ------------------------------------------------------------------------------
  ---- Test a role state query that tests the existence of a role in the context.
  ---- The crucial assignment is to create the role.
  ------------------------------------------------------------------------------
  case Test_RoleState_RoleStep
    aspect mm:Test
    external
      state TestState = exists context >> TestRole1
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on TestRole1
        only (Create)
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        create role TestRole1
        TestName = "(exists context >> TestRole1) - context step" for extern

    thing TestRole1

  ------------------------------------------------------------------------------
  ---- Test the property step.
  ---- The crucial assignment is to create the role.
  ------------------------------------------------------------------------------
  case Test_RoleState_PropertyStep
    aspect mm:Test
    external
      state TestState = context >> TestRole1 >> P1
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on TestRole1
        only (Create)
        props (P1) verbs (SetPropertyValue, Consult)
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        letA
          r <- create role TestRole1
        in
          P1 = true for r
          TestName = "(context >> TestRole1 >>> P1) - property step" for extern

    thing TestRole1
      property P1 (Boolean)

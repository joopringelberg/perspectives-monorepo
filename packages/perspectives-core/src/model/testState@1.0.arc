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
  case Test_RoleState_ContextStep
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

  ------------------------------------------------------------------------------
  ---- Test the extern step (DataTypeGetter ExternalRoleF).
  ------------------------------------------------------------------------------
  case Test_RoleState_ExternStep
    aspect mm:Test
    external
      state TestState = exists context >> extern
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "(exists context >> extern) - extern step" for extern

  ------------------------------------------------------------------------------
  ---- Test the binding step (DataTypeGetter FillerF).
  ------------------------------------------------------------------------------
  case Test_RoleState_BindingStep
    aspect mm:Test
    external
      state TestState = exists context >> Tester >> binding
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "(exists context >> Tester >> binding) - binding step" for extern

  ------------------------------------------------------------------------------
  ---- Test the identity step (DataTypeGetter IdentityF).
  ------------------------------------------------------------------------------
  case Test_RoleState_IdentityStep
    aspect mm:Test
    external
      state TestState = exists this
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "(exists this) - identity step" for extern

  ------------------------------------------------------------------------------
  ---- Test the modelname step (DataTypeGetter ModelNameF).
  ------------------------------------------------------------------------------
  case Test_RoleState_ModelNameStep
    aspect mm:Test
    external
      state TestState = (exists context >> Tester) and modelname >>= first == "model://joopringelberg.nl#u01vncbjik"
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "(modelname == model URI) - modelname step" for extern

  ------------------------------------------------------------------------------
  ---- Test the me step (DataTypeGetter MeF).
  ------------------------------------------------------------------------------
  case Test_RoleState_MeStep
    aspect mm:Test
    external
      state TestState = exists me
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "(exists me) - me step" for extern

  ------------------------------------------------------------------------------
  ---- Test the roleType step (TypeGetter TypeOfRoleF).
  ------------------------------------------------------------------------------
  case Test_RoleState_TypeOfRoleStep
    aspect mm:Test
    external
      state TestState = exists context >> Tester >> roleType
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "(exists context >> Tester >> roleType) - roleType step" for extern

  ------------------------------------------------------------------------------
  ---- Test the roleTypes step (TypeGetter RoleTypesF).
  ------------------------------------------------------------------------------
  case Test_RoleState_RoleTypesStep
    aspect mm:Test
    external
      state TestState = exists context >> contextType >> roleTypes
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "(exists context >> contextType >> roleTypes) - roleTypes step" for extern

  ------------------------------------------------------------------------------
  ---- Test the specialisesRoleType step (DataTypeGetterWithParameter).
  ------------------------------------------------------------------------------
  case Test_RoleState_SpecialisesRoleTypeStep
    aspect mm:Test
    external
      state TestState = context >> Tester >> roleType >> specialisesRoleType mm:Test$Tester
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "(context >> Tester >> roleType >> specialisesRoleType mm:Test$Tester) - step" for extern

  ------------------------------------------------------------------------------
  ---- Test unary not.
  ------------------------------------------------------------------------------
  case Test_RoleState_NotStep
    aspect mm:Test
    external
      state TestState = not false
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "(not false) - unary not step" for extern

  ------------------------------------------------------------------------------
  ---- Test unary available.
  ------------------------------------------------------------------------------
  case Test_RoleState_AvailableStep
    aspect mm:Test
    external
      state TestState = available context
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "(available context) - unary available step" for extern

  ------------------------------------------------------------------------------
  ---- Test comparisons and arithmetic binary combinators.
  ------------------------------------------------------------------------------
  case Test_RoleState_ComparisonStep
    aspect mm:Test
    external
      state TestState = (2 == 1 + 1) and (4 - 1 == 3) and (2 * 3 == 6) and (8 / 2 == 4) and (1 < 2) and (2 <= 2) and (3 > 2) and (3 >= 3) and (1 /= 2)
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "(arithmetic/comparison binary steps) - binary combinators" for extern

  ------------------------------------------------------------------------------
  ---- Test binary filledBy.
  ------------------------------------------------------------------------------
  case Test_RoleState_FilledByBinaryStep
    aspect mm:Test
    external
      state TestState = (context >> Tester) filledBy me
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "((context >> Tester) filledBy me) - binary filledBy step" for extern

  ------------------------------------------------------------------------------
  ---- Test binary fills.
  ------------------------------------------------------------------------------
  case Test_RoleState_FillsBinaryStep
    aspect mm:Test
    external
      state TestState = me fills (context >> Tester)
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "(me fills (context >> Tester)) - binary fills step" for extern

  ------------------------------------------------------------------------------
  ---- Test sequence/count composition (ComposeSequenceF with CountF).
  ------------------------------------------------------------------------------
  case Test_RoleState_CountSequenceStep
    aspect mm:Test
    external
      state TestState = (context >> Tester >>= count) == 1
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "((context >> Tester >>= count) == 1) - sequence/count step" for extern

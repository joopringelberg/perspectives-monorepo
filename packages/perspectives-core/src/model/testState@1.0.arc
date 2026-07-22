domain model://joopringelberg.nl#StateTestModel@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#StateTestModel
  use sensor for model://perspectives.domains#Sensor

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

  ------------------------------------------------------------------------------
  ---- roleType and translate steps
  ------------------------------------------------------------------------------
  case Test_RoleType_TranslateStep
    aspect mm:Test
    external
      -- Explanation: an External type is always mapped to the context type and then its translation.
      -- However, we have no translations for this model, so we end up with the Stable type identifier of the context.
      state TestState = roleType >> translate == "model://joopringelberg.nl#u01vncbjik$s72m2cv38v"
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "(roleType >> translate) - roleType and translate step" for extern

  ------------------------------------------------------------------------------
  ---- Test the binder/filled step (FilledF).
  ---- The binder step finds roles of a given type that are filled by me.
  ---- Since me fills Tester in this context, binder Tester returns the Tester instance.
  ------------------------------------------------------------------------------
  case Test_RoleState_FilledStep
    aspect mm:Test
    external
      state TestState = exists context >> me >> binder mm:Test_RoleState_FilledStep$Tester
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "(exists context >> me >> binder Tester) - binder/filled step" for extern

  ------------------------------------------------------------------------------
  ---- Test the indexedName step (IndexedContextName and IndexedRoleName).
  ---- sys:MySystem is an indexed context; sys:Me is an indexed role.
  ---- Both have indexed names so exists on both returns true.
  ------------------------------------------------------------------------------
  case Test_RoleState_IndexedNameStep
    aspect mm:Test
    external
      state TestState = (exists sys:MySystem >> indexedName) and (exists sys:Me >> indexedName)
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "((exists sys:MySystem >> indexedName) and (exists sys:Me >> indexedName)) - indexedName step" for extern

  ------------------------------------------------------------------------------
  ---- Test the roleType individual step ([role ...]).
  ---- Compares the runtime roleType of the external role with its own type constant.
  ---- Verifies that readable identifiers are correctly normalized to stable identifiers.
  ------------------------------------------------------------------------------
  case Test_RoleType_RoleTypeIndividualStep
    aspect mm:Test
    external
      state TestState = roleType == [role Test_RoleType_RoleTypeIndividualStep$External]
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "(roleType == [role Test_RoleType_RoleTypeIndividualStep$External]) - roleType individual step" for extern

  ------------------------------------------------------------------------------
  ---- Test the context type individual step ([context ...]).
  ---- Compares the contextType of the test context with its own type constant.
  ---- Verifies that readable identifiers are correctly normalized to stable identifiers.
  ------------------------------------------------------------------------------
  case Test_ContextType_ContextTypeIndividualStep
    aspect mm:Test
    external
      state TestState = context >> contextType == [context Test_ContextType_ContextTypeIndividualStep]
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "(context >> contextType == [context Test_ContextType_ContextTypeIndividualStep]) - context type individual step" for extern

  ------------------------------------------------------------------------------
  ---- Test the isInState step.
  ---- RunTest creates a TestRole1 instance and sets P1=true, activating TestRole1State.
  ---- The external state condition then checks isInState on that role.
  ------------------------------------------------------------------------------
  case Test_RoleState_IsInStateStep
    aspect mm:Test
    external
      state TestState = context >> TestRole1 >> isInState TestRole1State
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
          TestName = "(context >> TestRole1 >> isInState TestRole1State) - isInState step" for extern

    thing TestRole1
      state TestRole1State = exists P1
      property P1 (Boolean)

  ------------------------------------------------------------------------------
  ---- Test the regexp/matches step.
  ---- The expression "deze expressie" matches regexp "^deze.*" is always true.
  ------------------------------------------------------------------------------
  case Test_RoleState_RegExStep
    aspect mm:Test
    external
      state TestState = "deze expressie" matches regexp "^deze.*"
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "(deze expressie matches regexp ^deze.*) - regexp step" for extern

  ------------------------------------------------------------------------------
  ---- Test the variable (letE) step.
  ---- Binds TestRole1 instances to v, then reads boolean property P1 from v.
  ---- State is true when any TestRole1 has P1=true.
  ------------------------------------------------------------------------------
  case Test_RoleState_VariableStep
    aspect mm:Test
    external
      state TestState = 
        letE 
          v <- context >> TestRole1 
        in 
          v >> P1
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
          TestName = "(letE v = context >> TestRole1 in v >> P1) - variable step" for extern

    thing TestRole1
      property P1 (Boolean)

  case Test_FilledBy_Step
    aspect mm:Test
    external
      state TestState = exists context >> mm:Test_FilledBy_Step$Tester filledBy me
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "exists context mm:Test_FilledBy_Step$Tester filledBy me - filledBy step" for extern

  case Test_Fills_Step
    aspect mm:Test
    external
      state TestState = me fills (context >> mm:Test_Fills_Step$Tester)
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "me fills (context >> mm:Test_Fills_Step$Tester) - fills step" for extern
  
  case Test_Duration_Step
    aspect mm:Test
    external
      property Today = callExternal sensor:ReadSensor("clock", "now") returns DateTime
      state TestState = 
        letE
            today <- Today
            tomorrow <- today + 1 day
            nextweek <- today + 7 days
          in
            tomorrow == (((nextweek - 5 days) - 23 hours) - 59 minutes) - 60 seconds
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "Tomorrow == (((NextWeek - 5 days) - 23 hours) - 59 minutes) - 60 seconds) - duration step" for extern
  
  case Test_RoleIndividual_Step
    aspect mm:Test
    external
      property ThisRoleName = callExternal util:RoleIdentifier() returns String
      state TestState = (roleinstance (mm:Test_RoleIndividual_Step$External) ThisRoleName) == this
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "((roleinstance (mm:Test_RoleIndividual_Step$External) ThisRoleName) == this) - role individual step" for extern
  
  case Test_ContextIndividual_Step
    aspect mm:Test
    external
      property ThisContextName = context >> callExternal util:ContextIdentifier() returns String
      state TestState = (contextinstance (mm:Test_ContextIndividual_Step) ThisContextName) == this >> context
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "((contextinstance (mm:Test_ContextIndividual_Step) ThisContextName) == this >> context) - context individual step" for extern
  
  case Test_FillFrom_Step
    aspect mm:Test
    external
      state TestState = context >> Selection >>= count == 1
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      perspective on Role1
        only (Create)
      perspective on Role2
        only (Create)
      action RunTest
        TestName = "(context >> selectFrom BothRoles just mm:Test_FillFrom_Step$Role1 >>= count == 1) - fillFrom test step" for extern
        create role Role1
        create role Role2
    
    thing Role1

    thing Role2

    thing BothRoles = Role1 union Role2

    thing Selection = selectFrom BothRoles just Role1 in Test_FillFrom_Step

  case Test_Filter_Step
    aspect mm:Test
    external
      state TestState = (context >> filter Role3 with N == 1) >>= count == 1
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      perspective on Role3
        only (Create)
        props (N) verbs (SetPropertyValue, Consult)
      action RunTest
        letA
          one <- create role Role3
          two <- create role Role3
        in
          TestName = "(context >> filter Role3 with N == 1 >>= count == 1) - filter test step" for extern
          N = 1 for one
          N = 2 for two
    
    thing Role3
      property N (Number)

  case Test_Union_Step
    aspect mm:Test
    external
      state TestState = context >> (Role4A union Role4B) >>= count == 2
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      perspective on Role4A
        only (Create)
      perspective on Role4B
        only (Create)
      action RunTest
        create role Role4A
        create role Role4B
        TestName = "(context >> Role4A union Role4B >>= count == 2) - union test step" for extern
    
    thing Role4A

    thing Role4B

  case Test_Intersection_Step
    aspect mm:Test
    external
      state TestState = context >> (Role5A >> binding intersection Role5B >> binding) >>= count == 1
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      perspective on Role5A
        only (Create, Fill)
      perspective on Role5B
        only (Create, Fill)
      perspective on Filler1
        only (Create)
      action RunTest
        letA
          r1 <- create role Role5A
          r2 <- create role Role5B
          f <- create role Filler1
        in
          bind_ f to r1
          bind_ f to r2
          TestName = "(context >> (Role5A >> binding intersection Role5B >> binding) >>= count == 1) - intersection test step" for extern
    
    thing Role5A filledBy Filler1

    thing Role5B filledBy Filler1

    thing Filler1

  case Test_OrElse_Step
    aspect mm:Test
    external
      state TestState = context >> (Role6A >> binding orElse Role6B >> binding == Filler6 and Role6B >> binding orElse Role6A >> binding == Filler6)
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      perspective on Role6B
        only (Create, Fill)
      perspective on Filler6
        only (Create)
      action RunTest
        letA
          r <- create role Role6B
          f <- create role Filler6
        in
          bind_ f to r
          TestName = "(context >> (Role6A orElse Role6B == Role6B and Role6B orElse Role6A == Role6B)) - orElse test step" for extern
    
    thing Role6A filledBy Filler6

    thing Role6B filledBy Filler6

    thing Filler6

  case Test_Or_Step
    aspect mm:Test
    external
      state TestState = context >> Role7A >> ((Bool1 or Bool2) and (Bool2 or Bool1))
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      perspective on Role7A
        only (Create)
        props (Bool1, Bool2) verbs (SetPropertyValue, Consult)
      action RunTest
        letA
          r <- create role Role7A
        in
          TestName = "(context >> Role7A >> ((Bool1 or Bool2) and (Bool2 or Bool1))) - or test step" for extern
          Bool1 = true for r
          Bool2 = false for r
    
    thing Role7A 
      property Bool1 (Boolean)
      property Bool2 (Boolean)
  
  case Test_And_Step
    aspect mm:Test
    external
      state TestState = context >> Role8A >> ((not (Bool1 and Bool3)) and (not (Bool3 and Bool1)) and (Bool1 and Bool2))
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      perspective on Role8A
        only (Create)
        props (Bool1, Bool2, Bool3) verbs (SetPropertyValue, Consult)
      action RunTest
        letA
          r <- create role Role8A
        in
          TestName = "(context >> Role8A >> ((Bool1 and Bool2) and (Bool2 and Bool1))) - and test step" for extern
          Bool1 = true for r
          Bool2 = true for r
          Bool3 = false for r
    
    thing Role8A 
      property Bool1 (Boolean)
      property Bool2 (Boolean)
      property Bool3 (Boolean)

  case Test_Minimum_Step
    aspect mm:Test
    external
      state TestState = context >> Role9A >> (Num1 union Num2 >>= minimum == 1)
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      perspective on Role9A
        only (Create)
        props (Num1, Num2) verbs (SetPropertyValue, Consult)
      action RunTest
        letA
          r <- create role Role9A
        in
          TestName = "(context >> Role9A >> (Num1 union Num2 >>= minimum == 1)) - minimum test step" for extern
          Num1 = 1 for r
          Num2 = 2 for r
    
    thing Role9A 
      property Num1 (Number)
      property Num2 (Number)
  
  case Test_Maximum_Step
    aspect mm:Test
    external
      state TestState = context >> Role10A >> (Num1 union Num2 >>= maximum == 2)
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      perspective on Role10A
        only (Create)
        props (Num1, Num2) verbs (SetPropertyValue, Consult)
      action RunTest
        letA
          r <- create role Role10A
        in
          TestName = "(context >> Role10A >> (Num1 union Num2 >>= maximum == 2)) - maximum test step" for extern
          Num1 = 1 for r
          Num2 = 2 for r
    
    thing Role10A 
      property Num1 (Number)
      property Num2 (Number)

  case Test_Sum_Step
    aspect mm:Test
    external
      state TestState = context >> Role11A >> (Num1 union Num2 >>= sum == 3)
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      perspective on Role11A
        only (Create)
        props (Num1, Num2) verbs (SetPropertyValue, Consult)
      action RunTest
        letA
          r <- create role Role11A
        in
          TestName = "(context >> Role11A >> (Num1 union Num2 >>= sum == 3)) - sum test step" for extern
          Num1 = 1 for r
          Num2 = 2 for r
    
    thing Role11A 
      property Num1 (Number)
      property Num2 (Number)

  case Test_Product_Step
    aspect mm:Test
    external
      state TestState = context >> Role12A >> (Num1 union Num2 >>= product == 6)
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      perspective on Role12A
        only (Create)
        props (Num1, Num2) verbs (SetPropertyValue, Consult)
      action RunTest
        letA
          r <- create role Role12A
        in
          TestName = "(context >> Role12A >> (Num1 union Num2 >>= product == 6)) - product test step" for extern
          Num1 = 2 for r
          Num2 = 3 for r
    
    thing Role12A 
      property Num1 (Number)
      property Num2 (Number)

  case Test_First_Step
    aspect mm:Test
    external
      state TestState = context >> Role13A >> ((Num1 union Num2 >>= first == 1) and ((Num2 union Num1) >>= first == 2))
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (TestSucceeded) verbs (SetPropertyValue, Consult)
      perspective on Role13A
        only (Create)
        props (Num1, Num2) verbs (SetPropertyValue, Consult)
      action RunTest
        letA
          r <- create role Role13A
        in
          TestName = "(context >> Role13A >> ((Num1 union Num2 >>= first == 1) and ((Num2 union Num1) >>= first == 2))) - first test step" for extern
          Num1 = 1 for r
          Num2 = 2 for r
    
    thing Role13A 
      property Num1 (Number)
      property Num2 (Number)
domain model://joopringelberg.nl#SynchronisationTestModel@2.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#SynchronisationTestModel

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
          Name = "Test Sync App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:TestSyncApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:TestSyncApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestApp
    indexed mm:TestSyncApp
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
          
    -- We need to synchronise, hence PerspectivesUsers and not Persons.
    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
    
    -- To execute any test, run the action RunTest in the first PDR.
    -- To check if a test has succeeded, retrieve the value of TestSucceeded in the second PDR.
    context Tests (relational) filledBy Test

  case Test
    -- The automatic actions are contextualised in their specialisations,
    -- meaning that specialisations of Follower and Leader are created.
    on entry 
      do for Initializer
        bind me to Leader

    -- Why not on entry of Test, like we do for the Leader?
    -- The Test instance may not yet be bound to Tests, so AppFollower is not yet reachable.
    state AppfollowerReachable = exists extern >> binder Tests
      on entry
        do for Initializer
          -- Dit gaat fout als we Follower met AppFollower vullen in plaats van met de vuller van AppFollower.
          bind AppFollower >> binding to Follower

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
  ---- TESTS. All these tests construct something in pdrA and check if it is synchronised in pdrB.
  ---- The test description and name mention the crucial step in the ORIGINAL query that is tested.
  ---- I also mention the runtime inverted query key that is used to fetch the inverted query for the INVERTED step.
  ------------------------------------------------------------------------------
  
  ------------------------------------------------------------------------------
  ---- Create a role that is in scope of the Follower. Because it has no property, it is not synchronised.
  ------------------------------------------------------------------------------
  case Test_CreateRole
    aspect mm:Test
    external
      property TestFinished (Boolean)
      state TestSucceeded = TestFinished and (not exists context >> TestRole1)
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on TestRole1
        only (Create)
      perspective on extern
        props (TestFinished) verbs (SetPropertyValue, Consult)
      action RunTest
        create role TestRole1
        TestName = "Create a role" for extern
        TestFinished = true for extern

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on TestRole1
      perspective on extern
        props (TestFinished) verbs (Consult)

    thing TestRole1

  ------------------------------------------------------------------------------
  ---- Create a role that is in scope of the Follower and has a property. Because it has a property, it is synchronised.
  ---- It specifically tests the property setting step, viz inverted queries fetched with a RTPropertyKey.
  ------------------------------------------------------------------------------
  case Test_SetProperty
    aspect mm:Test
    external
      state TestSucceeded = context >> TestRole1 >> P == 1
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on TestRole1
        only (Create)
        props (P) verbs (Consult, SetPropertyValue)
      action RunTest
        letA
          tr <- create role TestRole1
        in
          P = 1 for tr
          TestName = "Test property step for enumerated property on perspective object" for extern

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on TestRole1
        props (P) verbs (Consult)

    thing TestRole1
      property P (Number)

  ------------------------------------------------------------------------------
  ---- Set a property on the filler of a role that is in scope of the Follower.
  ---- It specifically tests the property setting step, viz inverted queries fetched with a RTPropertyKey.
  ------------------------------------------------------------------------------
  case Test_SetProperty_on_Filler
    aspect mm:Test
    state LeaderExists = exists Leader
      on entry
        do for Leader
          letA
            trfiller <- create role TestRole2Filler
          in
            bind trfiller to TestRole2
    external
      state TestSucceeded = context >> TestRole2 >> P == 1
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on TestRole2
        only (Create, Fill)
      perspective on TestRole2Filler
        only (Create)
        props (P) verbs (Consult, SetPropertyValue)
      action RunTest
        P = 1 for TestRole2Filler
        TestName = "Test property step for enumerated property on filler of perspective object" for extern

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on TestRole2
        props (P) verbs (Consult)

    thing TestRole2  filledBy TestRole2Filler
    
    thing TestRole2Filler
      property P (Number)

  ------------------------------------------------------------------------------
  ---- Create a role that is in scope of the Follower and fill it. The Follower should be able to see that it is filled.
  ---- It specifically tests the binding step (filling a role), viz inverted queries fetched with a RTFilledKey.
  ------------------------------------------------------------------------------
  case Test_Binding_Step
    aspect mm:Test
    external
      state TestSucceeded = exists context >> RoleToFill >> binding
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on RoleToFill
        only (Create, Fill)
      action RunTest
        letA
          rtf <- create role RoleToFill
        in
          bind_ me to rtf
          TestName = "Test binding step for simplest perspective query" for extern

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on RoleToFill
        props (FirstName) verbs (Consult)
    
    user RoleToFill filledBy sys:TheWorld$PerspectivesUsers

  ------------------------------------------------------------------------------
  ---- Create a role that is in scope of the Follower and fill it. The Follower should be able to see that it is filled.
  ---- This tests a calculated property query (setting a property that is not on the perspective object).
  ---- Tests inverted queries fetched with a RTPropertyKey.
  ------------------------------------------------------------------------------
  case Test_SetProperty_in_CalculatedProperty
    aspect mm:Test
    external
      property Q (Boolean)
      state TestSucceeded = context >> Leader >> P
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      property P = context >> extern >> Q
      perspective on extern
        props (Q) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "Test property step in calculated property" for extern
        Q = true for extern

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on Leader
        props (P) verbs (Consult)

------------------------------------------------------------------------------
---- Test another case of a calculated property.
---- Here the perspective query ends with a binding step.
---- The crucial modification is to set the property.
------------------------------------------------------------------------------
  case Test_SetProperty_in_CalculatedProperty_BindingStep
    aspect mm:Test
    state LeaderExists = exists Leader
      on entry
        do for Leader
          letA
            endfiller <- create role EndFiller3
          in
            bind endfiller to Intermediate3
    external
      property Q (Boolean)
      state TestSucceeded = context >> EndFiller3 >> P
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on Intermediate3
        only (Create, Fill)
        props (Q) verbs (SetPropertyValue, Consult)
      perspective on EndFiller3
        only (Create)
      action RunTest
        TestName = "Test property step in calculated property on perspective query ending on binding step" for extern
        Q = true for Intermediate3

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on CalculatedEndFiller3
        props (P) verbs (Consult)
    
    thing Intermediate3 filledBy EndFiller3
      property Q (Boolean)

    thing EndFiller3
      property P = binder Intermediate3 >> Q

    thing CalculatedEndFiller3 = Intermediate3 >> binding     

------------------------------------------------------------------------------
---- Test another case of a calculated property.
---- Here the perspective query ends with a binder step.
------------------------------------------------------------------------------

------------------------------------------------------------------------------
---- This tests another case of a calculated property.
---- Here the crucial step is not to set the property but to close the path to it by filling a role.
---- The crucial modification is to fill the role.
---- The backwards path from EndFiller$Q to Follower is: Q to EndFiller -> EndFiller to Intermediate -> Intermediate to context. And then we find Follower.
---- The test is to close the gap between Intermediate and EndFiller.
---- Only then should we sync Endfiller with Q to Follower.
---- Tests inverted queries fetched with a RTFilledKey.
------------------------------------------------------------------------------
  case Test_Binding_in_CalculatedProperty
    aspect mm:Test
    external
      on entry
        do for Leader
          letA
            endfiller <- create role EndFiller
          in
            Q = true for endfiller
      state TestSucceeded = context >> Leader >> P
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      property P = context >> Intermediate >> binding >> Q
      perspective on EndFiller
        only (Create, Fill)
        props (Q) verbs (SetPropertyValue, Consult)
      action RunTest
        TestName = "Test binding step in calculated property" for extern
        bind EndFiller to Intermediate

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on Leader
        props (P) verbs (Consult)
    
    thing Intermediate filledBy EndFiller

    thing EndFiller
      property Q (Boolean)

------------------------------------------------------------------------------
---- This tests a query with traversal from a role to a role that it fills.
---- The crucial modification is to fill the role.
---- Tests inverted queries fetched with a RTFillerKey.
------------------------------------------------------------------------------
  case Test_Binder_in_CalculatedProperty -- Test_FILLER_step
    aspect mm:Test
    external
      on entry
        do for Leader
          letA
            intermediate <- create role Intermediate1
          in
            Q = true for intermediate
      state TestSucceeded = context >> EndFiller1 >> P
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on EndFiller1
        only (Create, Fill)
        props (P) verbs (Consult)
      perspective on Intermediate1
        only (Create, Fill)
        props (Q) verbs (SetPropertyValue, Consult)
      action RunTest
        letA
          endfiller <- create role EndFiller1
        in
          TestName = "Test binder step in a calculated property" for extern
          bind_ endfiller to Intermediate1

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on EndFiller1
        props (P) verbs (Consult)
    
    thing Intermediate1 filledBy EndFiller1
      property Q (Boolean)

    thing EndFiller1
      property P = binder Intermediate1 >> Q

------------------------------------------------------------------------------
---- This tests a query with a binding step for a calculated role.
---- It tests inverted queries fetched with a RTFilledKey.
------------------------------------------------------------------------------
  case Test_Binding_in_CalculatedRole
    aspect mm:Test
    external
      on entry
        do for Leader
          letA
            endfiller <- create role EndFiller2
          in
            Q = true for endfiller
      state TestSucceeded = context >> EndFiller2 >> Q
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on EndFiller2
        only (Create, Fill)
        props (Q) verbs (SetPropertyValue, Consult)
      perspective on Intermediate2
        only (Create, Fill)
      action RunTest
        TestName = "Test binding step in calculated role" for extern
        bind EndFiller2 to Intermediate2

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on CalculatedEndFiller
        props (Q) verbs (Consult)
      
    thing CalculatedEndFiller = Intermediate2 >> binding
    
    thing Intermediate2 filledBy EndFiller2

    thing EndFiller2
      property Q (Boolean)


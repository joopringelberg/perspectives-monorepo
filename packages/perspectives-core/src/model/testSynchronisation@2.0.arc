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
      perspective on Follower
        props (FirstName) verbs (Consult)

  ------------------------------------------------------------------------------
  ---- TESTS. All these tests construct something in pdrA and check if it is synchronised in pdrB.
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
          TestName = "Set a property" for extern

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on TestRole1
        props (P) verbs (Consult)

    thing TestRole1
      property P (Number)

  ------------------------------------------------------------------------------
  ---- Create a role that is in scope of the Follower and fill it. The Follower should be able to see that it is filled.
  ------------------------------------------------------------------------------
  case Test_BindRole
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
          TestName = "Bind a role" for extern

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on RoleToFill
        props (FirstName) verbs (Consult)
    
    user RoleToFill filledBy sys:TheWorld$PerspectivesUsers

  
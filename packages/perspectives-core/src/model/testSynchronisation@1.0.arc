domain model://joopringelberg.nl#SynchronisationTestModel@1.0
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
      
      -- Create new Tests, ready with a Leader.
      action CreateTest
        letA
          test <- create context Test bound to Tests
        in
          bind me to Leader in test >> binding >> context
    
    -- To execute any test, run the action RunTest in the first PDR.
    -- To check if a test has succeeded, retrieve the value of TestSucceeded in the second PDR.
    context Tests (relational) filledBy Test

  case Test
    external
      property TestName (String)
      property TestSucceeded = context >> TestRole1 >> P == 1

    user Leader filledBy (sys:TheWorld$PerspectivesUsers + sys:SocialEnvironment$Persons)
      perspective on Follower
        only (Create, Fill)
        props (FirstName) verbs (Consult)
      perspective on TestRole1
        only (Create)
        props (P) verbs (Consult, SetPropertyValue)
      action RunTest
        letA
          tr <- create role TestRole1
        in
          P = 1 for tr

    user Follower filledBy (sys:TheWorld$PerspectivesUsers + sys:SocialEnvironment$Persons)
      perspective on Leader
        props (FirstName) verbs (Consult)
      perspective on TestRole1
        props (P) verbs (Consult)

    thing TestRole1
      property P (Number)

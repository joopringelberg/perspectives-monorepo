domain model://joopringelberg.nl#TestDeltaOrdering@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#TestDeltaOrdering

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestDeltaOrderingApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Test Delta Ordering App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:TestDeltaOrderingApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:TestDeltaOrderingApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestDeltaOrderingApp
    indexed mm:TestDeltaOrderingApp
    aspect sys:RootContext
    external
    
    user Manager = sys:Me
      perspective on Tests
        only (CreateAndFill, Remove)
        props (Name) verbs (Consult, SetPropertyValue)
      perspective on Tests >> binding >> context >> Tester
        only (Create, Fill, Remove)
      perspective on IncomingTests
        props (Name) verbs (Consult)
      action CreateTest
        letA 
          test <- create context Test bound to Tests
        in
          bind me to Tester in test >> binding >> context

    context Tests (relational) filledBy Test

    context IncomingTests = me >> binder Tester >> context >> extern
  
  case Test
    external
      aspect sys:RoleWithName
    
    user Tester (relational) filledBy (sys:SocialEnvironment$Persons + sys:TheWorld$PerspectivesUsers)
      perspective on extern
        props (Name) verbs (Consult, SetPropertyValue)
      perspective on Tester
        only (Create, Fill, Remove)
        props (FirstName) verbs (Consult)
      perspective on DisposableRole
        only (Create, Remove)
        props (Name) verbs (Consult, SetPropertyValue)
    
    thing DisposableRole
      aspect sys:RoleWithName
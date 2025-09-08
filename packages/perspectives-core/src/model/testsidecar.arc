domain model://joopringelberg.nl#TestCuids
  use sys for model://perspectives.domains#System
  use ts for model://joopringelberg.nl#TestCuids

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestStableIdsModel1
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Test side car" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (ts:TestStableIdsModel >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  aspect user sys:Upgrader
  thing UpgradeSidecar
    aspect sys:SystemDataUpgrade
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestStableIdsModel1
    indexed ts:TestStableIdsModel
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
      property Test6 (String) 
    
    user Manager = sys:Me

    thing Something
  
  case AnotherCase
    aspect sys:ContextWithNotification
    state SomeState = exists AnotherThing1
      on entry
        notify Manager
          "Entering SomeState for {Manager >> FirstName}"
      state Nested = 1 == 2
        on entry
          do for Manager
            Test7 = "Hello " for AnotherThing1

    user Manager = sys:Me
      perspective on AnotherThing1
        props (Test7) verbs (SetPropertyValue)
    thing AnotherThing1
      property Test7 (String)
      property Test8 (String)
      property Test9 = Test7 + Test8
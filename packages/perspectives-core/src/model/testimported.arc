domain model://joopringelberg.nl#TestImported
  use sys for model://perspectives.domains#System
  use ts for model://joopringelberg.nl#TestImported

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestImports
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Test Imported" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (ts:TestImported >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  aspect user sys:Upgrader
  thing UpgradeImports
    aspect sys:SystemDataUpgrade
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestImports
    indexed ts:TestImported
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
    state AnotherState = Manager >> FirstName == "Joop"
      on entry
        notify Manager
          "Entering AnotherState for {Manager >> FirstName}"

    user Manager = sys:Me
      perspective on AnotherThing1
        props (Test7) verbs (SetPropertyValue)
      perspective on Manager
        props (FirstName, LastName) verbs (Consult)
      action MyAction
        Test7 = "Did action MyAction" for AnotherThing1
      screen
        who
          Manager
            master
              with props (FirstName)
            detail
              without props (LastName)
        what
          AnotherThing1
            master
            detail
        where

    thing AnotherThing1
      property Test7 (String)
      property Test8 (String)
      property Test9 = Test7 + Test8
      view ThingView (Test7, Test8, Test9)
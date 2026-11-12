domain model://joopringelberg.nl#MinimalModel@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#MinimalModel

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context MinimalApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Minimal Model App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:MyMinimalApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:MyMinimalApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case MinimalApp
    indexed mm:MyMinimalApp
    aspect sys:RootContext
    external
    
    user Manager = sys:Me

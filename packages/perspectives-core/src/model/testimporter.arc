domain model://joopringelberg.nl#TestImporter
  use sys for model://perspectives.domains#System
  use ts for model://joopringelberg.nl#TestImporter
  use ti for model://joopringelberg.nl#TestImported

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestImporter
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Test Importer" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (ts:TestImporter >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  aspect user sys:Upgrader
  thing UpgradeImporter
    aspect sys:SystemDataUpgrade
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestImporter
    indexed ts:TestImporter
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
    user Manager = sys:Me


domain model://joopringelberg.nl#FillFromTest@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#FillFromTest

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestFromApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "TestFrom App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:TestFromApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:TestFromApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestFromApp
    indexed mm:TestFromApp
    aspect sys:RootContext
    external
    
    user Manager = sys:Me

    user Employee filledBy (sys:TheWorld$PerspectivesUsers + sys:SocialEnvironment$Persons)

    user SpecialEmployee = selectFrom Employee just sys:SocialEnvironment$Persons

domain model://corbaars.nl#Simple
  use bl for model://corbaars.nl#Simple
  use sys for model://perspectives.domains#System

---------------------------------------------------------------------------------------
-- PREAMBULE
---------------------------------------------------------------------------------------

  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          app <- create context Simple
          start <- create role StartContexts in sys:MySystem
        in
          bind_ app >> extern to start
          Name = "Simple App" for start
    on exit
      do for sys:PerspectivesSystem$Installer
        letA
          indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (bl:MinimalApp >> extern)
          startcontext <- filter sys:MySystem >> StartContexts with filledBy (bl:MinimalApp >> extern)
        in
          remove context indexedcontext
          remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

---------------------------------------------------------------------------------------
-- END OF PREAMBULE
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
-- CREATE ROOT CONTEXT WITH ROOT MANAGER
---------------------------------------------------------------------------------------

  case Simple
      indexed bl:MinimalApp
      aspect sys:RootContext
      external
        aspect sys:RootContext$External
      user A = sys:Me
        perspective on B
          defaults
        perspective on C
          defaults
      thing B
        property One (Boolean)
      thing C
        property D (DateTime)
      user D = sys:Me
        perspective on C
          defaults

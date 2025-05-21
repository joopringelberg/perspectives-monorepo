domain model://joopringelberg.nl#Parser_3
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#Parser_3

  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context Simple
          start <- create role StartContexts in sys:MySystem
        in
          bind_ app >> extern to start
          Name = "Parser 3" for start

  aspect user sys:PerspectivesSystem$Installer

  case Simple
    indexed mm:MinimalApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    user Userrole = sys:Me
      perspective on Thingrole
        defaults
      screen
        who
        what
          Thingrole 
            master
            detail
        where
    thing Thingrole
      property Bolean (Boolean)


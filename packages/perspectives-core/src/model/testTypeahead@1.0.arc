domain model://joopringelberg.nl#TestTypeAhead@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#TestTypeAhead

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestTypeAheadApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Test typeahead" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:MyTestTypeAheadApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:MyTestTypeAheadApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestTypeAheadApp
    indexed mm:MyTestTypeAheadApp
    aspect sys:RootContext
    external
    
    user Manager = sys:Me
      perspective on Friend
        defaults
      action CreateFriend
        create role Friend
      screen
        who
        what
          row
            typeaheadfiller Friend
              fillfrom sys:MySocialEnvironment >> Persons
          row 
            typeaheadform Friend
              fillfrom sys:MySocialEnvironment >> Persons
        where

    user Friend filledBy sys:SocialEnvironment$Persons
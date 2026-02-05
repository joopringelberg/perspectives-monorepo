domain model://joopringelberg.nl#TestDoubleBindingRestriction@3.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#TestDoubleBindingRestriction

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestDoubleBindingRestrictionApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Test Double Binding Restriction" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:TestDoubleBindingRestrictionApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:TestDoubleBindingRestrictionApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestDoubleBindingRestrictionApp
    indexed mm:TestDoubleBindingRestrictionApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
    user Manager = sys:Me
      perspective on Peer
        only (Create, Fill, Remove)
        props (FirstName, LastName) verbs (Consult)
      perspective on BottomA
        only (Create, Fill, Remove)
        props (Name) verbs (Consult, SetPropertyValue)
      perspective on BottomB
        only (Create, Fill, Remove)
        props (Name) verbs (Consult, SetPropertyValue)
      perspective on ContainsBottom
        only (Create, Fill, Remove)
        props (Name) verbs (Consult)
      perspective on FilledByContainsBottomA
        only (Create, Fill, Remove)
        props (Name) verbs (Consult)

    -- The test is that we can currently fill Peer with the SocialEnvironment$Persons that is filled by the own user,
    -- while we cannot fill it with any other Person.
    -- NOT CONFIRMED. This is because all Persons instances are filled by PerspectivesUsers instances,
    user Peer filledBy (sys:SocialEnvironment$Persons + sys:TheWorld$PerspectivesUsers)

    thing Bottom
      property Name (String)
        readableName
    
    thing BottomA (relational)
      aspect mm:TestDoubleBindingRestrictionApp$Bottom
    
    thing BottomB (relational)
      aspect mm:TestDoubleBindingRestrictionApp$Bottom
    
    thing ContainsBottom (relational) filledBy (BottomA, BottomB)

    -- We should be able to fill this with a ContainsBottom instance that is filled with a BottomA instance,
    -- but not with a ContainsBottom instance that is filled with a BottomB instance.
    -- CONFIRMED!
    thing FilledByContainsBottomA (relational) filledBy (ContainsBottom + BottomA)
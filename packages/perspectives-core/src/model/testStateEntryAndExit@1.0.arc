domain model://joopringelberg.nl#TestStateEntryAndExit@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#TestStateEntryAndExit

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestStateEntryAndExitApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Test State Entry and Exit App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:TestStateEntryAndExitApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:TestStateEntryAndExitApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestStateEntryAndExitApp
    indexed mm:TestStateEntryAndExitApp
    aspect sys:RootContext
    aspect sys:ContextWithNotification
    external
    
    user Manager = sys:Me
      perspective on RoleWithState
        only (Create, Remove)
        props (State1Sentinel, Name) verbs (Consult, SetPropertyValue, RemovePropertyValue)
      perspective on RoleWithDelayedExit
        only (Create, Remove)
        props (State1Sentinel, Name) verbs (Consult, SetPropertyValue, RemovePropertyValue)

    -- On setting State1Sentinel to true, the role enters State1.
    -- We get a notification of entry.
    -- StateSentinel1 is set to false.
    -- However, the role does **not exit State1** - and accordingly, we do not get a notification of exit.
    -- I can see with the inspector that State1 is still holding.
    thing RoleWithState (relational)
      state State1 = State1Sentinel == true
        on entry
          do for Manager
            State1Sentinel = false
          notify Manager
            "Entered State 1 of RoleWithState {Name}"
        on exit 
          notify Manager
            "Exited State 1 of RoleWithState {Name}"
      property State1Sentinel (Boolean)
      property Name (String)

    -- On setting State1Sentinel to true, the role enters State1.
    -- We get a notification of entry.
    -- After 1 second, StateSentinel1 is set to false.
    -- The role exits State1 - however, instead of a notification, nothing happens, we see the 'processing' spinner.
    -- On the console of the SharedWorker I see the STATE TRACE message that the role exits State1.
    -- The inspector confirms this: State1 is no longer recorded in the role instance.
    -- Then follows an error on the console: typeError: J is not a function.
    thing RoleWithDelayedExit (relational)
      state State1 = State1Sentinel == true
        on entry
          do for Manager after 1 Seconds
            State1Sentinel = false
          notify Manager
            "Entered State 1 of RoleWithDelayedExit {Name}"
        on exit 
          notify Manager
            "Exited State 1 of RoleWithDelayedExit {Name}"
      property State1Sentinel (Boolean)  
      property Name (String)  
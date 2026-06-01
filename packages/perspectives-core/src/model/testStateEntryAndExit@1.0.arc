domain model://joopringelberg.nl#TestStateEntryAndExit@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#TestStateEntryAndExit
  use sensor for model://perspectives.domains#Sensor

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
      perspective on RoleWithFile
        only (Create, Remove)
        props (TheFile, Name, LastChangeDT, NrOfUploads) verbs (Consult, SetPropertyValue, RemovePropertyValue)

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
    -- The role exits State1. We do not see a notification but that may be due to a mechanism to suppress very fast notification sequences.
    -- The notification is generated.
    -- The inspector confirms that State1 is exited.
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
    
    -- Consider this an extension of RoleWithDelayedExit, but now with a file property.
    -- We use the file property to trigger the state re-evaluation.
    -- The condition that LastChangeDT must be at least 1 second ago is to make sure state ProcessFile exits.
    -- Without it, if execution is slow enough, we may never see the exit of ProcessFile.
    -- We would be past LastChangeDT immediately.
    -- Because time passes automatically but does not trigger re-evaluation through readSensor, 
    -- * ProcessTheFile is not registered on the role instance, while
    -- * the condition holds true nevertheless (after 1 second).
    -- * So if we then upload the file, triggering re-evaluation, state ProcessTheFile is entered again.
    -- The upshot is that here we have a mechanism to trigger an automatic action by uploading a file (even the same file).
    thing RoleWithFile
      on entry
        do for Manager
          LastChangeDT = callExternal sensor:ReadSensor( "clock", "now" ) returns DateTime
          NrOfUploads = 0

      state ProcessTheFile = (exists TheFile) and (callExternal sensor:ReadSensor( "clock", "now" ) returns DateTime > LastChangeDT + 1 seconds)
        on entry
          do for Manager after 1 Seconds
            LastChangeDT = callExternal sensor:ReadSensor( "clock", "now" ) returns DateTime
            NrOfUploads = NrOfUploads + 1
          notify Manager
            "Processing {Name}"

      property LastChangeDT (DateTime)
      property TheFile (File)
      property Name (String)
      property NrOfUploads (Number)

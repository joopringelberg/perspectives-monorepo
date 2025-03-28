domain model://joopringelberg.nl#MinimalModel
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
        in
          bind app >> extern to StartContexts in sys:MySystem
          Name = "Minimal App" for app >> extern

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
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
      aspect sys:RootContext$External
    
    user Manager = sys:Me
      perspective on ThatWillFill
        defaults
      
      perspective on ThatWillBeFilled
        defaults

      perspective on NestedContexts
        defaults
      
      perspective on NestedContexts >> binding >> ThatWillBeFilled
        defaults

    -- Be pattern for two user roles in the same context.
    user ThatWillFill
      on entry
        do for Manager
          bind origin to ThatWillBeFilled

    -- The user role that the other will be.
    user ThatWillBeFilled filledBy ThatWillFill

    context NestedContexts filledBy NestedContext
      -- The condition is necessary because the instance of NestedContexts will be
      -- available before the nexted context itself is created.
      state ContextExists = exists binding
        on entry
          do for Manager
            -- Notice that we will fill NestedContext$ThatWillBeFilled, not 
            -- MinimalApp$ThatWillBeFilled!
            bind origin to ThatWillBeFilled in binding >> context

  case NestedContext
    user ThatWillBeFilled
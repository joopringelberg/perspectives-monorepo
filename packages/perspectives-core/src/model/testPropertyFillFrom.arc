domain model://joopringelberg.nl#TestPropertyFillFrom@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#TestPropertyFillFrom

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestPropertyFillFromApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Test fillproperty" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:TestPropertyFillFromApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:TestPropertyFillFromApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestPropertyFillFromApp
    indexed mm:TestPropertyFillFromApp
    aspect sys:RootContext
    external
    
    user Manager = sys:Me
      perspective on ReferenceValues
        defaults
      perspective on RoleWithProperty
        only (Create, Remove)
        props (Prop1) verbs (SetPropertyValue, Consult)
      screen
        who
        what
          row
            table RoleWithProperty
              fillproperty Prop1 from ReferenceValues >> Value
        where
          ReferenceValues
            master
              with props (Value)
            detail
          RoleWithProperty
            master
              fillproperty Prop1 from ReferenceValues >> Value
            detail
              with props (Prop1)
              fillproperty Prop1 from ReferenceValues >> Value

    thing ReferenceValues (relational)
      property Value (String)
    
    thing RoleWithProperty (relational)
      property Prop1 (String)

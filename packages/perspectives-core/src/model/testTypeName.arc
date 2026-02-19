domain model://joopringelberg.nl#TestTypeName@1.0
  use ttn for model://joopringelberg.nl#TestTypeName
  use sys for model://perspectives.domains#System
  use util for model://perspectives.domains#Utilities

---------------------------------------------------------------------------------------
-- PREAMBULE
---------------------------------------------------------------------------------------

  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          app <- create context TestTypeName
          start <- create role StartContexts in sys:MySystem
        in
          bind_ app >> extern to start
          Name = "TestTypeName App" for start
    on exit
      do for sys:PerspectivesSystem$Installer
        letA
          indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (ttn:TestTypeNameApp >> extern)
          startcontext <- filter sys:MySystem >> StartContexts with filledBy (ttn:TestTypeNameApp >> extern)
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

  case TestTypeName
    indexed ttn:TestTypeNameApp
    aspect sys:RootContext
    external
    user Tester = sys:Me
      perspective on ThingWithTypeNameProperty
        only (Create, Remove)
        props (Name, TypeName, TypeNameViaRoleType) verbs (Consult)
        props (Name) verbs (SetPropertyValue)
    -- We hebben een dingrol nodig
    -- en dan geven we die een property TypeName die als waarde typeName krijgt
    thing ThingWithTypeNameProperty
      property Name (String)
        readableName
      property TypeNameViaRoleType = roleType >> translate
      property TypeName = callExternal util:RoleIdentifier() returns String
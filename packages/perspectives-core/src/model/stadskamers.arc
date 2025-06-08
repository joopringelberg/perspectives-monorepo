-- Stadskamers - Copyright Joop Ringelberg and Cor Baars 2021 - 2024

domain model://perspectives.domains#Stadskamers
  use sys for model://perspectives.domains#System
  use sk for model://perspectives.domains#Stadskamers
  use acc for model://perspectives.domains#BodiesWithAccounts
  use cdb for model://perspectives.domains#Couchdb
  use util for model://perspectives.domains#Utilities
  use p for model://perspectives.domains#Parsing
  use files for model://perspectives.domains#Files
  use sensor for model://perspectives.domains#Sensor

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- We must first create the context and then later bind it.
          -- If we try to create and bind it in a single statement, 
          -- we find that the Installer can just create RootContexts
          -- as they are the allowed binding of StartContexts.
          -- As a consequence, no context is created.
          couchdbapp <- create context StadskamersApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ couchdbapp >> extern to start
          Name = "Stadskamer App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (sk:MyStadskamerApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (sk:MyStadskamerApp >> extern)
      in
        remove context indexedcontext
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
-- The INDEXED context sk:MyStadskamerApp is the starting point containing all Stadskamers.
  -- There is NO PUBLIC PERSPECTIVE on this case.
  case StadskamersApp
    indexed sk:MyStadskamerApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
    -- the exit is triggered on removing a role instance of StartContexts
    on exit
      do for Manager
        delete context bound to Stadskamers

    user Manager = sys:SocialMe >> binding
      perspective on Stadskamers
        only (CreateAndFill, RemoveContext, DeleteContext)
        props (Name) verbs (Consult, SetPropertyValue)
  
    context Stadskamers (relational) filledBy Stadskamer
      state WhenCreated = exists binding
        perspective of Manager
          perspective on (Stadskamers >> binding >> context >> Administrators)
            only (Create, Fill)
        on entry
          do for Manager
            bind sys:SocialMe >> binding to Administrators in origin >> binding >> context

  case Stadskamer
    external
      property Name (String)
        readableName
    
    -- This role creates locations and employees.
    user Administrators (relational) filledBy sys:TheWorld$PerspectivesUsers
      perspective on Administrators
        only (Create, Fill, Remove)
        props (FirstName, LastName) verbs (Consult)
      
      perspective on Employees
        only (Create, Fill, Remove)
        props (FirstName, LastName) verbs (Consult)
      
      perspective on Locations
        only (CreateAndFill, Remove)
        props (Name) verbs (Consult, SetPropertyValue)

    user Employees (relational) filledBy sys:TheWorld$PerspectivesUsers
    
    context Locations (relational) filledBy (Location, SmallLocation)

  case Location
    external
      property Name (String)
        readableName
      property Street (String)
      property HouseNumber (String)
      property PostalCode (String)
      property City (String)
    
    user Initializer = filter extern >> binder Locations >> context >> Administrators with filledBy sys:Me >> binding
      perspective on LocationManager
        only (Create, Fill, Remove)
        props (FirstName, LastName) verbs (Consult)

    user LocationManager filledBy Employees
      perspective on ActivityCenters
        only (CreateAndFill, Remove)
        props (Name) verbs (Consult, SetPropertyValue)

    context ActivityCenters (relational) filledBy ActivityCenter
    
  case ActivityCenter
    external
      property Name (String)
        readableName
  
  case SmallLocation
    aspect sk:Location
    aspect sk:ActivityCenter
    external
      aspect sk:ActivityCenter$External
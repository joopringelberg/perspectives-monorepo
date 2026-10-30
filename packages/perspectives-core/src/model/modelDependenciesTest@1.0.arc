domain model://joopringelberg.nl#TestModelDependencies@1.0
  use sys for model://perspectives.domains#System
  use ru for model://joopringelberg.nl#TestModelDependencies
  use sensor for model://perspectives.domains#Sensor
  use cdb for model://perspectives.domains#Couchdb
  use cm for model://perspectives.domains#CouchdbManagement
  use p for model://perspectives.domains#Parsing
  use hyp for model://perspectives.domains#HyperContext
  use bs for model://perspectives.domains#BrokerServices
  use util for model://perspectives.domains#Utilities
  use mm for model://joopringelberg.nl#RepositoryTools@1.0

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Test Model Dependencies App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (ru:TestModelDependenciesApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (ru:TestModelDependenciesApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestApp
    indexed ru:TestModelDependenciesApp
    aspect sys:RootContext
    external
    
    user Manager = sys:Me
      perspective on Tests
        only (CreateAndFill, RemoveContext)
      perspective on Tests >> binding >> context >> Tester
        only (Create, Fill)
          
    -- To execute any test, run the action RunTest in the first PDR.
    -- To check if a test has succeeded, retrieve the value of TestSucceeded in the second PDR.
    context Tests (relational) filledBy mm:Test

    user BespokeDatabaseOwner filledBy cm:BespokeDatabase$Owner

  ------------------------------------------------------------------------------
  ---- COMPILE SYSTEM THAT REQUIRES COUCHDB@3.0
  ---- The required version is lower than what is available.
  ------------------------------------------------------------------------------
  case LowerDependency
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "System" for extern
        VersionNumber = "7.0" for extern
        TestName = "System requires Couchdb@3.0 but only Couchdb@4.0 is available." for extern
        ModelVersions = "Couchdb=3.0; Serialise=3.0; Sensor=3.0; Utilities=3.0" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

  ------------------------------------------------------------------------------
  ---- COMPILE SYSTEM THAT REQUIRES COUCHDB@5.0
  ---- The required version is higher than what is available.
  ------------------------------------------------------------------------------
  case HigherDependency
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "System" for extern
        VersionNumber = "7.0" for extern
        TestName = "System requires Couchdb@5.0 but only Couchdb@4.0 is available." for extern
        ModelVersions = "Couchdb=5.0; Serialise=3.0; Sensor=3.0; Utilities=3.0" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

  -- The version compatibility test is only run when we install or update the model.
  -- Use "model://perspectives.domains#Couchdb$UpdateModel".
  -- callEffect cdb:UpdateModel( VersionedModelURI, false )

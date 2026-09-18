domain model://joopringelberg.nl#RebootUniverse@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#RebootUniverse
  use sensor for model://perspectives.domains#Sensor
  use cdb for model://perspectives.domains#Couchdb
  use cm for model://perspectives.domains#CouchdbManagement
  use p for model://perspectives.domains#Parsing
  use hyp for model://perspectives.domains#HyperContext
  use bs for model://perspectives.domains#BrokerServices

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
          Name = "Reboot Universe Tests App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:RebootUniverseApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:RebootUniverseApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestApp
    indexed mm:RebootUniverseApp
    aspect sys:RootContext
    external
    
    user Manager = sys:Me
      perspective on Tests
        only (CreateAndFill, RemoveContext)
      perspective on Tests >> binding >> context >> Tester
        only (Create, Fill)
          
    -- To execute any test, run the action RunTest in the first PDR.
    -- To check if a test has succeeded, retrieve the value of TestSucceeded in the second PDR.
    context Tests (relational) filledBy Test

    user BespokeDatabaseOwner filledBy cm:BespokeDatabase$Owner

  case Test
    -- The automatic actions are contextualised in their specialisations,
    -- meaning that specialisation of Tester is created.
    on entry 
      do for Initializer
        bind me to Tester

    external
      property TestName (String)
      property TestSucceeded (Boolean)

    
    user Initializer = me
      perspective on Tester
        only (Create, Fill)

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      perspective on extern
        props (TestName, TestSucceeded) verbs (SetPropertyValue, Consult)

  ------------------------------------------------------------------------------
  ---- CLEANUP
  ---- Remove the databases created on the previous run.
  ------------------------------------------------------------------------------
  case Cleanup
    aspect mm:Test

    external
      property Finished (Boolean)
      state Success = Finished
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (Finished) verbs (SetPropertyValue, Consult)
      
      action RunTest
        letA
          url <- "https://perspectives.domains/"
        in
          -- Give Tester credentials.
          callEffect cdb:AddCredentials( url, "alice", "alice" )
          -- Remove cw_servers_and_repositories
          callEffect cdb:DeleteCouchdbDatabase( url, "cw_servers_and_repositories" )
          -- Remove cw_perspectives_domains
          callEffect cdb:DeleteCouchdbDatabase( url, "cw_perspectives_domains" )
          -- Remove models_perspectives_domains
          callEffect cdb:DeleteCouchdbDatabase( url, "models_perspectives_domains" )
          -- Remove the Bespoke database of Big Bang.
          callEffect cdb:DeleteCouchdbDatabase( url, "cw_bigbangsdatabase" )
          TestName = "Cleanup - remove the databases created on the previous run." for extern
          Finished = true for extern

  ------------------------------------------------------------------------------
  ---- MANAGECOUCHDB
  ---- 1. Create a CouchdbServer
  ------------------------------------------------------------------------------
  case ManageCouchdb
    aspect mm:Test
    state TesterExists = exists Tester
      on entry
        do for Tester
          callEffect cdb:AddCredentials( "https://perspectives.domains/", "alice", "alice" )

    external
      state Success = (exists cm:MyCouchdbApp >> CouchdbServers) and 
        (exists exists cm:MyCouchdbApp >> CouchdbServers >> binding >> context >> Admin)

        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
      
      -- Same perspective as cm:CouchdbManagementApp$Manager
      perspective on cm:CouchdbManagementApp$CouchdbServers
        only (CreateAndFill, RemoveContext, DeleteContext, Create, Fill)
        props (Name) verbs (Consult)
        props (Url, CouchdbServers$CouchdbPort, AdminUserName, AdminPassword, Name) verbs (SetPropertyValue)

      action RunTest
        letA
          server <- create role cm:CouchdbManagementApp$CouchdbServers in cm:MyCouchdbApp
        in
          TestName = "ManageCouchdb - create a server registration." for extern
          Url = "https://perspectives.domains/" for server
          CouchdbPort = "5987" for server
          AdminUserName = "alice" for server
          AdminPassword = "alice" for server

  ------------------------------------------------------------------------------
  ---- CREATE REPOSITORY PERSPECTIVES.DOMAINS
  ---- 1. Create a repository role.
  ---- 2. Set the NameSpace property to "perspectives.domains"
  ---- 3. Set the AdminEndorses property to true.
  ------------------------------------------------------------------------------
  case CreatePerspectivesDomainsRepository
    aspect mm:Test

    external
      state Success = (exists cm:MyCouchdbApp >> CouchdbServers) and 
        -- CouchdbServer has an Admin
        (exists exists cm:MyCouchdbApp >> CouchdbServers >> binding >> context >> Admin) and
        -- CouchdbServer has a Repository
        (exists cm:MyCouchdbApp >> CouchdbServers >> binding >> context >> Repositories >> binding)
        -- Repository has an Admin
        and (exists cm:MyCouchdbApp >> CouchdbServers >> binding >> context >> Repositories >> binding >> context >> Admin)

        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern

      -- Same perspective as cm:CouchdbServer$Admin      
      perspective on cm:CouchdbServer$Repositories
        all roleverbs
        props (Repositories$NameSpace, AdminEndorses, IsPublic, AdminLastName) verbs (Consult)
        props (IsPublic, NameSpace_, HasDatabases) verbs (SetPropertyValue)
        in object state WithoutExternalDatabase
          props (AdminEndorses) verbs (SetPropertyValue)
        in object state CreateDatabases
          props (IsPublic) verbs (SetPropertyValue)
        in object state NoNameSpace
          props (Repositories$NameSpace) verbs (SetPropertyValue, AddPropertyValue)

      action RunTest
        letA
          server <- cm:MyCouchdbApp >> CouchdbServers >> binding >> context >>= first
          repo <- create role cm:CouchdbServer$Repositories in server
        in
          TestName = "CreatePerspectivesDomainsRepository - create a repository." for extern
          NameSpace = "perspectives.domains" for repo
          AdminEndorses = true for repo

------------------------------------------------------------------------------
  ---- ADD MODEL
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel
    aspect mm:Test

    state TesterExists = exists Tester >> binding
    
      state ExistsRepository = exists Repository >> binding

        state ManifestIsConstructed = exists (filter Repository >> binding >> context >> Manifests with (LocalModelName == origin >> extern >> ModelName)) >> binding
          on entry
            do for Tester
              bind (filter Repository >> binding >> context >> Manifests with (LocalModelName == origin >> extern >> ModelName)) >> binding >>= first to Manifest
        
        -- -- Just a single Version is expected to exist for each Manifest.
        state VersionIsConstructed = exists (Manifest >> binding >> context >> Versions >> binding)
          on entry
            do for Tester
              bind (Manifest >> binding >> context >> Versions >> binding) >>= first to Version

    external
      property NameSpace (String)
      property ModelName (String)
      property VersionNumber (String)

      property StartTest (Boolean)
      property StartParsing (Boolean)
      property YamlGenerated (Boolean)

      state CreateManifest = StartTest
        on entry
          do for Tester after 20 Milliseconds
            letA
              manifest <- create role cm:Repository$Manifests in context >> Repository >> binding >> context
            in
              LocalModelName = ModelName for manifest
              EnteredModelCuid = callExternal p:GetLocalModelCuid( "model://" + NameSpace + "#" + ModelName ) returns String for manifest

      -- Is the Manifest role filled with the external role of the new ModelManifest?
      state CreateVersion = exists context >> Manifest >> binding
        on entry
          do for Tester after 20 Milliseconds
            letA
              version <- create role cm:ModelManifest$Versions in context >> Manifest >> binding >> context
            in
              Versions$Version = VersionNumber for version
      
      state CompileModel = exists context >> Version >> binding
        on entry
          do for Tester
            letA
              version <- context >> Version >> binding
            in
              create file "whatever" as "text/arc" in ArcFile for version
                callExternal p:GetLocalArcSource( version >> ModelURIReadable ) returns String 
              Store = "Repository" for version
              StartParsing = true

      state StartParsing = StartParsing
        on entry
          do for Tester after 20 Milliseconds
            letA
              version <- context >> Version >> binding
            in
              AutoUpload = true for version
      
      state AugmentYaml = letE
          translation <- context >> Version >> binding >> context >> Translation
        in
          (exists translation >> TranslationYaml)
        on entry
          do for Tester after 20 Milliseconds
            letA
              version <- context >> Version >> binding
            in
              callEffect cdb:UploadOldTranslation( context >> Version >> binding >> VersionedModelURI )
              -- LET OP: dit gebeurt ook in UploadToRepository!
              GenerateYaml = true for version >> context >> Translation
              YamlGenerated = true
      
      state Success = YamlGenerated
        on entry
          -- This ensures that we mark the test as succeeded in the next transaction, hopefully after yaml translation is complete.
          do for Tester after 500 Milliseconds
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (NameSpace, VersionNumber, ModelName, StartTest, StartParsing, YamlGenerated) verbs (SetPropertyValue, Consult)

      perspective on Repository
        only (CreateAndFill)
      
      perspective on Manifest
        only (CreateAndFill)
      
      perspective on Version
        only (CreateAndFill)

      -- Same perspective as cm:Repository$Admin      
      perspective on cm:Repository$Manifests
        only (Create, Fill, Delete, Remove, RemoveContext, DeleteContext, CreateAndFill)
        props (DomeinFileName, LocalModelName, EnteredModelCuid) verbs (SetPropertyValue, Consult)
        props (Description, ModelCuid) verbs (Consult)
        in object state ReadyToMake
          props (ModelCuid) verbs (SetPropertyValue)
      
      -- Same perspective as cm:ModelManifest$Author
      perspective on cm:ModelManifest$Versions
        only (Create, Fill, RemoveContext, CreateAndFill, Delete, DeleteContext)
        props (Versions$Version, Description, Patch, Build) verbs (Consult, SetPropertyValue)

      perspective on cm:VersionedModelManifest$External
        props (ArcFile, AutoUpload, Store) verbs (Consult, SetPropertyValue)

      perspective on cm:VersionedModelManifest$Translation
        props (GenerateYaml) verbs (Consult, SetPropertyValue)
      
      action RunTestTemplate
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "Files" for extern
        VersionNumber = "3.0" for extern
        TestName = "AddModel_Files - manifest, version and compiled model and translation for Files." for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    -- Is filled with the external role of the Repository.
    context Repository filledBy cm:Repository
    -- Is filled with the external role of the Manifest.
    context Manifest filledBy cm:ModelManifest
    -- Is filled with the external role of the VersionedModelManifest.
    context Version filledBy cm:VersionedModelManifest

------------------------------------------------------------------------------
  ---- COUCHDB
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ---- Cuid = nip6odtx4r
  ------------------------------------------------------------------------------
  case AddModel_Couchdb
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "Couchdb" for extern
        VersionNumber = "4.0" for extern
        TestName = "Add the model Couchdb" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

------------------------------------------------------------------------------
  ---- SERIALISE
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_Serialise
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "Serialise" for extern
        VersionNumber = "3.0" for extern
        TestName = "Add the model Serialise" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

------------------------------------------------------------------------------
  ---- SENSOR
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_Sensor
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "Sensor" for extern
        VersionNumber = "3.0" for extern
        TestName = "Add the model Sensor" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

------------------------------------------------------------------------------
  ---- UTILITIES
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_Utilities
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "Utilities" for extern
        VersionNumber = "3.0" for extern
        TestName = "Add the model Utilities" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

------------------------------------------------------------------------------
  ---- SYSTEM
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_System
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "System" for extern
        VersionNumber = "7.0" for extern
        TestName = "Add the model System" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

------------------------------------------------------------------------------
  ---- BODIESWITHACCOUNTS
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_BodiesWithAccounts
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "BodiesWithAccounts" for extern
        VersionNumber = "5.0" for extern
        TestName = "Add the model BodiesWithAccounts" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

------------------------------------------------------------------------------
  ---- PARSING
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_Parsing
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "Parsing" for extern
        VersionNumber = "3.0" for extern
        TestName = "Add the model Parsing" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

------------------------------------------------------------------------------
  ---- HELPLIB
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_HelpLib
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "HelpLib" for extern
        VersionNumber = "1.0" for extern
        TestName = "Add the model HelpLib" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

------------------------------------------------------------------------------
  ---- FILES
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_Files
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "Files" for extern
        VersionNumber = "3.0" for extern
        TestName = "Add the model Files" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

------------------------------------------------------------------------------
  ---- COUCHDBMANAGEMENT
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_CouchdbManagement
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "CouchdbManagement" for extern
        VersionNumber = "12.4" for extern
        TestName = "Add the model CouchdbManagement" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

------------------------------------------------------------------------------
  ---- RABBITMQ
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_RabbitMQ
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "RabbitMQ" for extern
        VersionNumber = "2.0" for extern
        TestName = "Add the model RabbitMQ" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

------------------------------------------------------------------------------
  ---- BROKERSERVICES
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_BrokerServices
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "BrokerServices" for extern
        VersionNumber = "6.1" for extern
        TestName = "Add the model BrokerServices" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version
------------------------------------------------------------------------------
  ---- HYPERCONTEXT
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_HyperContext
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "HyperContext" for extern
        VersionNumber = "1.0" for extern
        TestName = "Add the model HyperContext" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

------------------------------------------------------------------------------
  ---- INTRODUCTION
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_Introduction
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "Introduction" for extern
        VersionNumber = "1.0" for extern
        TestName = "Add the model Introduction" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

------------------------------------------------------------------------------
  ---- HELPPROJECT
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_HelpProject
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "HelpProject" for extern
        VersionNumber = "3.0" for extern
        TestName = "Add the model HelpProject" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

------------------------------------------------------------------------------
  ---- DISCONNECT
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_Disconnect
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "Disconnect" for extern
        VersionNumber = "1.1" for extern
        TestName = "Add the model Disconnect" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

------------------------------------------------------------------------------
  ---- REPOSITORYREGISTRY
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_RepositoryRegistry
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "RepositoryRegistry" for extern
        VersionNumber = "1.0" for extern
        TestName = "Add the model RepositoryRegistry" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

------------------------------------------------------------------------------
  ---- SHAREDFILESERVICES
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_SharedFileServices
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "perspectives.domains" for extern
        ModelName = "SharedFileServices" for extern
        VersionNumber = "4.0" for extern
        TestName = "Add the model SharedFileServices" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

------------------------------------------------------------------------------
  ---- MANAGE BROKER SERVICE
  ------------------------------------------------------------------------------
  case ManageBrokerService
    aspect mm:Test

    external
      state Success = exists bs:MyBrokers >> ManagedBrokers >> binding >> Name
        on entry
          do for Tester once settled
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester

      perspective on bs:BrokerService$External
        props (Url, Exchange, ManagementEndpoint, SelfRegisterEndpoint, Name) verbs (SetPropertyValue, Consult)

      perspective on cm:CouchdbServer$BespokeDatabases
        only (CreateAndFill)
        props (Endorsed, Public, EnteredDatabaseName) verbs (SetPropertyValue, Consult)

      perspective on cm:BespokeDatabase$Owner
        only (Create, Fill)

      perspective on bs:BrokerServices$ManagedBrokers
        only (Create)
        props (StorageLocation) verbs (SetPropertyValue, Consult)

      action RunTest
        letA
          -- Only when test ManageCouchdb has run, the PDR has a CouchdbServer available.
          couchdbserver <- cm:MyCouchdbApp >> CouchdbServers  >> binding >> context >>= first
          -- Create the BespokeDatabases role instance first and then set its EnteredDatabaseName property.
          -- Then, create the actual context and fill the role with it.
          -- All statements referring to publicbrokerservicedb should be postponed to the next transaction!
          publicbrokerservicedb <- create context cm:BespokeDatabase bound to cm:CouchdbServer$BespokeDatabases in couchdbserver
          owner <- create role cm:BespokeDatabase$Owner in publicbrokerservicedb >> binding >> context
          brokerservice <- create role bs:BrokerServices$ManagedBrokers in bs:MyBrokers
        in
          TestName = "Managing BrokerServices." for extern
          bind_ me to owner
          EnteredDatabaseName = "cw_bigbangsdatabase/" for publicbrokerservicedb
          Endorsed = true for publicbrokerservicedb
          -- Now state BespokeDatabase$External$CreateDb runs, creating the actual database and setting DatabaseName.
          
          Public = true for publicbrokerservicedb
          -- This sets the stage for BespokeDatabase$External$Publish to run, making the database public.
          
          once settled
            StorageLocation = owner >> cm:BespokeDatabase$Owner$BespokeDatabaseUrl for brokerservice
            -- This triggers State BrokerServices$ManagedBrokers$HasStorageLocation, which creates the BrokerService context.
          
          once settled
            Url = "wss://mycontexts.com:15673/ws" for brokerservice
            Exchange = "mycontexts" for brokerservice
            ManagementEndpoint = "https://mycontexts.com/rbmq/" for brokerservice
            SelfRegisterEndpoint = "https://mycontexts.com/rbsr/" for brokerservice
            Name = "Big Bangs BrokerService" for brokerservice

------------------------------------------------------------------------------
  ---- PUBLIC PAGES
  ---- 1. Create a public PublicPageCollections "System Pages" in hypercontext:HyperTextApp
  ---- 2. Add a PublicPages instance to the "System Pages" collection and fill it with a new PublicPage. Set its Title property to "StartPagina".
  ---- 3. Add a PublicPages instance to the "System Pages" collection and fill it with a new PublicPage. Set its Title property to "Instructions".
  ---- 4. Add a single unconditional TextBlocks instance to Instructions. Fill its MD property with content.
  ---- 5. Add three TextBlocks instances to Startpagina, each with a condition. Fill their MD properties with content.
  ----    Fill their Condition properties with appropriate conditions.
  ------------------------------------------------------------------------------
  case Add_public_pages
    aspect mm:Test

    external

      state Success = exists hyp:HyperTextApp >> hyp:HyperTexts$PublicPageCollections >> binding
        on entry
          do for Tester after 20 Milliseconds
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester

      perspective on extern

      perspective on hyp:HyperTexts$PublicPageCollections
        only (CreateAndFill)
        props (Name) verbs (SetPropertyValue, Consult)

      perspective on hyp:PublicPageCollection$Author
        only (Create, Fill)

      action RunTest
        letA
          pagecollection <- create context hyp:PublicPageCollection bound to hyp:HyperTexts$PublicPageCollections in hyp:HyperTextApp
        in
          TestName = "Creating public pages." for extern
          Name = "System Pages" for pagecollection
          bind Owner >> binding to hyp:PublicPageCollection$Author in pagecollection >> binding >> context

    user Owner = mm:RebootUniverseApp >> BespokeDatabaseOwner

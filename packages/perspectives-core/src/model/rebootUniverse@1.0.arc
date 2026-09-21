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
          -- callEffect cdb:DeleteCouchdbDatabase( url, "cw_servers_and_repositories" )
          -- Remove cw_perspectives_domains
          -- callEffect cdb:DeleteCouchdbDatabase( url, "cw_perspectives_domains" )
          -- Remove models_perspectives_domains
          -- callEffect cdb:DeleteCouchdbDatabase( url, "models_perspectives_domains" )
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
          -- Save for reference in case Add_public_pages.
          bind owner to BespokeDatabaseOwner in mm:RebootUniverseApp
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
      property CreateStartPage (Boolean)
      property CreateHelpPage (Boolean)

      state CreateStartPage = CreateStartPage
        on entry 
          do for Tester
            letA 
              pagecollection <- hyp:HyperTextApp >> hyp:HyperTexts$PublicPageCollections >> binding >> context
              page <- create context hyp:PublicPage named "StartPage" bound to hyp:PublicPageCollection$PublicPages in pagecollection
              -- Now state PublicPages$PageAvailable is triggered, creating the Page$Author in the public page.
              block1 <- create role hyp:Page$TextBlocks in page >> binding >> context
              block2 <- create role hyp:Page$TextBlocks in page >> binding >> context
              block3 <- create role hyp:Page$TextBlocks in page >> binding >> context
            in
              Title = "StartPagina" for page
              MD = <### Sign up to connect to peers
                    A Broker Service (that would allow you to connect to peers) is present in your installation, but have not yet signed up to it. Move to [[link:model://perspectives.domains#BrokerServices$MyBrokers|Broker Services Management]] page to read how to sign up.> 
                for block1
              Condition = "(exists bs:MyBrokers >> PublicBrokers) and not exists bs:MyBrokers >> Contracts" for block1
              MD = <### Get connected
                    MyContexts is most useful when you connect to other people. This installation does not yet have a means to connect to others. Move to the [[link:pub:https://perspectives.domains/cw_v74vfn21lx/#rbdaciupyn$External|Perspectives Broker Service]] page to get online. You will read further instructions there.> 
                for block2
              Condition = "not (exists bs:MyBrokers >> PublicBrokers)" for block2
              MD = <## Welcome to MyContexts!
                    Read our [[link:pub:https://perspectives.domains/cw_ro6a1vrf9y/#atog6qpw44$External|instructions]] for use if you need introductory guidance.> 
                for block3
              Condition = "true" for block3

              CreateHelpPage = true
      
      state CreateHelpPage = CreateHelpPage
        on entry 
          do for Tester
            letA 
              pagecollection <- hyp:HyperTextApp >> hyp:HyperTexts$PublicPageCollections >> binding >> context
              page <- create context hyp:PublicPage named "Instructions" bound to hyp:PublicPageCollection$PublicPages in pagecollection
              block1 <- create role hyp:Page$TextBlocks in page >> binding >> context
            in
              Title = "Instructions" for page
              MD = <## User Guide
                    This document describes how to access the functionality in the MyContexts program.

                    ### Perspectives on contexts
                    The user interface of MyContexts visualises perspectives on contexts. A context is just a collection of roles. You play a role in each context you're able to see or change. Associated with that role is a *perspective* that determines whether you can just inspect values, change them, delete them or add to them. It also determines whether you can change the _filler of a role_.

                    By opening a context (from an already open context) you navigate from screen to screen. Usually, you can open a context both in place of the open context, and on another tab or in a new browser window.

                    Apart from opening contexts, you can also open a _form_ to edit properties of a role.

                    ### Roles and Cards
                    The MyContexts interface rests on the concept of a **card**. A card represents a role instance. It can be selected, dragged and dropped. Once selected, keys or combinations of keys trigger specific behaviour on the role represented by the card.

                    #### Single (functional) roles
                    Sometimes, a context will accept just a single instance of a role. A taxi situation, for example, will usually be modelled with just a single Driver role.

                    #### Multi-Roles
                    On the other hand, there may be multiple Passengers in a taxi, hence this role would usually be modeled as a Multi-Role.

                    Visualising Multi-Roles is different than visualising functional roles. For the latter, a single card will do. A Multi-Role is visualised with a table.

                    This so-called RoleTable shows properties of the roles on the columns and an instance of the role on each row. The table will usually have a column that sports small cards, allowing you to manipulate the role as an entity (see below).

                    ### Behaviour associated with Roles
                    There are five things one can do with an existing role:

                    * **open it**. If the role represents a context, a screen is opened that gives your perspective on that context;
                    * **fill it** with another role;
                    * **fill another role** with it;
                    * **remove its filler**;
                    * **remove it from its context**, thereby destroying it (but not its filler).

                    It is up to the designer of the screens to associated zero or more of these behaviours with a given representation of that role (say, a card). For some behaviours, the user interface clearly indicates whether it is available on a particular role (e.g. by displaying a `+` button on the toolbar under a multi-role table). In some cases you'll have to find out by trying (e.g. whether you can remove a role, by selecting it and pressing `delete`).

                    ### Triggering behaviour
                    All behaviour can be triggered with both the keyboard and the mouse.

                    #### With the mouse
                    *  Click a card to select it.
                    *  Doubleclick a card to open it: for context- and external roles the context will be opened, for other roles a form to edit their properties.
                    *  Doubleclick a card while holding the `shift` or `alt` key to open it in a new window or on a new tab (which of the two will happen depends on browser type and its settings).
                    *  Drag a card and
                      * drop it on another card to fill that role with it;
                      * drop it on the Trash to remove it from its context and destroy it;
                      * drop it on the Pencil tool to edit its properties. This is useful mainly for context- and external roles (for double clicking them will open the context rather than the properties);
                      * drop it on the Unbind tool to remove its filling role;
                      * drop it on the toolbar at the top of the screen to put it on the card clipboard.
                    Cards can be dragged from one window to another.

                    #### With the keyboard
                    *  Click `tab` and `shift`-`tab` to move the focus around the screen.
                    *  Tab into a solitary card to select it.
                    *  When a card is selected
                      *  press `shift`-`space`, to open it;
                      *  press `alt`-`shift`-`space` to open it in a new window or on a new tab (which of the two will happen depends on browser type and its settings).
                      *  press `ctrl`-`c` to copy the card to the card Clipboard (in the upper left corner of the screen).
                      * press `backspace` to remove the role from its context and destroy it.
                    *  Once on the clipboard, the card can be dropped just as with the mouse:
                      * to drop it on another card, select that card and press `ctrl`-`v`;
                      * to drop it on the Pencil tool, select the Pencil tool and press `space` or `return`. This is useful mainly for context- and external roles (for double clicking them will open the context rather than the properties)(NOTE that this currently does not function: see [Issue 3](https://github.com/joopringelberg/inplace/issues/3#issue-1349288564));
                      * to drop it on the Unbind tool, select the Unbind tool and press `space` or `return`.
                      * to clear the clipboard, press `escape`.

                    #### In the RoleTable
                    RoleTables have extra ways to trigger behaviour:

                    *  use `left`- and `right` arrow keys to navigate from column to column (the `up` and `down` arrow keys will move from row to row, as in a list);
                    *  press `enter` to start editing a cell's value;
                    *  press `enter` while editing to save the changes and return to the navigating mode;
                    *  press `escape` while editing to discard the changes and return to the navigating mode;
                    *  press `shift`-`space` to select the row. this shows up as a selected card in the card column;

                    Using the mouse:

                    *  click any cell to select it;
                    *  `shift`-click a row to select it (that is, the card in the card column).

                    Notice that a selected card in the table supports the same keyboard triggers as a selected solitary card or a selected card in a list.

                    The table will 'remember' the selected cell as long as it is on the screen (it loses that memory on navigating to another context or when opening a role form). This memory shows up when you tab into the table.

                    #### Table controls
                    A table displays a toolbar just below it. This toolbar

                    * has a small card item that represents the selected row;
                    * has a clipboard icon that, when not greyed out, allows one to paste the role on the clipboard into the current role as filler;
                    * may have a drop-down menu with a thunderbolt icon. This menu holds row-specific actions you can perform.
                    + may have a drop-down menu with a plus icon. This menu holds role or context types. By selecting an entry you add a new row to the table representing an instance of the type.
                    * will have an _external link_ icon if the row represents a context that has a public perspective on it. By clicking the icon, you will open a new browser tab with MyContexts showing the public version of the context.

                    #### In the File component
                    Properties can have a range of type `File`. In a form, such properties are displayed as two fields and, depending on state, one or two buttons. 

                    The component has four states:

                    * empty
                    * filled
                    * readonly
                    * editable

                    The *empty* state displays a name field and a MIME type field, both plain string types (but only values that match the regular expression mentioned above are accepted as MIME type). It also displays an upload icon button. When a name and MIME type are entered for the first time, the control creates and stores a new file. The state then becomes `filled`. In the empty state, the component also functions as a dropzone (one can drop a file on it).

                    * On tabbing into the control, by default, the cursor will be in the name field (other than in *filled* state, the control does not have to be unlocked for editing when *empty*)
                    * press `right arrow` to move to the MIME field. Changes to the name will be preserved temporarily. Pressing `right arrow` again will focus on the upload icon button; pressing `right arrow` again moves the cursor back to the name field; changes to the MIME field are preserved temporarily.
                    * Press `space` when the focus is on the upload button to open the file selector dialog.
                    * Press `escape` to discard all changes.
                    * Press `enter` while in any field to actually save changes and to move the control to *filled* state. 

                    NOTE: The use of the left-arrow key is consistent with the way one can move through a table. However, as a consequence, one cannot move through _the text_ that has been entered in the control with the left-arrow key.

                    The *filled* state shows the name and mime type (and neither is editable). In this state, the control is draggable if a url is available (the payload will be a standard HTML File object). It is also a dropzone for such objects. 

                    NOTE: the draggable interface has not yet been implemented.

                    * The control state can be moved from *filled* to *editable* by selecting it and then pressing `enter`. The cursor will then be in the name field. The MIME value cannot be edited.
                    * The download button can be selected if a url is available.
                    * Press `space` on the download button to activate it.

                    *readonly* is like *filled*, but without the possibility to move to *editable*. If there is an url in the property value, the end user will be able to download the file.

                    When *editable*, the control displays two buttons: one to download the file, one to upload it. Both can be activated by selecting and pressing `space`. The name of the file may be changed; its MIME type cannot. Move from button to button or field by pressing `left arrow`. 

                    * Uploading a file will move the control back to *filled* state (after preserving changes).
                    * Pressing `enter` will preserve a change to the file name and move the control back to *filled* state.
                    * Pressing `escape` will discard changes and move the control back to *filled* state.

                    NOTE: When the user has not yet changed the file name, pressing `enter` has no effect. Press `escape` to leave the control.

                    In all states, the download button is only enabled if the control has a value for the database for the file. This will be 

                    * after creating a new file and 
                    * after uploading a file 
                    * when the property value coming through the PDR API contains the serialised structure that contains a database value.

                    ### Open a context from the navigation bar
                    The MyContexts application is opened on the url `https://www.mycontexts.com`. This will land you on the standard entry page that lets you choose an App from those listed in a bar on the left. However, it is also possible to jump right into a specific context by entering the name of that context right after the url. For example, enter `https://www.mycontexts.com?MySystem` to open just the MySystem context (showing you the Apps you've installed and those that are available in the Repository).

                    The name you enter after the question mark is matched to _indexed context names_ defined in models. Examples of indexed context names are:
                    *  MySystem
                    *  MyChats
                    *  MyManagedModels
                    *  MyBrokerServices

                    Just entering 'System' would be enough to land you in MySystem. If you enter 'My', however, you'll be presented the four names above and you can choose by clicking one of them.

                    NOTE: this currently does not work. See [Issue 4](https://github.com/joopringelberg/inplace/issues/4#issue-1349306843)

                    ### Navigating using the browser's tools
                    MyContexts stores the history of contexts that you've visited, in the browser's history. Consequently, you can navigate back (and forwards) using the browser's tools such as the navigation buttons.

                    Logging in to MyContexts is not, in this sense, a navigation action. Only when you open the System context does navigation start. As a reminder: the top entry in the browser's history list is the context you currently have on screen.

                    When you explicitly close a context (using the button in the menu), a new item is added to history with the title \"Closed {contextname}\" (where {contextname} has been replaced with the title of the context). If you navigate back from such a situation, you in effect re-open that context. This is useful when you have closed MyContexts. After logging in, by navigating back you pick up exactly where you left.

                    When you try to navigate away from MyContexts (e.g. by closing the tab) while a context is open, a warning is issued. Best practice is to first close the context and then navigate away (if you don't, others will think you still have that context open (in some situations, they can see that status)).

                    Finally, when you have navigated back to the point that the next time you go back would make you leave MyContexts, you'll get a warning if you try. It will say something to the effect of \"Leave site?\" (the exact wording is determined by the browser's makers). If you really want to exit, press `Leave`. You'll have to log in again if you want to resume MyContexts. Otherwise, press `Cancel`.> 
                for block1
              Condition = "true" for block1

              once settled
                TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester

      perspective on extern
        props (CreateStartPage, CreateHelpPage) verbs (SetPropertyValue, Consult)

      perspective on hyp:HyperTexts$PublicPageCollections
        only (CreateAndFill)
        props (Name) verbs (SetPropertyValue, Consult)

      perspective on hyp:PublicPageCollection$Author
        only (Create, Fill)
      
      perspective on hyp:PublicPageCollection$PublicPages
        only (CreateAndFill)
        props (Title) verbs (SetPropertyValue, Consult)
      
      perspective on hyp:Page$TextBlocks
        only (Create)
        props (Title, MD, Condition) verbs (SetPropertyValue, Consult)

      action RunTest
        letA
          pagecollection <- create context hyp:PublicPageCollection bound to hyp:HyperTexts$PublicPageCollections in hyp:HyperTextApp
        in
          TestName = "Creating public pages." for extern
          Name = "System Pages" for pagecollection
          bind Owner >> binding to hyp:PublicPageCollection$Author in pagecollection >> binding >> context
          -- Now PublicPageCollection$Author is bound, we can create the PublicPages (Author provides the BespokeDatabase to publish to).
          CreateStartPage = true for extern


    user Owner = mm:RebootUniverseApp >> BespokeDatabaseOwner

-- "model://joopringelberg.nl#p80ohyse8t@2.0"
domain model://joopringelberg.nl#RebootUniverse@2.0
  use sys for model://perspectives.domains#System
  use ru for model://joopringelberg.nl#RebootUniverse
  use sensor for model://perspectives.domains#Sensor
  use cdb for model://perspectives.domains#Couchdb
  use cm for model://perspectives.domains#CouchdbManagement
  use p for model://perspectives.domains#Parsing
  use hyp for model://perspectives.domains#HyperContext
  use bs for model://perspectives.domains#BrokerServices
  use util for model://perspectives.domains#Utilities
  use mm for model://joopringelberg.nl#RepositoryTools@1.0
  use rr for model://perspectives.domains#RepositoryRegistry

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
          IsSystemModel = false for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (ru:RebootUniverseApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (ru:RebootUniverseApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestApp
    indexed ru:RebootUniverseApp
    aspect sys:RootContext
    external
      property BigBangFinished (Boolean)
    
    user Manager = sys:Me
      perspective on Tests
        only (CreateAndFill, RemoveContext)
      perspective on Tests >> binding >> context >> Tester
        only (Create, Fill)
          
    -- To execute any test, run the action RunTest in the first PDR.
    -- To check if a test has succeeded, retrieve the value of TestSucceeded in the second PDR.
    context Tests (relational) filledBy mm:Test

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
        ModelVersions = "Couchdb=4.0; Serialise=3.0; Sensor=3.0; Utilities=3.0" for extern

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
        VersionNumber = "7.0" for extern
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
  ---- REPOSITORY TOOLS
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_RepositoryTools
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "joopringelberg.nl" for extern
        ModelName = "RepositoryTools" for extern
        VersionNumber = "1.0" for extern
        TestName = "Add the model RepositoryTools" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

  ------------------------------------------------------------------------------
  ---- REBOOT UNIVERSE
  ---- This case can be used as an aspect to create individual tests for concrete models.
  ------------------------------------------------------------------------------
  case AddModel_RebootUniverse
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        -- Set these in the specialised versions.
        NameSpace = "joopringelberg.nl" for extern
        ModelName = "RebootUniverse" for extern
        VersionNumber = "2.0" for extern
        TestName = "Add the model RebootUniverse" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

  ------------------------------------------------------------------------------
  ---- TEST MODELS
  ---- Add the test models used by the perspectives-core suites to the repository.
  ------------------------------------------------------------------------------
  case AddModel_SynchronisationTestModel
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        NameSpace = "joopringelberg.nl" for extern
        ModelName = "SynchronisationTestModel" for extern
        VersionNumber = "2.0" for extern
        TestName = "Add the model SynchronisationTestModel" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

  case AddModel_TwoPDRDestructiveTests
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        NameSpace = "joopringelberg.nl" for extern
        ModelName = "TwoPDRDestructiveTests" for extern
        VersionNumber = "1.0" for extern
        TestName = "Add the model TwoPDRDestructiveTests" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

  case AddModel_StateTestModel
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        NameSpace = "joopringelberg.nl" for extern
        ModelName = "StateTestModel" for extern
        VersionNumber = "1.0" for extern
        TestName = "Add the model StateTestModel" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

  case AddModel_SinglePDRDestructiveTests
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        NameSpace = "joopringelberg.nl" for extern
        ModelName = "SinglePDRDestructiveTests" for extern
        VersionNumber = "2.0" for extern
        TestName = "Add the model SinglePDRDestructiveTests" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

  case AddModel_TransactionExecutionTests
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        NameSpace = "joopringelberg.nl" for extern
        ModelName = "TransactionExecutionTests" for extern
        VersionNumber = "1.0" for extern
        TestName = "Add the model TransactionExecutionTests" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

  case AddModel_AMQPtestSetup
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        NameSpace = "joopringelberg.nl" for extern
        ModelName = "AMQPtestSetup" for extern
        VersionNumber = "1.0" for extern
        TestName = "Add the model AMQPtestSetup" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

  case AddModel_AMQPtestModel
    aspect mm:AddModel

    user Tester
      aspect mm:Test$Tester
      aspect mm:AddModel$Tester

      action RunTest
        NameSpace = "joopringelberg.nl" for extern
        ModelName = "AMQPtestModel" for extern
        VersionNumber = "1.0" for extern
        TestName = "Add the model AMQPtestModel" for extern

        bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
        StartTest = true for extern

    aspect context mm:AddModel$Repository
    aspect context mm:AddModel$Manifest
    aspect context mm:AddModel$Version

  -- case AddModel_TestModelDependencies
  --   aspect mm:AddModel

  --   user Tester
  --     aspect mm:Test$Tester
  --     aspect mm:AddModel$Tester

  --     action RunTest
  --       NameSpace = "joopringelberg.nl" for extern
  --       ModelName = "TestModelDependencies" for extern
  --       VersionNumber = "1.0" for extern
  --       TestName = "Add the model TestModelDependencies" for extern

  --       bind cm:MyCouchdbApp >> (filter CouchdbServers >> binding >> context >> Repositories with (Repositories$NameSpace == origin >> extern >> NameSpace)) >> binding >>= first to Repository
  --       StartTest = true for extern

  --   aspect context mm:AddModel$Repository
  --   aspect context mm:AddModel$Manifest
  --   aspect context mm:AddModel$Version

  ------------------------------------------------------------------------------
  ---- MANAGE BROKER SERVICE
  ---- Creates a BrokerService that is available as a public resource with identifier
  ---- "https://perspectives.domains/cw_bigbangsdatabase/#BigBangsBrokerService"

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

      perspective on bs:BrokerServices$ManagedBrokers
        only (Create)
        props (StorageLocation, GivenIdentifier) verbs (SetPropertyValue, Consult)

      action RunTest
        letA
          brokerservice <- create role bs:BrokerServices$ManagedBrokers in bs:MyBrokers
        in
          TestName = "Managing BrokerServices." for extern
          GivenIdentifier = "BigBangsBrokerService" for brokerservice
          StorageLocation = Owner >> cm:BespokeDatabase$Owner$BespokeDatabaseUrl for brokerservice
          -- This triggers State BrokerServices$ManagedBrokers$HasStorageLocation, which creates the BrokerService context.
          
          once settled
            Url = "wss://mycontexts.com:15673/ws" for brokerservice
            Exchange = "mycontexts" for brokerservice
            ManagementEndpoint = "https://mycontexts.com/rbmq/" for brokerservice
            SelfRegisterEndpoint = "https://mycontexts.com/rbsr/" for brokerservice
            Name = "Big Bangs BrokerService" for brokerservice
      
    user Owner = mm:RepositoryToolsApp >> mm:TestApp$BespokeDatabaseOwner

  ------------------------------------------------------------------------------
  ---- PUBLIC PAGES
  ---- 1. Create a public PublicPageCollections "System Pages" in hypercontext:HyperTextApp
  ---- 2. Add a PublicPages instance to the "System Pages" collection and fill it with a new PublicPage. Set its Title property to "StartPagina".
  ---- 3. Add a PublicPages instance to the "System Pages" collection and fill it with a new PublicPage. Set its Title property to "Instructions".
  ---- 4. Add a single unconditional TextBlocks instance to Instructions. Fill its MD property with content.
  ---- 5. Add three TextBlocks instances to Startpagina, each with a condition. Fill their MD properties with content.
  ----    Fill their Condition properties with appropriate conditions.
  ---- These pages have fixed identifiers:
  ----    * StartPagina:  pub:https://perspectives.domains/cw_bigbangsdatabase/#StartPage
  ----    * Instructions: pub:https://perspectives.domains/cw_bigbangsdatabase/#Instructions
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
                    MyContexts is most useful when you connect to other people. This installation does not yet have a means to connect to others. Move to the [[link:pub:https://perspectives.domains/cw_bigbangsdatabase/#BigBangsBrokerService$External|Perspectives Broker Service]] page to get online. You will read further instructions there.> 
                for block2
              Condition = "not (exists bs:MyBrokers >> PublicBrokers)" for block2
              MD = <## Welcome to MyContexts!
                    Read our [[link:pub:https://perspectives.domains/cw_bigbangsdatabase/#Instructions$External|instructions]] for use if you need introductory guidance.> 
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


    user Owner = mm:RepositoryToolsApp >> mm:TestApp$BespokeDatabaseOwner

  ------------------------------------------------------------------------------
  ---- CREATE THE REPOSITORY REGISTRY PUBLIC PAGE
  ---- Re-use bigbangsdatabase for the repository registry public page.
  ---- Create a PublicRepositoryOverview named "RepositoryRegistry". Then create an instance of the Manager role and fill it with Owner.
  ---- Finally, add the Perspectives.Domains Repository to the public page. Its identifier is pub:https://perspectives.domains/cw_servers_and_repositories/#perspectives_domains
  ------------------------------------------------------------------------------
  case CreateRepositoryRegistryPublicPage
    aspect mm:Test

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester

      perspective on rr:RepositoryOverview$TheRegistry
        only (CreateAndFill)
        props (Name) verbs (Consult, SetPropertyValue)
      
      perspective on rr:PublicRepositoryOverview$Manager
        only (Create, Fill)
      
      perspective on rr:PublicRepositoryOverview$Repositories
        only (Create, Fill)
      
      perspective on ru:RebootUniverseApp >> External
        props (BigBangFinished) verbs (SetPropertyValue)

      action RunTest
        letA
          publicrepositoryoverview <- create context rr:PublicRepositoryOverview named "RepositoryRegistry" bound to rr:RepositoryOverview$TheRegistry in rr:MyRepositoryOverview
        in
          TestName = "Create Repository Registry Public Page" for extern
          Name = "Repository Registry" for publicrepositoryoverview
          bind Owner >> binding to rr:PublicRepositoryOverview$Manager in publicrepositoryoverview >> binding >> context
          bind publicrole pub:https://perspectives.domains/cw_servers_and_repositories/#perspectives_domains$External (cm:Repository) to Repositories in publicrepositoryoverview >> binding >> context
          bind publicrole pub:https://perspectives.domains/cw_servers_and_repositories/#joopringelberg_nl$External (cm:Repository) to Repositories in publicrepositoryoverview >> binding >> context

          once settled
            TestSucceeded = true for extern
            -- NOTICE THAT WE SIGNAL THAT THE BIG BANG HAS FINISHED WHEN WE HAVE THE PUBLIC PAGES.
            -- THIS SIGNPOST MAY BE MOVED, EG TO AFTER SIGNING UP TO THE BROKER SERVICE.
            BigBangFinished = true for ru:RebootUniverseApp >> extern

    user Owner = mm:RepositoryToolsApp >> mm:TestApp$BespokeDatabaseOwner

  ------------------------------------------------------------------------------
  ---- SIGN UP TO BROKERSERVICE
  ---- Add the BrokerService with the following public resource identifier:
  ---- "https://perspectives.domains/cw_bigbangsdatabase/#BigBangsBrokerService"
  ---- Then Signup.
  ------------------------------------------------------------------------------
  case SignUpToBrokerService
    aspect mm:Test

    external
      property PublicServiceAvailable (Boolean)

      state Signup = PublicServiceAvailable
        on entry
          do for Tester once settled
            letA
              brokerservice <- bs:MyBrokers >> PublicBrokers >> binding >> context
              accountsinstance <- create context bs:BrokerContract bound to Accounts in brokerservice
            in
              bind me to AccountHolder in accountsinstance >> binding >> context
              -- Administrator from BrokerService fills Administrator in contract.
              bind accountsinstance >> context >> Administrator to Administrator in accountsinstance >> binding >> context
              -- Save for reference in state Success.
              bind accountsinstance >> binding to MyContract in context

      state Success = context >> MyContract >> Registered
        on entry
          do for Tester once settled
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester

      perspective on bs:BrokerService$Accounts
        only (CreateAndFill, Fill)
      
      perspective on bs:BrokerContract$AccountHolder
        only (CreateAndFill, Fill)
      
      perspective on bs:BrokerContract$Administrator
        only (CreateAndFill, Fill)
      
      perspective on bs:BrokerServices$PublicBrokers
        only (CreateAndFill, Fill)
      
      perspective on extern
        props (PublicServiceAvailable) verbs (SetPropertyValue)

      perspective on MyContract
        only (CreateAndFill, Fill)

      action RunTest
        letA
          brokerservice <- publicrole pub:https://perspectives.domains/cw_bigbangsdatabase/#BigBangsBrokerService$External (bs:BrokerService)
        in
          TestName = "Sign up to Broker Service" for extern
          bind brokerservice to PublicBrokers in bs:MyBrokers

          once settled
            PublicServiceAvailable = true for extern
    
    context MyContract filledBy bs:BrokerContract

  ------------------------------------------------------------------------------
  ---- ADD ALL EXTRA MODELS THAT NEED TO BE INCLUDED IN THE REBOOT UNIVERSE TO THIS INSTALLATION
  ---- We should use the Stable ModelUris.
  ---- Use the perspectives.domains and joopringelberg.nl repositories, remote or rebuilt locally.
  ---- Take the Manifest with a given readable ModelUri (filter on ModelURIReadable), then concatenate its
  ----  * ModelURI
  ----  * VersionToInstall
  ---- and call cdb:AddModelToLocalStore on the result.
  ------------------------------------------------------------------------------
  case AddExtraModels
    aspect mm:Test

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester

      action RunTest
        letA
          repository <- publicrole pub:https://perspectives.domains/cw_servers_and_repositories/#perspectives_domains$External (cm:Repository) >> context

          rabbitmqmanifest <- (filter (repository >> Manifests) with ModelURIReadable == "model://perspectives.domains#RabbitMQ") >>= first
          rabbitmqmodeluri <- rabbitmqmanifest >> ModelURI + "@" + rabbitmqmanifest >> VersionToInstall

          brokerservicesmanifest <- (filter (repository >> Manifests) with ModelURIReadable == "model://perspectives.domains#BrokerServices") >>= first
          brokerservicesmodeluri <- brokerservicesmanifest >> ModelURI + "@" + brokerservicesmanifest >> VersionToInstall
          
          hypercontextmanifest <- (filter (repository >> Manifests) with ModelURIReadable == "model://perspectives.domains#HyperContext") >>= first
          hypercontextmodeluri <- hypercontextmanifest >> ModelURI + "@" + hypercontextmanifest >> VersionToInstall

          introductionmanifest <- (filter (repository >> Manifests) with ModelURIReadable == "model://perspectives.domains#Introduction") >>= first
          introductionmodeluri <- introductionmanifest >> ModelURI + "@" + introductionmanifest >> VersionToInstall

          helpprojectmanifest <- (filter (repository >> Manifests) with ModelURIReadable == "model://perspectives.domains#HelpProject") >>= first
          helpprojectmodeluri <- helpprojectmanifest >> ModelURI + "@" + helpprojectmanifest >> VersionToInstall

          disconnectmanifest <- (filter (repository >> Manifests) with ModelURIReadable == "model://perspectives.domains#Disconnect") >>= first
          disconnectmodeluri <- disconnectmanifest >> ModelURI + "@" + disconnectmanifest >> VersionToInstall

          repositoryregistrymanifest <- (filter (repository >> Manifests) with ModelURIReadable == "model://perspectives.domains#RepositoryRegistry") >>= first
          repositoryregistrymodeluri <- repositoryregistrymanifest >> ModelURI + "@" + repositoryregistrymanifest >> VersionToInstall

          sharedfileservicesmanifest <- (filter (repository >> Manifests) with ModelURIReadable == "model://perspectives.domains#SharedFileServices") >>= first
          sharedfileservicesmodeluri <- sharedfileservicesmanifest >> ModelURI + "@" + sharedfileservicesmanifest >> VersionToInstall

          testrepository <- publicrole pub:https://joopringelberg.nl/cw_servers_and_repositories/#joopringelberg_nl$External (cm:Repository) >> context

          synchronisationtestmodelmanifest <- (filter (testrepository >> Manifests) with ModelURIReadable == "model://joopringelberg.nl#SynchronisationTestModel") >>= first
          synchronisationtestmodelmodeluri <- synchronisationtestmodelmanifest >> ModelURI + "@" + synchronisationtestmodelmanifest >> VersionToInstall

          twopdrdestructivetestsmanifest <- (filter (testrepository >> Manifests) with ModelURIReadable == "model://joopringelberg.nl#TwoPDRDestructiveTests") >>= first
          twopdrdestructivetestsmodeluri <- twopdrdestructivetestsmanifest >> ModelURI + "@" + twopdrdestructivetestsmanifest >> VersionToInstall

          statetestmodelmanifest <- (filter (testrepository >> Manifests) with ModelURIReadable == "model://joopringelberg.nl#StateTestModel") >>= first
          statetestmodelmodeluri <- statetestmodelmanifest >> ModelURI + "@" + statetestmodelmanifest >> VersionToInstall

          singlepdrdestructivetestsmanifest <- (filter (testrepository >> Manifests) with ModelURIReadable == "model://joopringelberg.nl#SinglePDRDestructiveTests") >>= first
          singlepdrdestructivetestsmodeluri <- singlepdrdestructivetestsmanifest >> ModelURI + "@" + singlepdrdestructivetestsmanifest >> VersionToInstall

          transactionexecutiontestsmanifest <- (filter (testrepository >> Manifests) with ModelURIReadable == "model://joopringelberg.nl#TransactionExecutionTests") >>= first
          transactionexecutiontestsmodeluri <- transactionexecutiontestsmanifest >> ModelURI + "@" + transactionexecutiontestsmanifest >> VersionToInstall

          amqptestmodelmanifest <- (filter (testrepository >> Manifests) with ModelURIReadable == "model://joopringelberg.nl#AMQPtestModel") >>= first
          amqptestmodelmodeluri <- amqptestmodelmanifest >> ModelURI + "@" + amqptestmodelmanifest >> VersionToInstall

        in
          callEffect cdb:AddModelToLocalStore( rabbitmqmodeluri )
          callEffect cdb:AddModelToLocalStore( brokerservicesmodeluri )
          callEffect cdb:AddModelToLocalStore( hypercontextmodeluri )
          callEffect cdb:AddModelToLocalStore( introductionmodeluri )
          callEffect cdb:AddModelToLocalStore( helpprojectmodeluri )
          callEffect cdb:AddModelToLocalStore( disconnectmodeluri )
          callEffect cdb:AddModelToLocalStore( repositoryregistrymodeluri )
          callEffect cdb:AddModelToLocalStore( sharedfileservicesmodeluri )
          callEffect cdb:AddModelToLocalStore( synchronisationtestmodelmodeluri )
          callEffect cdb:AddModelToLocalStore( twopdrdestructivetestsmodeluri )
          callEffect cdb:AddModelToLocalStore( statetestmodelmodeluri )
          callEffect cdb:AddModelToLocalStore( singlepdrdestructivetestsmodeluri )
          callEffect cdb:AddModelToLocalStore( transactionexecutiontestsmodeluri )
          callEffect cdb:AddModelToLocalStore( amqptestmodelmodeluri )

          once settled
            TestSucceeded = true for extern

  ------------------------------------------------------------------------------
  ---- BIG BANG: ONE CASE TO RULE THEM ALL
  ---- This is the ultimate test case that ensures all necessary models are added to the local store.
  ----  * Create an instance of each test to run;
  ----  * Instantiate and Fill the Tester role
  ----  * execute the RunTest action for the Tester role
  ------------------------------------------------------------------------------

  case ExecuteBigBang
    aspect mm:Test

    external
      state Success = binder ru:TestApp$Tests >> context >> extern >> BigBangFinished
        on entry
          do for Tester once settled
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester

      perspective on ru:RebootUniverseApp >> Tests
        only (CreateAndFill, Fill)

      action RunTest
        letA
          cleanup <- create context mm:Cleanup bound to Tests in ru:RebootUniverseApp
          managecouchdb <- create context mm:ManageCouchdb bound to Tests in ru:RebootUniverseApp
          createbigbangsdatabase <- create context mm:CreateBigBangsDatabase bound to Tests in ru:RebootUniverseApp
          createperspectivesdomainsrepository <- create context mm:CreatePerspectivesDomainsRepository bound to Tests in ru:RebootUniverseApp
          createjoopringelbergnlrepository <- create context mm:CreateJoopringelbergNlRepository bound to Tests in ru:RebootUniverseApp

          addmodelcouchdb <- create context AddModel_Couchdb bound to Tests in ru:RebootUniverseApp
          addmodelserialise <- create context AddModel_Serialise bound to Tests in ru:RebootUniverseApp
          addmodelsensor <- create context AddModel_Sensor bound to Tests in ru:RebootUniverseApp
          addmodelutilities <- create context AddModel_Utilities bound to Tests in ru:RebootUniverseApp
          addmodelsystem <- create context AddModel_System bound to Tests in ru:RebootUniverseApp
          addmodelbodieswithaccounts <- create context AddModel_BodiesWithAccounts bound to Tests in ru:RebootUniverseApp
          addmodelparsing <- create context AddModel_Parsing bound to Tests in ru:RebootUniverseApp
          addmodelhelplib <- create context AddModel_HelpLib bound to Tests in ru:RebootUniverseApp
          addmodelfiles <- create context AddModel_Files bound to Tests in ru:RebootUniverseApp
          addmodelcouchdbmanagement <- create context AddModel_CouchdbManagement bound to Tests in ru:RebootUniverseApp
          addmodelbrokerservices <- create context AddModel_BrokerServices bound to Tests in ru:RebootUniverseApp
          addmodelrabbitmq <- create context AddModel_RabbitMQ bound to Tests in ru:RebootUniverseApp
          addmodelhypercontext <- create context AddModel_HyperContext bound to Tests in ru:RebootUniverseApp
          addmodelintroduction <- create context AddModel_Introduction bound to Tests in ru:RebootUniverseApp
          addmodelhelpproject <- create context AddModel_HelpProject bound to Tests in ru:RebootUniverseApp
          addmodeldisconnect <- create context AddModel_Disconnect bound to Tests in ru:RebootUniverseApp
          addmodelrepositoryregistry <- create context AddModel_RepositoryRegistry bound to Tests in ru:RebootUniverseApp
          addmodelsharedfileservices <- create context AddModel_SharedFileServices bound to Tests in ru:RebootUniverseApp
          addmodelrepositorytools <- create context AddModel_RepositoryTools bound to Tests in ru:RebootUniverseApp
          addmodelrebootuniverse <- create context AddModel_RebootUniverse bound to Tests in ru:RebootUniverseApp
          addmodelsynchronisationtestmodel <- create context AddModel_SynchronisationTestModel bound to Tests in ru:RebootUniverseApp
          addmodeltwopdrdestructivetests <- create context AddModel_TwoPDRDestructiveTests bound to Tests in ru:RebootUniverseApp
          addmodelstatetestmodel <- create context AddModel_StateTestModel bound to Tests in ru:RebootUniverseApp
          addmodelsinglepdrdestructivetests <- create context AddModel_SinglePDRDestructiveTests bound to Tests in ru:RebootUniverseApp
          addmodeltransactionexecutiontests <- create context AddModel_TransactionExecutionTests bound to Tests in ru:RebootUniverseApp
          addmodelamqptestmodel <- create context AddModel_AMQPtestModel bound to Tests in ru:RebootUniverseApp
          addmodelamqptestsetup <- create context AddModel_AMQPtestSetup bound to Tests in ru:RebootUniverseApp

          managebrokerservice <- create context ManageBrokerService bound to Tests in ru:RebootUniverseApp
          addpublicpages <- create context Add_public_pages bound to Tests in ru:RebootUniverseApp
          createrepositoryregistrypublicpage <- create context CreateRepositoryRegistryPublicPage bound to Tests in ru:RebootUniverseApp
          signup <- create context SignUpToBrokerService bound to Tests in ru:RebootUniverseApp

        in
          TestName = "Big Bang" for extern
          -- The Tester role of each newly created test context is filled by the on entry action of mm:Test.
          -- That only happens when the transaction settles, so the sub-tests must run in a later stage.
          once settled
            runContextAction RunTest for Tester in cleanup >> binding >> context
            runContextAction RunTest for Tester in managecouchdb >> binding >> context
            runContextAction RunTest for Tester in createbigbangsdatabase >> binding >> context
            runContextAction RunTest for Tester in createperspectivesdomainsrepository >> binding >> context
            runContextAction RunTest for Tester in createjoopringelbergnlrepository >> binding >> context

            runContextAction RunTest for Tester in addmodelcouchdb >> binding >> context
            runContextAction RunTest for Tester in addmodelserialise >> binding >> context
            runContextAction RunTest for Tester in addmodelsensor >> binding >> context
            runContextAction RunTest for Tester in addmodelutilities >> binding >> context
            runContextAction RunTest for Tester in addmodelsystem >> binding >> context
            runContextAction RunTest for Tester in addmodelbodieswithaccounts >> binding >> context
            runContextAction RunTest for Tester in addmodelparsing >> binding >> context
            runContextAction RunTest for Tester in addmodelhelplib >> binding >> context
            runContextAction RunTest for Tester in addmodelfiles >> binding >> context
            runContextAction RunTest for Tester in addmodelcouchdbmanagement >> binding >> context
            runContextAction RunTest for Tester in addmodelbrokerservices >> binding >> context
            runContextAction RunTest for Tester in addmodelrabbitmq >> binding >> context
            runContextAction RunTest for Tester in addmodelhypercontext >> binding >> context
            runContextAction RunTest for Tester in addmodelintroduction >> binding >> context
            runContextAction RunTest for Tester in addmodelhelpproject >> binding >> context
            runContextAction RunTest for Tester in addmodeldisconnect >> binding >> context
            runContextAction RunTest for Tester in addmodelrepositoryregistry >> binding >> context
            runContextAction RunTest for Tester in addmodelsharedfileservices >> binding >> context
            runContextAction RunTest for Tester in addmodelrepositorytools >> binding >> context
            runContextAction RunTest for Tester in addmodelrebootuniverse >> binding >> context
            runContextAction RunTest for Tester in addmodelsynchronisationtestmodel >> binding >> context
            runContextAction RunTest for Tester in addmodeltwopdrdestructivetests >> binding >> context
            runContextAction RunTest for Tester in addmodelstatetestmodel >> binding >> context
            runContextAction RunTest for Tester in addmodelsinglepdrdestructivetests >> binding >> context
            runContextAction RunTest for Tester in addmodeltransactionexecutiontests >> binding >> context
            runContextAction RunTest for Tester in addmodelamqptestmodel >> binding >> context
            runContextAction RunTest for Tester in addmodelamqptestsetup >> binding >> context

            runContextAction RunTest for Tester in managebrokerservice >> binding >> context
            runContextAction RunTest for Tester in addpublicpages >> binding >> context
            runContextAction RunTest for Tester in createrepositoryregistrypublicpage >> binding >> context
            runContextAction RunTest for Tester in signup >> binding >> context

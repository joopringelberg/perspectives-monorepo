-- "model://joopringelberg.nl#ncr77pkxia"
domain model://joopringelberg.nl#RepositoryTools@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#RepositoryTools
  use sensor for model://perspectives.domains#Sensor
  use cdb for model://perspectives.domains#Couchdb
  use cm for model://perspectives.domains#CouchdbManagement
  use p for model://perspectives.domains#Parsing
  use hyp for model://perspectives.domains#HyperContext
  use bs for model://perspectives.domains#BrokerServices
  use util for model://perspectives.domains#Utilities

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
          Name = "Repository Tools Library" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:RepositoryToolsApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:RepositoryToolsApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestApp
    indexed mm:RepositoryToolsApp
    aspect sys:RootContext
    external
    
    user Manager = me
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
          callEffect cdb:DeleteCouchdbDatabase( url, "cw_joopringelberg_nl" )
          callEffect cdb:DeleteCouchdbDatabase( url, "models_joopringelberg_nl" )
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
          callEffect cdb:AddCredentials( "https://joopringelberg.nl/", "alice", "alice" )

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
  ---- Notice that the Repository will be identified by its NameSpace property, where dots are replaced by underscores.
  ---- So this case produces repository perspectives_domains, identified by pub:https://perspectives.domains/cw_servers_and_repositories/#perspectives_domains
  ------------------------------------------------------------------------------
  case CreatePerspectivesDomainsRepository
    aspect mm:Test

    external
      state Success =
          letE 
            couchdbserver <- cm:MyCouchdbApp >> CouchdbServers >> binding >> context
            repo <- filter couchdbserver >> Repositories >> binding with NameSpace == "perspectives.domains"
          in
            (exists couchdbserver >> Admin) and (exists repo) and (exists repo >> context >> Admin)
        
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
          TestName = "CreatePerspectivesDomainsRepository - create the repository perspectives.domains." for extern
          NameSpace = "perspectives.domains" for repo
          AdminEndorses = true for repo

  ------------------------------------------------------------------------------
  ---- CREATE REPOSITORY JOOPRINGELBERG.NL
  ---- 1. Create a repository role.
  ---- 2. Set the NameSpace property to "joopringelberg.nl"
  ---- 3. Set the AdminEndorses property to true.
  ---- Notice that the Repository will be identified by its NameSpace property, where dots are replaced by underscores.
  ---- So this case produces repository joopringelberg_nl, identified by pub:https://joopringelberg.nl/cw_servers_and_repositories/#joopringelberg_nl
  ------------------------------------------------------------------------------
  case CreateJoopringelbergNlRepository
    aspect mm:Test

    external
      state Success = 
          letE 
            couchdbserver <- cm:MyCouchdbApp >> CouchdbServers >> binding >> context
            repo <- filter couchdbserver >> Repositories >> binding with NameSpace == "joopringelberg.nl"
          in
            (exists couchdbserver >> Admin) and (exists repo) and (exists repo >> context >> Admin)
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
          TestName = "CreateJoopringelbergNlRepository - create the repository joopringelberg.nl." for extern
          NameSpace = "joopringelberg.nl" for repo
          AdminEndorses = true for repo

  ------------------------------------------------------------------------------
  ---- CREATE BIGBANGSDATABASE
  ---- Creates a Database with the stable name "cw_bigbangsdatabase".
  ---- "https://perspectives.domains/cw_bigbangsdatabase"
  ---- We use this to store the public pages Introduction and Instructions in, and
  ---- the BrokerService public page and the RepositoryRegistry public page.

  ------------------------------------------------------------------------------
  case CreateBigBangsDatabase
    aspect mm:Test

    external
      state Success = (exists mm:RepositoryToolsApp >> BespokeDatabaseOwner >> BespokeDatabaseUrl) and
        callExternal cdb:DatabaseExists( "https://perspectives.domains/", "cw_bigbangsdatabase" ) returns Boolean
        on entry
          do for Tester once settled
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester

      perspective on cm:CouchdbServer$BespokeDatabases
        only (CreateAndFill)
        props (Endorsed, Public, EnteredDatabaseName) verbs (SetPropertyValue, Consult)

      perspective on cm:BespokeDatabase$Owner
        only (Create, Fill)

      action RunTest
        letA
          -- Only when test ManageCouchdb has run, the PDR has a CouchdbServer available.
          couchdbserver <- cm:MyCouchdbApp >> CouchdbServers >> binding >> context >>= first
          bigbangsdatabase <- create context cm:BespokeDatabase bound to cm:CouchdbServer$BespokeDatabases in couchdbserver
          owner <- create role cm:BespokeDatabase$Owner in bigbangsdatabase >> binding >> context
        in
          TestName = "Create bigbangsdatabase." for extern
          bind_ couchdbserver >> Admin to owner
          -- Save for reference in case Add_public_pages.
          bind owner to BespokeDatabaseOwner in mm:RepositoryToolsApp
          EnteredDatabaseName = "cw_bigbangsdatabase/" for bigbangsdatabase
          once settled
            Endorsed = true for bigbangsdatabase
            Public = true for bigbangsdatabase

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

      -- Values like: "RepositoryTools=1.1; CouchdbManagement=12.5; Couchdb=4.0"
      property ModelVersions (String)

      property StartTest (Boolean)
      property StartParsing (Boolean)
      property YamlGenerated (Boolean)

      state CreateManifest = StartTest
        on entry
          do for Tester once settled
            letA
              manifest <- create role cm:Repository$Manifests in context >> Repository >> binding >> context
            in
              LocalModelName = ModelName for manifest
              EnteredModelCuid = callExternal p:GetLocalModelCuid( "model://" + NameSpace + "#" + ModelName ) returns String for manifest

      -- Is the Manifest role filled with the external role of the new ModelManifest?
      state CreateVersion = exists context >> Manifest >> binding
        on entry
          do for Tester once settled
            letA
              version <- create role cm:ModelManifest$Versions in context >> Manifest >> binding >> context
            in
              -- Setting the version number triggers state ReadyToMake and creates the VersionedModelManifest context.
              Versions$Version = VersionNumber for version

              once settled
                create file "whatever" as "text/arc" in ArcFile for version >> binding
                  callExternal util:ApplyModelVersions( ModelVersions, callExternal p:GetLocalArcSource( version >> ModelURIReadable ) returns String ) returns String
                Store = "Repository" for version >> binding

              once settled
                AutoUpload = true for version >> binding
      
      state AugmentYaml = exists context >> Version >> binding >> context >> Translation >> TranslationYaml
        on entry
          do for Tester once settled
            letA
              version <- context >> Version >> binding
            in
              callEffect cdb:UploadOldTranslation( context >> Version >> binding >> VersionedModelURI )
              -- LET OP: dit gebeurt ook in UploadToRepository!
              GenerateYaml = true for version >> context >> Translation

              once settled
                TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on extern
        props (NameSpace, VersionNumber, ModelName, StartTest, StartParsing, YamlGenerated, ModelVersions) verbs (SetPropertyValue, Consult)

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

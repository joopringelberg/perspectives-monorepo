-- CouchdbManagement - Copyright Joop Ringelberg and Cor Baars 2021 - 2024

domain model://perspectives.domains#CouchdbManagement
  use sys for model://perspectives.domains#System
  use cm for model://perspectives.domains#CouchdbManagement
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
          couchdbapp <- create context CouchdbManagementApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ couchdbapp >> extern to start
          Name = "Couchdb Management App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (cm:MyCouchdbApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (cm:MyCouchdbApp >> extern)
      in
        remove context indexedcontext
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  -- The INDEXED context cm:MyCouchdbApp, that is the starting point containing all CouchdbServers.
  -- There is NO PUBLIC PERSPECTIVE on this case.
  -- The end user user (playing Manager) should have a Server Admin account for each CouchdbServer that is added to the App.
  -- The credentials of each such Server Admin go into the CouchdbServer$Admin roles.
  case CouchdbManagementApp
    indexed cm:MyCouchdbApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
    -- the exit is triggered on removing a role instance of StartContexts
    on exit
      do for Manager
        delete context bound to CouchdbServers
        -- remove this context from PerspectivesSystem$IndexedContexts
        -- remove the model from ModelsInUse

    -- Every user manages his own CouchdbServers.
    -- This manager should be the Server admin of each CouchdbServer,
    -- in order to create databases and members.
    -- Becoming a Couchdb Server Admin should be managed outside Perspectives.
    -- The username should be the PerspectivesSystem$User ID.
    user Manager = sys:SocialMe >> binding
      perspective on CouchdbServers
        only (CreateAndFill, RemoveContext, DeleteContext, Create, Fill)
        props (Name) verbs (Consult)
        props (Url, CouchdbServers$CouchdbPort, AdminUserName, AdminPassword, Name) verbs (SetPropertyValue)
      perspective on Manager
        props (FirstName, LastName) verbs (Consult)
        
      -- Manager needs this action so he can set the Url before the CouchdbServer$Visitor tries to publish.
      action CreateServer
        create role CouchdbServers

      -- Manager needs this perspective for others to accept Admins created in state CouchdbServers$NoAdmin.
      perspective on CouchdbServers >> binding >> context >> CouchdbServer$Admin
        only (Create, Fill)
        props (AuthorizedDomain, Password, SpecificUserName) verbs (SetPropertyValue)
      
      screen "Couchdb Server Administration"
        who
          Manager
            master
              without props(FirstName)
            detail
        what
          markdown <### Managing Couchdb Servers for Perspectives
                      If you run a Couchdb server, you can use this app to create repositories and accounts on it.
                      A *Repository* is a collection of models and versions of those models.
                      Managed servers are listed under *where*.

                      #### Add a Couchdb Server as a managed server
                      Look up the url, port and credentials of the Couchdb server you want to manage.
                      Then:
                      * Click the item *Create Server* in the main menu.
                      * Fill in the URL of the Couchdb server. The URL must start with `https://` and end with a `/`.
                      * Fill in the port number of the Couchdb server. This is a number between 1 and 65535.
                      * Fill in the username and password of an Admin account on the Couchdb server.
                      >
        where
          CouchdbServers
            master
              markdown <### Couchdb Servers
                        On adding a new server, you will not be able to enter its name yet.
                        This is because the server is not yet created in Couchdb.
                        After entering all required information, the server will be created in Couchdb.
                        After that, visit the CouchdbServer context and enter a name.
                        >
              without props (Url, CouchdbServers$CouchdbPort, AdminUserName, AdminPassword)
            detail
      
    -- A new CouchdbServers instance comes complete with a CouchdbServer$Admin role
    -- filled with CouchdbManagementApp$Admin.
    context CouchdbServers (relational) filledBy CouchdbServer
      property Url (mandatory, String)
        pattern = "^https://[^\\/]+\\/$" "An url with the https scheme, ending on a slash"
      property CouchdbPort (mandatory, String)
        pattern = "^\\d{1,5}$" "A port number written as a string, consisting of up to 5 digits, maximally 65535."
      -- The Password and UserName of Manager (and therefore CouchdbServer$Admin) for Url.
      -- TODO: Deze properties zouden authorOnly moeten zijn!
      property AdminPassword (mandatory, String)
      property AdminUserName (mandatory, String)

      -- Add credentials to Perspectives State (not to the Couchdb_).
      -- This means that the PDR can now authenticate on behalf of the Admin with Couchdb_.
      -- These credentials are only effective in the current session. 
      -- They are added (persistently) to CouchdbServer$Admin as soon as that role is filled. From then on,
      -- this installation can authenticate with the server at Url.
      state AddCredentials = (((exists Url) and exists CouchdbServers$CouchdbPort) and exists AdminPassword) and (exists AdminUserName) and not exists binding
        on entry
          do for Manager
            callEffect cdb:AddCredentials( Url, AdminUserName, AdminPassword )

        state CreateServer = exists Url -- This might be replaced by 'true'.
          on entry
            do for Manager
              -- Create the databases in CouchdbServer_.
              -- Notice that Pouchdb only creates a database if it does not yet exist.
              -- This allows us to reconnect to a CouchdbServer_ if its CouchdbServer had been lost.
              letA 
                couchdburl <- "http://localhost:" + CouchdbServers$CouchdbPort + "/"
              in 
                callEffect cdb:CreateEntitiesDatabase( Url, "cw_servers_and_repositories", Url >> callExternal util:Replace( "https://", "" ) returns String )
                callEffect cdb:MakeDatabaseWriteProtected( Url, "cw_servers_and_repositories" )
                callEffect cdb:MakeDatabasePublic( Url, "cw_servers_and_repositories" )
                -- As the databases are now ready, we can create and publish the CouchdbServer.
                create_ context CouchdbServer bound to origin

      state NoAdmin = (exists binding) and not exists binding >> context >> CouchdbServer$Admin
        on entry
          do for Manager
            -- Manager 
            -- Tie these credentials to the CouchdbServer$Admin role. As this has aspect WithCredentials,
            -- from now on this set of credentials will be added to Perspectives State on startup.
            bind context >> Manager to Admin in binding >> context
            AuthorizedDomain = Url for  binding >> context >> Admin     -- zet dit wel voor een nieuwe CouchdbServer$Admin. Password en eventueel SpecificUserName moet hij zelf zetten.
            Password = AdminPassword for binding >> context >> Admin
            SpecificUserName = AdminUserName for binding >> context >> Admin
            -- Serverurl = Url for binding
            -- CouchdbPort = CouchdbPort for binding

  -------------------------------------------------------------------------------
  ---- COUCHDBSERVER
  -------------------------------------------------------------------------------
  -- For each CouchdbServer_ (=remote server with a Couchdb installation), there should be only one CouchdbServer instance.
  -- This context has a PUBLIC USER Visitor that causes a version of the context to be published.
  -- Visitor can see the Repositories and their descriptions, and sign up.
  -- To sign up, he should be able to see the admin.
  
  -- This context implements the BodyWithAccounts pattern.
  -- NOTE: a PerspectivesSystem$User should only fill either Admin, or Accounts!
  -- The PDR looks for credentials in either role and should find them just once.
  case CouchdbServer
    aspect acc:Body
    aspect sys:ContextWithNotification
    external
      -- The location of the CouchdbServer_. 
      property ServerUrl (functional) = binder CouchdbServers >> Url
      property CouchdbPort (functional) = binder CouchdbServers >> CouchdbServers$CouchdbPort
      property Name (String)
        readableName

      -- This covers the case we get a CouchdbServer that we did not create ourselves.
      -- If we do not refer to my indexed version of the CouchdbApp, this condition will fail because another user
      -- will have shared his App, too!
      state AdminReceivesServer = (not exists filter cm:MyCouchdbApp >> CouchdbServers with filledBy origin) and exists (filter context >> Admin with filledBy sys:SocialMe >> binding)
        perspective of Admin 
          perspective on extern >> binder CouchdbServers
            -- selfonly
            only (Create, Fill)
            props (Url, CouchdbServers$CouchdbPort) verbs (Consult, SetPropertyValue)
        perspective of Accounts
          perspective on extern >> binder CouchdbServers
            -- selfonly
            only (Create, Fill)
        on entry
          do for Admin
            letA
              server <- create role CouchdbServers in cm:MyCouchdbApp
            in 
              bind_ origin to server
              Url = ServerUrl for server
              CouchdbPort = CouchdbPort for server
          notify Admin
            "You now have an account with CouchdbServer {Name}"

      state AccountsReceivesServer = (not exists filter cm:MyCouchdbApp >> CouchdbServers with filledBy origin) and exists (filter context >> Accounts with filledBy sys:SocialMe >> binding)
        perspective of Accounts 
          perspective on extern >> binder CouchdbServers
            only (Create, Fill)
            props (Url, CouchdbServers$CouchdbPort) verbs (Consult, SetPropertyValue)
        on entry
          do for Accounts
            letA
              server <- create role CouchdbServers in cm:MyCouchdbApp
            in 
              bind_ origin to server
              Url = ServerUrl for server
              CouchdbPort = CouchdbPort for server
          notify Accounts
            "You now have an account with CouchdbServer {Name}"

        
      -- Exiting CouchdbServer$External
      on exit
        do for Admin
          callEffect cdb:DeleteCouchdbDatabase( ServerUrl, "cw_servers_and_repositories" )
          callEffect cdb:DeleteCouchdbDatabase( ServerUrl, "cw_servers_and_repositories_write" )
          callEffect cdb:EndReplication( ServerUrl, "cw_servers_and_repositories_write", "cw_servers_and_repositories" )
    
    -- Exiting CouchdbServer
    on exit
      do for Admin
        delete context bound to Repositories

    -- Admin in Couchdb of a particular server.
    -- This role requires credentials for the ServerUrl, because it updates CouchdbServer and creates and updates Repositories and Accounts.
    -- It requires write access to cw_servers_and_repositories.
    -- TODO Hernoem dit naar ServerAdmin.
    user Admin filledBy CouchdbManagementApp$Manager 
      -- As acc:Body$Admin, has full perspective on Accounts.
      -- These properties come from sys:WithCredentials
      -- WithCredentials$UserName - CALCULATED, either the SpecificUserName or the ID of the PerspectivesSystem$User.
      -- The next two properties are set on creating the admin (state CouchdbServerApp$CouchdbServers$NoAdmin)
      -- WithCredentials$SpecificUserName
      -- TODO: deze properties zouden authorOnly moeten zijn!
      -- WithCredentials$Password
      -- WithCredentials$AuthorizedDomain
      aspect acc:Body$Admin
      aspect sys:ContextWithNotification$NotifiedUser

      action CreateRepository
        create role Repositories

      perspective on Accounts
        all roleverbs
        props (FirstName, LastName) verbs (Consult)
        props (Password, AuthorizedDomain) verbs (Consult, SetPropertyValue)
      
      perspective on BespokeDatabases
        all roleverbs
        props (OwnerName, Description) verbs (Consult)
        props (Endorsed) verbs (SetPropertyValue)
      
      perspective on Admin
        props (FirstName, UserName) verbs (Consult)
        props (SpecificUserName, Password) verbs (SetPropertyValue)
        all roleverbs
      
      perspective on Visitor
        props (FirstName, LastName) verbs (Consult)

      perspective on External
        props (ServerUrl, CouchdbPort, ServerUrl) verbs (Consult)
        props (Name) verbs (SetPropertyValue, Consult)

      perspective on Repositories
        all roleverbs
        props (Repositories$NameSpace, AdminEndorses, IsPublic, AdminLastName) verbs (Consult)
        props (IsPublic, NameSpace_, HasDatabases) verbs (SetPropertyValue)
        in object state WithoutExternalDatabase
          props (AdminEndorses) verbs (SetPropertyValue)
        in object state CreateDatabases
          props (IsPublic) verbs (SetPropertyValue)
        in object state NoNameSpace
          props (Repositories$NameSpace) verbs (SetPropertyValue, AddPropertyValue)
        
        -- This is currently not very useful, because a Repositories instance will not enter state WithoutManifests 
        -- when its last Manifest is deleted. Negation by failure breaks on removing instances!
        -- in object state WithoutManifests
        --   action RemoveRepository
        --     remove role origin
            
      screen "Couchdb Server"
        who
          Admin
            master
              without props (FirstName, UserName, SpecificUserName, Password)
            detail
          Accounts
            master
              markdown <#### Accounts
                        The accounts of the Couchdb Server. Add an account by choosing *Empty role* from the accordion menu.
                        Then put a user role on the clipboard (or select the role you want if it is already on the clipboard).
                        Finally choose *Fill with the role on the clipboard* from the accordion menu
                        >
              without props (FirstName, Password, AuthorizedDomain, UserName)
            detail
        what
          row
            markdown <### Couchdb Server Administration
                        A Couchdb Server is the container of *Repositories* and *Bespoke Databases*.
                        The former are in essence a collection of models and versions of those models.
                        The latter are stores for contexts and roles, the primary stuff kept by Perspectives.
                        You need a bespoke database for example, to store *public pages* in.
                        >
          row
            form "Couchdb Server" External
        where
          Repositories
            master
              -- Domain is the readable name of ManifestCollection.
              without props (Repositories$NameSpace, AdminEndorses, AdminLastName, IsPublic, NameSpace_, HasDatabases)
            detail
              without props (AdminLastName, IsPublic, NameSpace_, HasDatabases)
          BespokeDatabases
            master
              markdown <### Bespoke databases
                        Databases owned by Accounts. 
                        >
              without props (Description, Endorsed, OwnerName)
            detail

    -- This role requires credentials for the ServerUrl, because it can remove itself.
    -- It requires write access to cw_servers_and_repositories.
    user Accounts (unlinked, relational) filledBy sys:TheWorld$PerspectivesUsers
      -- WithCredentials$Password
      -- WithCredentials$SpecificUserName
      -- WithCredentials$AuthorizedDomain
      aspect acc:Body$Accounts
      aspect sys:ContextWithNotification$NotifiedUser
      state Filled = (exists binding) and not exists Password
        on entry
          do for Admin
            letA
              serverurl <- context >> extern >> ServerUrl
            in
              -- This will set the properties of the object in transition, i.e. the Accounts instance; NOT the Admin instance!
              Password = callExternal util:GenSym() returns String
              AuthorizedDomain = serverurl
              -- NOTICE: we do not check whether UserName is already a registered user of the Couchdb installation at serverurl.
              callEffect cdb:CreateUser( serverurl, UserName, Password )
              callEffect cdb:MakeWritingMemberOf( serverurl, "cw_servers_and_repositories", UserName ) -- UserName is the ID of the PerspectivesSystem$User.
          do for Accounts
              callEffect cdb:AddCredentials( AuthorizedDomain, UserName, Password)
        state ThisIsMe = origin filledBy sys:SocialMe >> binding
          -- Accounts needs this perspective to be able to add the CouchdbServer to his cm:MyCouchdbApp!
          perspective of Accounts 
            perspective on extern >> binder CouchdbServers
              only (Create, Fill)
          on entry
            -- When a peer assigns the current user to the Accounts role,
            -- we make sure that the current user has the CouchdbServer bound
            -- in the CouchdbManagementApp.
            do for Accounts
              bind context >> extern to CouchdbServers in cm:MyCouchdbApp
            notify Accounts
              "You now have an account with CouchdbServer {context >> extern >> Name}"

      on exit
        do for Admin
          letA
            serverurl <- context >> extern >> ServerUrl
          in
            callEffect cdb:RemoveAsWritingMemberOf( serverurl, "cw_servers_and_repositories", UserName ) -- UserName is the ID of the PerspectivesSystem$User.
            callEffect cdb:DeleteUser( serverurl, UserName, Password )
      
      perspective on Accounts
        selfonly
        -- Add FirstName to make sure the filler of this role is sent to the peer himself!
        props (FirstName, Password, AuthorizedDomain) verbs (Consult)

      perspective on External
        props (ServerUrl, Name) verbs (Consult)

      perspective on Admin
        props (FirstName, LastName) verbs (Consult)
      
      perspective on Repositories
        props (Repositories$NameSpace, IsPublic, RepositoryUrl) verbs (Consult)
      
      perspective on BespokeDatabases
        only (CreateAndFill, RemoveContext)
        props (Description) verbs (Consult, SetPropertyValue)
      
      perspective on BespokeDatabases >> binding >> context >> Owner
        only (Fill)
            
      perspective on Visitor
        props (FirstName, LastName) verbs (Consult)


      action RequestDatabase
        letA
          db <- create context BespokeDatabase bound to BespokeDatabases
        in
          bind (sys:Me >> binder CouchdbServer$Accounts >>= first) to Owner in db >> binding >> context 
      
      perspective on MyBespokeDatabases
        props (Description) verbs (Consult)

      screen 
        who
          Accounts
            master
              props(LastName) verbs (Consult)
            detail
              props (FirstName, LastName, Password, AuthorizedDomain) verbs (Consult)
          Admin
            master
              props(LastName) verbs (Consult)
            detail
              props (FirstName, LastName) verbs (Consult)
        what
          row
            markdown <### Couchdb Server
                        You have an account with this server. In principle you can have a Bespoke Database on it.
                        Also, this account is a necessary condition for participating in a Repository other than as visitor.
                      >
            form External
        where
          Repositories
            master
              props (Repositories$NameSpace) verbs (Consult)
            detail
          BespokeDatabases
            master
              markdown <### Bespoke databases
                        Databases exclusively for you. 
                        >
              props (Name) verbs (Consult)
            detail
              props (Name, Description) verbs (Consult)

    -- The instance of CouchdbServer is published in the cw_servers_and_repositories database.
    -- TODO: als omkering van filtered queries volledig is, beperk dan het perspectief van Visitor tot PublicRepositories.
    public Visitor at extern >> ServerUrl + "cw_servers_and_repositories/" = sys:Me
      perspective on External
        props (Name) verbs (Consult)
      perspective on Repositories
        props (Repositories$NameSpace, IsPublic) verbs (Consult)
      
      -- Perspective on Admin in order to be able to sign up as an Account.
      perspective on Admin
        props (FirstName, LastName) verbs (Consult)
      
      screen
        who
          Admin
            master
              without props (FirstName)
            detail
        what
          row
            markdown <### Couchdb Server
                        This is a Couchdb Server. You can sign up for an account on it.
                        >
          row
            form External
        where
          Repositories
            master
              markdown <### Repositories
                          A repository is a collection of models and versions of those models.
                          You can visit the public repositores and install models from them.
                          >
              without props (IsPublic, Repositories$NameSpace)
            detail

    context PublicRepositories = filter Repositories with IsPublic

    -- A Repositories instance comes complete with an (empty) Admin role.
    -- Moreover, as a side effect, both a read- and write database are created
    -- in Couchdb and the write database replicates to the read database.
    context Repositories (relational) filledBy Repository
      property AdminEndorses (Boolean)
      -- The top level domain preceded by at least one subdomain, e.g. "perspectives.domains".
      property NameSpace (String)
        -- Only lowercase characters (a-z), digits (0-9), and any of the characters _, $, (, ), +, -, and / are allowed. Must begin with a letter.
        -- However, the parser refuses (, ) and /.
        -- ^[a-z][a-z0-9_$()+/-]*$ according to https://docs.couchdb.org/en/3.2.0/api/database/common.html and https://localhost:6984//_utils/docs/api/database/common.html#specifying-the-document-id
        pattern = "^[a-z]([a-z]|[0-9]|[\\._$+-])*$" "Only lowercase characters (a-z), digits (0-9), and any of the characters ., _, $, + and - are allowed. Must begin with a letter."
      property HasDatabases (Boolean)

      state CreateRepository = (exists Repositories$NameSpace) and not exists NameSpace_
        on entry
          do for Admin
            letA
              reponame <- Repositories$NameSpace >> callExternal util:Replace( ".", "_" ) returns String
            in
              create_ context Repository named reponame bound to origin
              -- Copy the namespace to the Repository, but replace dots with underscores.
              NameSpace_ = reponame for origin >> binding

      state CreateDatabases = (not HasDatabases) and (exists NameSpace_) and AdminEndorses and exists context >> Admin >> Password
        on entry
          do for Admin
            letA
              baseurl <- context >> extern >> ServerUrl
              couchdburl <- "http://localhost:" + context >> extern >> CouchdbPort + "/"
              readmodels <- "models_" + NameSpace_
              readinstances <- "cw_" + NameSpace_
            in 
              -- models
              callEffect cdb:CreateCouchdbDatabase( baseurl, readmodels )
              callEffect cdb:MakeDatabaseWriteProtected( baseurl, readmodels )
              callEffect cdb:MakeDatabasePublic( baseurl, readmodels )
              -- instances
              callEffect cdb:CreateEntitiesDatabase( baseurl, readinstances, Repositories$NameSpace )
              callEffect cdb:MakeDatabasePublic( baseurl, readinstances )
              callEffect cdb:MakeDatabaseWriteProtected( baseurl, readinstances )
              HasDatabases = true
      on exit
        do for Admin
          letA
            baseurl <- context >> extern >> ServerUrl
            readmodels <- "models_" + NameSpace_
            readinstances <- "cw_" + NameSpace_
          in 
            -- models
            callEffect cdb:DeleteCouchdbDatabase( baseurl, readmodels )
            -- instances
            callEffect cdb:DeleteCouchdbDatabase( baseurl, readinstances )

      -- THIS STATE WILL NOT BE REVISITED WHEN ALL MANIFESTS ARE REMOVED,
      -- because the role instance state is re-evaluated BEFORE the instance is actually thrown away.
      -- state WithoutManifests = not exists binding >> context >> Manifests

      state WithoutExternalDatabase = (not exists NameSpace_) or not AdminEndorses

      state NoNameSpace = not exists Repositories$NameSpace

      -- Ad Admin may exist already if the Repository is created by Accounts.
      state NoAdmin = AdminEndorses and not exists binding >> context >> Repository$Admin
        on entry
          do for Admin
            -- create role Admin in binding >> context
            bind context >> Admin to Admin in binding >> context

    context BespokeDatabases (relational) filledBy BespokeDatabase
    context MyBespokeDatabases = filter BespokeDatabases with binding >> context >> Owner filledBy sys:Me
    aspect thing sys:ContextWithNotification$Notifications
  -------------------------------------------------------------------------------
  ---- BESPOKEDATABASE
  -------------------------------------------------------------------------------
  case BespokeDatabase
    external
      property DatabaseLocation (String)
      property DatabaseName (String)
      property Endorsed (Boolean)
      property BaseUrl = binder BespokeDatabases >> context >> extern >> ServerUrl
      property OwnerName = context >> Owner >> LastName
      property Name (String)
        readableName
      property Description (String)
      property Public (Boolean)
      state CreateDb = Endorsed and (exists context >> Owner) and not exists DatabaseName
        on entry
          do for CBAdmin
            DatabaseName = "cw_" + callExternal util:GenSym() returns String + "/" 
            callEffect cdb:CreateCouchdbDatabase( BaseUrl, DatabaseName )
            DatabaseLocation = BaseUrl + DatabaseName
            callEffect cdb:MakeAdminOfDb( BaseUrl, DatabaseName, context >> Owner >> UserName )
      state Publish = (exists DatabaseName) and Public
        on entry
          do for CBAdmin
            callEffect cdb:MakeDatabasePublic( BaseUrl, DatabaseName )
        -- on exit
        --   do for CBAdmin
        --     callEffect cdb:MakeDatabasePrivate( BaseUrl, DatabaseName )
      on exit
        do for CBAdmin
          callEffect cdb:DeleteCouchdbDatabase( BaseUrl, DatabaseName )

    -- Owner will be an Admin of the BespokeDatabase.
    user Owner filledBy (CouchdbServer$Accounts, CouchdbServer$Admin)
      property BespokeDatabaseUrl = context >> extern >> DatabaseLocation
      perspective on External
        props (Public, Description, Name) verbs (Consult, SetPropertyValue)
        props (DatabaseLocation) verbs (Consult)
      perspective on Owner
        props (LastName) verbs (Consult)

    user CBAdmin = extern >> binder BespokeDatabases >> context >> Admin
      perspective on External
        props (DatabaseLocation, Endorsed, DatabaseName) verbs (Consult, SetPropertyValue)
        props (BaseUrl) verbs (Consult)
      perspective on Owner
        all roleverbs
        props (LastName) verbs (Consult)

  -------------------------------------------------------------------------------
  ---- REPOSITORY
  -------------------------------------------------------------------------------
  -- This context has a public perspective that allows the visitor to see the models.
  -- This contexts implements the BodyWithAccounts pattern.
  case Repository
    aspect acc:Body
    aspect sys:ManifestCollection
    aspect sys:ContextWithNotification

    state Endorsed = extern >> binder Repositories >> AdminEndorses
      perspective of Admin
        perspective on Authors
          only (Create, Fill, Remove)
          props (FirstName, LastName) verbs (Consult)

    -- Embedded contexts are not removed automatically with their embedder!
    on exit
      do for Admin
        delete context bound to Manifests

    external
      aspect sys:ManifestCollection$External
        -- NameSpace_
        -- Domain
      -- Only public repositories will be visible to Accounts of CouchdbServers.
      property IsPublic (mandatory, Boolean)
      -- The toplevel domain with at least one subdomain, such as perspectives.domains or professional.joopringelberg.nl.
      property NameSpace (functional) = binder Repositories >> Repositories$NameSpace
      property ModelsDatabase = "models_" + NameSpace_
      property InstancesDatabase = "cw_" + NameSpace_
      -- The location of the CouchdbServer_. 
      property ServerUrl (functional) = binder Repositories >> context >> extern >> ServerUrl
      property RepositoryUrl = "https://" + NameSpace + "/"
      property AdminLastName = context >> Admin >> LastName
      property RepoHasDatabases (functional) = binder Repositories >> HasDatabases

    -- We need the ServerAdmin in this context in order to configure the local Admin and to give Authors write access.
    user ServerAdmin (functional) = extern >> binder Repositories >> context >> CouchdbServer$Admin
      perspective on Admin
        only (Create, Fill, RemoveFiller, Remove)
        props (SpecificUserName, Password) verbs (SetPropertyValue)
        props (FirstName, LastName, Password, UserName) verbs (Consult)
        props (AuthorizedDomain) verbs (SetPropertyValue)
      perspective on Authors
        props (AuthorizedDomain) verbs (SetPropertyValue)

    -- This role requires credentials for the ServerUrl. It 'inherits' them from its filler.
    -- It also requires credentials for the RepositoryUrl, because it creates and updates Manifests.
    -- It requires write access to cw_servers_and_repositories and to cw_RepositoryUrl. 
    -- The latter is guaranteed as it is DatabaseAdmin of both databases. 
    -- NOTE: later, CouchdbServer$Accounts will also be allowed to fill this role.
    user Admin filledBy CouchdbServer$Admin
      aspect acc:Body$Admin
      aspect sys:ContextWithNotification$NotifiedUser
        -- WithCredentials$SpecificUserName
        -- We must use the next three properties to provide the autentication details to the PDR for the RepositoryUrl.
        -- WithCredentials$UserName (either the SpecificUserName or the User identifier)
        -- WithCredentials$Password
        -- WithCredentials$AuthorizedDomain
        -- As Admin, has a full perspective on Accounts.

      -- If we fill Admin with a role that already has an AuthorizedDomain, we will still set AuthorizedDomain on the Admin role itself.
      state IsFilled = (exists binding) and (not AuthorizedDomain == context >> extern >> RepositoryUrl) and context >> extern >> RepoHasDatabases
        on entry
          do for ServerAdmin
            -- Only the CouchdbServer$Admin has a Create and Fill perspective on
            -- Repository$Admin. So when this state arises, we can be sure that
            -- the current user is, indeed, a CouchdbServer$Admin.
            -- Hence the PDR will authenticate with his credentials.
            -- Admin adds Manifests to Repository, so must be able to store Repository on behalf of the public Visitor.
            letA
              serverurl <- context >> extern >> ServerUrl
            in
              -- models
              -- callEffect cdb:MakeAdminOfDb( serverurl, context >> extern >> WriteModels, UserName )
              callEffect cdb:MakeAdminOfDb( serverurl, context >> extern >> ModelsDatabase, UserName )
              -- instances
              -- callEffect cdb:MakeAdminOfDb( serverurl, context >> extern >> WriteInstances, UserName )
              callEffect cdb:MakeAdminOfDb( serverurl, context >> extern >> InstancesDatabase, UserName )
              -- Make authentication details available to the PDR (in fact only useful when the filler of Admin is not CouchdbServer$Admin)
              AuthorizedDomain = context >> extern >> RepositoryUrl
        
        state HasPassword = exists Password
          on entry
            do for ServerAdmin
              callEffect cdb:AddCredentials( AuthorizedDomain, UserName, Password)


        on exit
          do for ServerAdmin
            letA
              serverurl <- context >> extern >> ServerUrl
            in
              -- models
              -- callEffect cdb:RemoveAsAdminFromDb( serverurl, context >> extern >> WriteModels, UserName )
              callEffect cdb:RemoveAsAdminFromDb( serverurl, context >> extern >> ModelsDatabase, UserName )
              -- instances
              -- callEffect cdb:RemoveAsAdminFromDb( serverurl, context >> extern >> WriteInstances, UserName )
              callEffect cdb:RemoveAsAdminFromDb( serverurl, context >> extern >> InstancesDatabase, UserName )

      on exit 
        notify "You are no longer the administrator of the repository { context >> extern >> NameSpace_ }."

      
      perspective on External
        props (IsPublic, NameSpace_, RepositoryUrl) verbs (Consult)

      action CompileRepositoryModels
        callEffect p:CompileRepositoryModels( extern >> (RepositoryUrl + ModelsDatabase), extern >> (RepositoryUrl + InstancesDatabase) )
      
      perspective on Admin
        props (FirstName, SpecificUserName, UserName, Password) verbs (Consult)

      -- The design pattern for nested public contexts requires that Admin has write access
      -- to both the cw_servers_and_repositories and to the Repository database.
      perspective on Manifests
        only (Create, Fill, Delete, Remove, RemoveContext, DeleteContext, CreateAndFill)
        props (Description, LocalModelName) verbs (Consult)
        props (DomeinFileName) verbs (SetPropertyValue, Consult)
        in object state NoLocalModelName
          props (LocalModelName) verbs (SetPropertyValue)
      
      action CreateManifest
        create role Manifests
      
      perspective on Manifests >> binding >> context >> Author
        only (Create, Fill)

      perspective on Visitor
        props (FirstName, LastName) verbs (Consult)

      -- Moet in staat zijn om een instantie toe te voegen aan Accounts.
      -- perspective on Accounts

      screen
        who
          Admin
            master
              without props (FirstName, SpecificUserName, UserName, Password)
            detail
          Authors
            master
              without props(FirstName, UserName, Password)
            detail
        what
          row
            markdown <### Repository Administration
                      A repository is a collection of models and versions of those models.
                      You can add manifests to the repository, which are pointers to models in the CouchdbServer.
                      **Notice** the function in the main menu on the left, `Compile models in repository`. 
                      Use it to recompile all models in the repository when the shape of the model representation has changed.
                      >
            form "Repository" External
              props (RepositoryUrl, IsPublic) verbs (Consult)
        where
          Manifests
            master
              markdown <## Add a manifest
                        Add a manifest by opening the menu and creating a new empty role.
                        Then enter the *unqualified* name of the model in the column `Local model name`.
                        For the domain 'model://perspectives.domains#System' for example, this would be 'System'.
                      >
              -- Notice that we will have a table with both LocalModelName and Name. The latter is the ReadableName. 
              -- We cannot omit the readable name. It equals the LocalModelName, but we cannot edit it. 
              -- Hence we need both.
              without props (Description, DomeinFileName)
            detail
      
    -- This role requires credentials for the ServerUrl. It 'inherits' them from its filler.
    -- It also requires credentials for the RepositoryUrl, because it creates and updates Manifests.
    -- It requires write access to cw_servers_and_repositories and to cw_RepositoryUrl.
    -- It actually does not require write access to models_RepositoryUrl, but as Authors fill ModelManifest$Author
    -- (and that role actually requires write access to models_RepositoryUrl), this is a convenient place to arrange that.
    user Authors (relational) filledBy CouchdbServer$Accounts
      aspect acc:Body$Accounts
      state Filled = (exists binding) and not AuthorizedDomain == context >> extern >> RepositoryUrl
        on entry
          do for ServerAdmin
            letA
              serverurl <- context >> extern >> ServerUrl
            in
              AuthorizedDomain = context >> extern >> RepositoryUrl
              callEffect cdb:MakeWritingMemberOf( serverurl, context >> extern >> ModelsDatabase, UserName ) -- UserName is the ID of the PerspectivesSystem$User.
              callEffect cdb:MakeWritingMemberOf( serverurl, context >> extern >> InstancesDatabase, UserName ) -- UserName is the ID of the PerspectivesSystem$User.
      state Domain = exists AuthorizedDomain
        on entry
          do
            callEffect cdb:AddCredentials( AuthorizedDomain, UserName, Password)
      on exit
        do for ServerAdmin
          letA
            serverurl <- context >> extern >> ServerUrl
          in
            callEffect cdb:RemoveAsWritingMemberOf( serverurl, context >> extern >> ModelsDatabase, UserName ) -- UserName is the ID of the PerspectivesSystem$User.
            callEffect cdb:RemoveAsWritingMemberOf( serverurl, context >> extern >> InstancesDatabase, UserName ) -- UserName is the ID of the PerspectivesSystem$User.

      perspective on External
        props (IsPublic, NameSpace_, RepositoryUrl) verbs (Consult)

      perspective on Manifests
        only (Create, Fill, Delete, RemoveContext, Remove)
        props (Description, LocalModelName) verbs (Consult)
        props (DomeinFileName) verbs (SetPropertyValue, DeleteProperty)
        in object state NoLocalModelName
          props (LocalModelName) verbs (SetPropertyValue)
      action CreateManifest
        create role Manifests
      
      perspective on Manifests >> binding >> context >> Author
        only (Create, Fill)
      
      perspective on Authors
        props (FirstName, LastName, AuthorizedDomain) verbs (Consult)
      
      perspective on Visitor
        props (FirstName, LastName) verbs (Consult)

      screen
        who
          Admin
            master
              without props (FirstName)
            detail
          Authors
            master
              without props(FirstName, AuthorizedDomain)
            detail
        what
          row
            markdown <### Repository Administration
                      A repository is a collection of models and versions of those models.
                      You can add manifests to the repository, which are pointers to models in the CouchdbServer.
                      >
            form "Repository" External
              props (RepositoryUrl, IsPublic) verbs (Consult)
        where
          Manifests
            master
              markdown <## Add a manifest
                        Add a manifest by opening the menu and creating a new empty role.
                        Then enter the *unqualified* name of the model in the column `Local model name`.
                        For the domain 'model://perspectives.domains#System' for example, this would be 'System'.
                      >
              -- Notice that we will have a table with both LocalModelName and Name. The latter is the ReadableName. 
              -- We cannot omit the readable name. It equals the LocalModelName, but we cannot edit it. 
              -- Hence we need both.
              without props (Description, DomeinFileName)
            detail    
    
    user Accounts (relational, unlinked) filledBy (CouchdbServer$Accounts, CouchdbServer$Admin)

    -- The instances of Repository are published in the cw_servers_and_repositories database.
    public Visitor at extern >> ServerUrl + "cw_servers_and_repositories/" = sys:Me
      perspective on Manifests
        props (LocalModelName, ModelManifest$External$Name, Description) verbs (Consult)
      perspective on extern
        props (NameSpace_, NameSpace) verbs (Consult)

      screen
        who
        what
          row
            markdown <### Repository
                      This repository is a collection of manifests describing models and versions of those manifests.
                      Visit a manifest context to install a new app or to update an existing app.
                      >
          row
            form External
              props (NameSpace) verbs (Consult)
        where
          Manifests
            master
              markdown <## Manifests
                        A manifest describes a model / App.
                      >
              without props (Description, LocalModelName)
            detail
              without props (LocalModelName)

    -- This role is in the public Visitor perspective. These are all models that
    -- are stored in this Repository.
    context Manifests (relational) filledBy ModelManifest
      aspect sys:ManifestCollection$Manifests
      -- LocalModelName
      state NoLocalModelName = not exists LocalModelName
      state ReadyToMake = (exists LocalModelName) and not exists binding
        on entry
          do for Admin
            letA 
              manifestname <- (context >> extern >> NameSpace_ + "-" + LocalModelName)
            in
              -- As the PDR derives this name from the modelURI, we have to name the ModelManifest with its LocalModelName.
              create_ context ModelManifest named manifestname bound to origin
              bind currentactor to Author in origin >> binding >> context
              DomeinFileName = manifestname + ".json" for origin >> binding

          do for Authors
            letA 
              manifestname <- (context >> extern >> NameSpace_ + "-" + LocalModelName)
            in
              -- As the PDR derives this name from the modelURI, we have to name the ModelManifest with its LocalModelName.
              create_ context ModelManifest named manifestname bound to origin
              bind currentactor to Author in origin >> binding >> context
              DomeinFileName = manifestname + ".json" for origin >> binding
    aspect thing sys:ContextWithNotification$Notifications

  case ModelManifest 
    aspect sys:ModelManifest

    -- If a Version was recommended but is no longer so, the action in this state
    -- will set the RecommendedVersion.
    state NoRecommendations = not exists filter Versions with IsRecommended
      on entry
        do for Author
          VersionToInstall = extern >> HighestVersion for External

    external
      aspect sys:ModelManifest$External
      -- Description
      -- IsLibrary
      -- DomeinFileName
      -- Notice that we have to register the LocalModelName on the filled context role in the collection,
      -- so we can create ModelManifest with a user-defined name. 
      -- The Model URI (the 'logical name' of the model), e.g. model://perspectives.domains#System.
      property ModelURI (functional) = "model://" + context >> Repository >> Repository$External$NameSpace + "#" + binder Manifests >> LocalModelName >>= first
      -- The URL of the Repository (and it will refer to the ServerUrl).
      property RepositoryUrl (functional) = binder Manifests >> context >> extern >> RepositoryUrl
      -- The URL of the Instances database of the Repository.
      -- Rename to InstancesURL
      property PublicUrl = RepositoryUrl + context >> Repository >> InstancesDatabase + "/"
      -- The highest version number
      property HighestVersion = context >> Versions >> Versions$Version >>= maximum
      -- The version recommended by the Author
      property RecommendedVersion = (filter context >> Versions with IsRecommended) >> Versions$Version
      -- The version to install. It depends on a calculation, but we have to store it explicitly 
      -- so we can even retrieve it on system startup.
      -- This property must be published.
      -- PDRDEPENDENCY
      property VersionToInstall (String)
      property RecomputeVersionToInstall (Boolean)
      
      -- This might become a standard pattern.
      -- It is triggered by setting RecomputeVersionToInstall to true on removing a Version.
      state RecomputeVersion = RecomputeVersionToInstall
        on entry
          do for Author
            -- However, when we enter this state, the Version role instance hasn't yet been actually removed.
            -- We then cause this state to exit.
            RecomputeVersionToInstall = false
        on exit
          do for Author
            -- By the time the exit is evaluated, the Version role instance has been removed and HighestVersion 
            -- evaluates to the correct value.
            VersionToInstall = HighestVersion
    
    -- Embedded contexts are not removed automatically with their embedder!
    on exit
      do for Author
        delete context bound to Versions
    
    -- The external role of the Repository.
    context Repository (functional) = extern >> binder Manifests >> context >> extern

    -- Inherits credentials for the ServerUrl and RepositoryUrl, and write access to the ModelsDatabase and the InstancesDatabase of the Repository.
    user Author (relational) filledBy (Repository$Authors, Repository$Admin)
      perspective on extern
        props (Description, IsLibrary, VersionToInstall, RecomputeVersionToInstall) verbs (Consult, SetPropertyValue)
      -- NOTA BENE: dit betekent dat installaties proberen instanties van ModelsInUse te publiceren!
      perspective on sys:MySystem >> ModelsInUse
        only (Fill, Remove)
        props (ModelToRemove) verbs (SetPropertyValue)
      perspective on Versions
        only (Create, Fill, RemoveContext, CreateAndFill, Delete, DeleteContext)
        props (Versions$Version, Description, Patch, Build) verbs (Consult, SetPropertyValue)
        action UpdateModel
          letA
            -- DomeinFileName without version of the origin.
            dfilename <- context >> extern >> DomeinFileName
            -- Notice that because Versions is a published role, there are no backlinks to roles it fills.
            basicmodel <- (filter sys:MySystem >> ModelsInUse with VersionedModelManifest$External$DomeinFileName == dfilename) >>= first
          in 
            callEffect cdb:UpdateModel( VersionedModelURI, false )
            ModelToRemove = VersionedModelURI for basicmodel
            bind_ origin >> binding to basicmodel
      perspective on Versions >> binding >> context >> Author
        only (Fill, Create)
        props (FirstName, LastName) verbs (Consult)
      perspective on Author
        all roleverbs
        props (FirstName, LastName) verbs (Consult)
      action CreateVersion
        create role Versions
      screen
        who
          Author
            master
              without props (FirstName)
            detail
        what
          row
            markdown <### Model Manifest
                      A manifest describes a model and its versions.
                      >
            form External
              without props (RecomputeVersionToInstall, IsLibrary)
              props (Description, VersionToInstall, DomeinFileName) verbs (Consult, SetPropertyValue)
        where
          Versions
            master
              markdown <## Add a version
                        In order to add a version of your manifest, use the action `CreateVersion` from the menu on the top left.
                        Open the details of the row that appears. Add a version number. A version should be of the form "Major.Minor" where both
                        components should be integers. For example: "1.0" or "2.11".
                      >
              without props (Versions$Version, Description, Patch, Build)
            detail
    
    -- A public version of ModelManifest is available in the database cw_<NameSpace>.
    public Visitor at extern >> PublicUrl = sys:Me
      perspective on extern
        props (DomeinFileName, Description, IsLibrary, VersionToInstall) verbs (Consult)
      -- NOTA BENE: dit betekent dat installaties proberen instanties van ModelsInUse te publiceren!
      perspective on sys:MySystem >> ModelsInUse
        only (Fill, Remove)
        props (ModelToRemove) verbs (SetPropertyValue)
      perspective on Versions
        props (Versions$Version, Description, VersionedModelURI, VersionedModelManifest$External$DomeinFileName, Patch, Build) verbs (Consult)
        action StartUsing
          -- This method also adds an instance of ModelsInUse and adds the VersionedModelURI to property ModelToRemove.
          -- It also sets InstalledPatch and InstalledBuild.
          callEffect cdb:AddModelToLocalStore( VersionedModelURI )
          
        action UpdateModel
          letA
            -- DomeinFileName without version of the origin.
            dfilename <- context >> extern >> DomeinFileName
            -- Notice that because Versions is a published role, there are no backlinks to roles it fills.
            basicmodel <- (filter sys:MySystem >> ModelsInUse with VersionedModelManifest$External$DomeinFileName == dfilename) >>= first
          in 
            callEffect cdb:UpdateModel( VersionedModelURI, false )
            ModelToRemove = VersionedModelURI for basicmodel
            bind_ origin >> binding to basicmodel
          -- notify Visitor
          --   "You updated to version {VersionedModelURI}."
        -- Update imported models first.
        -- NOTE: the registration of imported models is not updated! I.e. their ModelsInUse roles will still be bound to old versions.
        action UpdateModelWithDependencies
          callEffect cdb:UpdateModel( VersionedModelURI, true )
      
      screen
        who
        what 
          row
            markdown <### Model Manifest
                      A manifest describes a model and its versions.
                      >
            form External
              without props (IsLibrary, DomeinFileName)
        where
          Versions
            master
              markdown <## Versions
                        The versions of the manifest. You can install a version by selecting the item `Start using` from the menu.
                        Once you have installed a version, you can update it by selecting the item `Update model` from the menu.
                      >
              without props (Versions$Version, Description, VersionedModelURI, VersionedModelManifest$External$DomeinFileName, Patch, Build)
            detail

    context Versions (relational) filledBy VersionedModelManifest
      aspect sys:ModelManifest$Versions
      -- Version
      -- VersionedLocalModelName
      state ReadyToMake = (exists Versions$Version) and (not exists binding) and not exists Patch
        on entry
          do for Author
            letA
              v <- (context >> extern >> RecommendedVersion) orElse (context >> extern >> HighestVersion)
              versionname <- (context >> extern >> binder Manifests >> context >> extern >> NameSpace_ >>= first) + "-" + VersionedLocalModelName
            in
              -- As the PDR derives this name from the modelURI, we have to name the ModelManifest with its LocalModelName.
              create_ context VersionedModelManifest named versionname bound to origin
              bind currentactor to VersionedModelManifest$Author in origin >> binding >> context
              -- NOTE that we conceivably might add a version with a lower number than the highest.
              VersionToInstall = v for context >> extern
              -- dit werkt
              Patch = 0
              Build = 0
      on exit
        do for Author
          RecomputeVersionToInstall = true for context >> extern

  case VersionedModelManifest
    aspect sys:VersionedModelManifest
    aspect sys:ContextWithNotification

    state NoTranslation = not exists Translation
      on entry
        do for Author
          create role Translation

    external
      aspect sys:VersionedModelManifest$External
      -- VersionedModelManifest$External$Description
      -- VersionedModelManifest$External$DomeinFileName
      -- VersionedModelManifest$External$Version
      -- VersionedModelManifest$External$Patch
      -- VersionedModelManifest$External$Build
      -- The Version property is registered on ModelManifest$Versions so we can use it to create a DNS URN for it (it must be a public resource)
      -- PDRDEPENDENCY
      property ModelURI (functional) = binder Versions >> context >> extern >> ModelManifest$External$ModelURI
      property VersionedModelURI = VersionedModelManifest$External$ModelURI + "@" + External$Version
      property ArcFile (File)
        pattern = "text/arc" "Only .arc files (Perspectives Language source files) are allowed, so use `text//arc."
      property ArcSource = callExternal files:FileText( ArcFile ) returns String
      property ArcFeedback (String)
        minLength = 81
      property PublicUrl (functional) = binder Versions >> context >> extern >> PublicUrl
      -- Only one VersionedModelManifest can be the recommended version at a time.
      property IsRecommended (Boolean)
      -- The (Javascript) DateTime value of the last Arc file upload.
      property LastChangeDT (DateTime)
      -- The readable date and time of the last upload.
      property LastUpload = callExternal util:FormatDateTime( LastChangeDT, "nl-NL", "{\"dateStyle\": \"short\", \"timeStyle\": \"short\"}" ) returns String
      property MustUpload (Boolean)
      -- This must be writerOnly.
      -- The property will have no value in a new installation, effectively preventing upload on receiving a new Version
      property AutoUpload (Boolean)
      -- Whether the model is stored in the local store or in the repository, or not at all.
      property Store (String)
        enumeration = ("Not", "Locally", "Repository")
      -- Whether the model is loaded internally immediately. 
      property ApplyInSession (Boolean)

      on exit
        do for Author
          -- Delete the DomeinFile.
          callEffect p:RemoveFromRepository( VersionedModelManifest$External$VersionedModelURI )

      state BecomesRecommended = IsRecommended
        on entry
          do for Author
            letA
              previousversion <- binder Versions >> context >> extern >> VersionToInstall >>= first
            in
              -- no other version can be recommended
              IsRecommended = false for filter binder Versions >> context >> Versions with Versions$Version == previousversion
              -- Set the version to download.
              VersionToInstall = Version for binder Versions >> context >> extern
      
      -- This state triggers UploadToRepository.
      state ProcessArc = AutoUpload and (exists ArcSource) and ((callExternal sensor:ReadSensor( "clock", "now" ) returns DateTime > LastChangeDT + 10 seconds) or not exists LastChangeDT)
        on entry
          do for Author
            ArcFeedback = callExternal p:ParseAndCompileArc( ModelURI, ArcSource ) returns String
            LastChangeDT = callExternal sensor:ReadSensor( "clock", "now" ) returns DateTime
            MustUpload = true

      state AfterSuccesfulParse = (ArcFeedback matches regexp "^OK") and MustUpload
        -- NOTE. This state triggers GenerateYaml.
        -- Why not roll up both states into one? I hope this design ensures that the required files are available.!
        state UploadToRepository = Store == "Repository"
          on entry
            do for Author
              -- This will upload an empty Translations table, too.
              callEffect p:UploadToRepository( VersionedModelManifest$External$VersionedModelURI, 
                callExternal util:ReplaceR( "bind publicrole.*in sys:MySystem", "", ArcSource ) returns String)
              Build = Build + 1
              MustUpload = false
              GenerateYaml = true for context >> Translation
            notify Author
              "Version {External$Version} (build {Build}) has been uploaded to the repository for {binder Versions >> context >> Repository >> NameSpace >>= first}."
        
        state StoreInLocalDatabase = Store == "Locally"
          on entry
            do for Author
              callEffect p:StoreModelLocally( VersionedModelManifest$External$VersionedModelURI, ArcSource )
              Build = Build + 1
              MustUpload = false
            notify Author
              "Version {External$Version} (build {Build}) has been stored in the local store."

        state ApplyImmediately = ApplyInSession
          on entry
            do for Author
              callEffect p:ApplyImmediately( ModelURI, ArcSource )
              MustUpload = false
            notify Author
              "Version {External$Version} (build {Build}) has been applied to the current session."
        
        state NoAction = not ApplyInSession
          on entry
            do for Author
              MustUpload = false
            notify Author
              "Version {External$Version} (build {Build}) has not been stored in the local store or applied to the current session."
      
    thing Translation
      property FileName = context >> extern >> VersionedModelURI + ".yaml"
        readableName
      -- Whether the Author wants to generate a new Yaml file for translation.
      property GenerateYaml (Boolean)
      -- The (Javascript) DateTime value of the last Yaml file upload.
      property LastYamlChangeDT (DateTime)

      property ModelTranslation = callExternal p:GenerateFirstTranslation( context >> extern >> VersionedModelURI ) returns String
      property TranslationYaml (File)
        pattern = "text/yaml" "Only .yaml files for translation are allowed, so use `application//x-yaml."
      -- Construct a (new version of the) YAML file that the Author can download to add translations.
      -- Invariant for new VersionedModelManifests: a Translation table is uploaded to the repository.
      state GenerateYaml = GenerateYaml and context >> extern >> ArcFeedback matches regexp "^OK"
        on entry
          do for Author
            letA
                -- Given the invariant (a Translation table will be available in the Repository on this VersionedModelManifest), 
                -- we can start generating an augmented model translation immediately.
                modeltranslation <- callExternal p:AugmentModelTranslation( ModelTranslation, context >> extern >> VersionedModelURI) returns String
                -- modeltranslation <- callExternal p:GenerateFirstTranslation( context >> extern >> VersionedModelURI ) returns String
                text <- callExternal p:GetTranslationYaml( modeltranslation ) returns String
              in
                create file FileName as "text/yaml" in TranslationYaml
                  text
                GenerateYaml = false
      
      -- Construct a new TranslationTable from the YAML that the author has enriched with translations.
      -- Will be triggered on each new version of TranslationYaml, whether generated in state GenerateYaml or uploaded by the end user.
      state GenerateTranslationTable = (exists TranslationYaml) and ((callExternal sensor:ReadSensor( "clock", "now" ) returns DateTime > LastYamlChangeDT + 10 seconds) or (not exists LastYamlChangeDT)) and context >> extern >> ArcFeedback matches regexp "^OK"
        on entry
          do for Author
            letA
              modeltranslation <- callExternal p:ParseYamlTranslation( TranslationYaml ) returns String
            in
              callEffect p:GenerateTranslationTable( modeltranslation, context >> extern >> VersionedModelURI)
              LastYamlChangeDT = callExternal sensor:ReadSensor( "clock", "now" ) returns DateTime


    user Author (relational) filledBy cm:ModelManifest$Author
      aspect sys:ContextWithNotification$NotifiedUser
      perspective on extern
        props (DomeinFileName, Version, ArcSource, LastUpload) verbs (Consult)
        props (ArcFile, ArcFeedback, Description, IsRecommended, Build, Patch, LastChangeDT, MustUpload, AutoUpload, Store, ApplyInSession) verbs (Consult, SetPropertyValue)
      perspective on Translation
        only (Create, Remove, Delete)
        props (TranslationYaml, GenerateYaml, LastYamlChangeDT) verbs (Consult, SetPropertyValue, DeleteProperty)
        props (FileName, ModelTranslation) verbs (Consult)
        action GenerateTranslationTable
          letA
            modeltranslation <- callExternal p:ParseYamlTranslation( context >> Translation >> TranslationYaml ) returns String
          in
            callEffect p:GenerateTranslationTable( modeltranslation, context >> extern >> VersionedModelURI)
            LastYamlChangeDT = callExternal sensor:ReadSensor( "clock", "now" ) returns DateTime for context >> Translation

      perspective on Manifest
        props (VersionToInstall) verbs (Consult, SetPropertyValue)
      perspective on Manifest >> context >> Versions
        props (IsRecommended) verbs (SetPropertyValue)
      perspective on Author
        all roleverbs
        props (FirstName, LastName) verbs (Consult)
      screen
        who
          Author
            master
              without props (FirstName)
            detail
        what
          tab "Manifest"
            form External
              without props (ArcFile, ArcFeedback, LastChangeDT, MustUpload, AutoUpload, Store, ApplyInSession, ArcSource)
          tab "Compile"
            row
              markdown <### Compile your model
                        Select the `ArcFile` control and press enter. Then upload your file. There are rules:

                        * It's domain declaration *must* be in the repository Domain (or namespace. If you are not sure about that: move twice up to the wider context and 
                        read the Domain of the Repository).
                        * Its local name *must* equal the name of the manifest (move up to the wider context to read that).
                        ---
                        Compiling will take some time. Watch the `ArcFeedback` field for possible errors and correct them.
                        On succes, the compiled model is uploaded automatically to the repository.
                        Don't forget to update the model in your local installation!
                      >
            row
              form External
                without props (Description, IsRecommended, Version, Build, Patch, ArcSource, MustUpload, DomeinFileName, AutoUpload, Store, ApplyInSession)
            row 
              markdown <#### Where to store
                        Storage: choose from 
                        * **Not**: the model is not stored anywhere, but only compiled.
                        * **Locally**: the model is stored in the local store, so it is available for you only.
                        * **Repository**: the model is stored in the repository, so it is available for all users.
                        
                        #### Compiling and applying immediately
                        **Apply immediately**: choose whether the model is loaded into the current session.
                        **Auto upload** means that the model is automatically compiled when you upload it.
                      >
            row
              form External
                without props (DomeinFileName, Version, ArcSource, LastUpload, ArcFile, ArcFeedback, Description, IsRecommended, Build, Patch, LastChangeDT, MustUpload)
          tab "Translate"
            row
              markdown <## Translate your model
                        * Adhere to the convention of using English in the model, and use the translation file to translate the model into your own language.
                        * The `Yaml translation file` is a YAML file that contains all the model's readable names and descriptions.
                        It is generated automatically when you compile the model.
                        You can download it, edit it in a text editor, and upload it again.
                        * Take care to preserve the indentation (only use 'soft tabs'. A tab should be two spaces).
                        * To upload your augmented translation, select the `Yaml translation file` control and press enter. Then upload your file.
                      >
            row
              form Translation
                without props (ModelTranslation)
        where
    
    public Visitor at (extern >> PublicUrl) = sys:Me
      perspective on extern
        props (Version, Description, IsRecommended, Patch, Build) verbs (Consult) -- ModelURI geeft een probleem. Probeer VersionedModelManifest$External$ModelURI.
      perspective on Manifest
        props (ModelURI) verbs (Consult) 
      
      screen 
        who
        what 
          row
            markdown <### Model Version
                      A model version is a specific version of a model, with a specific name and description.
                      >
          row
            form External
              without props (IsRecommended)
        where
          markdown <### How to install or update a version
                    Move to the wider context (the Manifest) to see all versions of the model. Install or update from there.
                    >

    context Manifest (functional) = extern >> binder Versions >> context >> extern
    aspect thing sys:ContextWithNotification$Notifications
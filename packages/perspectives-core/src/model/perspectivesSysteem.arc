-- Copyright Joop Ringelberg and Cor Baars 2019, 2020, 2021, 2022, 2023, 2024

-- PDRDEPENDENCY
domain model://perspectives.domains#System
  use sys for model://perspectives.domains#System
  use cdb for model://perspectives.domains#Couchdb
  use ser for model://perspectives.domains#Serialise
  use sensor for model://perspectives.domains#Sensor
  use util for model://perspectives.domains#Utilities

  -- model:System (short for model://perspectives.domains#System) is booted in a unique way.
  -- Other models rely on there being an instance of sys:PerspectivesSystem and a Installer role in it.
  -- That precondition obviously fails for this model.
  -- Consequently, we create both in code. 
  -- We also create their indexed names, so we can refer to them below.
  -- The instance of TheWorld and the instance of PerspectivesUsers that represents the person that creates this installation,
  -- has to be created in the PDR as well. We also add the public key of the end user.
  on entry
    do for sys:PerspectivesSystem$Installer
      letA
        start <- create role StartContexts in sys:MySystem
      in
        bind_ sys:MySystem >> extern to start
        Name = "My System" for start
        IsSystemModel = true for start


        -- NOTE that the following line only compiles correctly when
        --    * the referred Repository and the type describing its model (model://perspectives.domains/CouchdbManagement) are available;
        -- which in turn requires a previously existing installation with both the System and CouchdbManagement model available to compile them.
        -- This is because the line below is a forward reference to a CouchdbManagement type.
        -- This is a Catch22 situation that fully plays out when we recompile all models of an installation. 
        -- For that reason, it is removed from the source that is included in the DomeinFile. Instead, it is added during system setup.

        -- Add the perspectives.domains repository as BaseRepository:
        -- bind publicrole pub:https://perspectives.domains/cw_servers_and_repositories/#perspectives_domains$External to BaseRepository in sys:MySystem

  state FirstInstallation = (callExternal util:SystemParameter( "IsFirstInstallation" ) returns Boolean) and (exists sys:TheWorld >> PerspectivesUsers)
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- When a Person is used to fill a user role, SocialEnvironment will be shared with peers.
          mysocialenvironment <- create context SocialEnvironment named "TheSocialEnvironment"
        in 
          -- notice that there is a state in User that is entered as soon as we have 
          -- SocialEnvironment$Me, on entry of which we fill sys:Me.
          bind mysocialenvironment >> extern to SocialEnvironment in sys:MySystem

  -- PDRDEPENDENCY
  aspect user sys:PerspectivesSystem$Installer

  -- Used as model://perspectives.domains#System$RoleWithId$Id in the PDR code.
  thing RoleWithId
    -- PDRDEPENDENCY
    property Id = callExternal util:RoleIdentifier() returns String

  -- PDRDEPENDENCY
  -- Used to add chat to a context.  
  thing Chat
    property Messages (relational, String)
      messageProperty
    property Media (relational, String)
      mediaProperty
  
  -- PDRDEPENDENCY
  user WithCredentials
    -- | The role identifier of the filler of SocialEnvironment$Me - that is, the unique identifier of this user
    -- | in the Perspectives Universe (a PerspectivesUsers instance).
    -- | This includes the storage scheme!
    -- | A consequence of this design is that every account with an external service obtained through a Perspectives model
    -- | will have this identifier as user name.
    -- | Also, every account that has been established outside of Perspectives and must be registered with Perspectives,
    -- | should have this identifier as user name.
    -- PDRDEPENDENCY
    property UserName = SpecificUserName orElse callExternal util:BottomIdentifier() returns String
    -- property UserName = SpecificUserName orElse callExternal util:SystemIdentifier() returns String
    property SpecificUserName (String)
    -- PDRDEPENDENCY
    property Password (String)
    -- PDRDEPENDENCY
    property AuthorizedDomain (String)

  user Identifiable
      -- PDRDEPENDENCY
      property LastName (mandatory, String)
        readableName
      property FirstName (mandatory, String)
      -- If cancelled, the user's peers will stop synchronizing with him/her.
      -- PDRDEPENDENCY
      property Cancelled (Boolean)
      -- Having a PublicKey is a proxy for having an installation.
      -- A Persons instance without a keypair can fill User roles, but does not
      -- participate (yet) in the Perspectives Universe.
      -- the private key is never available in Perspectives data, only as an object in IndexedDB.
      -- PDRDEPENDENCY
      property PublicKey (String)

  -- TheWorld is shared by everyone. It is identified by def:#TheWorld.
  case TheWorld
    indexed sys:TheWorld
    aspect sys:RootContext

    -- PDRDEPENDENCY
    user PerspectivesUsers (relational)
      aspect sys:Identifiable
      -- The unique key provided to each new participant in the Perspectives Trusted Network by one of his peers.
      -- It can be used to store a limited number of media files in the perspectives sharedfile storage.
      -- PDRDEPENDENCY (actually, a MyContexts dependency)
      property SharedFileServerKey (String)
      state NoKey = (not HasKey) and (exists sys:SocialMe >> SharedFileServerKey)
        on entry
          do for Initializer
            SharedFileServerKey = callExternal util:GetSharedFileServerKey( sys:SocialMe >> SharedFileServerKey ) returns String
            HasKey = true
      property HasKey (Boolean)

      perspective on PerspectivesUsers
        selfonly
        props (SharedFileServerKey, HasKey) verbs (Consult)
    
    user NonPerspectivesUsers (relational)
      aspect sys:Identifiable

    -- PDRDEPENDENCY
    user Initializer = sys:Me
      perspective on PerspectivesUsers
        only (Create)
        props (Identifiable$PublicKey, SharedFileServerKey, HasKey) verbs (SetPropertyValue, AddPropertyValue)
 
  -- MySocialEnvironment is the same on all of my devices.
    -- PDRDEPENDENCY
  case SocialEnvironment
    indexed sys:MySocialEnvironment
    -- As we share SocialEnvironment over installations, this will happen only in the first installation.
    -- SocialEnvironment is created on condition of there being an instance of PerspectivesUsers.
    aspect sys:RootContext
    external 
      aspect sys:RootContext$External
    state InitMe = (callExternal util:SystemParameter( "IsFirstInstallation" ) returns Boolean) and not exists Me 
      on entry
        do for SystemUser
          bind sys:TheWorld >> PerspectivesUsers >>= first to Me
          bind sys:TheWorld >> PerspectivesUsers >>= first to Persons
    -- To fill other user roles: require Persons as user role filler if there is no need to consider a natural person
    -- to be a peer with whom one wants to synchronize. Require PerspectivesSystem$Users otherwise.
    -- Using Persons rather than TheWorld$PerspectivesUsers or TheWorld$NonPerspectivesUsers creates a layer of indirection
    -- that allows us to switch rather painlessly from NonPerspectivesuser to PerspectivesUsers.
    -- Persons will be synchronized between peers because they will have a perspective on SocialEnvironment$Me with the properties of sys:Identifiable
    -- We also make sure that each Persons instance we know about has access to all our System$User identities, so he/she can synchronize to us.
    -- PDRDEPENDENCY
    user Persons (relational, unlinked) filledBy (PerspectivesUsers, NonPerspectivesUsers)
      perspective on Me
        props (Cancelled, LastName, FirstName, PublicKey) verbs (Consult)

    user Me filledBy PerspectivesUsers
      aspect sys:RoleWithId
      indexed sys:SocialMe
      property MyIdentity (File)

    user SystemUser = sys:Me
      perspective on Me
        only (Create, Fill)
        props (FirstName, LastName, PublicKey, MyIdentity) verbs (Consult)
        props (MyIdentity, Cancelled) verbs (SetPropertyValue, Consult)
        action ExportForAnotherInstallation
          letA
            text <- callExternal ser:SerialiseFor( [role model://perspectives.domains#System$SocialEnvironment$SystemUser], context >> extern ) returns String
          in
            create file ("identity_of_" + LastName + ".json") as "text/json" in MyIdentity for origin
              text
        action Cancel
          Cancelled = true
      perspective on Persons
        only (Create, Fill)
        props (FirstName, LastName) verbs (SetPropertyValue, Consult)
      -- Use this perspective to select a PerspectivesUsers instance to replace the filler of an instance of Persons
      -- that previously was filled by a NonPerspectivesUsers instance.
      perspective on sys:TheWorld >> PerspectivesUsers
        props (FirstName, LastName) verbs (Consult)
        props (Cancelled) verbs (SetPropertyValue)
  
  -- PDRDEPENDENCY
  case PerspectivesSystem
    -- PDRDEPENDENCY
    indexed sys:MySystem
    aspect sys:RootContext
    aspect sys:ContextWithNotification
    aspect sys:ContextWithSettings

    state NoCaches = not exists SystemCaches
      on entry
        do for User
          create context Caches bound to SystemCaches
    
    -- This is for installations that come after the first one.
    state NoSocialEnvironment = (not exists SocialEnvironment) and (exists sys:MySocialEnvironment)
      on entry
        do for User
          bind sys:MySocialEnvironment >> extern to SocialEnvironment

    external
      aspect sys:RootContext$External
      aspect sys:ContextWithSettings$External
      -- PDRDEPENDENCY
      property ConnectedToAMQPBroker (Boolean)
      -- PDRDEPENDENCY
      property CardClipBoard (String)
      property ShowLibraries (Boolean)
      property MyContextsVersion = callExternal util:SystemParameter( "MyContextsVersion" ) returns String
      property PDRVersion = callExternal util:SystemParameter( "PDRVersion" ) returns String
      -- E.g. '30-01-2024'. This property will be automatically changed right after midnight or as soon as the PDR starts up.
      property CurrentDate (Date)
      -- E.g. '13:00', 14:00', etc. This property will be automatically changed right after the hour or as soon as the PDR starts up.
      property CurrentHour (Time)
      property CurrentLanguage (String)
        enumeration = ("nl", "en")
        setting
      property PreviousLanguage (String)
        enumeration = ("nl", "en")
      -- PDRDEPENDENCY
      -- Should be true to show System, BrokerServices and CouchdbManagement from the user.
      property ShowSystemApps (Boolean)
        setting
      
      state LanguageChanged = not (CurrentLanguage == PreviousLanguage)
        on entry
          do for User
            PreviousLanguage = CurrentLanguage
            callEffect util:SetCurrentLanguage( "currentLanguage", CurrentLanguage )

      view ShowLibraries (ShowLibraries)

    -- PDRDEPENDENCY
    -- To fill other user roles: require User if one wants to synchronize with the natural person 
    -- represented by that User. Require SocialEnvironment$Persons otherwise.
    -- Why fill User with Persons? Whenever we have a PerspectivesSystem$User instance, there is, 
    -- by construction, a PerspectivesUsers behind it. We do not need the extra indirection here.
    -- However, to be able to use Persons effectively when we don't care whether or not there is 
    -- a PerspectivesUsers behind it, we have to have Persons for PerspectivesUsers, too.

    -- If User is not filled with Persons, we can never fill a role that requires Persons with User. That may be inconvenient.
    -- OF WE MOETEN ME VULLEN MET PERSONS.
    -- TODO. WHAT ABOUT SOCIALENVIRONMENT$ME?
    user User (mandatory) filledBy PerspectivesUsers
      aspect sys:ContextWithNotification$NotifiedUser
      -- This will happen on importing SocialEnvironment and Me from another installation.
      -- If we import a peer's data, we will get a User instance that is already filled.
      state FillWithPerspectivesUser = (not exists binding) and exists sys:MySocialEnvironment >> Me
        on entry
          do for User
            -- User has a sufficient perspective on itself to do this.
            bind_ sys:MySocialEnvironment >> Me >> binding >> binder Persons >>= first to origin

      -- PDRDEPENDENCY
      property Channel = (binder Initiator union binder ConnectedPartner) >> context >> extern >> ChannelDatabaseName
      -- User instances need not have a value for this property. It is used in the PDR to
      -- ensure serialisation of the User role.
      -- PDRDEPENDENCY
      property Id (String)
      -- property Id = callExternal util:RoleIdentifier() returns String

      -- PDRDEPENDENCY
      indexed sys:Me
      view VolledigeNaam (FirstName, LastName)
      perspective on User
        only (Create, Fill)
        props (LastName, FirstName, PublicKey) verbs (SetPropertyValue)
        props (Channel) verbs (Consult)
      perspective on StartContexts
        props (Name) verbs (Consult)
      perspective on PinnedContexts
        only (CreateAndFill, Remove, Delete)
      perspective on RecentContexts
        only (CreateAndFill, Remove, Delete)
        props (LastShownOnScreen) verbs (Consult, SetPropertyValue)
      perspective on Contacts
        props (FirstName, LastName) verbs (Consult, SetPropertyValue)
      perspective on ActiveContacts
        props (FirstName, LastName) verbs (Consult, SetPropertyValue)
      perspective on OutgoingInvitations >> binding >> context >> Inviter
        only (Create, Fill)
      perspective on OutgoingInvitations
        only (CreateAndFill, Remove, RemoveContext)
        props (InviterLastName) verbs (Consult)
      action CreateInvitation
        letA
          invitation <- create context Invitation bound to OutgoingInvitations
        in
          bind sys:Me to Inviter in invitation >> binding >> context
      perspective on External
        props (ShowLibraries, CurrentLanguage, PreviousLanguage, ShowSystemApps) verbs (Consult, SetPropertyValue)
        props (MyContextsVersion, PDRVersion, CurrentDate, CurrentHour) verbs (Consult)
      -- Notice that these roles are filled with the public version of VersionedModelManifest$External.
      -- We can actually only show properties that are in that perspective.
      perspective on ModelsInUse
        only (Remove, Create, CreateAndFill, Fill, RemoveFiller)
        props (ModelName, Description, Version, Patch, Build) verbs (Consult)
        props (InstalledPatch, InstalledBuild, UpdateOnBuild, DetachedFiller) verbs (SetPropertyValue)
        in object state Filled
          action Detach
            DetachedFiller = binding >> callExternal util:RoleIdentifier() returns String
            unbind_ origin >> binding from origin
        in object state UnFilled
          action Reattach
            bind_ (roleinstance (sys:VersionedModelManifest$External) DetachedFiller) to origin

      perspective on ModelsToUpdate
        props (ModelName, Description, Version) verbs (Consult)
        in object state InstallUpdate
          action InstallPatch
            callEffect cdb:UpdateModel( ModelToRemove, false )
            InstalledPatch = Patch
            InstalledBuild = Build
        in object state InstallBuild
          action UpdateBuild
            callEffect cdb:UpdateModel( ModelToRemove, false )
            InstalledBuild = Build
      perspective on BaseRepository
        props (Domain) verbs (Consult)
      perspective on Repositories
        only (CreateAndFill, Remove)
        props (Domain) verbs (Consult)
      perspective on PendingInvitations
        props (InviterLastName, Message) verbs (Consult)
      perspective on SystemCaches
        defaults
        action ResetInvertedQueryDatabase
          callEffect cdb:ClearAndFillInvertedQueriesDatabase()
      perspective on SocialEnvironment
        only (CreateAndFill, Fill)
      
      perspective on ItemsOnClipboard
        only (Create, Fill, Remove, Delete)
        props (Name) verbs (Consult)
        props (Selected, ClipboardData) verbs (SetPropertyValue, Consult)
      perspective on SelectedClipboardItem
        props (ClipboardData) verbs (Consult)
      perspective on Apps
        props (Name) verbs (Consult)
      
      screen
        who 
          ActiveContacts
            master 
              without props (FirstName)
            detail
              props (FirstName, LastName) verbs (Consult)
        what
          row 
            markdown <### Perspectives system
                      This is the Perspectives system, representing your installation.

                      * Create and accept invitations to connect with other users.
                      * Go to an App store to install a new App.
                      * On the left, under **Who** you see your contacts.
                      >
          row  
            form External
              without props (ShowLibraries, CurrentLanguage, PreviousLanguage)
        where
          OutgoingInvitations
            master
              markdown <#### Outgoing invitations
                        These are the invitations you have sent to other users.
                        Create a new invitation either in the main menu on the left, or by [[action: CreateInvitation|clicking here]].
                        >
            detail
          ModelsInUse
            master
              without props (ModelName, Description, Version, Patch, Build, InstalledPatch, InstalledBuild, UpdateOnBuild, DetachedFiller)
            detail
              without props (DetachedFiller)
          ModelsToUpdate
            master
              without props (ModelName, Description, Version)
            detail
          BaseRepository
            master
              markdown <#### Base Repository
                        This is the base repository of Perspectives.
                        It contains the models that are used to run Perspectives.
                        >
            detail
          Repositories
            master
              markdown <#### Repositories
                        These are the repositories that you have access to.
                        Open a repository to see the models it contains.
                        >
            detail
          PendingInvitations
            master
              without props (Message)
            detail

    context SocialEnvironment filledBy SocialEnvironment

    -- In effect, this will filter out the Serialization persona.
    user Contacts = filter (callExternal cdb:RoleInstances( "model://perspectives.domains#System$TheWorld$PerspectivesUsers" ) returns sys:TheWorld$PerspectivesUsers) with (exists PublicKey) and (not this == sys:SocialMe >> binding)
    
    user ActiveContacts = filter Contacts with not Cancelled

    -- user Contacts = sys:MySocialEnvironment >> Persons

    -- PDRDEPENDENCY
    user Installer
      perspective on StartContexts
        only (Create, CreateAndFill, Remove, RemoveContext, Fill)
        props (Name, IsSystemModel) verbs (SetPropertyValue)
      perspective on IndexedContexts
        only (Create, Fill, Remove, RemoveContext)
        props (IndexedContexts$Name) verbs (SetPropertyValue)
      perspective on IndexedRoles
        only (Create, Fill, Remove, RemoveContext)
        props (IndexedRoles$Name) verbs (SetPropertyValue)
      perspective on BaseRepository
        only (CreateAndFill)
      perspective on SocialEnvironment
        only (CreateAndFill, Fill)

    user WWWUser = sys:Me
      perspective on User
        props (FirstName, LastName) verbs (Consult, SetPropertyValue)
      perspective on ItemsOnClipboard
        only (Create, Fill, Remove)
        props (Name) verbs (Consult)
        props (Selected) verbs (SetPropertyValue, Consult)
      perspective on AllNotifications
        props (Message) verbs (Consult)
      perspective on PinnedContexts
        only (Remove)
      perspective on RecentContexts
        only (Remove)
        props (LastShownOnScreen) verbs (Consult)
      perspective on ActualRecentContexts

    context OutgoingInvitations (relational) filledBy Invitation

    context BaseRepository filledBy ManifestCollection

    context Repositories (relational) filledBy ManifestCollection

    context AllRepositories = BaseRepository union Repositories

    -- PDRDEPENDENCY
    context ModelsInUse (relational) filledBy sys:VersionedModelManifest
      -- Includes a semantic version number. The PDR will set this value.
      -- PDRDEPENDENCY
      property ModelToRemove (String)
      -- PDRDEPENDENCY
      property InstalledPatch (Number)
      -- PDRDEPENDENCY
      property InstalledBuild (Number)
      property UpdateOnBuild (Boolean)
      property DetachedFiller (String)
      state InstallBuild = UpdateOnBuild and InstalledBuild < Build
        on entry
          notify User
            "Installing build {Build} of { ModelName } (replacing {InstalledBuild})."
          do for User
            callEffect cdb:UpdateModel( ModelToRemove, false )
            InstalledBuild = Build

      state InstallUpdate = InstalledPatch < Patch
        on entry
          notify User
            "Installing a new patch for { ModelName }."
          do for User
            callEffect cdb:UpdateModel( ModelToRemove, false )
            InstalledPatch = Patch
      state Filled = exists binding
      state UnFilled = (not exists binding) and exists DetachedFiller
          
      on exit
        -- notify User
        --   "Model {ModelToRemove} has been removed completely."
        do for User
          callDestructiveEffect cdb:RemoveModelFromLocalStore ( ModelToRemove )
    
    context ModelsToUpdate = filter ModelsInUse with isInState InstallUpdate or isInState InstallBuild

    -- context ModelsToUpdate = filter ModelsInUse with 
    --   -- Compare the Patch part of the Semantic Versioning string (the least or rightmost part).
    --   callExternal util:SelectR( "\\.(\\d+)$", ModelToRemove ) returns Number
    --   -- Dit is gebaseerd op het verkeerde idee dat ik in een VersionedModel het patch deel zou aanpassen.
    --     < callExternal util:SelectR( "\\.(\\d+)$", Version ) returns Number

    -- All context types that have been declared to be 'indexed' have an instance that fills this role.
    -- PDRDEPENDENCY
    context IndexedContexts (mandatory) filledBy sys:RootContext
      -- PDRDEPENDENCY
      property Name (mandatory, String)
        readableName

    -- All role types that have been declared to be 'indexed' have an instance that fills this role.
    -- PDRDEPENDENCY
    thing IndexedRoles (relational)
      -- PDRDEPENDENCY
      property Name (mandatory, String)
        readableName


    context Channels = User >> (binder Initiator union binder ConnectedPartner) >> context >> extern

    -- PDRDEPENDENCY
    -- StartContexts should be bound to Contexts that share an Aspect and that Aspect should have a name on the External role.
    -- These are the 'apps' of Perspectives.
    context StartContexts (relational) filledBy sys:RootContext

    -- PDRDEPENDENCY
    context Apps = letE
        showsystemapps <- extern >> ShowSystemApps
      in
        -- By default, if a StartContext has no value for IsSystemModel, that is interpreted as false.
        -- Hence `not IsSystemModel` will be true. In other words, by default all StartContexts are shown,
        -- irrespective of the value of ShowSystemApps.
        filter StartContexts with ((not IsSystemModel) or showsystemapps)

    context PendingInvitations = callExternal cdb:PendingInvitations() returns sys:Invitation$External

    thing Databases (mandatory, relational)
      -- Name is one of post, data, models.
      property Name (mandatory, String)
        readableName
      property Identifier (mandatory, String)

    -- A calculated role representing all available Notifications (from any context).
    context AllNotifications = callExternal cdb:RoleInstances( "model://perspectives.domains#System$ContextWithNotification$Notifications" ) returns sys:ContextWithNotification$Notifications

    context AllSettings = callExternal cdb:RoleInstances( "model://perspectives.domains#System$ContextWithSettings$External" ) returns sys:ContextWithSettings$External

    -- PDRDEPENDENCY
    context PinnedContexts (relational)

    -- PDRDEPENDENCY
    context RecentContexts (relational) filledBy sys:RecentContext
      property LastShownOnScreen (DateTime)
    
    context ActualRecentContexts = RecentContexts >> binding

    context SystemCaches (mandatory) filledBy Caches

    aspect thing sys:ContextWithNotification$Notifications

    -- PDRDEPENDENCY
    -- Notice we do not have a restriction on the filler. This is because we want to be able to fill it with any role kind and type.
    thing ItemsOnClipboard (relational)
      state IsSelected = Selected
        on entry
          do for User
            letA
              -- We have to filter, because when there actually was another item that is selected, right now there are two!
              -- LET OP: ZODRA WE MODELLEN IN DE WWW GUI COMPILEREN (MET DE NIEUWE PDR) KAN DE CONSTRUCTIE MET >>= first VERVALLEN.
              previousselected <- filter context >> SelectedClipboardItem with not (this >>= first == origin)
            in
              Selected = false for previousselected

    -- PDRDEPENDENCY
      property Selected (Boolean)
    -- PDRDEPENDENCY
      property ClipboardData (String)
      property Name = callExternal util:SelectR( "\"cardTitle\":\"(.+?)\"", ClipboardData ) returns String
        readableName
    
    -- PDRDEPENDENCY
    thing SelectedClipboardItem = filter ItemsOnClipboard with Selected

  -- A Collection of System Caches.
  case Caches
    aspect sys:ContextWithScreenState

    state NoCaches = not exists Cache
      on entry 
        do for Manager
          letA
            contextcache <- create role Cache
            rolecache <- create role Cache
            domeincache <- create role Cache
            querycache <- create role Cache
          in
            Name = "contextcache" for contextcache
            Name = "rolecache" for rolecache
            Name = "domaincache" for domeincache
            Name = "querycache" for querycache

    external
      aspect sys:ContextWithScreenState$External

    user Manager = sys:Me
      perspective on Cache
        defaults

    thing Cache (relational)
      -- aspect sys:Settings
      property Name (String)
        readableName
      property Size (Number)
      -- property Limit (Number)
      --   setting
      -- property PreviousLimit (Number)
      -- state Changed = not (Limit == PreviousLimit)
      --   on entry
      --     do for User
      --       PreviousLimit = Limit
      --       callEffect util:IdbSet( Name, Limit )

      state StartReading = context >> extern >> IsOnScreen
        on entry
          do for Manager every 2 Seconds
            Size = callExternal sensor:ReadSensor ( Name, "size" ) returns Number


  -- Use this as an aspect in contexts that should store their own notifications.
  -- PDRDEPENDENCY
  case ContextWithNotification
    -- PDRDEPENDENCY
    context Notifications (relational)
      -- PDRDEPENDENCY
      property Message (String)
    user NotifiedUser
      perspective on Notifications
        only (Remove, Delete)
        props (Message) verbs (Consult)
        action DeleteNotifications
          delete role Notifications

  case ContextWithScreenState
    external 
      property IsOnScreen (Boolean)

  case PhysicalContext
    user UserWithAddress
      -- The public URL of the PDR of the UserWithAddress.
      -- PDRDEPENDENCY
      property Host (String)
      -- The port where Couchdb listens.
      -- PDRDEPENDENCY
      property Port (Number)
      -- The public URL of the RelayServer of the UserWithAddress
      -- PDRDEPENDENCY
      property RelayHost (String)
      -- The port where Couchdb listens on the RelayServer.
      -- PDRDEPENDENCY
      property RelayPort (String)

  case RecentContext
  -- A Channel is shared by just two users.
  -- PDRDEPENDENCY
  case Channel
    aspect sys:PhysicalContext
    external
      -- PDRDEPENDENCY
      property ChannelDatabaseName (mandatory, String)
    -- PDRDEPENDENCY
    user Initiator filledBy sys:PerspectivesSystem$User
      aspect sys:PhysicalContext$UserWithAddress
      perspective on ConnectedPartner
      perspective on Initiator
    -- PDRDEPENDENCY
    user ConnectedPartner filledBy sys:PerspectivesSystem$User
      -- The public URL of the PDR of the partner.
      aspect sys:PhysicalContext$UserWithAddress
      perspective on Initiator
      perspective on ConnectedPartner
    user Me = filter (Initiator union ConnectedPartner) with filledBy sys:Me
    user You = filter (Initiator union ConnectedPartner) with not filledBy sys:Me
        
  case RootContext
    -- PDRDEPENDENCY
    external
      property Name (mandatory, String)
        readableName
      property IsSystemModel (Boolean)
    -- PDRDEPENDENCY
    user RootUser filledBy sys:TheWorld$PerspectivesUsers

  -- PDRDEPENDENCY
  -- Used to declare an external role as bearing settings.
  -- The properties that have facet 'Settings' are the ones that are used to construct the settings panel in the client.
  case ContextWithSettings
    external

  case Invitation
    state NoInviter = not exists Inviter
    state NoInvitee = not exists Invitee
    state UnlockInvitation = extern >> CorrectCodeEntered

    external
      property SerialisedInvitation (File)
      property Addressing = "This is an invitation to connect, sent by " + InviterLastName + ". Please contact this person to retrieve the security code needed to unlock the invitation.\n"
      property Message (String)
        maxLength = 256
      property CompleteMessage = Addressing + Message
      property ConfirmationCode (Number)
      property EnteredCode (Number)
      property InviterLastName = context >> Inviter >> LastName
        readableName
      property IWantToInviteAnUnconnectedUser (Boolean)
      property CorrectCodeEntered = ConfirmationCode == EnteredCode
      state Message = exists Message
      state CreateInvitation = exists ConfirmationCode
        on entry
          do for Inviter
            letA
              transaction <- callExternal ser:SerialiseFor( ((filter origin >> context >> contextType >> roleTypes with specialisesRoleType model://perspectives.domains#System$Invitation$Invitee) orElse [role model://perspectives.domains#System$Invitation$Invitee])) returns String
              invitation <- callExternal util:CreateInvitation( CompleteMessage, transaction, ConfirmationCode ) returns String
            in
              create file ("invitation_of_" + InviterLastName + ".json") as "text/json" in SerialisedInvitation for origin
                invitation

    user Inviter (mandatory) filledBy sys:TheWorld$PerspectivesUsers
      perspective on Invitee
        props (FirstName, LastName, HasKey) verbs (Consult)
      perspective on External
        props (Message, ConfirmationCode, SerialisedInvitation) verbs (SetPropertyValue, Consult)
        props (CompleteMessage) verbs (Consult)
        in object state Message
          action CreateInvitation
            ConfirmationCode = callExternal util:Random(100000, 999999) returns Number
      
      screen 
        who 
          Invitee
            master
              without props (FirstName, HasKey)
            detail
              without props (HasKey)
        what
          row 
            markdown <### Invite a new person to Perspectives
                      You can invite a person to Perspectives by sending an **invitation file**.
                      Enter a message. It will be appended to the standard message below.
                      After entering the message, an action button will appear to create the invitation file.
                      The button is shown with a lightning bolt icon.
                      Excecute the action and then download the file.
                      Send the file to the person you want to invite.
                      The file contains a security code that the person must enter to unlock the invitation.
                      After creating the file, the security code has apppeared, too.
                      Wait for the Invitee to contact you for the security code.
                      Only if you're sure the person who contacts you is the person you want to invite,
                      give him/her the security code.
                      After that, the person can fill the role of Invitee with his/her own data.
                      These will become available to you, too.
                      In this way you'll have established a connection in a safe way.
                      >
          row 
            form External
              without props (CompleteMessage, ConfirmationCode, SerialisedInvitation)
          row 
            form External
              without props (Message)
        where 

    user Invitee (mandatory) filledBy Guest
      perspective on Inviter
        props (FirstName, LastName, HasKey) verbs (Consult)
      perspective on extern
        props (InviterLastName, Message, ConfirmationCode, Addressing) verbs (Consult)
      screen
        who 
          Inviter
            master
              without props (FirstName, HasKey)
            detail
              without props (HasKey)
        what
          row 
            markdown <### Invitation to Perspectives
                      You have accepted this invitation to Perspectives.
                      >
          row 
            form External
              without props (Addressing, ConfirmationCode)
        where 
          -- The Guest will fill the Invitee role with his/her own data.
          -- The Guest will not be able to fill the Inviter role.

    -- Without the filter, the Inviter will count as Guest and its bot will fire for the Inviter, too.
    -- user Guest = (filter sys:SocialMe >> binding with not fills (currentcontext >> Inviter))
    user Guest = sys:SocialMe >> binding
      perspective on extern
        props (InviterLastName, Message, EnteredCode) verbs (Consult)
        props (EnteredCode) verbs (SetPropertyValue)
      perspective on Invitee
        props (FirstName, LastName) verbs (Consult)
        in context state NoInvitee
          only (Create, Fill)
          props (FirstName, LastName) verbs (Consult)
      perspective on Inviter
        only (Create, Fill)
        props (FirstName, LastName) verbs (Consult)
      in context state UnlockInvitation
        action AcceptInvitation
          bind sys:SocialMe >> binding to Invitee
      screen
        who 
          Inviter
            master
              without props (FirstName)
            detail
          Invitee
            master
              without props (LastName)
            detail
        what
          row
            markdown <### Invitation to Perspectives
                      You have received an invitation to Perspectives.
                      To unlock the invitation, you must enter the security code that the Inviter has given you.
                      >
              when not External >> CorrectCodeEntered
            markdown <### Invitation to Perspectives
                      You have been invited to Perspectives.
                      If you want to accept the invitation, you must fill the Invitee role with your own data.
                      [[action: AcceptInvitation|Accept by clicking here]].
                      >
              when External >> CorrectCodeEntered
          row 
            form External
              without props (Addressing)
        where 
          -- The Guest will fill the Invitee role with his/her own data.
          -- The Guest will not be able to fill the Inviter role.

  -- To be used as Aspect in model:CouchdbManagement$Repository
  case ManifestCollection
    external
      -- This is the namespace of the models in this repository, but dots are replaced by underscores.
      -- It is computed from Repositories$NameSpace on creating the repository (CouchdbManagement)
      property NameSpace_ (mandatory, String)
      -- This we show on screen.
      property Domain = NameSpace_ >> callExternal util:Replace( "_", "." ) returns String
        readableName
    
    context Manifests (relational) filledBy ModelManifest
      -- This is the local name, unqualified, of the model, e.g. "JoopsModel" or "System".
      -- It must be entered through a form. We need it on this role so we can create an instance
      -- of ModelManifest with this name.
      property LocalModelName (String)

  case ModelManifest
    aspect sys:RootContext
    -- PDRDEPENDENCY
    external
      aspect sys:RootContext$External
      property Name = binder Manifests >> LocalModelName >>= first
        readableName
      property Description (mandatory, String)
      property IsLibrary (mandatory, Boolean)
      property IsSystemModel (Boolean)
      -- The value of this property will be set automatically by the Couchdb:VersionedModelManifest$Author.
      -- It must be a local DomeinFileId, e.g. perspectives_domains-System.json (WITHOUT the version!)
      -- PDRDEPENDENCY
      property DomeinFileName (mandatory, String)
      
    context Versions (relational) filledBy VersionedModelManifest
      -- This value must be entered by the user. 
      -- We need it on this role so we can create an instance of VersionedModelManifest based on
      -- the Version value.
      property Version (mandatory, String)
        pattern = "^[0-9]+\\.[0-9]+(?:\\.dev)?$" "The form MAJOR.MINOR where both are integer numbers, or MAJOR.MINOR.dev."
      -- E.g. "System@1.0.0"
      property VersionedLocalModelName = (context >> extern >> binder Manifests >> LocalModelName >>= first) + "@" + Versions$Version

  case VersionedModelManifest
    external
      property ModelName (functional) = binder Versions >> context >> extern >> binder Manifests >> LocalModelName
      property Description (mandatory, String)
        minLength = 81
      -- Notice that we have to register the DomeinFileName on the context role in the collection (ModelManifest$Versions),
      -- to serve in the pattern that creates a DNS URI, so it can be a public resource.
      -- PDRDEPENDENCY
      property DomeinFileName (functional) = binder Versions >> context >> extern >> ModelManifest$External$DomeinFileName
      property Version (functional)        = binder Versions >> Versions$Version
      property VersionName = ModelName + "@" + Version
        readableName
      -- PDRDEPENDENCY
      property Patch (Number)
      -- PDRDEPENDENCY
      property Build (Number)
    user Visitor = sys:Me
      perspective on extern
        props (Description, DomeinFileName) verbs (Consult)

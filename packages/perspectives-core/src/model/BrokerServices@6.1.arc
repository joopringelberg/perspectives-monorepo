-- Copyright Joop Ringelberg and Cor Baars, 2020, 2021 -15
-- A model to maintain AMQP Broker Services.
domain model://perspectives.domains#BrokerServices
  use sys for model://perspectives.domains#System
  use bs for model://perspectives.domains#BrokerServices
  use util for model://perspectives.domains#Utilities
  use rabbit for model://perspectives.domains#RabbitMQ
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
          app <- create context BrokerServices
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Broker Services App" for start
          IsSystemModel = true for start
  
  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (bs:MyBrokers >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (bs:MyBrokers >> extern)
      in
        remove context indexedcontext
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  -------------------------------------------------------------------------------
  ---- BROKER SERVICES
  -------------------------------------------------------------------------------

  context Broker (relational) filledBy BrokerService

  -- The entry point (the `application`), available as bs:MyBrokers.
  case BrokerServices
    -- PDRDEPENDENCY
    indexed bs:MyBrokers
    aspect sys:RootContext
    external
      aspect sys:RootContext$External    
    -- The BrokerServices I manage.
    context ManagedBrokers (relational) filledBy BrokerService
      property StorageLocation (String)
        pattern = "^https://.*" "A url with the https scheme"
      state HasStorageLocation = exists StorageLocation
        on entry
          do for Manager
            create_ context BrokerService bound to origin
    
    context PublicBrokers (relational) filledBy BrokerService
      state NoContract = not exists (filter binding >> context >> Accounts with binding >> context >> AccountHolder >> binding == me)
    
    -- A contract for the BrokerService I use.
    context Contracts = me >> binder AccountHolder >> context >> extern

    -- PDRDEPENDENCY
    -- Arbitrarily take the first registered (=active) contract to be the one that is in use.
    context ContractInUse (functional) = filter Contracts with IsInUse >>= first

    user Manager = sys:Me
      perspective on ManagedBrokers
        only (Create, Fill, CreateAndFill, Remove)
        props (Name) verbs (Consult)
        props (StorageLocation) verbs (Consult, SetPropertyValue)
      action AddNewBroker
        create role ManagedBrokers
      perspective on Contracts
        props (Name, UseExpiresOn, IsInUse) verbs (Consult)
        props (IsInUse) verbs (SetPropertyValue)
        in object state Terminated
          perspective on Contracts >> binder Accounts
            only (RemoveContext)
          action DeleteMyContract
            remove context binder Accounts
      perspective on ContractInUse
        props (Name, UseExpiresOn) verbs (Consult)
      perspective on PublicBrokers
        only (Create, Fill, Remove)
        props (Name) verbs (Consult)
        in object state NoContract
          perspective on PublicBrokers >> binding >> context >> Accounts
            only (CreateAndFill, Remove, RemoveContext)
          perspective on PublicBrokers >> binding >> context >> Accounts >> binding >> context >> AccountHolder
            only (Create, Fill)
          perspective on PublicBrokers >> binding >> context >> Accounts >> binding >> context >> Administrator
            only (Create, Fill)
          action SignUp
            letA
              accountsinstance <- create context BrokerContract bound to Accounts in origin >> binding >> context
            in
              bind me to AccountHolder in accountsinstance >> binding >> context
              bind accountsinstance >> context >> Administrator to Administrator in accountsinstance >> binding >> context

      screen
        who
        what
          row
            markdown <## Get connected
                      MyContexts is most useful when you connect to other people. 
                      This installation has not yet a means to connect to others. 
                      Move to the [[link:pub:https://perspectives.domains/cw_v74vfn21lx/#rbdaciupyn$External|Perspectives Broker Service]] 
                      page to get online. You will read further instructions there.
                     >
              when not (exists bs:MyBrokers >> PublicBrokers)
            markdown <## Sign up to a service
                      You cannot yet connect to peers, but a service to do so is available below.
                      Select a row with a service (there is probably just one) by clicking it.
                      Then, in the toolbar below, click the bolt icon. A menu will drop down.
                      Click the "SignUp" item. 
                      
                      ***NOTE*** that by signing up, you will share your first- and last name 
                      with the manager of the service. He or she will be able to contact you 
                      through MyContexts.

                      A little later, an icon will appear in the top left part of your screen, 
                      indicating you can now connect with peers.
                      >
              when (exists bs:MyBrokers >> PublicBrokers) and (not exists bs:MyBrokers >> Contracts)
            markdown <## You can connect to peers
                      This installation is subscribed to a Broker Service and is able to exchange information 
                      automatically with peers you are in contact with (including the manager of this service).
                      To stop using this service, open the Contract and terminate it. Then execute action "Delete my contract" in the toolbar below (the bolt icon).
                     >
              when exists bs:MyBrokers >> Contracts
          row
            form ContractInUse
        where
          PublicBrokers
            master
              markdown <### Public broker service(s)
                        These are the public broker services that you can sign up to. 
                        You can only sign up to a single service.
                      >
            detail
          Contracts
            master
              markdown <### Your broker service contracts
                        These are the broker services you have signed up to. 
                        You can only have a single active contract.
                      >
              without props (Name, UseExpiresOn, IsInUse)
            detail
          ManagedBrokers
            master
              markdown <### Managed broker services
                        These are the broker services you manage. You can add new ones, remove existing ones, 
                        or change their storage location. Add an empty role instance, then specify its storage location
                        (identifying a Bespoke Database). Upon saving, the broker service context will be created.
                        **NOTE**: Most people will **not** manage broker services, but use the ones that are available.
                      >
              --with props (Name, StorageLocation)
            detail
          
  -- A Managed service.
  -- PDRDEPENDENCY
  case BrokerService
    state NoAdministrator = not exists Administrator
      on entry
        do for BrokerService$Guest
          bind me to Administrator
          PublicUrl = extern >> binder ManagedBrokers >> StorageLocation for extern
    state InitializeUpgrading = sys:MySystem >> extern >> OnStartup and not exists SystemDataUpgrade
      on entry
        do for Upgrader
          create role SystemDataUpgrade

    external
      property Name (mandatory, String)
        readableName
      -- PDRDEPENDENCY
      property Url (mandatory, String)
        pattern = "^wss://[^\\/]+:[0-9]+\\/ws$" "A url with the wss scheme, ending on a port followed by '/ws'"
      -- PDRDEPENDENCY
      property Exchange (mandatory, String)
      -- For mycontexts this is "https://mycontexts.com/rbmq/".
      property ManagementEndpoint (mandatory, String)
        pattern = "^https://[^\\/]+\\/rbmq\\/$" "A url with the https scheme, ending on '/rbmq/'"
      -- For mycontexts this is "https://mycontexts.com/rbsr/".
      property SelfRegisterEndpoint (mandatory, String)
        pattern = "^https://[^\\/]+\\/rbsr\\/$" "A url with the https scheme, ending on '/rbsr/'"
      property PublicUrl (String)
      property ContractPeriod (Day)
      property GracePeriod (Day)
      property TerminationPeriod (Day)
      property ServiceDescription (MarkDown)
        minLength = 100  
      
    -----------------
    ---- DATA UPGRADE
    user Upgrader = me
      perspective on SystemDataUpgrade
        only (Create, Fill)
        props (LastHandledUpgrade) verbs (SetPropertyValue, Consult)
    thing SystemDataUpgrade
      aspect sys:SystemDataUpgrade
      -- One-time initialisation to version 3.0.69
      on entry
        do for Upgrader
          LastHandledUpgrade = "3.0.68"
      -- Next upgrade to version 3.0.73
      state Upgrade3_0_73 = callExternal util:IsUpgradeTo( "3.0.73", LastHandledUpgrade ) returns Boolean
        on entry
          do for Upgrader
            LastHandledUpgrade = "3.0.73"
          do for Administrator
            bind_ me to context >> Administrator
    -----------------

    user Administrator filledBy (sys:TheWorld$PerspectivesUsers + sys:SocialEnvironment$Persons)
      -- The credentials of Administrator for the remote RabbitMQ server.
      property AdminUserName (String)
      property AdminPassword (String)

      state WithCredentials = (exists AdminUserName) and (exists AdminPassword)
        perspective on Accounts
          only (CreateAndFill, Remove, RemoveContext)
          props(LastNameOfAccountHolder) verbs (Consult)
      perspective on Administrator
        only (Fill, RemoveFiller)
        props (FirstName, LastName) verbs (Consult)
        props (AdminUserName, AdminPassword, SharedFileServerKey) verbs (SetPropertyValue)
      perspective on extern
        props (Name, Url, ManagementEndpoint, SelfRegisterEndpoint, Exchange, ServiceDescription, TerminationPeriod, GracePeriod, ContractPeriod) verbs (Consult, SetPropertyValue)
        props (PublicUrl) verbs (Consult)
      
      screen
        who
          Administrator
            master
              markdown <### Administrator
                        As you are the manager of this BrokerService, it is important that you have a key for the shared file storage service (if any).
                        This key allows you to hand out keys to people who connect to you to get an account on your Broker Service.
                        It also allows you to upload files to the shared file storage that your connected peers can download.
                        This key should be manually added to the state file of the shared file storage relay service.
                        See [perspectives-sharedfilestorage](https://github.com/joopringelberg/perspectives-sharedfilestorage) for more information.
                      >
              without props (FirstName, AdminUserName, AdminPassword, HasKey, SharedFileServerKey)
            detail
              without props (HasKey)
        what
          row 
            markdown <### Broker service
                      Perspectives needs a broker service to exchange information with peers.
                      Currently, we rely on [RabbitMQ](https://www.rabbitmq.com/), a popular open source message broker.
                      If you are the administrator of such a service, you can fill in the details below.
                      Notice the **Public URL**: this is the URL that other people can use to connect to your service. It is, 
                      in fact, the location of this context as seen by its Visitor role.
                      The **Self Register Endpoint** is used as a proxy for the RabbitMQ server, to enable self-signup.
                      This service can be set up using the [perspectives-rabbitmq-service](https://github.com/joopringelberg/perspectives-rabbitmq-service)
                      open source project.
                      The **Management Endpoint** is a location on the server that runs RabbitMQ. It should be configured
                      (e.g. in Apache) as a reverse proxy for RabbitMQ, that is used by the BrokerServices App to register 
                      and service accounts on RabbitMQ. Only the Administrator user uses that endpoint.
                      The **Description of the service**, finally is what is seen by Visitors and other non-Administrator roles.
                    >
          row
            form "Broker Service" External
        where
          Accounts
            master
              markdown <### Accounts
                        These are the accounts at the Broker Service. 
                        You can create new accounts, or remove existing ones.
                      >
            detail

    user Guest = sys:Me
      perspective on Administrator
        only (Fill, Create)
      perspective on extern
        props (PublicUrl, ServiceDescription) verbs (SetPropertyValue)

    public Visitor at (extern >> PublicUrl) = sys:Me
      perspective on extern
        props (Name, SelfRegisterEndpoint, PublicUrl, Url, Exchange, ServiceDescription, ContractPeriod, GracePeriod, TerminationPeriod) verbs (Consult)
      perspective on Administrator
        props (FirstName, LastName, HasKey) verbs (Consult)
      perspective on Accounts
        only (Create, Fill, CreateAndFill)
      -- TODO: waarom dit perspectief?
      perspective on Accounts >> binding >> context >> Administrator
        only (Create, Fill)
      perspective on MyPublicBrokers
        only (CreateAndFill, Fill)
      action AddThisServer
        bind extern to PublicBrokers in bs:MyBrokers

      screen
        who
          Administrator
            master
              markdown <### Administrator
                        This is the administrator of the Broker Service. 
                        You can contact him or her if you have questions about the service.
                       >
              without props (FirstName, HasKey)
            detail
              without props (HasKey)
        what
          row
            markdown <### Broker service
                      This is a 'broker service'. It is a program running on 
                      a webserver that programs use to exchange messages. MyContexts
                      installations use it to synchronize information.
                    >
          row
            markdown External
              props (ServiceDescription) verbs (Consult)
          row
            markdown <### Do you want to add this service?
                      This service is not yet available in your installation.
                      You might want to add it so you can sign up to it afterwards in order
                      to connect to other people on MyContexts.
                      [[action: AddThisServer|Add the service]]
                    >
              when not (exists bs:MyBrokers >> PublicBrokers)
            markdown <### Service available, sign up!
                      This service is available in your installation. However, you have not signed up to it.
                      Move to the [[link:model://perspectives.domains#BrokerServices$MyBrokers|Broker Services Management]] 
                      page to read how to sign up.
                    >
              when (exists bs:MyBrokers >> PublicBrokers) and (not exists bs:MyBrokers >> Contracts)
          row
            form "This service" External
              without props (SelfRegisterEndpoint, PublicUrl, Url, Exchange, ServiceDescription, ContractPeriod, GracePeriod, TerminationPeriod)
        where
          MyPublicBrokers
            master
              markdown <### Public broker services
                        These are the public broker services that you are signed up to (or can sign up to). 
                        You can only sign up to a single service.
                      >
            detail

    -- PDRDEPENDENCY
    context Accounts (relational, unlinked) filledBy BrokerContract

    context MyPublicBrokers = bs:MyBrokers >> PublicBrokers

  -- The contract between an end user and a BrokerService.
  -- PDRDEPENDENCY
  case BrokerContract
    aspect sys:Invitation
    aspect sys:ContextWithNotification
    state NoAdministrator = not exists Administrator
      on entry
        do for sys:Invitation$Guest
          -- Guest has a sufficient perspective on Administrator in state Invitation$NoInviter, which corresponds to this state NoAdministrator.
          bind extern >> binder model://perspectives.domains#BrokerServices$BrokerService$Accounts >> context >> Administrator >>= first to Administrator
    state NoAccountHolder = (exists Administrator) and (not exists AccountHolder)
      on entry
        do for BrokerContract$Administrator
          -- This is the role that the Invitee/AccountHolder will fill with himself if she accepts the BrokerContract.
          create role AccountHolder
    state CanRegister = (not exists Queues) and (exists AccountHolder) and (exists Administrator) and exists extern >> SelfRegisterEndpoint
      on entry
        do for AccountHolder
          letA 
            queue <- create role Queues
            queueid <- callExternal util:GenSym() returns String
          in
            AccountName = callExternal util:GenSym() returns String for AccountHolder
            AccountPassword = callExternal util:GenSym() returns String for AccountHolder
            QueueName = queueid for queue
            Registered = callExternal rabbit:SelfRegisterWithRabbitMQ(
              extern >> SelfRegisterEndpoint,
              AccountHolder >> AccountName,
              AccountHolder >> AccountPassword,
              queueid) returns Boolean for External
            IsInUse = true for extern
            bind_ (sys:MySystem >> extern) to EmptyQueue
            callEffect rabbit:StartListening()

    state Terminated = (extern >> Registered) and (sys:MySystem >> extern >> CurrentDate) > (extern >> TerminatesOn) or (extern >> ContractTerminated)
      on entry
        do for BrokerContract$Administrator
          callEffect rabbit:DeleteAMQPaccount(
            extern >> ManagementEndpoint,
            Administrator >> AdminUserName,
            Administrator >> AdminPassword,
            AccountHolder >> AccountName)
          -- Deleting the queue will cause it to be removed from the RabbitMQ server.
          delete role Queues
          Registered = false for extern
          ContractTerminated = true for extern
          IsInUse = false for extern
        notify AccountHolder
          "Your account at the BrokerService { extern >> Name } has been terminated."

    -- Initialise Upgrading
    state InitializeUpgrading = sys:MySystem >> extern >> OnStartup and not exists SystemDataUpgrade
      on entry
        do for Upgrader
          create role SystemDataUpgrade

    -- On exiting, both the queue and the account are deleted. See the exit states of AccountHolder and Queue respectively.
    
    external
      aspect sys:Invitation$External
      -- PDRDEPENDENCY
      property Url = binder Accounts >> context >> extern >> Url
      property ManagementEndpoint = binder Accounts >> context >> extern >> ManagementEndpoint
      property SelfRegisterEndpoint = binder Accounts >> context >> extern >> SelfRegisterEndpoint
      -- PDRDEPENDENCY
      property Exchange = binder Accounts >> context >> extern >> Exchange
      property Name = binder Accounts >> context >> extern >> Name
      property FirstNameOfAccountHolder = context >> AccountHolder >> FirstName
      property LastNameOfAccountHolder = context >> AccountHolder >> LastName
        readableName
      -- We use this on system startup.
      -- PDRDEPENDENCY
      property CurrentQueueName = sys:MySystem >> extern >> binder Queues >> QueueName
      property Registered (Boolean)
      property IsInUse (Boolean)
      property UseExpiresOn (Date)
      property GracePeriodExpiresOn (Date)
      property TerminatesOn (Date)

      property ContractTerminated (Boolean)

      state Terminated = ContractTerminated

      view Account (FirstNameOfAccountHolder, LastNameOfAccountHolder)

      on entry
        do for AccountHolder
          letA 
            now <- callExternal sensor:ReadSensor("clock", "now") returns Date
          in 
            UseExpiresOn = (now + context >> Service >> ContractPeriod)
            GracePeriodExpiresOn = (UseExpiresOn + context >> Service >> GracePeriod)
            TerminatesOn = (GracePeriodExpiresOn + context >> Service >> TerminationPeriod)

      state Active = Registered
        on entry
          notify AccountHolder
            "You now have an account at the BrokerService { Name }"
        state ExpiresSoon = Registered and (sys:MySystem >> extern >> CurrentDate) > UseExpiresOn
          on entry
            notify AccountHolder
              "Your lease of the BrokerService has ended. Within {context >> Service >> GracePeriod} days, you will no longer be able to receive information from peers."
              
    -----------------
    ---- DATA UPGRADE
    user Upgrader = me
      perspective on SystemDataUpgrade
        only (Create, Fill)
        props (LastHandledUpgrade) verbs (SetPropertyValue, Consult)
    thing SystemDataUpgrade
      aspect sys:SystemDataUpgrade
      aspect sys:ModelDataUpgrade
      -- One-time initialisation to version 3.0.68
      on entry
        do for Upgrader
          LastHandledUpgrade = "3.0.68"
      -- Next upgrade to version 3.0.73
      state Upgrade3_0_73 = callExternal util:IsUpgradeTo( "3.0.73", LastHandledUpgrade ) returns Boolean and not (context >> AccountHolder == me)
        on entry
          do for Upgrader
            LastHandledUpgrade = "3.0.73"
          do for AccountHolder
            bind_ me to context >> AccountHolder
    -----------------

    user AccountHolder filledBy (sys:TheWorld$PerspectivesUsers + sys:SocialEnvironment$Persons)
      aspect sys:Invitation$Invitee
      aspect sys:ContextWithNotification$NotifiedUser
      -- PDRDEPENDENCY
      property AccountName (mandatory, String)
      -- PDRDEPENDENCY
      property AccountPassword (mandatory, String)
      -- PDRDEPENDENCY
      -- property QueueName (mandatory, String)

      -- Create an account on the RabbitMQ server. It is ready for the AccountHolder to listen to,
      -- but no other users can reach him yet.
      -- We'll know the BrokerContract has arrived at the Administrator after a peer executed Signup,
      -- when a Queue already exists.
      state PrepareAccount = (not exists binding) and not exists context >> Queues
        on entry
          do for BrokerContract$Administrator
            letA 
              queue <- create role Queues
              queueid <- callExternal util:GenSym() returns String
            in
              AccountName = callExternal util:GenSym() returns String
              AccountPassword = callExternal util:GenSym() returns String
              QueueName = queueid for queue
              callEffect rabbit:PrepareAMQPaccount(
                context >> extern >> ManagementEndpoint,
                context >> Administrator >> AdminUserName,
                context >> Administrator >> AdminPassword,
                AccountName,
                AccountPassword,
                queueid)
      
      -- state StartService = (exists binding) and context >> extern >> Registered
      --   on entry
      --     do for AccountHolder
      --       bind_ (sys:MySystem >> extern) to (context >> EmptyQueue)
      --       callEffect rabbit:StartListening()

      perspective on extern
        props (Url, Exchange, CurrentQueueName) verbs (Consult)
        props (Registered, UseExpiresOn, GracePeriodExpiresOn, TerminatesOn, ContractTerminated, IsInUse) verbs (SetPropertyValue)
        in object state Active
          action TerminateContract
            ContractTerminated = true
      perspective on AccountHolder
        all roleverbs
        props (AccountName, AccountPassword) verbs (Consult, SetPropertyValue)
      
      perspective on Queues
        only (Create, Fill, Remove)
        props (QueueName) verbs (SetPropertyValue, Consult)

      screen
        who
          Administrator
            master
              markdown <### Administrator
                        This is the administrator of the Broker Service. 
                        You can contact him or her if you have questions about the service.
                       >
              without props (FirstName, HasKey)
            detail
              without props (HasKey)
          AccountHolder
            master
              without props (AccountName, AccountPassword)
            detail
        what
          row
            markdown <### Account holder
                      This is your account at the Broker Service. 
                      See below when it expires. To cancel it, first click the bolt icon in the toolbar below.
                    >
          row
            form "Broker Service" External
              without props (Registered, GracePeriodExpiresOn, ContractTerminated, Url, Exchange, CurrentQueueName, InviterLastName, Message, ConfirmationCode, Addressing)
              props (Name, UseExpiresOn) verbs (Consult)
          row
            table "Queues" Queues
              props (QueueName) verbs (Consult)
        where

    context EmptyQueue (functional) = filter Queues with not exists binding

    user Administrator filledBy bs:BrokerService$Administrator
      aspect sys:Invitation$Inviter

      perspective on Administrator
        props (AdminUserName, AdminPassword) verbs (Consult)

      perspective on AccountHolder
        all roleverbs
        props (AccountName, AccountPassword) verbs (Consult, SetPropertyValue)
        props (Cancelled) verbs (Consult)
      
      perspective on Queues
        only (Create, Fill, DeleteContext, Delete)
        props (QueueName) verbs (Consult, SetPropertyValue)
      
      -- If this contract is due to self-signup, Administrator needs this perspective to know that this contract
      -- fills an Accounts role in the service
      perspective on extern
        props (Name, ContractTerminated, ManagementEndpoint) verbs (Consult)
        props (Registered, ContractTerminated, TerminatesOn, IsInUse) verbs (SetPropertyValue)
        in object state Active
          action TerminateContract
            ContractTerminated = true

      screen
        who
          AccountHolder
            master
              without props (FirstName, AccountName, AccountPassword, Cancelled, HasKey)
            detail
              without props (HasKey)
        what
          row
            form "Broker Service" External
              without props (Name, ContractTerminated, ManagementEndpoint, TerminatesOn, Registered)
          row
            table Queues
        where

    aspect user sys:Invitation$Guest
    aspect thing sys:ContextWithNotification$Notifications

    context Queues (relational) filledBy sys:PerspectivesSystem
      property QueueName (String)
        readableName
      on exit
        do for BrokerContract$Administrator
          callEffect rabbit:DeleteQueue(
            context >> extern >> ManagementEndpoint,
            context >> Administrator >> AdminUserName,
            context >> Administrator >> AdminPassword,
            QueueName)

    context Service (functional) = extern >> binder Accounts >> context >> extern
  
    aspect context sys:ContextWithNotification$Notifications
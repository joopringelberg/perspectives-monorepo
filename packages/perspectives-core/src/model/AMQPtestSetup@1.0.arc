-- Copyright Joop Ringelberg and Cor Baars, 2026.
-- CUID = whku0vufat
domain model://joopringelberg.nl#AMQPtestSetup@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#AMQPtestSetup
  use bs for model://perspectives.domains#BrokerServices

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestSetupApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Test AMQP Setup App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:AMQPtestSetupApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:AMQPtestSetupApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestSetupApp
    indexed mm:AMQPtestSetupApp
    aspect sys:RootContext
    external
    
    user Manager = sys:Me
      perspective on bs:MyBrokers >> ManagedBrokers
        only (CreateAndFill, RemoveContext)
      perspective on bs:MyBrokers >> ManagedBrokers >> binding
        props (Name, Url, Exchange, ManagementEndpoint, SelfRegisterEndpoint, ContractPeriod, GracePeriod, TerminationPeriod) verbs (Consult, SetPropertyValue)
      perspective on bs:MyBrokers >> PublicBrokers >> binding >> context >> Administrator
        props (AdminUserName, AdminPassword) verbs (Consult, SetPropertyValue)
      perspective on bs:MyBrokers >> PublicBrokers
        only (Create, Fill)
      
      action SetupBrokerService
        letA
          broker <- create context bs:BrokerService bound to bs:BrokerServices$ManagedBrokers in bs:MyBrokers
        in
          Name = "Test Broker" for broker
          Url = "wss://mycontexts.com:15673/ws" for broker
          Exchange = "mycontexts" for broker
          ManagementEndpoint = "https://mycontexts.com/rbmq/" for broker
          SelfRegisterEndpoint = "https://mycontexts.com/rbsr/" for broker

          ContractPeriod = 1 day for broker
          GracePeriod = 1 day for broker
          TerminationPeriod = 1 day for broker

          bind broker >> binding to PublicBrokers in bs:MyBrokers

      action SetCredentials
        letA
          admin <- bs:MyBrokers >> ManagedBrokers >> binding >> context >> Administrator
        in
          AdminUserName = "joopring" for admin
          AdminPassword = "vOsisdebest5!" for admin

      -- All set to run action SignUp from the test PDR, which will create a new contract in the BrokerService.


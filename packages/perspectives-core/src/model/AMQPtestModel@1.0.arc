-- Copyright Joop Ringelberg and Cor Baars, 2026.
-- CUID = xyyehk9bpc
domain model://joopringelberg.nl#AMQPtestModel@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#AMQPtestModel
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
          Name = "Test AMQP Sync App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:AMQPtestSyncApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:AMQPtestSyncApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestApp
    indexed mm:AMQPtestSyncApp
    aspect sys:RootContext
    external
    
    user Manager = sys:Me
      perspective on Tests
        only (CreateAndFill, RemoveContext)
      perspective on Tests >> binding >> context >> Leader
        only (Create, Fill)
      perspective on Follower
        only (Create, Fill)
        props (FirstName) verbs (Consult)
      perspective on bs:MyBrokers >> ManagedBrokers
        only (CreateAndFill, RemoveContext)
      perspective on bs:MyBrokers >> ManagedBrokers >> binding
        props (Name, Url, Exchange, ManagementEndpoint, SelfRegisterEndpoint, ContractPeriod, GracePeriod, TerminationPeriod) verbs (Consult, SetPropertyValue)
      perspective on bs:MyBrokers >> PublicBrokers >> binding >> context >> Administrator
        props (AdminUserName, AdminPassword) verbs (Consult, SetPropertyValue)
      perspective on bs:MyBrokers >> PublicBrokers
        only (Create, Fill)      
          
    -- We need to synchronise, hence PerspectivesUsers and not Persons.
    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
    
    -- To execute any test, run the action RunTest in the first PDR.
    -- To check if a test has succeeded, retrieve the value of TestSucceeded in the second PDR.
    context Tests (relational) filledBy Test

  case Test

    -- Why not on entry of Test, like we do for the Leader?
    -- The Test instance may not yet be bound to Tests, so AppFollower is not yet reachable.
    state AppfollowerReachable = exists extern >> binder Tests
      on entry
        do for Initializer
          -- The automatic actions are contextualised in their specialisations,
          -- meaning that specialisations of Leader and Follower are created.
          -- Dit gaat fout als we Follower met AppFollower vullen in plaats van met de vuller van AppFollower.
          bind AppFollower >> binding to Follower
          -- Follower will not have Test instances bound in TestApp$Tests, so this is a surefire way 
          -- to execute this action only for Leader.
          bind me to Leader

    external
      property TestName (String)
      property TestSucceeded (Boolean)
    
    user AppFollower (functional) = extern >> binder Tests >> context >> Follower
    user Initializer = me
      perspective on Leader
        only (Create, Fill)
      perspective on Follower
        only (Create, Fill)

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      perspective on extern
        props (TestName) verbs (SetPropertyValue, Consult)

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      perspective on Leader
        props (FirstName) verbs (Consult)
      perspective on extern
        props (TestName) verbs (Consult)
        props (TestSucceeded) verbs (Consult, SetPropertyValue)

  ------------------------------------------------------------------------------
  ---- TESTS. All these tests construct something in pdrA and check if it is synchronised in pdrB.
  ---- The test description and name mention the crucial step in the ORIGINAL query that is tested.
  ---- I also mention the runtime inverted query key that is used to fetch the inverted query for the INVERTED step.
  ---- We have tests for the five different types of steps in a perspective query: context, role, binder, filled and property.
  ------------------------------------------------------------------------------
  
  ------------------------------------------------------------------------------
  ---- Create a role that is in scope of the Follower and has a property. Because it has a property, it is synchronised.
  ---- It specifically tests the property setting step, viz inverted queries fetched with a RTPropertyKey.
  ------------------------------------------------------------------------------
  case Test_SetProperty
    aspect mm:Test
    external
      state TestSucceeded = context >> TestRole1 >> P == 1
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on TestRole1
        only (Create)
        props (P) verbs (Consult, SetPropertyValue)
      action RunTest
        letA
          tr <- create role TestRole1
        in
          P = 1 for tr
          TestName = "Test property step for enumerated property on perspective object" for extern

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on TestRole1
        props (P) verbs (Consult)

    thing TestRole1
      property P (Number)

  ------------------------------------------------------------------------------
  ---- Make Follower terminate the contract.
  ------------------------------------------------------------------------------
  case Test_Leader_Terminates_Contract
    aspect mm:Test
    external
      -- This is to see if Leader has the required resources.
      state ManagedBrokersRoleExists = exists bs:MyBrokers >> ManagedBrokers
        state ManagedBrokerExists = exists bs:MyBrokers >> ManagedBrokers >> binding
        -- Deze toestand wordt nooit geldig. Hypothese: het lukt niet de rollen uit de database op te halen.
        -- Wel als we Accounts niet unlinked maken.
          state AccountsRolesExist = exists bs:MyBrokers >> ManagedBrokers >> binding >> context >> Accounts
            state ContractsExist = exists bs:MyBrokers >> ManagedBrokers >> binding >> context >> Accounts >> binding
      -- This is for Follower: it cannot be in terms of ManagedBrokers.
      state TestSucceeded = (exists bs:MyBrokers >> ContractInUse) and not exists bs:MyBrokers >> ContractInUse >> context >> Queues
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on FollowerBrokerContract
        props (ContractTerminated) verbs (Consult, SetPropertyValue)
      action RunTest
        ContractTerminated = true for FollowerBrokerContract
        TestName = "BrokerService Administrator terminates contract of Follower" for extern
    
    -- context FollowerBrokerContract = (filter 
    --   bs:MyBrokers >> PublicBrokers >> binding >> context >> Accounts >> binding 
    --   with context >> AccountHolder filledBy (origin >> Follower >> binding)) >>= first

    -- This role is for the Manager to see: it is in terms of ManagedBrokers.
    context FollowerBrokerContract =  
      bs:MyBrokers >> ManagedBrokers >> binding >> context >> Accounts >> binding 

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on FollowerBrokerContract
        props (ContractTerminated) verbs (Consult)


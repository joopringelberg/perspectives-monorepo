-- Copyright Joop Ringelberg and Cor Baars, 2026.
-- CUID = xyyehk9bpc
domain model://joopringelberg.nl#AMQPtestModel@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#AMQPtestModel
  use bs for model://perspectives.domains#BrokerServices@7.0
  use cm for model://perspectives.domains#CouchdbManagement@12.4

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
        props (Name, Url, Exchange, ManagementEndpoint, SelfRegisterEndpoint) verbs (Consult, SetPropertyValue)
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

  case Create_A_Bespoke_Database
    aspect mm:Test
    external
      property BespokeDatabaseReady (Boolean)
      property TestContextCreated (Boolean)

      state DatabaseReady = BespokeDatabaseReady
        on entry
          do for Follower
            letA
              testcontext <- create context mm:TestContext named "bobscontext" bound to TestContexts
              testdata <- create role mm:TestContext$TestData in testcontext >> binding >> context
            in
              Value = "Created by Bob" for testdata

              once settled
                TestContextCreated = true

      state ContextCreated = TestContextCreated
        state TestSucceeded = exists publicrole pub:https://perspectives.domains/cw_bobsdatabase/#bobscontext (mm:TestContext)
          on entry
            do for Follower once settled
              TestSucceeded = true

    context BespokeDatabase filledBy cm:BespokeDatabase
    context TestContexts (relational) filledBy mm:TestContext

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Leader
      perspective on extern
        props (BespokeDatabaseReady) verbs (SetPropertyValue)
        props (TestContextCreated) verbs (Consult)
      perspective on cm:CouchdbServer$Accounts
        only (Create, Fill)
      perspective on cm:CouchdbServer$BespokeDatabases
        only (CreateAndFill)
        props (Endorsed, Public, EnteredDatabaseName) verbs (SetPropertyValue, Consult)
      perspective on cm:BespokeDatabase$Owner
        only (Create, Fill)
      action RunTest
        letA
          bob <- (filter bs:MyBrokers >> ManagedBrokers >> binding >> context >> Accounts >> binding >> context >> AccountHolder >> binding
            with (FirstName == "bob") and (LastName == "bob_last")) >>= first
          server <- (filter cm:MyCouchdbApp >> CouchdbServers with Url == "https://perspectives.domains/")
            >> binding >> context >>= first
          account <- create role cm:CouchdbServer$Accounts in server
          database <- create context cm:BespokeDatabase bound to cm:CouchdbServer$BespokeDatabases in server
          owner <- create role cm:BespokeDatabase$Owner in database >> binding >> context
        in
          TestName = "Create a bespoke database for Bob" for extern
          bind_ bob to account
          bind_ account to owner
          EnteredDatabaseName = "cw_bobsdatabase/" for database
          Endorsed = true for database
          Public = true for database

          once settled
            BespokeDatabaseReady = true for extern

    user Follower filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Follower
      perspective on extern
        props (BespokeDatabaseReady) verbs (Consult)
        props (TestContextCreated) verbs (Consult, SetPropertyValue)
      perspective on TestContexts
        only (CreateAndFill)
      perspective on TestContexts >> binding >> context >> mm:TestContext$TestData
        only (Create)
        props (Value) verbs (SetPropertyValue)

  case TestContext
    external
    public Visitor at "https://perspectives.domains/cw_bobsdatabase/" = sys:Me
      perspective on TestData
        props (Value) verbs (Consult)

    thing TestData
      property Value (String)

-- Copyright Joop Ringelberg and Cor Baars, 2026.
-- CUID = bssu4gc7kx
domain model://joopringelberg.nl#BrokerServiceSignupTests@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#BrokerServiceSignupTests
  use bs for model://perspectives.domains#BrokerServices

  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          app <- create context TestApp
          start <- create role StartContexts in sys:MySystem
        in
          bind_ app >> extern to start
          Name = "Broker Service Signup Tests" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:BrokerServiceSignupTestsApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  case TestApp
    indexed mm:BrokerServiceSignupTestsApp
    aspect sys:RootContext
    external

    user Manager = sys:Me
      perspective on Tests
        only (CreateAndFill, RemoveContext)
      perspective on Tests >> binding >> context >> Tester
        only (Create, Fill)

    context Tests (relational) filledBy Test

  case Test
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

  case SignUpToBrokerService
    aspect mm:Test

    external
      property PublicServiceAvailable (Boolean)
      state SignUp = PublicServiceAvailable
        on entry
          do for Tester once settled
            letA
              brokerservice <- bs:MyBrokers >> PublicBrokers >> binding >> context
              accountsinstance <- create context bs:BrokerContract bound to Accounts in brokerservice
            in
              bind me to AccountHolder in accountsinstance >> binding >> context
              bind accountsinstance >> context >> Administrator to Administrator in accountsinstance >> binding >> context
              bind accountsinstance >> binding to MyContract in context

      state Success =
        (context >> MyContract >> Registered)
          and (context >> MyContract >> binding >> context >> AccountHolder >> binding >> FirstName == "bob")
          and (context >> MyContract >> binding >> context >> AccountHolder >> binding >> LastName == "bob_last")
          and (context >> MyContract >> binding >> context >> Administrator >> binding >> FirstName == "alice")
          and (context >> MyContract >> binding >> context >> Administrator >> binding >> LastName == "alice_last")
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
          TestName = "Bob signs up to Alice's Broker Service" for extern
          bind brokerservice to PublicBrokers in bs:MyBrokers

          once settled
            PublicServiceAvailable = true for extern

    context MyContract filledBy bs:BrokerContract

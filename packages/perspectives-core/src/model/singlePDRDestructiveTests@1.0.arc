domain model://joopringelberg.nl#SinglePDRDestructiveTests@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#SinglePDRDestructiveTests
  use sensor for model://perspectives.domains#Sensor

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
          Name = "Single PDR Destructive Tests App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:SinglePDRDestructiveTestApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:SinglePDRDestructiveTestApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestApp
    indexed mm:SinglePDRDestructiveTestApp
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

  case Test
    -- The automatic actions are contextualised in their specialisations,
    -- meaning that specialisation of Tester is created.
    on entry 
      do for Initializer
        bind me to Tester

    external
      property TestName (String)
      property TestSucceeded (Boolean)
      property TestFinished (Boolean)

    
    user Initializer = me
      perspective on Tester
        only (Create, Fill)

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      perspective on extern
        props (TestName, TestSucceeded, TestFinished) verbs (SetPropertyValue, Consult)

  ------------------------------------------------------------------------------
  ---- TESTS. All these tests first create something.
  ---- Then they destroy it. This should trigger a state change, which is checked.
  ---- The TestName describes the destructive operation.
  ------------------------------------------------------------------------------
  
  ------------------------------------------------------------------------------
  ---- Remove a role.
  ------------------------------------------------------------------------------
  case Test_RemoveRole
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          create role TestRole1
    external
      state TestFinished = TestFinished
        on entry
          do for Tester
            after 1 Seconds remove role context >> TestRole1
        state TestSucceeded = not exists context >> TestRole1
          on entry
            do for Tester
              TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on TestRole1
        only (Create, Remove)
      action RunTest
        TestName = "Remove a role" for extern
        TestFinished = true for extern

    thing TestRole1

  ------------------------------------------------------------------------------
  ---- Set a property value and then remove that value.
  ------------------------------------------------------------------------------
  case Test_DeleteProperty
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          letA
            tr <- create role TestRole1
          in
            P = 1 for tr
    external
      state TestSucceeded = not exists context >> TestRole1 >> P
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on TestRole1
        only (Create)
        props (P) verbs (Consult, SetPropertyValue, DeleteProperty)
      action RunTest
        TestName = "Delete a property value" for extern
        delete property P from TestRole1

    thing TestRole1
      property P (Number)

  ------------------------------------------------------------------------------
  ---- Create a role that is in scope of the Follower and set a property value.
  ---- Then remove that value.
  ------------------------------------------------------------------------------
  case Test_RemoveProperty
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          letA
            tr <- create role TestRole1
          in
            P = 1 for tr
    external
      state TestSucceeded = not exists context >> TestRole1 >> P
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on TestRole1
        only (Create)
        props (P) verbs (Consult, SetPropertyValue, RemovePropertyValue)
      action RunTest
        TestName = "Remove a property value" for extern
        P =- 1 for TestRole1

    thing TestRole1
      property P (Number)

  ------------------------------------------------------------------------------
  ---- Create a role that is in scope of the Follower and set a property value.
  ---- Then remove that value.
  ------------------------------------------------------------------------------
  case Test_RemoveOnePropertyValue
    aspect mm:Test
    state TesterAvailable = exists Tester 
      on entry
        do for Tester
          letA
            tr <- create role TestRole1
          in
            P = 1 for tr
            P =+ 2 for tr
    external
      state TestSucceeded = context >> TestRole1 >> P >>= count == 1
        on entry
          do for Tester
            TestSucceeded = true

    user Tester filledBy (sys:TheWorld$PerspectivesUsers)
      aspect mm:Test$Tester
      perspective on TestRole1
        only (Create)
        props (P) verbs (Consult, SetPropertyValue, RemovePropertyValue, AddPropertyValue)
      action RunTest
        TestName = "Remove one property value while one value remains" for extern
        P =- 1 for TestRole1

    thing TestRole1
      property P (Number)

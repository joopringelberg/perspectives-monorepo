domain model://joopringelberg.nl#TestActionNameInMarkdown@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#TestActionNameInMarkdown

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
          Name = "Test action name in markdown" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:MyTestApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:MyTestApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestApp
    indexed mm:MyTestApp
    aspect sys:RootContext
    external
      property Arbitrary (String)
    
    user Manager = sys:Me
      perspective on extern
        props (Arbitrary) verbs (Consult, SetPropertyValue)
        action SetArbitraryValue
          Arbitrary = "Value set by action"
      screen
        who
        what
          markdown <### Test action name in markdown
                    This is a test to check whether action names are properly rendered in markdown. 
                    [[action:SetArbitraryValue|Set Arbitrary Value]]
                    This action should compile well.
                    [[action:NonExistingAction|Non Existing Action]]
                    This action does not exist and should result in a compilation error:
                    (UnknownMarkDownAction) The action 'SetArbitraryValue' is not defined for the role 'CR CalculatedRoleType model://joopringelberg.nl#TestActionNameInMarkdown$TestApp$Manager'! Between line 49, column 20 and line 56, column 9).
                    >
        where
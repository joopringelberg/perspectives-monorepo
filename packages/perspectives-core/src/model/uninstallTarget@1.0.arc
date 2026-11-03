-- Copyright Joop Ringelberg and Cor Baars, 2026.
-- A minimal model that is removed by test T27 in model://joopringelberg.nl#TransactionExecutionTests.
-- The test harness (test/transactionExecutionTests.purs) compiles and installs it from this source
-- under the Stable model URI model://joopringelberg.nl#ms9lflili1@1.0, so it need not be published.
-- It has an indexed context, an indexed role and an entry in StartContexts.
-- On purpose, the model root context has NO on exit that cleans these up: the PDR should do so on removing the model.
domain model://joopringelberg.nl#UninstallTarget@1.0
  use sys for model://perspectives.domains#System
  use ut for model://joopringelberg.nl#UninstallTarget

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          app <- create context TargetApp
          start <- create role StartContexts in sys:MySystem
        in
          bind_ app >> extern to start
          Name = "Uninstall Target App" for start
          IsSystemModel = true for start

  aspect user sys:PerspectivesSystem$Installer

  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TargetApp
    indexed ut:MyUninstallTargetApp
    aspect sys:RootContext

    on entry
      do for Manager
        create role TargetThing

    external

    user Manager = sys:Me
      perspective on TargetThing
        only (Create, Remove)

    -------------------------------------------------------------------------------
    ---- INDEXED ROLE
    -------------------------------------------------------------------------------
    thing TargetThing
      indexed ut:MyUninstallTargetThing

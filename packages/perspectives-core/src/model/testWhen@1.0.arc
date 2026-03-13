-- Copyright Joop Ringelberg and Cor Baars, 2026
-- A model to test the generic `when` construct for conditional screen elements.
--
-- Tests two variants:
-- 1. In a classic (freeform) screen: `when <step> <screenElement>`
-- 2. In a who-what-where screen: `when <step>` guards a master-detail block

domain model://joopringelberg.nl#TestWhen@1.0
  use sys for model://perspectives.domains#System
  use tw for model://joopringelberg.nl#TestWhen

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestWhenApp
          start <- create role StartContexts in sys:MySystem
        in
          bind_ app >> extern to start
          Name = "Test When App" for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (tw:MyTestWhenApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  -- The entry point of the test application.
  case TestWhenApp
    indexed tw:MyTestWhenApp
    aspect sys:RootContext
    external

    -- The Manager is the sole user of this test app.
    user Manager = sys:Me
      perspective on Settings
        only (Create, Remove)
        props (ShowExtraDetails, ShowTable) verbs (Consult, SetPropertyValue)
      perspective on MainInfo
        only (Create, Remove)
        props (Name, Description) verbs (Consult, SetPropertyValue)
      perspective on ExtraInfo
        only (Create, Remove)
        props (ExtraField1, ExtraField2) verbs (Consult, SetPropertyValue)
      perspective on Items
        only (Create, Remove)
        props (ItemName, ItemValue) verbs (Consult, SetPropertyValue)
      perspective on Projects
        only (CreateAndFill, RemoveContext)

      -- Classic (freeform) screen – demonstrates `when` guarding individual screen elements.
      --
      -- 1. `when Settings >> ShowExtraDetails` guards a form for ExtraInfo.
      --    Toggle the ShowExtraDetails checkbox in the Settings form to show/hide ExtraInfo.
      --
      -- 2. `when Settings >> ShowTable` guards a table of Items.
      --    Toggle the ShowTable checkbox to show/hide the Items table.
      --
      -- 3. Nested: a `when` block containing both a markdown header and a form.
      --
      -- The condition is evaluated against the whole context, so
      -- `Settings >> ShowExtraDetails` means:
      --   - get the Settings role of this context
      --   - get the ShowExtraDetails property value of that role
      screen "Test When"
        who
        what
          row
            form Settings
          row
            form MainInfo
          -- 1. Conditionally show ExtraInfo form based on Settings >> ShowExtraDetails
          row 
            when Settings >> ShowExtraDetails
              form ExtraInfo
          -- 2. Conditionally show the Items table based on Settings >> ShowTable
          row
            when Settings >> ShowTable
              table Items
          -- 3. Nested when: show a markdown header together with a form
          row
            when Settings >> ShowExtraDetails
              row
                markdown "### Extra Details are visible"
              form ExtraInfo
        where
          when Settings >> ShowTable
            Items
              master
                props (ItemName) verbs (Consult)
              detail

    -- A single Settings role that holds the flags controlling visibility.
    thing Settings
      property ShowExtraDetails (Boolean)
      property ShowTable (Boolean)

    -- Main information that is always shown.
    thing MainInfo
      property Name (String)
      property Description (String)

    -- Extra information that is only shown when Settings >> ShowExtraDetails is true.
    thing ExtraInfo
      property ExtraField1 (String)
      property ExtraField2 (String)

    -- A relational role for the table test.
    thing Items (relational)
      property ItemName (String)
      property ItemValue (Number)

    -- Context role linking to TestWhenProject instances.
    context Projects (relational) filledBy TestWhenProject

  -------------------------------------------------------------------------------
  ---- WHO-WHAT-WHERE SCREEN TEST CONTEXT
  -------------------------------------------------------------------------------
  -- TestWhenProject demonstrates `when` in the who, what, and where sections of
  -- a who-what-where style screen.
  --
  -- Set ShowTasks = true to reveal the Tasks master-detail in the `what` section.
  -- Set ShowContexts = true to reveal the SubProjects master-detail in the `where` section.
  case TestWhenProject
    external
      aspect sys:RoleWithName

    -- Visibility flags for the who-what-where screen.
    thing Settings
      property ShowTasks (Boolean)
      property ShowContexts (Boolean)

    -- Tasks to show conditionally in the `what` section.
    thing Tasks (relational)
      property TaskName (String)
      property Priority (String)

    -- Sub-projects to show conditionally in the `where` section.
    context SubProjects (relational) filledBy TestWhenProject

    user Manager = sys:Me
      perspective on Settings
        only (Create, Remove)
        props (ShowTasks, ShowContexts) verbs (Consult, SetPropertyValue)
      perspective on Tasks (relational)
        only (Create, Remove)
        props (TaskName, Priority) verbs (Consult, SetPropertyValue)
      perspective on SubProjects
        only (CreateAndFill, RemoveContext)
      perspective on extern
        props (Name) verbs (Consult, SetPropertyValue)

      -- Who-what-where screen demonstrating `when` in `what` and `where` sections.
      --
      -- Toggle Settings >> ShowTasks to show/hide the Tasks master-detail in `what`.
      -- Toggle Settings >> ShowContexts to show/hide the SubProjects master-detail in `where`.
      screen "Test When (who-what-where)"
        who
        what
          -- `when` in the `what` section: Tasks master-detail only visible when ShowTasks = true.
          when Settings >> ShowTasks
            Tasks
              master
                props (TaskName) verbs (Consult)
              detail
        where
          -- `when` in the `where` section: SubProjects master-detail only visible when ShowContexts = true.
          when Settings >> ShowContexts
            SubProjects
              master
                props (Name) verbs (Consult)
              detail


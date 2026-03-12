-- Copyright Joop Ringelberg and Cor Baars, 2026
-- A model to test the generic `when` construct for conditional screen elements.
--
-- The `when` construct allows any screen element (form, table, markdown, row, column)
-- to be conditionally shown based on a step expression evaluated against the context.
--
-- Syntax:
--   when <step-expression>
--     <screen-element>

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
      perspective on Items (relational)
        only (Create, Remove)
        props (ItemName, ItemValue) verbs (Consult, SetPropertyValue)

      -- The screen demonstrates the `when` construct in several ways:
      --
      -- 1. `when Settings >> ShowExtraDetails` guards a form for ExtraInfo.
      --    Toggle the ShowExtraDetails checkbox in the Settings form to show/hide ExtraInfo.
      --
      -- 2. `when Settings >> ShowTable` guards a table of Items.
      --    Toggle the ShowTable checkbox to show/hide the Items table.
      --
      -- 3. Nested: a `when` block containing both a markdown and a form.
      --
      -- The condition is evaluated against the whole context, so
      -- `Settings >> ShowExtraDetails` means:
      --   - get the Settings role of this context
      --   - get the ShowExtraDetails property value of that role
      screen "Test When"
        what
          row
            form Settings
          row
            form MainInfo
          -- 1. Conditionally show ExtraInfo form based on Settings >> ShowExtraDetails
          when Settings >> ShowExtraDetails
            form ExtraInfo
          -- 2. Conditionally show the Items table based on Settings >> ShowTable
          when Settings >> ShowTable
            table Items
          -- 3. Nested when: show a markdown header together with a form
          when Settings >> ShowExtraDetails
            row
              markdown "### Extra Details are visible"
            form ExtraInfo

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

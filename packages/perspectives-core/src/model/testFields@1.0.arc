-- Copyright Joop Ringelberg and Cor Baars, 2021, 2026
-- A model to test smart field controls.

domain model://joopringelberg.nl#TestFields@1.0
  use sys for model://perspectives.domains#System
  use tf for model://joopringelberg.nl#TestFields

  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          app <- create context TestFieldsApp
          start <- create role StartContexts in sys:MySystem
        in
          bind_ app >> extern to start
          Name = "TestFields Management" for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (tf:TheTestFields >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (tf:TheTestFields >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  

  -- The entry point (the `application`), available as tf:TheTestFields.
  case TestFieldsApp
    indexed tf:TheTestFields
    aspect sys:RootContext

    external
    
    user Tester = sys:Me
      perspective on TestRole
        props (Text, Bool, ADate, ANumber, AnEmail, WeekDay, Appel) verbs (Consult, SetPropertyValue)
        only (Remove, Create)
      perspective on TestTable
        defaults
      perspective on CopyOfTestTable
        defaults
      screen "Test screen"
        who
        what
          row
            form TestRole
              props (Text, WeekDay, ADate) verbs (Consult)
          row
            table "With props style" TestTable
              with props (Text, ADate)
          row
            table "Without props style" TestTable
              without props (Text, ADate)
        where
          TestTable
            master
              without props (Text, ADate)
              props (Text, Bool, ADate, ANumber, AnEmail) verbs (Consult, SetPropertyValue)
            detail
              with props (Text, Bool)
              props (Text, Bool, ADate, ANumber, AnEmail) verbs (Consult, SetPropertyValue)
          CopyOfTestTable
            master
              props (Bool, ANumber, AnEmail) verbs (Consult, SetPropertyValue)
            detail
              props (Text, Bool) verbs (Consult, SetPropertyValue)

    thing TestRole
      property Text (mandatory, String)
        minLength = 100
        maxLength = 200
      property Bool (Boolean)
      property ADate (Date)
        minInclusive = '2022-04-15'
      property ANumber (Number)
        minInclusive = 10
        maxInclusive = 80
      property AnEmail (Email)
        minLength = 10
      property WeekDay (String)
        enumeration = ("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      property Appel (String)
        pattern = ".*appel.*" "Any word with the string `appel` in it."

    thing TestTable (relational)
      property Text (mandatory, String)
      property Bool (Boolean)
      property ADate (DateTime)
      property ANumber (mandatory, Number)
      property AnEmail (Email)
    
    thing CopyOfTestTable = TestTable

domain model://test.local#Test
  case TestCase1
    user Self
      perspective on ARole
        props (Prop1) verbs (SetPropertyValue)
      on entry
        do
          Prop1 = false for context >> ARole
    thing ARole
      property Prop1 (mandatory, Boolean)

  case TestCase2
    user Self
      perspective on ARole
        props (Prop1) verbs (SetPropertyValue)
      state SomeState = context >> ARole >> Prop1
        on entry
          do
            Prop1 = false for context >> ARole
    thing ARole
      property Prop1 (mandatory, Boolean)

  case TestCase3
    user Self
      perspective on ARole
        props (Prop1) verbs (SetPropertyValue)
      state SomeState = context >> NestedContext >> binding >> Prop2
        on entry
          do
            Prop1 = false for context >> ARole
    thing ARole
      property Prop1 (mandatory, Boolean)
    context NestedContext filledBy SubCase1
    case SubCase1
      external
        property Prop2 (mandatory, Boolean)

  case TestCase4
    user Self
      perspective on ARole
        props (Prop1) verbs (SetPropertyValue)
      state SomeState = context >> NestedContext >> binding >> context >> SubCaseRole1 >> Prop2
        on entry
          do
            Prop1 = false for context >> ARole
    thing ARole
      property Prop1 (mandatory, Boolean)
    context NestedContext filledBy SubCase2
    case SubCase2
      thing SubCaseRole1
        property Prop2 (mandatory, Boolean)

  case TestCase5
    context NestedContext filledBy SubCase3
    case SubCase3
      external
        property Prop2 (mandatory, Boolean)
      thing SubCaseRole3
        property Prop2 (mandatory, Boolean)
      user Self
        perspective on ARole
          props (Prop1) verbs (SetPropertyValue)
        state SomeState = context >> extern >> Prop2
          on entry
            do
              Prop1 = false for context >> ARole
      thing ARole
        property Prop1 (mandatory, Boolean)

  case TestCase6
    thing AnotherRole
      property Prop3 (mandatory, Boolean)
    context NestedContext6 filledBy SubCase4
    case SubCase4
      user Self
        perspective on ARole
          props (Prop1) verbs (SetPropertyValue)
        state SomeState = context >> extern >> binder NestedContext6 >> context >> AnotherRole >> Prop3
          on entry
            do
              Prop1 = false for context >> ARole
      thing ARole
        property Prop1 (mandatory, Boolean)

  case TestCase7
    state SomeState = exists (filter Candidates with (CandidateName == origin >> extern >> WantedName))

    external
      property WantedName (String)

    thing Candidates
      property CandidateName (String)

  case TestCase8
    thing Candidates
      property CandidateName (String)
      state SomeState = exists (filter context >> Candidates with (CandidateName == origin >> CandidateName))

  case TestCase9
    state SomeState = exists (filter Repository >> binding >> context >> Manifests with (LocalModelName == origin >> extern >> ModelName)) >> binding

    external
      property ModelName (String)

    context Repository filledBy RepositoryContext

  case RepositoryContext
    external

    context Manifests filledBy ManifestContext
      property LocalModelName (String)

  case ManifestContext
    external

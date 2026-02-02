-- Copyright Joop Ringelberg and Cor Baars 2019, 2020, 2021, 2023, 2025
domain model://perspectives.domains#Introduction
  use sys for model://perspectives.domains#System
  use intro for model://perspectives.domains#Introduction

-------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- We must first create the context and then later bind it.
          -- If we try to create and bind it in a single statement, 
          -- we find that the Installer can just create RootContexts
          -- as they are the allowed binding of StartContexts.
          -- As a consequence, no context is created.
          app <- create context IntroductionApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Introduction App" for start
  
  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (intro:MyIntroductions >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (intro:MyIntroductions >> extern)
      in
        remove context indexedcontext
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  -- The entry point (the `application`), available as intro:MyIntroductions.
  case IntroductionApp
    indexed intro:MyIntroductions
    aspect sys:RootContext
    aspect sys:ContextWithNotification
    external
      aspect sys:RootContext$External

    context Introductions (relational) filledBy Introduction
    context IncomingIntroductions = sys:SocialMe >> binding >> binder Introducee >> context >> extern
    user Manager = me
      perspective on Introductions
        only (CreateAndFill, Remove, RemoveContext)
        props (Title) verbs (Consult, SetPropertyValue)
      perspective on Manager
        props (FirstName, LastName) verbs (Consult)
      perspective on IncomingIntroductions
        props (Title) verbs (Consult)
      screen 
        who
          Manager
            master
              props (FirstName) verbs (Consult)
            detail
              props (FirstName, LastName) verbs (Consult)
        what
          markdown <### Introducing acquiantances
                    This is the place where you can introduce people to each other. 
                    After introducing them, they will be able to communicate with each other through MyContexts.
                    Within an introduction context, the introducer and introducees can chat with each other.
                    >
        where
          Introductions
            master 
              markdown <### Introductions
                        These are the persons that have been introduced to each other by you.
                        >
              props (Title) verbs (Consult)
            detail
              props (Title) verbs (Consult)
          IncomingIntroductions
            master 
              markdown <### Incoming introductions
                        You have been introduced to these persons by others.
                        >
              props (Title) verbs (Consult)
            detail
              props (Title) verbs (Consult)

  case Introduction 
    state NoIntroducer = not exists Introducer
      on entry
        do for Guest
          bind me to Introducer
    external
      property Title (String)
        readableName
    user Guest = me
      perspective on Introducer
        only (Create, Fill)

    user Introducer filledBy (sys:TheWorld$PerspectivesUsers + sys:SocialEnvironment$Persons)
      state ActionsCanBeTaken = exists binding
        on entry
          do
            create role Conversation
      perspective on extern
        props (Title) verbs (Consult, SetPropertyValue)
        action StartConversation
          create role Conversation
      perspective on Introducer
        props (FirstName) verbs (Consult)
      perspective on Introducee
        only (Create, Fill)
        props (FirstName, LastName) verbs (Consult)
      perspective on Conversation
        only (Create, RemoveContext, Remove)
        props (Messages, Media) verbs (AddPropertyValue, Consult)
      screen
        who
          Introducee
            master
              markdown <### Introducees
                        These are the persons that have been introduced to each other by you.
                        >
              without props (FirstName)
            detail
        what
          markdown <### Introduction
                    The purpose of this context is to introduce people to each other.
                    You can chat with the others under *Who*.
                    >
          External
            master
              props (Title) without (SetPropertyValue)
            detail
        where

    user Introducee (relational) filledBy (sys:TheWorld$PerspectivesUsers + sys:SocialEnvironment$Persons)
      perspective on extern
        props (Title) verbs (Consult)
        action StartConversation
          create role Conversation
      perspective on Introducer
        props (FirstName, LastName) verbs (Consult)
      perspective on Introducee
        props (FirstName, LastName) verbs (Consult)
      perspective on Conversation
        only (Create, Remove, RemoveContext)
        props (Messages, Media) verbs (AddPropertyValue, Consult)
      screen
        who
          Introducer
            master
              props (FirstName) verbs (Consult)
            detail
              props (FirstName, LastName) verbs (Consult)
          Introducee
            master
              markdown <### Introducees
                        These are the persons that have been introduced to you.
                        >
              props (FirstName) verbs (Consult)
            detail
              props (FirstName, LastName) verbs (Consult)
        what
          markdown <### Introduction
                    You have been introduced to others by the introducer of this context.
                    You can chat with the others under *Who*.
                    The introducees have been added to your contacts.
                    >
          External
            master
              props (Title) verbs (Consult)
            detail
              props (Title) verbs (Consult)
        where
      
    thing Conversation
      aspect sys:Chat

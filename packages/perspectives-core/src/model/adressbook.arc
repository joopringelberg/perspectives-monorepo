-- Copyright (C) 2026 Joop Ringelberg
-- All rights reserved.
-- Unauthorized copying, redistribution, or commercial use is prohibited without written permission.

domain model://joopringelberg.nl#Addressbook
  use sys for model://perspectives.domains#System
  use ab for model://joopringelberg.nl#Addressbook

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context AddressbookApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Address book" for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (ab:MyAddressbook >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (ab:MyAddressbook >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case AddressbookApp
    indexed ab:MyAddressbook
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
    user Manager = sys:Me
      perspective on AllPersons
        props (FirstName, LastName) verbs (Consult)
        in object state Unfilled
          only (Create, Fill)
        in object state NonUser
          only (Remove)
      perspective on OtherPersons
        only (Create)
        props (FirstName, LastName) verbs (Consult, SetPropertyValue)
      perspective on Peers
        props (FirstName, LastName) verbs (Consult)
        
      action AddOtherPerson
        letA 
          nuser <- create role NonPerspectivesUsers in sys:TheWorld
        in
          -- Het is de vraag of het perspectief tijdig is.
          bind nuser to Persons in sys:MySocialEnvironment
      
      screen
        column
          row
            markdown <## Medegebruikers
                      Deze mensen zitten ook op MyContexts.
                      Kopieer iemand uit deze tabel als je gegevens met hem of haar
                      wilt delen.
                     >
          row
            table Peers
          row 
            markdown <## Andere personen
                      Deze mensen maken geen gebruik van MyContexts.
                      Je kunt geen gegevens met hen delen.
                      Voeg een nieuwe persoon toe met de actie [[action: AddAnotherUser| Voeg persoon toe]].
                     >
          row
            table OtherPersons
        column
          row 
            markdown <## Alle personen
                      Kopieer iemand uit deze tabel als het niet uitmaakt of 
                      iemand wel of geen Perspectives gebruikt.
                     >
          row
            table AllPersons

          

    user AllPersons = sys:MySocialEnvironment >> Persons

    user Peers = sys:TheWorld >> PerspectivesUsers

    user OtherPersons = sys:TheWorld >> NonPerspectivesUsers

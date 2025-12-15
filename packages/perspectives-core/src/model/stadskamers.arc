-- Stadskamers - Copyright Joop Ringelberg and Cor Baars 2025

domain model://joopringelberg.nl#Stadskamers
  use sys for model://perspectives.domains#System
  use sk for model://joopringelberg.nl#Stadskamers
  
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
          couchdbapp <- create context StadskamersApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ couchdbapp >> extern to start
          Name = "Stadskamer App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (sk:MyStadskamerApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (sk:MyStadskamerApp >> extern)
      in
        remove context indexedcontext
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
-- The INDEXED context sk:MyStadskamerApp is the starting point containing all Stadskamers.
  case StadskamersApp
    indexed sk:MyStadskamerApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
    -- the exit is triggered on removing a role instance of StartContexts
    on exit
      do for Manager
        delete context bound to Stadskamers

    user Manager = sys:SocialMe >> binding
      perspective on Stadskamers
        only (CreateAndFill, RemoveContext, DeleteContext)
        props (Name) verbs (Consult, SetPropertyValue)
      
      screen
        who
        what
          row
            markdown <### Dit is de Stadskamer App 
                      Hier kun je Stadskamer-organisaties aanmaken en beheren. 
                      Je kunt vier rollen spelen:
                      
                      * **Deelnemer**: als je een Stadskamer bezoekt voor zijn activiteiten.
                      * *Ervaringsdeskundige* (deze rol is er nog niet in de app): als je een Stadskamer bezoekt om te helpen bij de activiteiten.
                      * **Medewerker**: als je in dienst bent bij een Stadskamer.
                      * **Beheerder**: als je de app inricht voor een specifieke Stadskamer.
                      
                      Kies in het menu linksboven de rol die je wilt spelen. Het scherm past zich aan de rol aan.
                      >
        where
          Stadskamers
            master
            detail

    user Deelnemer = sys:SocialMe >> binding
      perspective on MyRoutes
        props (Name) verbs (Consult)
      screen
        who
        what
          row
            markdown <### Dit is de Stadskamer App 
                      Dit is jouw Stadskamer app. Onder *Waar*, hoofdje *Routes* zie je jouw route.
                      >
        where
          MyRoutes
            master
            detail

    user Medewerker = sys:SocialMe >> binding
      perspective on Stadskamers
        props (Name) verbs (Consult)
            
      perspective on MyCases
        props (Name) verbs (Consult)

      screen
        who
        what
          row
            markdown <### Dit is de Stadskamer App 
                      Je bent medewerker van een Stadskamer-organisatie.
                      Onder *Waar*, hoofdje *Stadskamers* zie je de Stadskamer waar je werkt.
                      Onder *Wat*, hoofdje *Mijn caseload* zie je de routes die je begeleidt.
                      >
        where
          Stadskamers
            master
            detail
          MyCases
            master
            detail


    context Stadskamers (relational) filledBy Stadskamer
      state WhenCreated = exists binding
        perspective of Manager
          perspective on (Stadskamers >> binding >> context >> Administrators)
            only (Create, Fill)
        on entry
          do for Manager
            bind sys:SocialMe >> binding to Administrators in origin >> binding >> context
    
    context MyRoutes = sys:SocialMe >> binding >> binder Deelnemer >> context >> extern
    context MyCases =  sys:SocialMe >> binding >> binder Begeleider >> context >> extern

  ----------------------------------
  case Stadskamer
    external
      property Name (String)
        readableName
      state StadskamerReceived = 
        (not exists filter sk:MyStadskamerApp >> Stadskamers with filledBy origin) 
          and (exists filter context >> Employees with filledBy sys:SocialMe >> binding)
        perspective of Employees
          perspective on extern >> binder Stadskamers
            only (CreateAndFill)
        perspective of Administrators
          perspective on extern >> binder Stadskamers
            only (CreateAndFill)
        on entry
          do for Employees
            bind origin to Stadskamers in sk:MyStadskamerApp
          do for Administrators
            bind origin to Stadskamers in sk:MyStadskamerApp
    
    -- This role creates locations and employees.
    user Administrators (relational) filledBy sys:TheWorld$PerspectivesUsers
      perspective on Administrators
        only (Create, Fill, Remove)
        props (FirstName, LastName) verbs (Consult)
      
      perspective on Employees
        only (Create, Fill, Remove)
        props (FirstName, LastName) verbs (Consult)
      
      perspective on Locations
        only (CreateAndFill, Remove)
        props (Name) verbs (Consult, SetPropertyValue)
      
      perspective on Ambassadeurs
        only (Create, Fill, Remove)
        props (FirstName, LastName) verbs (Consult)
      
      screen 
        who
          Administrators
            master
              without props (FirstName)
            detail
          Employees
            master
              without props (FirstName)
            detail
          Ambassadeurs
            master
              without props (FirstName)
            detail
        what
          row
            markdown <### Stadskamer: de organisatie
                      Hier kun je medewerkers en locaties beheren. 
                      Er zijn twee soorte locaties: grote, met meerder activiteitencentra, en kleine, met één activiteitencentrum.
                      Zo maak je een Locatie:

                      * maak een Locatie of een Kleine Locatie aan onder 'Waar' - Locaties
                      * zorg dat de Medewerker die de locatie gaat beheren, op het clipboard staat
                      * open de Locatie die je hebt aangemaakt
                      * maak een lege rol aan onder 'Wie' - LocatieManager
                      * vul de rol met de Medewerker-rol op het clipboard
                      >
        where
          Locations
            master
            detail

    user Employees (relational) filledBy sys:TheWorld$PerspectivesUsers
      perspective on Employees
        props (FirstName, LastName) verbs (Consult)

      perspective on Locations
        props (Name) verbs (Consult)

      screen 
        who
          Employees
            master
              without props (FirstName)
            detail
        what
          row
            markdown <### Stadskamer: de organisatie
                      Hier zie je je collegas onder Wie.
                      Kijk onder 'Locaties' voor de verschillende vestigingen.
                      >
        where
          Locations
            master
            detail
    
    user Ambassadeurs (relational) filledBy Employees
      perspective on Employees
        props (FirstName, LastName) verbs (Consult)
      perspective on Routes
        only (CreateAndFill, Remove)
        props (Name) verbs (Consult)
      perspective on Routes >> binding >> context >> Ambassadeur
        only (Create, Fill, Remove)
      perspective on Ambassadeurs
        props (FirstName, LastName) verbs (Consult)

      action CreateRoute
        letA
          route <- create context Route bound to Routes
        in
          bind currentactor to Ambassadeur in route >> binding >> context
      
      perspective on MyRoutes
        props (Name) verbs (Consult)
      
      screen
        who
          Ambassadeurs
            master
              without props (FirstName)
            detail
          Employees
            master
              without props (FirstName)
            detail
        what
          row
            markdown <### Stadskamer: routes
                      Hier kun je routes beheren. 
                      Maak een route aan met de knop [[action: CreateRoute|Nieuwe route]] (is ook een actie in het menu linksboven).
                      >
        where
          Routes
            master
            detail

    context Locations (relational) filledBy (Location, SmallLocation)

    context Routes (relational) filledBy Route

    context MyRoutes = filter Routes with binding >> context >> Ambassadeur filledBy sys:SocialMe >> binding

  
  ----------------------------------
  case Route
    external
      property Name = context >> Deelnemer >> LastName
        readableName
    on entry
      do for Ambassadeur
        create role Financiering
        create role RisicoInventarisatie
    
    ----------------------------------
    user Deelnemer filledBy sys:TheWorld$PerspectivesUsers
      property Vraag (String)
        minLength = 100
      property Verheldering (String)
        minLength = 100

      property BSN (Number)
        minLength = 9
        maxLength = 9
      property Geslacht (String)
        enumeration = ("man", "vrouw", "anders")
      property Geboortedatum (Date)
      property NoodTelefoon (String)
      property ReceiveAllEmails (Boolean)
      property Bijzonderheden (String)
      

      perspective on Deelnemer
        props (FirstName, LastName, Vraag, Verheldering) verbs (Consult)
        props (NoodTelefoon, ReceiveAllEmails) verbs (Consult, SetPropertyValue, DeleteProperty)
      perspective on RisicoInventarisatie
        only (Create, Fill)
        props (Medicatie, SignaleringsPlan, Agressie, Vallen, Diabetes, Epilepsie, Zicht, Gehoor) verbs (Consult, SetPropertyValue)
        props (Name) verbs (Consult)
      perspective on Begeleider
        props (FirstName, LastName) verbs (Consult)
      perspective on Ambassadeur
        props (FirstName, LastName) verbs (Consult)
      perspective on MyActivities
        props (Name) verbs (Consult)
      -- Dit gaat alleen werken met een publieke rol in Location.
      -- perspective on Location
      --   props (Name) verbs (Consult)
      screen
        who
          Ambassadeur
            master
              without props (FirstName)
            detail
          Begeleider
            master
              without props (FirstName)
            detail
          Deelnemer
            master
              without props (FirstName, Street, HouseNumber, PostalCode, City, EmailAddress, Phone, BSN, Geslacht, Geboortedatum, NoodTelefoon, ReceiveAllEmails, Vraag, Verheldering, Bijzonderheden)
            detail
              without props (Vraag, Verheldering, NoodTelefoon, ReceiveAllEmails)
        what
          tab "Route"
            row
            markdown <### Dit is je route
                      Hier zie je de vraag die je hebt gesteld bij het intakegesprek.
                      >
            row
              form Deelnemer
                without props (FirstName, LastName, Street, HouseNumber, PostalCode, City, EmailAddress, Phone, BSN, Geslacht, Geboortedatum, NoodTelefoon, ReceiveAllEmails)
          tab "Risico-inventarisatie"
            form RisicoInventarisatie
        where
          MyActivities
            master
            detail


    ----------------------------------
    user Begeleider filledBy Stadskamer$Employees
      perspective on Begeleider
        props (FirstName, LastName) verbs (Consult)
      perspective on Deelnemer
        props (FirstName, LastName) verbs (Consult)
        props (Vraag, Verheldering, Street, HouseNumber, PostalCode, City, EmailAddress, Phone, BSN, Geslacht, Geboortedatum, NoodTelefoon, ReceiveAllEmails, Bijzonderheden) verbs (Consult, SetPropertyValue, DeleteProperty)
      perspective on Ambassadeur
        props (FirstName, LastName) verbs (Consult)
      perspective on RisicoInventarisatie
        only (Create, Fill)
        props (Medicatie, SignaleringsPlan, Agressie, Vallen, Diabetes, Epilepsie, Zicht, Gehoor) verbs (Consult, SetPropertyValue)
        props (Name) verbs (Consult)
      perspective on Persoonsbeeld
        only (Create, Fill)
        props (Diagnose, Levensgeschiedenis, Lichamelijk, Cognitief, SociaalEmotioneel) verbs (Consult, SetPropertyValue)
        props (Name) verbs (Consult)
      perspective on MyActivities
        props (Name) verbs (Consult)

      screen
        who
          Ambassadeur
            master
              without props (FirstName)
            detail
          Begeleider
            master
              without props (FirstName)
            detail
          Deelnemer
            master
              without props (FirstName, Street, HouseNumber, PostalCode, City, EmailAddress, Phone, BSN, Geslacht, Geboortedatum, NoodTelefoon, ReceiveAllEmails, Vraag, Verheldering, Bijzonderheden)
            detail
              without props (Street, HouseNumber, PostalCode, City, EmailAddress, Phone, BSN, Geslacht, Geboortedatum, NoodTelefoon, ReceiveAllEmails, Vraag, Verheldering, Bijzonderheden)
        what
          tab "Basisgegevens"
            row
            markdown <### Een deelnemer die je begeleidt
                      Hier zie je de vraag die de deelnemer heeft gesteld bij het intakegesprek.
                      >
            row
              form Deelnemer
                without props (Vraag, Verheldering, Bijzonderheden)
          tab "Route"
            form Deelnemer
              without props (FirstName, LastName, Street, HouseNumber, PostalCode, City, EmailAddress, Phone, BSN, Geslacht, Geboortedatum, NoodTelefoon, ReceiveAllEmails)
          tab "Risico-inventarisatie"
            form RisicoInventarisatie
          tab "Persoonsbeeld"
            form Persoonsbeeld
        where
          MyActivities
            master
            detail

    ----------------------------------
    user Ambassadeur filledBy Stadskamer$Employees
      perspective on Begeleider
        only (Create, Fill, Remove)
        props (FirstName, LastName) verbs (Consult)
      perspective on Deelnemer
        only (Create, Fill, Remove)
        props (Vraag, Verheldering, Street, HouseNumber, PostalCode, City, EmailAddress, Phone, BSN, Geslacht, Geboortedatum, NoodTelefoon, ReceiveAllEmails, Bijzonderheden) verbs (Consult, SetPropertyValue, DeleteProperty)
        props (FirstName, LastName) verbs (Consult)
      perspective on Ambassadeur
        props (FirstName, LastName) verbs (Consult)
      perspective on Financiering
        only (Create, Fill)
        props (Gemeente, WMOOverigeGemeenten, PGB, WLZ, Overig) verbs (Consult, SetPropertyValue)
        props (BSN) verbs (Consult)
      perspective on RisicoInventarisatie
        only (Create, Fill)
        props (Medicatie, SignaleringsPlan, Agressie, Vallen, Diabetes, Epilepsie, Zicht, Gehoor) verbs (Consult, SetPropertyValue)
        props (Name) verbs (Consult)
      perspective on Persoonsbeeld
        only (Create, Fill)
        props (Diagnose, Levensgeschiedenis, Lichamelijk, Cognitief, SociaalEmotioneel, Houdingswaarden, Omgevingsvoorwaarden, Overige) verbs (Consult, SetPropertyValue)
        props (Name) verbs (Consult)
      
      screen
        who
          Ambassadeur
            master
              without props (FirstName)
            detail
          Begeleider
            master
              without props (FirstName)
            detail
          Deelnemer
            master
              without props (FirstName, Street, HouseNumber, PostalCode, City, EmailAddress, Phone, BSN, Geslacht, Geboortedatum, NoodTelefoon, ReceiveAllEmails, Vraag, Verheldering, Bijzonderheden)
            detail
              without props (Street, HouseNumber, PostalCode, City, EmailAddress, Phone, BSN, Geslacht, Geboortedatum, NoodTelefoon, ReceiveAllEmails)
        what
          tab "Basisgegevens"
            row
              markdown <### Route van deelnemer

                        * Vul een deelnemer in en wijs een begeleider toe.
                        * Doe een intake.
                        >
            row
              form Deelnemer
                without props (Vraag, Verheldering, Bijzonderheden)
          tab "Route"
            form Deelnemer
              without props (FirstName, LastName, Street, HouseNumber, PostalCode, City, EmailAddress, Phone, BSN, Geslacht, Geboortedatum, NoodTelefoon, ReceiveAllEmails)
          tab "Financiering"
            form Financiering
          tab "Risico-inventarisatie"
            form RisicoInventarisatie
          tab "Persoonsbeeld"
            form Persoonsbeeld
        where

    ----------------------------------
    thing Financiering
      property Naam = context >> Deelnemer >> LastName
        readableName
      property BSN = context >> Deelnemer >> BSN
      property Gemeente (String)
        enumeration = ("Doetinchem", "Oost-Gelre", "Berkelland", "Monterland", "Bronckhorst")
      property WMOOverigeGemeenten (Boolean)
      property PGB (Boolean)
      property WLZ (Boolean)
      property Overig (String)
    ----------------------------------
    thing RisicoInventarisatie
      property Name = context >> Deelnemer >> LastName
        readableName
      property Medicatie (String)
      property SignaleringsPlan (String)
      property Agressie (String)
      property Vallen (String)
      property Diabetes (String)
      property Epilepsie (String)
      property Zicht (String)
      property Gehoor (String)

    ----------------------------------
    thing Persoonsbeeld
      property Name = context >> Deelnemer >> LastName
        readableName
      property Diagnose (String)
      property Levensgeschiedenis (String)
      property Lichamelijk (String)
      property Cognitief (String)
      property SociaalEmotioneel (String)
      property Houdingswaarden (String)
      property Omgevingsvoorwaarden (String)
      property Overige (String)
    ----------------------------------
    context MyActivities = Deelnemer >> binder ActivititeitDeelnemer >> context >> extern

    context Location (functional) = Begeleider >> binding >> binder EmployeeOnLocation >> context >> extern
    ----------------------------------

  ----------------------------------
  case Location
    external
      aspect sys:Addressable
      property Name (String)
        readableName
    
    user Initializer = filter (extern >> binder Locations >> context >> Administrators) with filledBy (sys:SocialMe >> binding)
      perspective on LocationManager
        only (Create, Fill, Remove)
        props (FirstName, LastName) verbs (Consult)
      screen 
        who
          LocationManager
            master
              without props (FirstName)
            detail
        what
          row
            markdown <### Deze locatie heeft een manager nodig
                      Hier kun je een manager aanstellen voor deze locatie. 
                      De manager kan dan activiteitencentra beheren.

                      * maak een lege rol aan onder 'Wie' - LocatieManager
                      * vul met een Medewerker-rol op het clipboard.
                      >
        where

    user LocationManager filledBy Employees
      perspective on ActivityCenters
        only (CreateAndFill, RemoveContext)
        props (Name) verbs (Consult, SetPropertyValue)
      
      perspective on AllEmployees
        props (FirstName, LastName) verbs (Consult)
      
      perspective on EmployeeOnLocation
        only (Create, Fill, Remove)
        props (FirstName, LastName) verbs (Consult)
      
      screen
        who
          EmployeeOnLocation
            master
              without props (FirstName)
            detail
          AllEmployees
            master
              without props (FirstName)
            detail
        what
          row
            markdown <### Locatie: activiteitencentra
                      Hier kun je medewerkers en activiteitencentra beheren.

                      Zo voeg je een medewerker toe aan deze locatie:
                      
                      * Kopieer een medewerker uit de lijst onder *Alle medewerkers* naar het clipboard.
                      * Maak onder *Wie*, *collegas op deze lokatie* een nieuwe rol aan en vul die met de medewerker op het clipboard.
                      
                      Voorbeelden van activiteitcentra zijn: sport, kunst, muziek, koken, etc.
                      Een locatie kan meerdere activiteitencentra hebben.

                      Zo maak je een Activiteitencentrum aan:

                      * maak een Activiteitencentrum aan onder 'Waar' - Activiteitencentrum
                      * zorg dat de Medewerker die de Regisseur van het centrum wordt, op het clipboard staat
                      * open het centrum dat je hebt aangemaakt
                      * maak een lege rol aan onder 'Wie' - Regisseur
                      * vul de rol met de Medewerker-rol op het clipboard
                      >
        where
          ActivityCenters
            master
            detail

    user EmployeeOnLocation filledBy Employees
      perspective on LocationManager
        props (FirstName, LastName) verbs (Consult)
      perspective on EmployeeOnLocation
        props (FirstName, LastName) verbs (Consult)
      perspective on ActivityCenters
        props (Name) verbs (Consult)
      perspective on AllRoutesOnLocation
        props (Name) verbs (Consult)
      
      screen
        who
          LocationManager
            master
              without props (FirstName)
            detail
          EmployeeOnLocation
            master
              without props (FirstName)
            detail
        what
          row
            markdown <### Locatie
                      Je werkt op deze locatie.
                      Je bent betrokken bij (één of meerdere van) de activiteitencentra die hier zijn.
                      >
        where
          ActivityCenters
            master
            detail
          AllRoutesOnLocation
            master
            detail

    user AllEmployees = extern >> binder Locations >> context >> Employees
    -- Dit moet een rol met een publiek perspectief worden.
    user Bezoeker = sys:SocialMe >> binding
      perspective on ActivityCenters
        props (Name) verbs (Consult)
      screen 
        who
        what
          row
            markdown <### Locatie: activiteitencentra
                      Hier zie je wat je op deze locatie kunt doen (kijk onder *Waar*, hoofdje *Activiteitencentra*).
                      >
        where
          ActivityCenters
            master
            detail
    context ActivityCenters (relational) filledBy ActivityCenter
    context AllRoutesOnLocation = 
      letE 
        loc <- extern
      in 
        filter (extern >> binder Locations >> context >> Routes >> binding >> context with Location == loc) >> extern

  ----------------------------------
  case ActivityCenter
    external
      property Name (String)
        readableName
    user Initializer = extern >> binder ActivityCenters >> context >> LocationManager
      perspective on Regisseur
        only (Create, Fill, Remove)
        props (FirstName, LastName) verbs (Consult)
      screen 
        who
          Regisseur
            master
              without props (FirstName)
            detail
        what
          row
            markdown <### Deze locatie heeft nog geen Regisseur
                      Hier kun je de Regisseur benoemen.
                      De Regisseur organiseert de activiteiten.

                      * maak een lege rol aan onder 'Wie' - Regisseur
                      * vul met een Medewerker-rol op het clipboard.
                      >
                    when not exists Regisseur >> binding
            markdown <### Deze locatie heeft al een Regisseur
                      Je hebt eerder een Regisseur benoemd.
                      >
                    when exists Regisseur >> binding

        where
    
    user Regisseur filledBy Employees
      perspective on Activities
        only (CreateAndFill, Remove)
        props (Name) verbs (Consult, SetPropertyValue)
      perspective on Regisseur
        props (FirstName, LastName) verbs (Consult)
      perspective on Coworkers
        only (Create, Fill, Remove)
        props (FirstName, LastName) verbs (Consult)
      perspective on ActivityDeelnemers
        props (FirstName, LastName) verbs (Consult)
      screen
        who
          Regisseur
            master
              without props (FirstName)
            detail
          Coworkers
            master
              without props (FirstName)
            detail
          ActivityDeelnemers
            master
              without props (FirstName)
            detail
        what
          row
            markdown <### Activiteitencentrum: activiteiten
                      Hier kun je activiteiten beheren. 
                      Een activiteitencentrum kan meerdere activiteiten hebben.

                      Zo maak je een Activiteit aan:

                      * maak een Activiteit aan onder 'Wat' - Activiteiten
                      * zorg dat de Medewerker die de Organisator van de activiteit wordt, op het clipboard staat
                      * open de activiteit die je hebt aangemaakt
                      * maak een lege rol aan onder 'Wie' - Organisator
                      * vul de rol met de Medewerker-rol op het clipboard
                      >
        where
          Activities
            master
            detail

    user Coworkers filledBy Employees
      perspective on Coworkers
        props (FirstName, LastName) verbs (Consult)
      perspective on Activities
        props (Name) verbs (Consult)
      perspective on Regisseur
        props (FirstName, LastName) verbs (Consult)
      perspective on ActivityDeelnemers
        props (FirstName, LastName) verbs (Consult)
      screen
        who
          Regisseur
            master
              without props (FirstName)
            detail
          Coworkers
            master
              without props (FirstName)
            detail
          ActivityDeelnemers
            master
              without props (FirstName)
            detail
        what
          row
            markdown <### Activiteitencentrum: activiteiten
                      Je werkt in dit activiteitencentrum.
                      >
        where
          Activities
            master
            detail
    
    user Bezoeker = sys:SocialMe >> binding
      perspective on Activities
        props (Name) verbs (Consult)
      screen 
        who
        what
          row
            markdown <### Activiteitencentrum: activiteiten
                      Hier zie je wat je in dit activiteitencentrum kunt doen (kijk onder *Waar*, hoofdje *Activiteiten*).
                      >
        where
          Activities
            master
            detail
    context Activities (relational) filledBy Activity

    context ActivityCases = Regisseur >> binding >> binder Begeleider >> context >> extern 
      union Coworkers >> binding >> binder Begeleider >> context >> extern 
    
    user ActivityDeelnemers = ActivityCases >> context >> Deelnemer
    

  ----------------------------------
  case Activity
    external
      property Name (String)
        readableName
      property Description (String)
        minLength = 100
      property Weekday (String)
        enumeration = ("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
      property StartTime (DateTime)
      property EndTime (DateTime)
    
    user Initializer = filter (extern >> binder Activities >> context >> Regisseur) with filledBy (sys:SocialMe >> binding)
      perspective on Organizer
        only (Create, Fill, Remove)
      screen 
        who
          Organizer
            master
              without props (FirstName)
            detail
        what
          row
            markdown <### Deze activiteit heeft een Organisator nodig
                      
                      * maak een lege rol aan onder 'Wie' - Organisator
                      * vul met een Medewerker-rol op het clipboard.
                      >
        where
    
    user ActivititeitDeelnemer filledBy Deelnemer
      perspective on External
        props (Name, Description, Weekday, StartTime, EndTime) verbs (Consult)
      perspective on Organizer
        props (FirstName, LastName) verbs (Consult)
      perspective on ActivititeitDeelnemer
        props (FirstName, LastName) verbs (Consult)
      screen 
        who
          Organizer
            master
              without props (FirstName)
            detail
          ActivititeitDeelnemer
            master
              without props (FirstName)
            detail
        what
          row
            markdown <### Activiteit: deelname
                      Je bent deelnemer aan deze activiteit.
                      >
          row
            form External
        where

    user Organizer filledBy Employees
      perspective on External
        props (Name, Description, Weekday, StartTime, EndTime) verbs (Consult, SetPropertyValue)
      perspective on Organizer
        props (FirstName, LastName) verbs (Consult)
      perspective on ActivititeitDeelnemer
        only (Create, Fill, Remove)
        props (FirstName, LastName) verbs (Consult)
      perspective on Bijeenkomsten
        only (CreateAndFill, Remove)
        props (Datum) verbs (Consult, SetPropertyValue)
      perspective on Bijeenkomsten >> binding >> context >> Aanwezigen
            only (Create, Fill)
      action MaakBijeenkomst
        create context Bijeenkomst bound to Bijeenkomsten
      screen 
        who
          Organizer
            master
              without props (FirstName)
            detail
          ActivititeitDeelnemer
            master
              without props (FirstName)
            detail
        what
          row
            markdown <### Activiteit
                      Jij bent verantwoordelijk voor deze activiteit.
                      Beschrijf je activiteit
                      >
          row 
            form External
        where
          Bijeenkomsten
            master
              markdown <### Activiteit: bijeenkomsten
                          Hier kun je bijeenkomsten aanmaken om aanwezigheid te registreren [[action:MaakBijeenkomst|Maak bijeenkomst]]. 
                          Een activiteit kan meerdere bijeenkomsten hebben.
                          Vul de datum van de bijeenkomst in!
                          >
            detail
    -- Dit moet een rol met een publiek perspectief worden.
    user Bezoeker = sys:SocialMe >> binding
      perspective on External
        props (Name, Description, Weekday, StartTime, EndTime) verbs (Consult)
      screen 
        who
        what
          row
            markdown <### Activiteit
                      Hier zie je wat je kunt doen in deze activiteit.
                      >
          row 
            form External
        where
    context Bijeenkomsten (relational) filledBy Bijeenkomst
      on entry
        do for Organizer
          bind context >> ActivititeitDeelnemer to Aanwezigen in binding >> context
  
  ----------------------------------
  case Bijeenkomst
    external
      property Datum (Date)
        readableName
    user Organizer = extern >> binder Bijeenkomsten >> context >> Organizer
      perspective on Aanwezigen
        only (Create, Fill, Remove)
        props (FirstName, LastName) verbs (Consult)
        props (Aanwezig) verbs (Consult, SetPropertyValue)
      screen 
        who
        what
          row
            markdown <### Bijeenkomst: deelnemers
                      * Vink deelnemers aan die aanwezig zijn.
                      * Je kunt zo nodig deelnemers toevoegen.
                      * Je mag afwezige deelnemers verwijderen, maar dat is niet nodig.
                      >
          row
            table Aanwezigen
        where
    user Aanwezigen (relational) filledBy Deelnemer
      property Aanwezig (Boolean)
      
  ----------------------------------
  case SmallLocation
    aspect sk:Location
    aspect sk:ActivityCenter
    external
      aspect sk:ActivityCenter$External
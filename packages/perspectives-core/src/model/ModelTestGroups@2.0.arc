-- Copyright Joop Ringelberg 2026. 

domain model://perspectives.domains#ModelTestGroups@2.0
  use sys for model://perspectives.domains#System
  use mtg for model://perspectives.domains#ModelTestGroups
  use cm for model://perspectives.domains#CouchdbManagement

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context ModelTestGroupApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Model Test Group App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mtg:MyTestGroupApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mtg:MyTestGroupApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case ModelTestGroupApp
    indexed mtg:MyTestGroupApp
    aspect sys:RootContext
    external
    
    user Manager = me
      perspective on ModelTestGroups
        only (CreateAndFill, RemoveContext)
        props (Name) verbs (Consult, SetPropertyValue)

    context ModelTestGroups (relational) filledBy ModelTestGroup

  case ModelTestGroup
    on entry
      do for Initializer
        bind me to Developer
    state AddToApp = (exists Developer) and not exists extern >> binder ModelTestGroups
      on entry
        do for Initializer
          bind extern to ModelTestGroups in mtg:MyTestGroupApp

    external
      aspect sys:RoleWithName
    
    user Initializer = me
      perspective on Developer
        only (Create, Fill)
      perspective on extern >> binder ModelTestGroups
        only (Create, Fill)
    
    user Developer filledBy (sys:TheWorld$PerspectivesUsers + sys:SocialEnvironment$Persons)
      perspective on Participants
        only (Create, Fill, Remove)
        props (FirstName,LastName) verbs (Consult)
        props (UpdateModel) verbs (Consult, SetPropertyValue)
      
      perspective on Model
        only (Create, Fill, Remove)
      
      action UpdateParticipantsModel
        UpdateModel = true for Participants

      action CreateModel
        create role Model
      
      screen
        who
          Participants
            master
              fillfrom sys:TheWorld >> PerspectivesUsers >> binder sys:SocialEnvironment$Persons
              with props (LastName)
            detail
              with props (FirstName, LastName)
        what
          row
            markdown <### Manage your model test group
                      * Add participants on the left.
                      * [[action: CreateModel|Create a model role first]]
                      * Then copy a Versions instance in a ModelManifest and fill the new Model role with it. 

                      Update the model in your participants installation with the context action [[action: UpdateParticipantsModel| Update model]].
                     >
        where
          Model
            master
              only (Fill)
            detail

    user Participants (relational) filledBy (sys:TheWorld$PerspectivesUsers + sys:SocialEnvironment$Persons)
      property UpdateModel (Boolean)
      state RunUpdate = UpdateModel
        on entry
          do for Participants
            runRoleAction UpdateModel for Visitor on (context >> Model >> binding) in (context >> Model >> binding >> context)
            UpdateModel = false
      
      perspective on Model
        props (VersionedLocalModelName) verbs (Consult)

      perspective on Participants
        selfonly
        props (UpdateModel) verbs (Consult, SetPropertyValue)
      
      perspective on Developer
        props (FirstName, LastName) verbs (Consult)

      screen
        who
          Developer
            master
              with props (FirstName, LastName)
            detail
        what
          row
            markdown <### The developer can update your installation
                      By participating in this model group, you grant the developer the right
                      to update the model in your installation at his discretion.
                     >
        where
          Model
            master
            detail

    thing Model filledBy cm:ModelManifest$Versions
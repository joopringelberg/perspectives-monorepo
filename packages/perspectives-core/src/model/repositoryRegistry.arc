-- RepositoryRegistry - Copyright Joop Ringelberg 2026

domain model://perspectives.domains#RepositoryRegistry@1.0
  use sys for model://perspectives.domains#System
  use rr for model://perspectives.domains#RepositoryRegistry
  use cm for model://perspectives.domains#CouchdbManagement

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context RepositoryOverview
          start <- create role StartContexts in sys:MySystem
        in
          bind_ app >> extern to start
          Name = "Repository Registry" for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (rr:MyRepositoryOverview >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case RepositoryOverview
    indexed rr:MyRepositoryOverview
    aspect sys:RootContext

    user Manager = me
      perspective on TheRegistry
        only (CreateAndFill)
        props (Name) verbs (Consult)

      screen 
        who
        what
          row
            markdown <### Repository Registry
                      Maintain an overview of all known repositories.
                      Initialize the registry under **where**: creat the it there.
                      Then move to the registry and assume the Initizalizer to fill its Manager role 
                      with a Bespoke Database owner. The database owned by the Manager will be used to store the public version of the registry.
                      To use this app as an ordinary user, switch to the 'End User' role.
                      >
        where
          TheRegistry
            master
              with props (Name)
            detail
              with props (Name)

    context TheRegistry filledBy PublicRepositoryOverview
      aspect sys:RoleWithId

    -- The ordinary user can see which repositories are already registered
    -- and can visit the public overview to discover new ones.
    user OrdinaryUser (default) = me
      perspective on MyRepositories
        props (Domain) verbs (Consult)

      screen
        who
        what
          row
            markdown <### Repository Registry
                      Under `where` you find the repositories that are registered in your installation.
                      A repository is a collection of models (apps) that you can install.
                      Visit the [[link:pub:https://perspectives.domains/cw_v74vfn21lx/#zqys0g055e$External|public repository overview]] to discover 
                      new repositories and add them to your installation.
                      >
        where
          MyRepositories
            master
              with props (Domain)
            detail

    -- Calculated role: the repositories the user has already registered in their system.
    context MyRepositories = sys:MySystem >> Repositories

  case PublicRepositoryOverview
    external
      aspect sys:RoleWithName
      
    user Initializer = me
      perspective on Manager
        only (Create, Fill, Remove)
        props (FirstName, LastName, BespokeDatabaseUrl) verbs (Consult)
      perspective on extern
        props (Name) verbs (Consult, SetPropertyValue)
      
      screen
        who
          Manager
            master
              with props (FirstName, LastName)
            detail
              with props (BespokeDatabaseUrl)
        what
          row
            markdown <### Initialize Repository Registry
                      Your task is to create an instance of the Manager role and then fill it with an instance of a BespokeDatabase owner.
                      The database owned by the Manager will be used to store the public version of the registry.
                      Then switch to the Manager role to add repositories to the registry.
                      >
          row
            form External
        where

    -- The Manager is the one who maintains the list of repositories.
    -- Must be filledBy a role that has access to a BespokeDatabase,
    -- following the pattern of PublicPageCollection$Author.
    user Manager filledBy (sys:TheWorld$PerspectivesUsers + cm:BespokeDatabase$Owner)
      perspective on Manager
        props (FirstName, LastName) verbs (Consult)
      perspective on Repositories
        only (Create, Fill, Remove)
        props (NameSpace_, Domain, RepositoryUrl) verbs (Consult)
      screen
        who
          Manager
            master
            detail
        what
          row
            markdown <### Repository Registry
                      This is an overview of all known repositories.
                      A repository is a collection of models (apps) that you can install.
                      Add a repository by opening it in a browser and using its 'Add this repository' action,
                      or by adding it here manually.
                      >

        where
          Repositories
            master
              with props (Domain)
            detail

    -- The multirol for repositories. Filled by cm:Repository.
    -- The Manager fills these roles with the public address of a concrete Repository.
    context Repositories (relational) filledBy cm:Repository

    -- A public role so visitors can see and open the repositories.
    public Visitor (default) at Manager >> BespokeDatabaseUrl = me
      perspective on Repositories
        props (NameSpace_, Domain, RepositoryUrl) verbs (Consult)
      perspective on extern
        props (Name) verbs (Consult)

      screen
        who
        what
          row
            markdown <### Repository Registry
                      This is an overview of all known repositories.
                      Visit a repository to browse its models and install apps.
                      >
        where
          Repositories
            master
              with props (Domain)
            detail

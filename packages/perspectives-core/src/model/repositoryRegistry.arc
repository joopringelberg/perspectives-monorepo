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
    external
      
    user Initializer = me
      perspective on Manager
        all roleverbs

    -- The Manager is the one who maintains the list of repositories.
    -- Must be filledBy a role that has access to a BespokeDatabase,
    -- following the pattern of PublicPageCollection$Author.
    user Manager filledBy (sys:TheWorld$PerspectivesUsers + cm:BespokeDatabase$Owner)
      perspective on extern
        defaults
      perspective on Manager
        props (FirstName, LastName) verbs (Consult)
      perspective on Repositories
        all roleverbs
        props (NameSpace_, Domain, RepositoryUrl) verbs (Consult)
      perspective on MyRegistries
        props (Domain) verbs (Consult)
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

    -- The ordinary user can see which repositories are already registered
    -- and can visit the public overview to discover new ones.
    user OrdinaryUser = sys:Me
      perspective on MyRegistries
        props (Domain) verbs (Consult)
      screen
        who
        what
          row
            markdown <### Repository Registry
                      Below you find the repositories that are registered in your installation.
                      A repository is a collection of models (apps) that you can install.
                      Visit the [[link:pub:PLACEHOLDER_URL|public repository overview]] to discover 
                      and add new repositories.
                      >
        where
          MyRegistries
            master
              with props (Domain)
            detail

    -- The multirol for repositories. Filled by cm:Repository.
    -- The Manager fills these roles with the public address of a concrete Repository.
    context Repositories (relational) filledBy cm:Repository

    -- Calculated role: the repositories the user has already registered in their system.
    context MyRegistries = sys:MySystem >> Repositories

    -- A public role so visitors can see and open the repositories.
    public Visitor at Manager >> BespokeDatabaseUrl = sys:Me
      perspective on Repositories
        props (NameSpace_, Domain, RepositoryUrl) verbs (Consult)
      perspective on extern
        defaults

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

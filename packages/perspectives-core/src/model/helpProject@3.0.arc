domain model://perspectives.domains#HelpProject@3.0
  use sys for model://perspectives.domains#System
  use mm for model://perspectives.domains#HelpProject
  use cm for model://perspectives.domains#CouchdbManagement
  use helplib for model://perspectives.domains#HelpLib

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context HelpProjectApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Help Project App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:MyHelpProjects >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:MyHelpProjects >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  ---- The top-level context of the HelpProject model is HelpProjectApp. It is indexed in the system.
  ---- We use it to collect HelpProject instances. Each such project is dedicated to a version of a model.
  -------------------------------------------------------------------------------
  case HelpProjectApp
    indexed mm:MyHelpProjects
    aspect sys:RootContext
    external
    
    user Manager = me
      perspective on AllProjects >> binding >> context >> Author
        only (Create, Fill)
      perspective on AllProjects
        only (CreateAndFill, Remove)
        props (ModelUri) verbs (Consult)

    context AllProjects filledBy HelpProject
      state ProjectExists = exists binding
        on entry
          do for Manager
            bind me to Author in binding >> context


  -------------------------------------------------------------------------------
  ---- HELPPROJECT
  ---- A help project is dedicated to a version of a model.
  -------------------------------------------------------------------------------
  case HelpProject
    external
      property ModelUri = context >> Model >> cm:VersionedModelManifest$External$ModelURI

    user Author filledBy (sys:TheWorld$PerspectivesUsers)
      perspective on ConversationBranches
        only (CreateAndFill, Remove)
        props (State) verbs (SetPropertyValue)
      perspective on ConversationBranches >> binding >> context >> Author
        only (Create, Fill, Remove)
      perspective on Model
        only (Create, Fill, Remove)
      perspective on CoAuthors
        only (Create, Fill, Remove)

    user CoAuthors filledBy (sys:TheWorld$PerspectivesUsers)
      perspective on ConversationBranches
        only (CreateAndFill, Remove)
      perspective on ContextYamls
        props (DocumentName, DocumentKind, ContextType, cm:VersionedModelManifest$ConversationSources$ConversationYaml) verbs (Consult)

    thing ContextYamls = Model >> binding >> context >> cm:VersionedModelManifest$ConversationSources

    context Model filledBy cm:VersionedModelManifest

    context ConversationBranches filledBy ConversationBranch
      state BranchExists = exists binding
        on entry
          do for Author
            bind me to Author in binding >> context
            State = "Draft" for binding
  
  -------------------------------------------------------------------------------
  ---- CONVERSATION BRANCH
  ---- A conversation branch is a variant of a conversation. 
  ---- It is created by a user who is either the Author or the CoAuthors of the HelpProject.
  ---- It is initialized either empty or with a conversation text that is rendered from 
  ---- the yaml document of the context in which the conversation appears.
  ---- The GUI passes on the following identifying information:
  ----   * stableContextType: the (stable) identifier of the ContextType of the context in which the conversation appears.
  ----   * audienceRoleType: the (stable) identifier of the RoleType of the user role for which the conversation is intended.
  ----   * targetRoleType: the (stable) identifier of the RoleType of the user role that is the perspective object of the user role.
  ----   * perspectiveId: a string identifying the perspective.
  -------------------------------------------------------------------------------
  case ConversationBranch
    external
      property ConversationLabel (String)
      -- The surface conversation text.
      property ConversationText (String)
      -- The string value of the ContextType of the context in which the conversation appears.
      -- It will be filled from the GUI the end user applies to edit a single conversation.
      property ContextType (String)
      property AudienceRoleType (String)
      property TargetRoleType (String)
      property PerspectiveId (String)
      property State (String)
       enumeration = ("Draft", "PullRequest", "Merged", "Rejected")
    
      -- As soon as the GUI has filled the identifying information, we can render the conversation text.
      -- This is a once-only operation, because the identifying information is stable and does not change.
      state ReadyToRenderText = (exists ContextType) and (exists AudienceRoleType) and (exists TargetRoleType) and (exists PerspectiveId)
        on entry
          do for Author
            ConversationText = callExternal helplib:ToConversationText( ContextType, AudienceRoleType, TargetRoleType, PerspectiveId) returns String
          
    state Draft = extern >> State == "Draft"
    state PullRequest = extern >> State == "PullRequest"
    state Merged = extern >> State == "Merged"
    state Rejected = extern >> State == "Rejected"

    user Author filledBy (sys:TheWorld$PerspectivesUsers)
      in context state PullRequest
        perspective on extern
          props (ConversationText) verbs (Consult, SetPropertyValue)
      -- Every time the user edits the conversation text, we render it to yaml and merge it into the context.
      action MergeLocally
        letA
          yaml <- callExternal helplib:ToConversationYaml( extern >> ConversationText ) returns String
        in 
          callEffect helplib:MergeConversationYamlLocally( extern >> ContextType, extern >> AudienceRoleType, extern >> TargetRoleType, extern >> PerspectiveId, yaml )

    user CoAuthors filledBy (sys:TheWorld$PerspectivesUsers)
      in context state Draft
        perspective on extern
          props (ConversationText) verbs (Consult, SetPropertyValue)

    thing ContextYamls = extern >> binder ConversationBranches >> context >> ContextYamls

    

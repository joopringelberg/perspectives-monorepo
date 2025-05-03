domain model://joopringelberg.nl#Disconnect
  use sys for model://perspectives.domains#System
  use dc for model://joopringelberg.nl#Disconnect

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context Disconnect
          start <- create role StartContexts in sys:MySystem
        in
          bind_ app >> extern to start
          Name = "Disconnect from peers" for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (dc:DisconnectApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  case Disconnect
    indexed dc:DisconnectApp
    aspect sys:RootContext

    state NoManager = not exists Manager
      on entry 
        do for Initializer
          bind sys:SocialMe >> binding to Manager

    user Initializer = sys:Me
      perspective on Manager
        only (Create, Fill)

    user Manager filledBy sys:TheWorld$PerspectivesUsers
      perspective on DisconnectedPeers
        all roleverbs
        props (Peer, Disconnected) verbs (Consult)
      perspective on DisconnectedPeers >> binding >> context >> Disconnecter
        all roleverbs
      perspective on IncomingDisconnections
        props (Peer) verbs (Consult)
      perspective on Manager
        props (FirstName, LastName) verbs (Consult)
      screen
        who
          markdown <## Roles
                    There is just a single role one can take on in this context: that of the Manager, who 
                    decides which peers to disconnect from. To disconnect a peer means that the peer is
                    no longer informed about changes that might be relevant to him or her.
                    >
          Manager
            master "Master"
              props (FirstName) verbs (Consult)
            detail
              props (FirstName, LastName) verbs (Consult)
        what
            markdown <## Disconnect yourself from a peer
                      It may happen that you do no longer want to exchange information with a peer.
                      You can use this screen to disconnect yourself from someone.
                      This means that information you enter that might be relevant to the peer will not be
                      sent to him or her.
                      >
        where
          DisconnectedPeers
            master "Disconnected peers"
              props (Peer) verbs (Consult)
            detail
              props (Peer, Disconnected) verbs (Consult)

    context DisconnectedPeers (relational) filledBy DisconnectedPeer
      state GiveMeARole = exists binding
        on entry
          do for Manager
            bind currentactor to Disconnecter in binding >> context
    
    -- All DisconnectedPeers that do not fill an instance of DisconnectedPeers in this context.
    context IncomingDisconnections = filter callExternal cdb:RoleInstances( "model://joopringelberg.nl#Disconnect$DisconnectedPeer$External" ) returns dc:DisconnectedPeer$External
      with not (binder DisconnectedPeers >> context >>= first) == dc:DisconnectApp
    -- context IncomingDisconnections = sys:SocialMe >> binder Disconnected >> context >> extern

    context ActualDisconnectedPeers = DisconnectedPeers >> binding

  case DisconnectedPeer
    aspect sys:ContextWithNotification

    external
      property Peer = context >> Disconnected >> LastName
        readableName
      property Disconnected = context >> Disconnected >> Cancelled

    aspect thing sys:Chat

    user Disconnecter filledBy sys:TheWorld$PerspectivesUsers
      aspect sys:ContextWithNotification$NotifiedUser
      state ActionsCanBeTaken = exists binding
        on entry
          do
            create role sys:Chat
          notify "You can now start a chat with the peer."
      perspective on Disconnecter
        props (FirstName, LastName) verbs (Consult)
      perspective on Disconnected
        only (Create, Fill)
        props (FirstName, LastName) verbs (Consult)
        props (Cancelled, Reconnect) verbs (Consult, SetPropertyValue)
      perspective on sys:Chat
        only (Create, RemoveContext, Remove)
        props (Messages, Media) verbs (AddPropertyValue, Consult)
      action Disconnect
        Cancelled = true for Disconnected
      action Reconnect
        Cancelled = false for Disconnected
        Reconnect = true for Disconnected
      action StartChat
        create role sys:Chat
      screen 
        who
          Disconnecter
            master
              props (FirstName) verbs (Consult)
            detail
              props (FirstName, LastName) verbs (Consult)
          Disconnected
            master
              props (FirstName) verbs (Consult)
            detail
              props (FirstName, LastName, Cancelled) verbs (Consult)
        what
        where
    
    user Disconnected filledBy sys:TheWorld$PerspectivesUsers
      property Reconnect (Boolean)

      on entry
        do for Disconnected
          Cancelled = true for context >> Disconnecter
      state Reconnect = Reconnect
        on entry
          do for Disconnected
            Cancelled = false for context >> Disconnecter

      perspective on Disconnecter
        props (FirstName, LastName) verbs (Consult)
        props (Cancelled) verbs (SetPropertyValue)

      perspective on Disconnected
        props (FirstName, LastName, Reconnect) verbs (Consult)

      perspective on sys:Chat
        only (Create, RemoveContext, Remove)
        props (Messages, Media) verbs (AddPropertyValue, Consult)
        
    aspect thing sys:ContextWithNotification$Notifications
import * as React from 'react';
import { Accordion, Col, Container, Navbar, NavDropdown, Offcanvas, Row, Tab, Tabs, DropdownDivider, Modal, OverlayTrigger, Tooltip, Button, Form } from 'react-bootstrap';

// Add axe to the Window interface for TypeScript
declare global {
  interface Window {
    axe?: any;
  }
}
import './styles/www.css';
import './styles/accessibility.css'
import {addRoleToClipboard, externalRoleType, i18next, FileDropZone, sendTransactionToProxy} from 'perspectives-react';
import { ContextInstanceT, ContextType, CONTINUOUS, FIREANDFORGET, PDRproxy, RoleInstanceT, RoleType, ScreenDefinition, SharedWorkerChannelPromise, Unsubscriber, RoleOnClipboard, PropertySerialization, ValueT, Perspective, InspectableContext, InspectableRole, Warning } from 'perspectives-proxy';
import {AppContext, deconstructContext, deconstructLocalName, EndUserNotifier, externalRole, initUserMessaging, ModelDependencies, PerspectivesComponent, PSContext, UserMessagingPromise, UserMessagingMessage, ChoiceMessage, UserChoice, InspectableContextView, InspectableRoleInstanceView} from 'perspectives-react';
import { constructPouchdbUser, getInstallationData } from './installationData';
import { Me } from './me';
import { Apps } from './apps';
import ensureExternalRole from './ensureExternalRole';
import { What } from './what';
import { Who } from './who';
import { Clipboard } from './clipboard';
import { Where } from './where';
import { NotificationsDisplayer } from './notifications';
import Settings from './settingsPanel';
import { subscribeToAllNotifications } from './systemNotifications';
import { syncWithCouchDB } from './syncWIthCouchdb';
import { MyRoleTypes } from './myRoleTypes';
import ConnectedToAMQP from './connectedToAMQP';
import { InternetConnectivityCheck } from './internetConnectivityCheck';
import { changeLanguage } from './i18next';
import { About } from './about';
import FlippingTitle from './flippingTitle';

const mycontextStartPage = __STARTPAGE__ as RoleInstanceT; 

type Section = 'who' | 'what' | 'where' | 'none';

interface WWWComponentProps {
  onMounted : () => void;
}

interface WWWComponentState {
  isSmallScreen: boolean;
  title: string;
  doubleSection: Section;
  whatOnly: boolean;
  showNotifications: boolean;
  leftPanelContent: 'about' | 'me' | 'settings' | 'apps' | 'myroles' | false;
  activeSection: Section;
  systemIdentifier: ContextInstanceT;
  systemUser: RoleInstanceT
  openContext?: RoleInstanceT
  openContextType?: ContextType
  openContextUserType?: RoleType
  screen?: ScreenDefinition;
  endUserMessage: UserMessagingMessage;
  choiceMessage: ChoiceMessage;
  actions?: Record<string, string>;
  roleOnClipboard?: RoleOnClipboard;
  isOnline: boolean;
  // Inspector modal state
  showInspector: boolean;
  inspectorMode?: 'context' | 'role';
  inspectableContext?: InspectableContext;
  inspectableRole?: InspectableRole;
  // No subscription handle needed; we fetch on-demand.
  // Import transaction modal state
  showImportTransaction: boolean;
  invitationData?: { message: string; transaction: any; confirmation: string; };
  importConfirmationCode: string;
  importCodeInvalid: boolean;
}

class WWWComponent extends PerspectivesComponent<WWWComponentProps, WWWComponentState> {
  getScreenUnsubscriber: Unsubscriber | undefined;
  getMeForContextUnsubscriber: Unsubscriber | undefined;

  constructor(props:  WWWComponentProps) {
    super(props);
    const component = this;
    this.state = 
      { isSmallScreen: false
      , title: 'MyContexts'
      , doubleSection: 'what'
      , whatOnly: false
      , showNotifications: false
      , leftPanelContent: false
      , activeSection: 'what' 
      , systemIdentifier: '' as ContextInstanceT
      , systemUser: '' as RoleInstanceT
      , screen: undefined
      , endUserMessage: {title: ''}
      , choiceMessage: {title: '', choices: {}, chosen: () => {}}
      , isOnline: false
      , openContext: mycontextStartPage
      , showInspector: false
      , inspectorMode: undefined
      , inspectableContext: undefined
      , inspectableRole: undefined
      , showImportTransaction: false
      , invitationData: undefined
      , importConfirmationCode: ''
      , importCodeInvalid: false
    };
    this.checkScreenSize = this.checkScreenSize.bind(this);
    this.getScreenUnsubscriber = undefined;
    this.getMeForContextUnsubscriber = undefined;
    initUserMessaging(
      function ( message )
        {
          const p = new Promise(function(resolve)
            { 
              message.acknowledge = resolve
            });
          component.setState( {endUserMessage: message});
          return p.then( function()
          {
            component.setState( { endUserMessage: {title: '', message: undefined}} );
          })
        });
        // Add a way for the proxy to inform the user of warnings that consist of caught errors that did not lead to a crash.
    PDRproxy.then( pproxy => pproxy.setUserMessageChannel( (warnings: Warning[]) => UserMessagingPromise.then( um => 
      warnings.forEach( warning => um.addMessageForEndUser(
        { title: i18next.t("userMessagingPanel_title", { ns: 'mycontexts' })
        , message: i18next.t("userMessagingPanel_message", { ns: 'mycontexts', warning: warning.message }) 
        , error: warning.error }) ) ) ) );
  }

  componentDidMount() {
    const component = this;
    SharedWorkerChannelPromise.then( pdrHandler => {
      getInstallationData().then( installationData => {
        const systemIdentifier = "def:#" + installationData.perspectivesUserId! + installationData.deviceName! as ContextInstanceT;
        pdrHandler
          .runPDR( installationData.perspectivesUserId!, 
            constructPouchdbUser(installationData), 
            { isFirstInstallation: true, 
              useSystemVersion: null,
              myContextsVersion: __MYCONTEXTS_VERSION__
            })
          .then ( () => {
            PDRproxy.then( pproxy => {
              component.props.onMounted()

              // Compute systemUser immediately and store it; do not wait for clipboard subscription.
              // const resolvedSystemUser = (systemIdentifier + "$" + deconstructLocalName( ModelDependencies.sysUser)) as RoleInstanceT
              const resolvedSystemUser = (systemIdentifier + "$User") as RoleInstanceT
              component.setState({ systemIdentifier, systemUser: resolvedSystemUser })

              component.addUnsubscriber(
                pproxy.subscribeSelectedRoleFromClipboard(
                  function (clipBoardContents : RoleOnClipboard[])
                  {
                    const roleOnClipboard = clipBoardContents[0];
                    if (roleOnClipboard && roleOnClipboard !== component.state.roleOnClipboard)
                    {
                      // A new role on the clipboard. Add it to the state.
                      component.setState(
                        { systemIdentifier
                        , roleOnClipboard: roleOnClipboard
                        , systemUser: resolvedSystemUser
                        });
                    }
                    else if (!roleOnClipboard)
                    {
                      // The clipboard is empty. Remove the role from the state.
                      component.setState(
                        { systemIdentifier
                        , systemUser: resolvedSystemUser
                        , roleOnClipboard: undefined
                        });
                    }
                    else {
                      component.setState({ systemUser: resolvedSystemUser });
                    }
                  }
              ))
              // Only sync with CouchDB in development mode
              if (import.meta.env.DEV) {
                console.log('Development mode: Syncing with CouchDB');
                syncWithCouchDB(installationData.perspectivesUserId! + installationData.deviceName! + "_entities")
                  .then(() => syncWithCouchDB(installationData.perspectivesUserId! + installationData.deviceName! + "_models"));
              }
              // Subscribe to all notifications
              component.addUnsubscriber(subscribeToAllNotifications(systemIdentifier));
              // Subscribe to language changes
              component.addUnsubscriber(pproxy.getProperty( externalRole( systemIdentifier ), ModelDependencies.currentLanguage, ModelDependencies.systemExternal, 
                ( languages : ValueT[] ) => {
                  if (languages[0]) {
                    changeLanguage( languages[0] as string )
                  }
                } ) );
              // Make sure the restart property is false.
              pproxy.setProperty( externalRole( systemIdentifier ), ModelDependencies.restart, "false" as ValueT, ModelDependencies.sysUser );
              // Subscribe to the property that signals restart.
              component.addUnsubscriber(pproxy.getProperty( externalRole( systemIdentifier ), ModelDependencies.restart, ModelDependencies.systemExternal, 
                ( restartBools : ValueT[] ) => {
                  if (restartBools[0] == "true") {
                    window.location.reload();
                  }
                } ) );
              // Open the default screen or the one specified in the URL.
              component.prepareMyContextsScreen(resolvedSystemUser);
              })
            }
          )
          .catch( e => UserMessagingPromise.then( um =>
            um.addMessageForEndUser(
              { title: i18next.t("app_startPDR_title", { ns: 'mycontexts' })
              , message: i18next.t("app_startPDR_message", {ns: 'mycontexts'})
              , error: e.toString()
              })));
        })});
    window.addEventListener('resize', this.checkScreenSize);
    this.checkScreenSize()
    
    // HISTORY MANAGEMENT
    window.onpopstate = function(e)
    {
      if (e.state.title)
      {
        document.title = e.state.title;
      }
      if (e.state.selectedContext)
      {
        // console.log("Popping previous state, now on " + (e.state.selectedContext ? "context state " + e.state.selectedContext : "roleform state " + e.state.selectedRoleInstance));
        component.setState(
          { openContext: e.state.selectedContext, activeSection: 'what' } );
        e.stopPropagation();
      }
      else
      {
        // In this situation, the next backwards navigation exits MyContexts.
        // We need a modal dialog that returns a boolean result reflecting the users choice:
        //  true: yes, I want to leave MyContexts;
        //  false: no, I don't want to leave MyContexts.
        // If true, accept navigation.
        // If false, abort navigation.
        component.setState(
          { openContext: mycontextStartPage }
        );
        addEventListener("beforeunload", 
          (e => e.preventDefault()), 
          {capture: true});
      }
    };

    // Add message event listener for ServiceWorker
    if ('serviceWorker' in navigator) {
      navigator.serviceWorker.addEventListener('message', (event) => {
        // console.log('Received message from ServiceWorker:', event.data);
        if (event.data && event.data.type === 'NOTIFICATION_CLICK') {
          const roleId = event.data.roleId;
          if (roleId) {
            // console.log('Opening context from notification click:', roleId);
            component.tryToOpenContext(roleId);
          }
        }
      });      
    }

    // Add event listener for OpenContext event
    document.body.addEventListener(
      'OpenContext', 
      (e : CustomEvent) => {
        e.stopPropagation();
        component.tryToOpenContext(e.detail);  
      }, 
      false);

    document.addEventListener('keydown', this.handleKeyboardNavigation as EventListener);
  }

  componentDidUpdate(prevProps: Readonly<WWWComponentProps>, prevState: Readonly<WWWComponentState>): void {
    const component = this;
    const promises : Promise<any>[] = [];
    if (this.state.openContext !== prevState.openContext) {
      if (this.getMeForContextUnsubscriber) {
        promises.push( PDRproxy.then( pproxy => pproxy.send(component.getMeForContextUnsubscriber!, function(){})));
      }
      if (this.getScreenUnsubscriber) {
        promises.push( PDRproxy.then( pproxy => pproxy.send(component.getScreenUnsubscriber!, function(){})));
      }
      Promise.all( promises )
        .then( () => component.getScreen( component.state.openContext! ))
      this.runAccessibilityScan('main-content');
    }
  }

  componentWillUnmount() {
    window.removeEventListener('resize', this.checkScreenSize);
    document.removeEventListener('keydown', this.handleKeyboardNavigation);
  }

  // Inspector helpers
  openInspectorForContext = (contextId: ContextInstanceT) => {
    const component = this;
    PDRproxy.then(pproxy =>
      pproxy.getInspectableContext(contextId).then(inspectableContext => component.setState({ inspectableContext: inspectableContext, showInspector: true, inspectorMode: 'context', inspectableRole: undefined }))
    );
  };

  openInspectorForRole = (roleId: RoleInstanceT) => {
    const component = this;
    PDRproxy.then(pproxy =>
      pproxy.getInspectableRole(roleId).then(inspectableRole => component.setState({ inspectableRole: inspectableRole, showInspector: true, inspectorMode: 'role', inspectableContext: undefined }))
    );
  };

  closeInspector = () => {
    this.setState({ showInspector: false, inspectorMode: undefined, inspectableContext: undefined, inspectableRole: undefined });
  };

  tryToOpenContext( s : string)
  {
    const component = this;
    ensureExternalRole( s )
      .then(
        function(result)
        {
          const erole = result.value as RoleInstanceT;
          switch (result.tag)
          {
            case "RoleInstance":
              if (component.state.openContext !== erole)
              {
                PDRproxy.then( function( pproxy )
                  {
                    pproxy.getRoleName( erole, function (nameArr)
                      {
                        document.title = nameArr[0];
                        history.pushState({ selectedContext: erole, title: nameArr[0] }, "");
                      },  
                      FIREANDFORGET);
                  });
                // console.log("Pushing context state " + e.detail);
                component.setState(
                  { openContext: erole, leftPanelContent: false, activeSection: 'what' } );
              }
              break;
            case "Choices":
              component.setState( 
                {choiceMessage:
                  { title: ""
                  , message: i18next.t("app_opencontext_title", { ns: 'mycontexts' })
                  , choices: result.value
                  , chosen: (choice) => component.setState({openContext: choice as RoleInstanceT, activeSection: 'what'})
                  }} );
            break;
          }})
      .catch(err => UserMessagingPromise.then( um => 
        um.addMessageForEndUser(
          { title: i18next.t("app_opencontext_title", { ns: 'mycontexts' }) 
          , message: i18next.t("app_opencontext_message", {context: s, ns: 'mycontexts'})
          , error: err.toString()
        })));
  }

  checkScreenSize(){
    function computeDoubleSection( )
    {
      if (window.innerWidth < 768)
      {
        return "none";
      }
      else if (component.state.isSmallScreen)
      {
        return "what";
      }
      else
      {
        return component.state.doubleSection;
      }
    }
    const component = this;
    const topNavbar = document.querySelector('#top-navbar');
    const bottomNavbar = document.querySelector('#bottom-navbar');
    const mobileTabs = document.querySelector('#mobile-tabs');
    const whoHeader = document.querySelector('#whoHeader');
    // Includes the padding of the navbar.
    const topNavbarHeight = topNavbar ? (topNavbar as HTMLElement).offsetHeight : 40;
    const bottomNavbarHeight = bottomNavbar ? (bottomNavbar as HTMLElement).offsetHeight : 40;
    const mobileTabsHeight = mobileTabs ? (mobileTabs as HTMLElement).offsetHeight : 48;
    const bodyHeight = window.innerHeight;
    const mobileContentHeight = bodyHeight - (mobileTabsHeight + topNavbarHeight + bottomNavbarHeight) - 4;
    
    // Set the CSS variable for the navbar height. This is incorporated in the CSS style full-height.
    document.documentElement.style.setProperty('--top-navbar-height', `${topNavbarHeight}px`);
    document.documentElement.style.setProperty('--bottom-navbar-height', `${bottomNavbarHeight}px`);
    document.documentElement.style.setProperty('--tabs-height', `${mobileTabsHeight}px`); 
    document.documentElement.style.setProperty('--mobile-content-height', `${mobileContentHeight}px`); 
    document.documentElement.style.setProperty('--who-header-height', `${whoHeader ? (whoHeader as HTMLElement).offsetHeight : 0}px`);  
    this.setState(
      { isSmallScreen: window.innerWidth < 768, doubleSection: computeDoubleSection() } );
  }

  prepareMyContextsScreen( systemUserOverride?: RoleInstanceT )
  {
    const effectiveSystemUser = systemUserOverride ?? this.state.systemUser;
     // Returns the roleId whose lastShownOnScreen is the most recent.
     // If none have that property, returns undefined.
     function mostRecentRoleId(
       perspective: Perspective,
       sortProp: string
     ): RoleInstanceT | undefined {
       let best: RoleInstanceT | undefined = undefined;
       let max = -Infinity;
     
       for (const { roleId, propertyValues } of Object.values(perspective.roleInstances)) {
         const v = propertyValues[sortProp]?.values?.[0];
         const n = v != null ? Number((v as unknown) as string) : NaN;
         if (!Number.isNaN(n) && n > max) {
           max = n;
           best = roleId;
         }
       }
       return best;
     }
     
     const component = this;
     const params = new URLSearchParams(document.location.search.substring(1));
     
     if (params.get("opencontext"))
     {
       this.tryToOpenContext( decodeURIComponent( params.get("opencontext")!) );
     }
     else {
       PDRproxy.then((PDRproxy) => {
         this.addUnsubscriber(
           PDRproxy.getPerspectiveForUser(
             effectiveSystemUser,
             ModelDependencies.recentContexts,
             ModelDependencies.WWWUser,
             (perspectives: Perspective[]) => {
               const p = perspectives[0];
               const mostRecent = mostRecentRoleId(p, ModelDependencies.lastShownOnScreen);
                 if (mostRecent === undefined) {
                   component.openWelcomePage();
                 } else {
                   PDRproxy.getBinding( mostRecent, bindings => {
                     if (bindings.length > 0) {
                       component.tryToOpenContext(bindings[0]);
                     }
                   });
                 }
               },
             FIREANDFORGET
             )
         );
       });
   
     }
   }

  openWelcomePage()
  {
    document.title = "Welcome to MyContexts";
    history.pushState({ selectedContext: mycontextStartPage, title: "Welcome to MyContexts" }, "");
    this.getScreen(mycontextStartPage)
  }

  getScreen (externalRole : RoleInstanceT)
  {
    const component = this;
    const context = deconstructContext(externalRole) as ContextInstanceT
    PDRproxy
      .then( pproxy => pproxy.getContextType( context ) )
      .then( contextType => 
        {
          PDRproxy.then( pproxy => pproxy.getMeForContext(
            externalRole,
            // userRoles includes roles from aspects.
            function(userRoles)
            {
              // It may happen that there are no user role types.
              if ( userRoles.length == 0)
              {
                return UserMessagingPromise.then( um => 
                  um.addMessageForEndUser(
                    { title: i18next.t("screen_no_usertype_for_context_title", { ns: 'preact' }) 
                    , message: i18next.t("screen_no_usertype_for_context_message", {ns: 'preact'})
                    , error: "No result from GetMeFromContext"
                    }));
              }
              else
              {
                if (component.getScreenUnsubscriber) 
                {
                  pproxy.send( component.getScreenUnsubscriber, function(){})
                    .then( () => component.fetchScreen(contextType, userRoles[0], context));
                }
                else
                {
                  component.fetchScreen(contextType, userRoles[0], context);
                }
              }
            },
            CONTINUOUS))
            .then( unsubscriber => component.getMeForContextUnsubscriber = unsubscriber )
          })
      .catch(e => UserMessagingPromise.then( um => 
        {
          um.addMessageForEndUser(
            { title: i18next.t("screen_computestate_title", { ns: 'preact' }) 
            , message: i18next.t("screen_computestate_message", {ns: 'preact'})
            , error: e.toString()
          });
          return false;})) as Promise<boolean>;
  }

  // Ends a previous screen subscription for the current user and establishes a new one.
  fetchScreen (contextType : ContextType, userRoleType: RoleType, context: ContextInstanceT )
  {
    const component = this;
    PDRproxy.then(function(pproxy)
      {
        pproxy.getScreen(
          userRoleType
          , context
          , contextType
          , function( screens : ScreenDefinition[] ) 
            {
              pproxy.getContextActions( userRoleType, context)
                .then( actions => 
                  component.setState(
                    { screen: screens[0]
                    , openContextType: contextType
                    , openContextUserType: userRoleType
                    , actions
                    , title: screens[0].title ? screens[0].title : "MyContexts"}))
                .catch(e => UserMessagingPromise.then( um => 
                  um.addMessageForEndUser(
                    { title: i18next.t("app_contextactions_title", { ns: 'mycontexts' }) 
                    , message: i18next.t("app_contextactions_message", {context, ns: 'mycontexts'})
                    , error: e.toString()
                  })));
            }
          , CONTINUOUS
          ,function()
          {
            component.setState({screen: undefined});
          }
        ).then( function(unsubscriber)
          {
            component.getScreenUnsubscriber = unsubscriber;
          });
      });
  }

  runAction( actionName : string )
  {
    const component = this;
    PDRproxy.then(
      function (pproxy)
      {
          pproxy.contextAction(
            deconstructContext( component.state.openContext! ) as ContextInstanceT
            , component.state.openContextUserType!  // authoringRole
            , actionName)
          .catch(e => UserMessagingPromise.then( um => 
            um.addMessageForEndUser(
              { title: i18next.t("action_title", { ns: 'preact' }) 
              , message: i18next.t("action_message", {ns: 'preact', action: component.state.actions![actionName]})
              , error: e.toString()
              })));  
        });
  }

  notificationsAndClipboard() {
    const component = this;
    const panelTitle = i18next.t("notificationsAndClipboard_title", {ns: 'mycontexts'});
    
    return (
      <Offcanvas 
        show={this.state.showNotifications} 
        onHide={() => {
          component.setState({showNotifications: false});
          // Return focus to the element that opened the panel
          document.querySelector('.bi-arrow-up')?.parentElement?.focus();
        }} 
        placement='bottom' 
        scroll={true} 
        style={{ height: '50vh' }}
        id="notifications-offcanvas"
        onEntered={() => component.runAccessibilityScan('notifications-offcanvas')}
        aria-labelledby="notifications-title"
      >
        <Offcanvas.Header closeButton>
          <Offcanvas.Title id="notifications-title">{panelTitle}</Offcanvas.Title>
        </Offcanvas.Header>
        <Offcanvas.Body id="notifications-content">
          <Accordion defaultActiveKey="0">
            <Accordion.Item eventKey="0">
              <Accordion.Header>{ i18next.t("clipboard", {ns: 'mycontexts'})}</Accordion.Header>
              <Accordion.Body>
                <Clipboard systemuser={component.state.systemUser}/>
              </Accordion.Body>
            </Accordion.Item>
            <Accordion.Item eventKey="1">
              <Accordion.Header>{ i18next.t("settings_notifications", {ns: 'mycontexts'})}</Accordion.Header>
              <Accordion.Body>
              { component.state.openContext ?
                <NotificationsDisplayer 
                externalroleid={component.state.openContext}
                shownotifications={true}
                navigateto={(state) => component.setState({openContext: state, activeSection: 'what'})}
                />
                :
                null
              }
              </Accordion.Body>
            </Accordion.Item>
            <Accordion.Item eventKey="2">
              <Accordion.Header>{ i18next.t("allNotifications", {ns: 'mycontexts'})}</Accordion.Header>
              <Accordion.Body>
              { component.state.openContext ?
                <NotificationsDisplayer 
                externalroleid={ externalRole( component.state.systemIdentifier )}
                shownotifications={true}
                showAllNavigations={true}
                navigateto={(state) => component.setState({openContext: state, activeSection: 'what'})}
                />
                :
                null
              }
              </Accordion.Body>
            </Accordion.Item>
          </Accordion>
        </Offcanvas.Body>
      </Offcanvas>
    );
  }

  leftPanel() {
    const component = this;
    let content;
    const panelTitle = this.state.leftPanelContent ? i18next.t('leftPanel_' + this.state.leftPanelContent, {ns: 'mycontexts'}) : "";
    
    switch (this.state.leftPanelContent) {
      case 'about':
        content = <About />
        break;
      case 'me':
        content = <Me systemuser={component.state.systemUser}/>;
        break;
      case 'apps':
        content = <Apps systemuser={component.state.systemUser}/>;
        break;
      case 'settings':
        content = <Settings/>;
        break;
      case 'myroles':
        content = <MyRoleTypes externalRoleId={component.state.openContext!} title={component.state.title} currentroletype={component.state.openContextUserType!}/>;
        break;
      default:
        return null;
    }
    
    return (
      <Offcanvas 
        show={this.state.leftPanelContent} 
        onHide={() => component.setState({leftPanelContent:false})} 
        placement='start' 
        scroll={true} 
        style={{ height: '100vh' }}
        onEntered={() => component.runAccessibilityScan('left-panel-offcanvas')}
        id="left-panel-offcanvas"
        aria-labelledby="left-panel-title"
        >
        <Offcanvas.Header closeButton>
          <Offcanvas.Title id="left-panel-title">{panelTitle}</Offcanvas.Title>
        </Offcanvas.Header>
        <Offcanvas.Body id="left-panel-content">
          { content }
        </Offcanvas.Body>
    </Offcanvas>
    );
  }

  renderMobile ()
  {
    const component = this;
    return (
      <PSContext.Provider value={
        {contextinstance: deconstructContext( this.state.openContext!) as ContextInstanceT
        , contexttype: this.state.openContextType!
        , myroletype: this.state.openContextUserType!}}>
        <Container fluid className='px-0'>
          {component.renderTopNavBar()}
          <main id="main-content">
            <Tabs
              id="mobile-tabs"
              activeKey={this.state.activeSection}
              onSelect={(k: string | null) => {
                component.setState({ 'activeSection': k as Section });
                }}
              fill
              role="navigation"
              aria-label="Section navigation"
              >
              <Tab eventKey="who" title={ i18next.t("www_who", {ns: 'mycontexts'}) } className='bg-primary full-mobile-height px-2 scrollable-content' style={{'--bs-bg-opacity': '.2'} as React.CSSProperties}>
                { this.state.screen?.whoWhatWhereScreen ?
                  <Who 
                    screenelements={ this.state.screen.whoWhatWhereScreen.who } 
                    showTablesAndForm={!this.state.isSmallScreen || this.state.doubleSection == "who"}
                  />
                  : 
                  <p className='bg-light-subtle'>Ga ergens heen</p>
                }
              </Tab>
              <Tab eventKey="what" title={ i18next.t("www_what", {ns: 'mycontexts'}) } className='bg-primary full-mobile-height px-2 scrollable-content' style={{'--bs-bg-opacity': '.4'} as React.CSSProperties}>
                { this.state.screen?.whoWhatWhereScreen ? 
                    <What screenelements={  this.state.screen.whoWhatWhereScreen.what } showTablesAndForm={!this.state.isSmallScreen || this.state.doubleSection == "what"}/>
                    : 
                  <div>Ga ergens heen.</div>
                }
              </Tab>
              <Tab eventKey="where" title={ i18next.t("www_where", {ns: 'mycontexts'}) } className='bg-primary full-mobile-height px-2 scrollable-content' style={{'--bs-bg-opacity': '.6'} as React.CSSProperties}>
              { this.state.screen?.whoWhatWhereScreen ? 
                <Where 
                  screenelements={ this.state.screen.whoWhatWhereScreen.whereto } 
                  showTablesAndForm={!this.state.isSmallScreen || this.state.doubleSection == "where"} 
                  systemUser={component.state.systemUser}
                  systemIdentifier={component.state.systemIdentifier}
                  openContext={component.state.openContext}
                  />
                : 
                  <div>Ga ergens heen.</div>
                }
              </Tab>
            </Tabs>
          </main>
          { component.renderBottomNavBar() }
        <EndUserNotifier message={component.state.endUserMessage}/>
        { component.renderInspector() }
        { component.renderImportTransactionModal() }
        <UserChoice message={component.state.choiceMessage}/>
      </Container>
      </PSContext.Provider>
    );
  }

  renderTopNavBar() {
    const component = this;
    return (<header> {/* Add header landmark */}
      <Navbar bg="primary" expand="xs" className="py-0 ps-2" id="top-navbar" role="navigation" aria-label="Main navigation">
      <NavDropdown 
        title={
          <>
            <i className="bi bi-list text-light fs-2" aria-hidden="true"></i>
            <span className="visually-hidden">{i18next.t("mainMenu", {ns: "mycontexts"})}</span>
          </>
        } 
        className="hide-caret px-2 py-1" 
        id="nav-dropdown"
        aria-label="Main menu navigation"
      >
        <NavDropdown.Item onClick={() => component.setState({leftPanelContent: 'about'})}>About...</NavDropdown.Item>
        <NavDropdown.Item onClick={() => component.setState({leftPanelContent: 'me'})}>Me</NavDropdown.Item>
        <NavDropdown.Item onClick={() => component.setState({leftPanelContent: 'apps'})}>Apps</NavDropdown.Item>
        <NavDropdown.Item onClick={() => component.setState({leftPanelContent: 'settings'})}>Settings</NavDropdown.Item>
        { component.state.openContext ? <NavDropdown.Item onClick={ () => component.setState( {leftPanelContent: 'myroles'} ) }>{ i18next.t("www_myroles", {ns: 'mycontexts'}) }</NavDropdown.Item> : null }
        <DropdownDivider />
        <NavDropdown.Item onClick={() => component.setState({showImportTransaction: true})}>{ i18next.t("www_importTransaction", {ns: 'mycontexts'}) }</NavDropdown.Item>
        { component.state.actions && Object.keys( component.state.actions ).length > 0 ?
          <>
          <DropdownDivider />
          { Object.keys( component.state.actions ).map( action => <NavDropdown.Item key={action} onClick={() => component.runAction(action)}>{ component.state.actions![action]}</NavDropdown.Item>) }
          </>
          : null }
        { component.state.openContext ? <NavDropdown.Item onClick={ () => component.pinContext( component.state.openContext! ) }>{ i18next.t("www_pincontext", {ns: 'mycontexts'}) }</NavDropdown.Item> : null }
        { component.state.openContext ? <NavDropdown.Item onClick={ () => component.copyContext( component.state.openContext! ) }>{ i18next.t("www_copycontext", {ns: 'mycontexts'}) }</NavDropdown.Item> : null }
      </NavDropdown>
      <Navbar.Brand href="#home" className='me-auto text-light navbar-title'>
        {/* TODO If the available width is large enough, display both context- and role name. */}
        <FlippingTitle
          title={this.state.title} 
          userRoleType={this.state.screen ? this.state.screen.userRole : (this.state.openContextUserType ? deconstructLocalName(this.state.openContextUserType) : '')} 
        />

      </Navbar.Brand>
      <InternetConnectivityCheck reportBack={ (isOnline : boolean) => component.state.isOnline !== isOnline ? component.setState({isOnline}) : null}/>
      {component.state.systemIdentifier ? <ConnectedToAMQP roleinstance={ externalRole( component.state.systemIdentifier )} isOnline={component.state.isOnline} /> : null}
    </Navbar>
    </header>);
  }

  renderInspector() {
    const component = this;
    const { showInspector, inspectorMode, inspectableContext, inspectableRole } = this.state;
    const headerTitle = inspectorMode === 'context' && inspectableContext
      ? { mainLabel: `Context: ${inspectableContext.title}`, idSuffix: `(${inspectableContext.id})`, tooltip: inspectableContext.ctype }
      : inspectorMode === 'role' && inspectableRole
      ? { mainLabel: `Role: ${inspectableRole.title}`, idSuffix: `(${inspectableRole._id})`, tooltip: inspectableRole.rtype }
      : { mainLabel: i18next.t("inspector_title", { ns: 'mycontexts', defaultValue: 'Inspector' }), idSuffix: undefined, tooltip: undefined };

    return (
      <Modal show={showInspector} onHide={this.closeInspector} size="lg" centered>
        <Modal.Header closeButton>
          <Modal.Title>
            {headerTitle.tooltip ? (
              <OverlayTrigger placement="right" overlay={<Tooltip>{headerTitle.tooltip}</Tooltip>}>
                <span>
                  <span>{headerTitle.mainLabel}</span>
                  {headerTitle.idSuffix ? (
                    <span style={{ fontSize: '16px', marginLeft: '0.5rem' }}>{headerTitle.idSuffix}</span>
                  ) : null}
                </span>
              </OverlayTrigger>
            ) : (
              <span>
                <span>{headerTitle.mainLabel}</span>
                {headerTitle.idSuffix ? (
                  <span style={{ fontSize: '16px', marginLeft: '0.5rem' }}>{headerTitle.idSuffix}</span>
                ) : null}
              </span>
            )}
          </Modal.Title>
        </Modal.Header>
        <Modal.Body>
          {inspectorMode === 'context' ? (
            inspectableContext ? (
              <InspectableContextView data={inspectableContext} showRole={component.openInspectorForRole} />
            ) : (
              <div>Loading…</div>
            )
          ) : inspectorMode === 'role' ? (
            inspectableRole ? (
              <InspectableRoleInstanceView
                data={inspectableRole}
                showRole={component.openInspectorForRole}
                showContext={component.openInspectorForContext}
              />
            ) : (
              <div>Loading…</div>
            )
          ) : null}
        </Modal.Body>
      </Modal>
    );
  }

  closeImportModal() {
    this.setState({
      showImportTransaction: false,
      invitationData: undefined,
      importConfirmationCode: '',
      importCodeInvalid: false,
    });
  }

  handleImportFile(file: File) {
    const component = this;
    if (file.type !== "application/json") {
      UserMessagingPromise.then( um => um.addMessageForEndUser(
        { title: i18next.t("www_importTransaction", {ns: 'mycontexts'})
        , message: i18next.t("www_importTransaction_notJson", {ns: 'mycontexts'})
        , error: undefined
        }));
      return;
    }
    file.text().then(text => {
      let json: any;
      try {
        json = JSON.parse(text);
      } catch (e) {
        UserMessagingPromise.then( um => um.addMessageForEndUser(
          { title: i18next.t("www_importTransaction", {ns: 'mycontexts'})
          , message: i18next.t("www_importTransaction_invalidJson", {ns: 'mycontexts'})
          , error: (e as Error).toString()
          }));
        return;
      }
      if (json.message !== undefined && json.transaction !== undefined && json.confirmation !== undefined) {
        // It's an invitation: parse the nested transaction string.
        let transaction: any;
        try {
          transaction = typeof json.transaction === 'string' ? JSON.parse(json.transaction) : json.transaction;
        } catch (e) {
          UserMessagingPromise.then( um => um.addMessageForEndUser(
            { title: i18next.t("www_importTransaction", {ns: 'mycontexts'})
            , message: i18next.t("www_importTransaction_invalidTransaction", {ns: 'mycontexts'})
            , error: (e as Error).toString()
            }));
          return;
        }
        component.setState({
          invitationData: { message: json.message, transaction, confirmation: json.confirmation },
          importConfirmationCode: '',
          importCodeInvalid: false,
        });
      } else if (json.timeStamp !== undefined && json.deltas !== undefined) {
        // It's a raw transaction; import directly.
        sendTransactionToProxy(json);
        component.closeImportModal();
      } else {
        UserMessagingPromise.then( um => um.addMessageForEndUser(
          { title: i18next.t("www_importTransaction", {ns: 'mycontexts'})
          , message: i18next.t("www_importTransaction_notValid", {ns: 'mycontexts'})
          , error: undefined
          }));
      }
    });
  }

  renderImportTransactionModal() {
    const component = this;
    const { invitationData, importConfirmationCode, importCodeInvalid } = component.state;
    return (
      <Modal show={component.state.showImportTransaction} onHide={() => component.closeImportModal()} centered>
        <Modal.Header closeButton>
          <Modal.Title>{ i18next.t("www_importTransaction", {ns: 'mycontexts'}) }</Modal.Title>
        </Modal.Header>
        <Modal.Body>
          { !invitationData ?
            <FileDropZone
              handlefile={(file: File) => component.handleImportFile(file)}
              extension=".json"
              collapsenavbar={() => component.closeImportModal()}
            >
              <div className="text-center p-4 border border-secondary rounded">
                <i className="bi bi-cloud-upload fs-1" aria-hidden="true"></i>
                <p>{ i18next.t("www_importTransaction_dropHere", {ns: 'mycontexts'}) }</p>
              </div>
            </FileDropZone>
            :
            <div>
              <p>{invitationData.message}</p>
              <Form.Group>
                <Form.Label>{ i18next.t("www_importTransaction_confirmationCode", {ns: 'mycontexts'}) }</Form.Label>
                <Form.Control
                  type="text"
                  value={importConfirmationCode}
                  onChange={(e) => component.setState({importConfirmationCode: e.target.value, importCodeInvalid: false})}
                  isInvalid={importCodeInvalid}
                />
                <Form.Control.Feedback type="invalid">
                  { i18next.t("www_importTransaction_invalidCode", {ns: 'mycontexts'}) }
                </Form.Control.Feedback>
              </Form.Group>
            </div>
          }
        </Modal.Body>
        { invitationData ?
          <Modal.Footer>
            <Button variant="secondary" onClick={() => component.closeImportModal()}>
              { i18next.t("genericClose", {ns: 'mycontexts'}) }
            </Button>
            <Button variant="primary" onClick={() => {
              if (importConfirmationCode === invitationData.confirmation) {
                sendTransactionToProxy(invitationData.transaction);
                component.closeImportModal();
              } else {
                component.setState({importCodeInvalid: true});
              }
            }}>
              { i18next.t("www_importTransaction_submit", {ns: 'mycontexts'}) }
            </Button>
          </Modal.Footer>
          : null
        }
      </Modal>
    );
  }

  // For the bottom navigation
  renderBottomNavBar() {
    const component = this;

    return (
      <nav aria-label="History navigation"> {/* Add nav landmark */}
        <Navbar id="bottom-navbar" fixed="bottom" bg="primary" expand="xs" className="justify-content-between py-0 px-3">
          <Navbar.Brand 
            onClick={e => {window.history.back(); e.stopPropagation();}} 
            onKeyDown={(e) => {if (e.key === ' ' || e.key === 'Enter') window.history.back(); e.stopPropagation();}} 
            role="button" 
            tabIndex={0} 
            aria-label={i18next.t("bottom_goBack", {ns: 'mycontexts'})}>
              <i className="bi bi-arrow-left text-light"></i>
          </Navbar.Brand>
          <Navbar.Brand 
            onClick={e => { component.setState({ showNotifications: true }); e.stopPropagation(); }} 
            onKeyDown={e => { if (e.key === ' ' || e.key === 'Enter') { component.setState({ showNotifications: true }); e.stopPropagation(); }}} 
            role="button" 
            tabIndex={0} 
            aria-label={i18next.t("bottom_showNotifications", {ns: 'mycontexts'})}>
              <i className="bi bi-arrow-up text-light"></i>
          </Navbar.Brand>
          <Navbar.Brand 
            onClick={e => {window.history.forward(); e.stopPropagation();}} 
            onKeyDown={(e) => {if (e.key === ' ' || e.key === 'Enter') window.history.forward(); e.stopPropagation();}} 
            role="button" 
            tabIndex={0} 
            aria-label={i18next.t("bottom_goForward", {ns: 'mycontexts'})}>
              <i className="bi bi-arrow-right text-light"></i>
          </Navbar.Brand>
        </Navbar>
      </nav>
      );
  }

  pinContext( externalRole : RoleInstanceT )
  {
    const component = this;
    PDRproxy.then( pproxy => pproxy.bind(
        component.state.systemIdentifier,
        ModelDependencies.pinnedContexts,
        ModelDependencies.system,
        { id: undefined
        , properties: {} as PropertySerialization
        , binding: externalRole
        },
        component.state.openContextUserType!
        ).then( function() {
          UserMessagingPromise.then( um => 
            um.addMessageForEndUser(
              { title: i18next.t("www_pincontext_title", {ns: 'mycontexts'}) 
              , message: i18next.t("www_pincontext_message", {ns: 'mycontexts'})
              , error: undefined
            }));
    }));
  }

  copyContext( eRole : RoleInstanceT )
  {
    navigator.clipboard.writeText(eRole);
    addRoleToClipboard(eRole,
      {
        roleData: {
          rolinstance: eRole,
          cardTitle: this.state.title || "No title",
          roleType: externalRoleType( this.state.openContextType! ),
          contextType: this.state.openContextType!,
        },
        addedBehaviour: ["fillARole"],
        myroletype: this.state.openContextUserType!,
      },
      externalRole( this.state.systemIdentifier ),
      this.state.openContextUserType!
      ).catch((e) =>
            UserMessagingPromise.then((um) =>
              um.addMessageForEndUser({
                title: i18next.t("clipboardSet_title", { ns: "preact" }),
                message: i18next.t("clipboardSet_message", { ns: "preact" }),
                error: e.toString(),
              })
            )
          );
  }

  renderDesktop() {
    const component = this;
    return (
      <PSContext.Provider value={
        { contextinstance: deconstructContext( this.state.openContext!) as ContextInstanceT
        , contexttype: this.state.openContextType!
        , myroletype: this.state.openContextUserType!}}>
        <Container fluid className='px-0'>
          {component.renderTopNavBar()}
          <main id="main-content">
            <div tabIndex={0} className="content-section-area">
              <Row className='mx-0 px-0'>
                <Col 
                  className='bg-primary full-height animated-column' 
                  xs={ this.state.whatOnly ? 1 : this.state.doubleSection === "who" ? 6 : 3 } 
                  style={{'--bs-bg-opacity': '.1'} as React.CSSProperties}>
                  <Row id="whoHeader" onClick={() => component.setState( {'doubleSection': "who"} )}>
                    <h2 className='text-center text-dark column-heading' aria-keyshortcuts="alt+1" tabIndex={0}>{ i18next.t("www_who", {ns: 'mycontexts'}) }</h2>
                  </Row>
                  <Row className='px-1 full-www-content-height scrollable-content px-0'>
                    { this.state.screen?.whoWhatWhereScreen ?
                      <Who screenelements={ this.state.screen.whoWhatWhereScreen.who } 
                        showTablesAndForm={this.state.isSmallScreen || this.state.doubleSection == "who"}
                      />
                      :
                      <p className='bg-light-subtle'>Ga ergens heen</p>
                    }
                  </Row>
                </Col>
                <Col 
                  className='bg-primary animated-column' 
                  xs={ this.state.whatOnly ? 10 : this.state.doubleSection === "what" ? 6 : 3} 
                  style={{'--bs-bg-opacity': '.2'} as React.CSSProperties}>
                  <Row onClick={() => component.setState( {'doubleSection': "what"} )}
                    onDoubleClick={() => component.setState( {'whatOnly': !component.state.whatOnly} )}
                  >
                    <h2 className='text-center column-heading' aria-keyshortcuts="alt+2" tabIndex={0}>{ i18next.t("www_what", {ns: 'mycontexts'}) }</h2>
                  </Row>
                  {/* In the desktop, MSComponent will render a row with px-1 */}
                  {/* Here we render either an arbitrary screen: {tag: "FreeFormScreen", elements: MainScreenElements}, or all TableFormDef elements in the {tag: "TableForms", elements: TableFormDef[]} variant of What. */}
                  <Row className="full-www-content-height scrollable-content px-0">
                  {this.state.screen?.whoWhatWhereScreen ? 
                        <What screenelements={  this.state.screen.whoWhatWhereScreen.what } showTablesAndForm={this.state.isSmallScreen || this.state.doubleSection == "what"}/>
                        : 
                      <div>Ga ergens heen.</div>
                      }
                  </Row>
                </Col>
                <Col 
                  className='bg-primary full-height animated-column'
                  xs={ this.state.whatOnly ? 1 : this.state.doubleSection === "where" ? 6 : 3} 
                  style={{'--bs-bg-opacity': '.3'} as React.CSSProperties}>
                  <Row onClick={() => component.setState( {'doubleSection': "where"} )}>
                    <h2 className='text-center column-heading' aria-keyshortcuts="alt+3" tabIndex={0}>{ i18next.t("www_where", {ns: 'mycontexts'}) }</h2>
                  </Row>  
                  <Row className="px-1 full-www-content-height scrollable-content px-0" style={{overflow: 'auto'}}>
                  { this.state.screen?.whoWhatWhereScreen ? 
                    <Where 
                      screenelements={  this.state.screen.whoWhatWhereScreen.whereto } 
                      showTablesAndForm={this.state.isSmallScreen || this.state.doubleSection == "where"} 
                      systemUser={component.state.systemUser}
                      systemIdentifier={component.state.systemIdentifier}
                      openContext={component.state.openContext}
                    />
                    : 
                      <div>Ga ergens heen.</div>
                    }
                  </Row>
                </Col>
              </Row>
            </div>
          </main>
          { component.renderBottomNavBar() }
          <EndUserNotifier message={component.state.endUserMessage}/>
          { component.renderInspector() }
          { component.renderImportTransactionModal() }
          <UserChoice message={component.state.choiceMessage}/>
        </Container>
      </PSContext.Provider>);
  }

  // Add to WWWComponent class
  handleKeyboardNavigation = (e: KeyboardEvent) => {    
    // Open inspector with Ctrl+I
    if (e.ctrlKey && (e.key === 'i' || e.key === 'I')) {
      if (this.state.openContext) {
        const contextId = deconstructContext(this.state.openContext) as ContextInstanceT;
        this.openInspectorForContext(contextId);
        e.preventDefault();
        return;
      }
    }
    if (e.altKey && e.key === '¡') {
      // Focus Who section
      this.setState({'doubleSection': "who"});
      e.preventDefault();
    } else if (e.altKey && e.key === '€') {
      // Focus What section
      this.setState({'doubleSection': "what"});
      e.preventDefault();
    } else if (e.altKey && e.key === '£') {
      // Focus Where section
      this.setState({'doubleSection': "where"});
      e.preventDefault();
    }
  };

  runAccessibilityScan(elementId: string = 'main-content') {
    // Only run in development
    if (import.meta.env.DEV) {
      // Wait for content to fully render
      setTimeout(() => {
        // Check if axe is available
        if (window.axe && window.axe.run) {
          console.log('Running accessibility scan for element:', elementId);
          window.axe.run(document.getElementById(elementId) || document.body)
            .then((results: any) => {
              if (results.violations.length > 0) {
                console.group('Accessibility issues found:');
                results.violations.forEach((violation: any) => {
                  console.log(
                    `%c${violation.impact} impact: ${violation.help}`,
                    'font-weight: bold; color: ' + (violation.impact === 'serious' ? '#d93025' : '#e37400')
                  );
                  console.log(`More info: ${violation.helpUrl}`);
                  
                  // Process each node with the violation
                  violation.nodes.forEach((node: any, index: number) => {
                    // Get the actual DOM elements
                    const elements = node.target.map((selector: string) => {
                      try {
                        return document.querySelector(selector);
                      } catch (e) {
                        return null;
                      }
                    }).filter(Boolean);
                    
                    // Log with interactive reference
                    if (elements.length) {
                      console.groupCollapsed(`Element ${index + 1}: ${node.target.join(', ')}`);
                      console.log('HTML:', elements[0].outerHTML.slice(0, 150) + '...');
                      console.log('Suggested fix:', violation.description);
                      console.log('Element reference:', elements[0]);
                      console.groupEnd();
                      
                      // Temporarily highlight the element on the page
                      const originalOutline = elements[0].style.outline;
                      const originalPosition = elements[0].style.position;
                      const originalZIndex = elements[0].style.zIndex;
                      
                      elements[0].style.outline = '3px solid red';
                      elements[0].style.position = 'relative';
                      elements[0].style.zIndex = '10000';
                      
                      // Reset after 3 seconds
                      setTimeout(() => {
                        elements[0].style.outline = originalOutline;
                        elements[0].style.position = originalPosition;
                        elements[0].style.zIndex = originalZIndex;
                      }, 3000);
                    }
                  });
                });
                console.groupEnd();
              } else {
                console.log('No accessibility issues found!');
              }
            });
        }
      }, 500); // Shorter timeout since the offcanvas is simpler
    }
  }

  render() { 
    const component = this;
    return ( 
          <AppContext.Provider value={
              { systemExternalRole: externalRole(component.state.systemIdentifier)
              , systemIdentifier: component.state.systemIdentifier
              , systemUser: component.state.systemUser
              , roleOnClipboard: component.state.roleOnClipboard
              }}
              >
            {/* Add skip link at the very top */}
            <a href="#main-content" className="skip-link visually-hidden-focusable">
              { i18next.t("skip_to_content", {ns: 'mycontexts'}) }
            </a>
            
            { this.state.openContext ? this.state.isSmallScreen ? this.renderMobile() : this.renderDesktop() : null }
            {this.notificationsAndClipboard()}
            {this.leftPanel()}
            <div className="keyboard-nav-instructions" id="keyboard-instructions">
              Press Tab to navigate between sections. Use arrow keys to navigate within a section. 
              Press Enter to select an item. Press Escape to return to the main navigation.
            </div>
          </AppContext.Provider>);
    }
}

export default WWWComponent
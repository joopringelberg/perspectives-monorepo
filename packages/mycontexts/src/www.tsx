import React from 'react';
import { Accordion, Col, Container, Navbar, NavDropdown, Offcanvas, Row, Tab, Tabs, DropdownDivider } from 'react-bootstrap';
import './www.css';
import {i18next} from 'perspectives-react';
import { ContextInstanceT, ContextType, CONTINUOUS, FIREANDFORGET, PDRproxy, RoleInstanceT, RoleType, ScreenDefinition, SharedWorkerChannelPromise, Unsubscriber, RoleOnClipboard, PropertySerialization, ValueT } from 'perspectives-proxy';
import {AppContext, deconstructContext, deconstructLocalName, EndUserNotifier, externalRole, initUserMessaging, ModelDependencies, PerspectivesComponent, PSContext, UserMessagingPromise, UserMessagingMessage, ChoiceMessage, UserChoice} from 'perspectives-react';
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

type Section = 'who' | 'what' | 'where' | 'none';

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
}

class WWWComponent extends PerspectivesComponent<{}, WWWComponentState> {
  screenUnsubscriber: Unsubscriber | undefined;

  constructor(props: {}) {
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
    };
    this.checkScreenSize = this.checkScreenSize.bind(this);
    this.screenUnsubscriber = undefined;
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
    PDRproxy.then( pproxy => pproxy.setUserMessageChannel( (message : string) => UserMessagingPromise.then( um => 
      um.addMessageForEndUser(
        { title: i18next.t("userMessagingPanel_title", { ns: 'mycontexts' }) 
        , message: i18next.t("userMessagingPanel_message", {error: message, ns: 'mycontexts'})
        , error: undefined
      }))));
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
                      , systemUser: systemIdentifier + "$" + deconstructLocalName( ModelDependencies.sysUser) as RoleInstanceT
                      });
                  }
                  else if (!roleOnClipboard)
                  {
                    // The clipboard is empty. Remove the role from the state.
                    component.setState(
                      { systemIdentifier
                      , systemUser: systemIdentifier + "$" + deconstructLocalName( ModelDependencies.sysUser) as RoleInstanceT
                      , roleOnClipboard: undefined
                      });
                  }
                }
              )
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
                ( languages : ValueT[] ) => changeLanguage( languages[0]as string ) ) );
              // Open the default screen or the one specified in the URL.
              component.prepareMyContextsScreen();
              })
            }
          )});
        })
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
          { openContext: undefined }
        );
        addEventListener("beforeunload", 
          (e => e.preventDefault()), 
          {capture: true});
      }
    };

    // Add message event listener for ServiceWorker
    if ('serviceWorker' in navigator) {
      navigator.serviceWorker.addEventListener('message', (event) => {
        console.log('Received message from ServiceWorker:', event.data);
        if (event.data && event.data.type === 'NOTIFICATION_CLICK') {
          const roleId = event.data.roleId;
          if (roleId) {
            console.log('Opening context from notification click:', roleId);
            component.tryToOpenContext(roleId);
          }
        }
      });
      
      // Register the service worker if not already registered
      navigator.serviceWorker.register('/www/notification-worker.js')
        .then(registration => {
          console.log('ServiceWorker registered from WWW component');
        })
        .catch(err => {
          console.error('Error registering ServiceWorker from WWW component:', err);
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

  }

  componentDidUpdate(prevProps: Readonly<{}>, prevState: Readonly<WWWComponentState>, snapshot?: any): void {
    if (this.state.openContext !== prevState.openContext) {
      this.getScreen(this.state.openContext!);
    }
  }

  componentWillUnmount() {
    window.removeEventListener('resize', this.checkScreenSize);
  }

  tryToOpenContext( s : string)
  {
    const component = this;
    ensureExternalRole( s )
      .then(
        function(result)
        {
          switch (result.tag)
          {
            case "RoleInstance":
              const erole = result.value;
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
    const navbar = document.querySelector('#top-navbar');
    const mobileTabs = document.querySelector('#mobile-tabs');
    const whoHeader = document.querySelector('#whoHeader');
    // Includes the padding of the navbar.
    const navbarHeight = navbar ? (navbar as HTMLElement).offsetHeight : 40;
    const mobileTabsHeight = mobileTabs ? (mobileTabs as HTMLElement).offsetHeight : 48;
    // Set the CSS variable for the navbar height. This is incorporated in the CSS style full-height.
    document.documentElement.style.setProperty('--navbar-height', `${navbarHeight}px`);
    document.documentElement.style.setProperty('--tabs-height', `${mobileTabsHeight}px`); 
    document.documentElement.style.setProperty('--who-header-height', `${whoHeader ? (whoHeader as HTMLElement).offsetHeight : 0}px`);  
    this.setState(
      { isSmallScreen: window.innerWidth < 768, doubleSection: computeDoubleSection() } );
  }

  prepareMyContextsScreen( )
  {
    const component = this;
    const params = new URLSearchParams(document.location.search.substring(1));
    
    if (params.get("opencontext"))
    {
      this.tryToOpenContext( decodeURIComponent( params.get("opencontext")!) );
    }
    else {
      component.openWelcomePage( );
    }
  }

  openWelcomePage()
  {
    const component = this;
    const mycontextStartPage = __STARTPAGE__ as RoleInstanceT; 
    document.title = "Welcome to MyContexts";
    history.pushState({ selectedContext: mycontextStartPage, title: "Welcome to MyContexts" }, "");
    component.setState( {openContext: mycontextStartPage, activeSection: 'what' } );
  }

  getScreen (externalRole : RoleInstanceT)
  {
    const component = this;
    const context = deconstructContext(externalRole) as ContextInstanceT
    PDRproxy
      .then( pproxy => pproxy.getContextType( context ) )
      .then( contextType => 
        {
          component.addUnsubscriber(
            PDRproxy.then( pproxy => pproxy.getMeForContext(
              externalRole,
              // userRoles includes roles from aspects.
              function(userRoles)
              {
                // It may happen that there are no user role types.
                if ( userRoles.length == 0)
                {
                  UserMessagingPromise.then( um => 
                    um.addMessageForEndUser(
                      { title: i18next.t("screen_no_usertype_for_context_title", { ns: 'preact' }) 
                      , message: i18next.t("screen_no_usertype_for_context_message", {ns: 'preact'})
                      , error: "No result from GetMeFromContext"
                      }));
                }
                else
                {
                  component.fetchScreen(contextType, userRoles[0], context);
                }
              },
              CONTINUOUS)))
          })
      .catch(e => UserMessagingPromise.then( um => 
        {
          um.addMessageForEndUser(
            { title: i18next.t("screen_computestate_title", { ns: 'preact' }) 
            , message: i18next.t("screen_computestate_message", {ns: 'preact'})
            , error: e.toString()
          });
          return false;})) as Promise<Boolean>;
  }

  // Ends a previous screen subscription for the current user and establishes a new one.
  fetchScreen (contextType : ContextType, userRoleType: RoleType, context: ContextInstanceT )
  {
    const component = this;
    PDRproxy.then(function(pproxy)
      {
        if (component.screenUnsubscriber)
        {
          pproxy.send(component.screenUnsubscriber, function(){});
        }
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
            component.screenUnsubscriber = unsubscriber;
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
              , message: i18next.t("action_message", {ns: 'preact', action: actionName})
              , error: e.toString()
              })));  
        });
  }

  notificationsAndClipboard() {
    const component = this;
    return (
      <Offcanvas show={this.state.showNotifications} onHide={() => component.setState({showNotifications:false})} placement='bottom' scroll={true} style={{ height: '50vh' }}>
        <Offcanvas.Header closeButton>
          <Offcanvas.Title>{ i18next.t("notificationsAndClipboard_title", {ns: 'mycontexts'})}</Offcanvas.Title>
        </Offcanvas.Header>
        <Offcanvas.Body>
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
    switch (this.state.leftPanelContent) {
      case 'about':
        content = <p>About</p>;
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
      <Offcanvas show={this.state.leftPanelContent} onHide={() => component.setState({leftPanelContent:false})} placement='start' scroll={true} style={{ height: '100vh' }}>
        <Offcanvas.Header closeButton>
          <Offcanvas.Title>{ this.state.leftPanelContent ? i18next.t( 'leftPanel_' + this.state.leftPanelContent, {ns: 'mycontexts'}) : ""}</Offcanvas.Title>
        </Offcanvas.Header>
        <Offcanvas.Body>
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
          <Tabs
            id="mobile-tabs"
            activeKey={this.state.activeSection}
            onSelect={(k: string | null) => {
              const key = k || 'what';
              component.setState({ 'activeSection': k as Section });
              }}
            fill
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
          <Navbar fixed="bottom" bg="primary" expand="xs" className="justify-content-between py-0 px-3">
            <Navbar.Brand onClick={() => window.history.back()}>
              <i className="bi bi-arrow-left text-light"></i>
            </Navbar.Brand>
            <Navbar.Brand onClick={() => component.setState({ showNotifications: true })}>
              <i className="bi bi-arrow-up text-light"></i>
            </Navbar.Brand>
            <Navbar.Brand onClick={() => window.history.forward()}>
              <i className="bi bi-arrow-right text-light"></i>
            </Navbar.Brand>
        </Navbar>
        <EndUserNotifier message={component.state.endUserMessage}/>
        <UserChoice message={component.state.choiceMessage}/>
      </Container>
      </PSContext.Provider>
    );
  }

  renderTopNavBar() {
    const component = this;
    return (<Navbar bg="primary" expand="xs" className="py-0 ps-2" id="top-navbar">
      <NavDropdown title={<i className="bi bi-list text-light fs-2"></i>} className="me-auto hide-caret px-2 py-1" id="nav-dropdown">
        <NavDropdown.Item onClick={() => component.setState({leftPanelContent: 'about'})}>About...</NavDropdown.Item>
        <NavDropdown.Item onClick={() => component.setState({leftPanelContent: 'me'})}>Me</NavDropdown.Item>
        <NavDropdown.Item onClick={() => component.setState({leftPanelContent: 'apps'})}>Apps</NavDropdown.Item>
        <NavDropdown.Item onClick={() => component.setState({leftPanelContent: 'settings'})}>Settings</NavDropdown.Item>
        { component.state.openContext ? <NavDropdown.Item onClick={ () => component.setState( {leftPanelContent: 'myroles'} ) }>{ i18next.t("www_myroles", {ns: 'mycontexts'}) }</NavDropdown.Item> : null }
        { component.state.actions && Object.keys( component.state.actions ).length > 0 ?
          <>
          <DropdownDivider />
          { Object.keys( component.state.actions ).map( action => <NavDropdown.Item key={action} onClick={() => component.runAction(action)}>{ component.state.actions![action]}</NavDropdown.Item>) }
          </>
          : null }
        { component.state.openContext ? <NavDropdown.Item onClick={ () => component.pinContext( component.state.openContext! ) }>{ i18next.t("www_pincontext", {ns: 'mycontexts'}) }</NavDropdown.Item> : null }
      </NavDropdown>
      <Navbar.Brand href="#home" className='text-light navbar-title'>{this.state.title}</Navbar.Brand>
      <InternetConnectivityCheck reportBack={ (isOnline : boolean) => component.setState({isOnline})}/>
      <ConnectedToAMQP roleinstance={ externalRole( component.state.systemIdentifier )} isOnline={component.state.isOnline} />
    </Navbar>);
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

  renderDesktop() {
    const component = this;
    return (
      <PSContext.Provider value={
        { contextinstance: deconstructContext( this.state.openContext!) as ContextInstanceT
        , contexttype: this.state.openContextType!
        , myroletype: this.state.openContextUserType!}}>
        <Container fluid className='px-0'>
          {component.renderTopNavBar()}
          <Row className='mx-0'>
            <Col 
              className='bg-primary full-height animated-column' 
              xs={ this.state.whatOnly ? 1 : this.state.doubleSection === "who" ? 6 : 3 } 
              style={{'--bs-bg-opacity': '.2'} as React.CSSProperties}>
                <Row id="whoHeader" onClick={() => component.setState( {'doubleSection': "who"} )}><h4 className='text-center'>{ i18next.t("www_who", {ns: 'mycontexts'}) }</h4></Row>
                <Row className='px-1 full-www-content-height scrollable-content'>
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
              style={{'--bs-bg-opacity': '.4'} as React.CSSProperties}>
              <Row onClick={() => component.setState( {'doubleSection': "what"} )}
                onDoubleClick={() => component.setState( {'whatOnly': !component.state.whatOnly} )}
              >
                <h4 className='text-center'>{ i18next.t("www_what", {ns: 'mycontexts'}) }</h4>
              </Row>
              {/* In the desktop, MSComponent will render a row with px-1 */}
              {/* Here we render either an arbitrary screen: {tag: "FreeFormScreen", elements: MainScreenElements}, or all TableFormDef elements in the {tag: "TableForms", elements: TableFormDef[]} variant of What. */}
              <Row className="full-www-content-height scrollable-content">
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
              style={{'--bs-bg-opacity': '.6'} as React.CSSProperties}>
              <Row onClick={() => component.setState( {'doubleSection': "where"} )}  ><h4 className='text-center'>{ i18next.t("www_where", {ns: 'mycontexts'}) }</h4></Row>  
              <Row className="px-1 full-www-content-height scrollable-content" style={{overflow: 'auto'}}>
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
            <Navbar fixed="bottom" bg="primary" expand="xs" className="justify-content-between py-0 px-3">
            <Navbar.Brand onClick={() => window.history.back()}>
              <i className="bi bi-arrow-left text-light"></i>
            </Navbar.Brand>
            <Navbar.Brand onClick={() => component.setState({ showNotifications: true })}>
              <i className="bi bi-arrow-up text-light"></i>
            </Navbar.Brand>
            <Navbar.Brand onClick={() => window.history.forward()}>
              <i className="bi bi-arrow-right text-light"></i>
            </Navbar.Brand>
          </Navbar>
          <EndUserNotifier message={component.state.endUserMessage}/>
          <UserChoice message={component.state.choiceMessage}/>
        </Container>
      </PSContext.Provider>);
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
            { this.state.openContext ? this.state.isSmallScreen ? this.renderMobile() : this.renderDesktop() : null }
            {this.notificationsAndClipboard()}
            {this.leftPanel()}
          </AppContext.Provider>);
    }
}

export default WWWComponent;

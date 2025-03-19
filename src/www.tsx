import React, { Component } from 'react';
import { Accordion, Col, Container, Nav, Navbar, NavDropdown, Offcanvas, Row, Tab, Tabs } from 'react-bootstrap';
import './www.css';
import i18next from 'i18next';
import { ContextInstanceT, ContextType, CONTINUOUS, FIREANDFORGET, PDRproxy, RoleInstanceT, RoleType, ScreenDefinition, SharedWorkerChannelPromise, Unsubscriber, What as WhatDef } from 'perspectives-proxy';
import {AppContext, deconstructContext, deconstructLocalName, externalRole, ModelDependencies, PerspectivesComponent, PSContext, UserMessagingPromise} from 'perspectives-react';
import { constructPouchdbUser, getInstallationData } from './installationData';
import { Me } from './me';
import { Apps } from './apps';
import ensureExternalRole from './ensureExternalRole';
import { What } from './what';
import { Who } from './who';

type Section = 'who' | 'what' | 'where';

interface WWWComponentState {
  isSmallScreen: boolean;
  title: string;
  doubleSection: Section;
  showNotifications: boolean;
  leftPanelContent: 'about' | 'me' | 'settings' | 'apps' | false;
  activeSection: Section;
  systemIdentifier: ContextInstanceT;
  systemUser: RoleInstanceT
  openContext?: RoleInstanceT
  openContextType?: ContextType
  openContextUserType?: RoleType
  screen?: ScreenDefinition;
}

class WWWComponent extends PerspectivesComponent<{}, WWWComponentState> {
  screenUnsubscriber: Unsubscriber | undefined;

  constructor(props: {}) {
    super(props);
    this.state = 
      { isSmallScreen: false
      , title: 'MyContexts'
      , doubleSection: 'what'
      , showNotifications: false
      , leftPanelContent: false
      , activeSection: 'what' 
      , systemIdentifier: '' as ContextInstanceT
      , systemUser: '' as RoleInstanceT
      , screen: undefined
    };
    this.checkScreenSize = this.checkScreenSize.bind(this);
    this.screenUnsubscriber = undefined;
  }

  componentDidMount() {
    const component = this;
    SharedWorkerChannelPromise.then( pdrHandler => {
      getInstallationData().then( installationData => {
        pdrHandler.runPDR( installationData.perspectivesUserId!, 
          constructPouchdbUser(installationData), 
          { isFirstInstallation: true, 
            useSystemVersion: null,
            myContextsVersion: __MYCONTEXTS_VERSION__
          }).then ( succeeded => {
            const systemIdentifier = "def:#" + installationData.perspectivesUserId! + installationData.deviceName! as ContextInstanceT;
            this.setState(
              { systemIdentifier
              , systemUser: systemIdentifier + "$" + deconstructLocalName( ModelDependencies.sysUser) as RoleInstanceT
              })
          }
          );})});
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
          { openContext: e.state.selectedContext } );
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

    function listenToOpenContext(e: CustomEvent)
    {
      e.stopPropagation();
      ensureExternalRole( e.detail )
        .then(
          function(erole)
          {
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
                { openContext: erole, leftPanelContent: false });
            }
          })
        .catch(err => UserMessagingPromise.then( um => 
          um.addMessageForEndUser(
            { title: i18next.t("app_opencontext_title", { ns: 'mycontexts' }) 
            , message: i18next.t("app_opencontext_message", {context: e.detail, ns: 'mycontexts'})
            , error: err.toString()
          })));
      e.stopPropagation();
    }
    document.body.addEventListener('OpenContext', listenToOpenContext as EventListener, false);
  }

  componentDidUpdate(prevProps: Readonly<{}>, prevState: Readonly<WWWComponentState>, snapshot?: any): void {
    if (this.state.openContext !== prevState.openContext) {
      this.getScreen(this.state.openContext!);
    }
  }

  checkScreenSize(){
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
      { isSmallScreen: window.innerWidth < 768 });
  }

  componentWillUnmount() {
    window.removeEventListener('resize', this.checkScreenSize);
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
              component.setState({screen: screens[0], openContextType: contextType, openContextUserType: userRoleType});
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

  notificationsAndClipboard() {
    const component = this;
    return (
      <Offcanvas show={this.state.showNotifications} onHide={() => component.setState({showNotifications:false})} placement='bottom' scroll={true} style={{ height: '50vh' }}>
        <Offcanvas.Header closeButton>
          <Offcanvas.Title>Clipboard & Notifications</Offcanvas.Title>
        </Offcanvas.Header>
        <Offcanvas.Body>
        <Accordion defaultActiveKey="0">
          <Accordion.Item eventKey="0">
            <Accordion.Header>Clipboard</Accordion.Header>
            <Accordion.Body>
              <ul>
                <li>Item 1</li>
                <li>Item 2</li>
                <li>Item 3</li>
              </ul>
            </Accordion.Body>
          </Accordion.Item>
          <Accordion.Item eventKey="1">
            <Accordion.Header>Notifications</Accordion.Header>
            <Accordion.Body>
              <ul>
                <li>Notification 1</li>
                <li>Notification 2</li>
                <li>Notification 3</li>
              </ul>
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
        content = <p>Settings</p>;
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
    return (<Container fluid className='px-0'>
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
        <Tab eventKey="who" title="Wie" className='bg-info full-mobile-height px-2' style={{'--bs-bg-opacity': '.2'} as React.CSSProperties}>
          { this.state.screen?.whoWhatWhereScreen ?
            <Who screenelements={ this.state.screen.whoWhatWhereScreen.who } showTablesAndForm={this.state.isSmallScreen || this.state.doubleSection == "who"}/>
            :
            <p className='bg-light-subtle'>Ga ergens heen</p>
          }
        </Tab>
        <Tab eventKey="what" title="Wat" className='bg-info full-mobile-height px-2' style={{'--bs-bg-opacity': '.4'} as React.CSSProperties}>
          { this.state.screen?.whoWhatWhereScreen ? 
            (<PSContext.Provider value={{contextinstance: deconstructContext( this.state.openContext!) as ContextInstanceT, contexttype: this.state.openContextType!, myroletype: this.state.openContextUserType!}}>
              <What screenelements={  this.state.screen.whoWhatWhereScreen.what }/> 
            </PSContext.Provider>)
            : 
            <div>Ga ergens heen.</div>
          }
        </Tab>
        <Tab eventKey="where" title="Waar" className='bg-info full-mobile-height px-2' style={{'--bs-bg-opacity': '.6'} as React.CSSProperties}>
          <p className='bg-light-subtle'>Weergave van de perspectieven op waar.</p>
        </Tab>
      </Tabs>
      <Navbar fixed="bottom" bg="info" expand="xs" className="justify-content-center py-0">
        <Navbar.Brand onClick={() => component.setState({ showNotifications: true })}>
          <i className="bi bi-arrow-up"></i>
        </Navbar.Brand>
    </Navbar>
    </Container>
    );
  }

  renderTopNavBar() {
    const component = this;
    return (<Navbar bg="info" expand="xs" className="py-0" id="top-navbar">
      <NavDropdown title={<i className="bi bi-list"></i>} className="me-auto">
        <NavDropdown.Item onClick={() => component.setState({leftPanelContent: 'about'})}>About...</NavDropdown.Item>
        <NavDropdown.Item onClick={() => component.setState({leftPanelContent: 'me'})}>Me</NavDropdown.Item>
        <NavDropdown.Item onClick={() => component.setState({leftPanelContent: 'apps'})}>Apps</NavDropdown.Item>
        <NavDropdown.Item onClick={() => component.setState({leftPanelContent: 'settings'})}>Settings</NavDropdown.Item>
      </NavDropdown>
      <Navbar.Brand href="#home" className='text-light flex-grow-1 d-flex justify-content-center align-items-center'>{this.state.title}</Navbar.Brand>
    </Navbar>);
  }

  renderDesktop() {
    const component = this;
    return (<Container fluid className='px-0'>
      {component.renderTopNavBar()}
      <Row className='mx-0'>
        <Col 
          className='bg-info full-height' 
          xs={ this.state.doubleSection === "who" ? 6 : 3} 
          style={{'--bs-bg-opacity': '.2'} as React.CSSProperties}>
            <Row id="whoHeader" onClick={() => component.setState( {'doubleSection': "who"} )}><h4 className='text-center'>Wie</h4></Row>
            <Row className='px-1 full-www-content-height'>
              { this.state.screen?.whoWhatWhereScreen ?
                <Who screenelements={ this.state.screen.whoWhatWhereScreen.who } showTablesAndForm={this.state.isSmallScreen || this.state.doubleSection == "who"}/>
                :
                <p className='bg-light-subtle'>Ga ergens heen</p>
              }
            </Row>
        </Col>
        <Col 
          className='bg-info' 
          xs={ this.state.doubleSection === "what" ? 6 : 3} 
          style={{'--bs-bg-opacity': '.4'} as React.CSSProperties}>
          <Row onClick={() => component.setState( {'doubleSection': "what"} )}  ><h4 className='text-center'>Wat</h4></Row>
          {/* In the desktop, MSComponent will render a row with px-1 */}
          {/* Here we render either an arbitrary screen: {tag: "FreeFormScreen", elements: MainScreenElements}, or all TableFormDef elements in the {tag: "TableForms", elements: TableFormDef[]} variant of What. */}
          <Row className="full-www-content-height">
          {this.state.screen?.whoWhatWhereScreen ? 
              (<PSContext.Provider value={{contextinstance: deconstructContext( this.state.openContext!) as ContextInstanceT, contexttype: this.state.openContextType!, myroletype: this.state.openContextUserType!}}>
                <What screenelements={  this.state.screen.whoWhatWhereScreen.what }/> 
              </PSContext.Provider>)
              : 
              <div>Ga ergens heen.</div>
              }
          </Row>
        </Col>
        <Col 
          className='bg-info' 
          xs={ this.state.doubleSection === "where" ? 6 : 3} 
          style={{'--bs-bg-opacity': '.6'} as React.CSSProperties}>
          <Row onClick={() => component.setState( {'doubleSection': "where"} )}  ><h4 className='text-center'>Waar</h4></Row>  
          <Row className='px-1 className="full-www-content-height"'>
            <p className='bg-light-subtle'>Here we render all TableFormDef elements that make up the Whereto part of the screen (representing the context roles), as Master-Slave components. </p>
            <p className='bg-light-subtle'>Rendering of the recent contexts.</p>
            <p className='bg-light-subtle'>Rendering of the pinned contexts.</p>
          </Row>
        </Col>
      </Row>
      <Navbar fixed="bottom" bg="info" expand="xs" className="justify-content-center py-0">
        <Navbar.Brand onClick={() => component.setState({ showNotifications: true })}>
          <i className="bi bi-arrow-up"></i>
        </Navbar.Brand>
      </Navbar>
    </Container>);
  }

  render() {
    const component = this;
    return ( 
          <AppContext.Provider value={
              { systemExternalRole: externalRole(component.state.systemIdentifier)
              , systemIdentifier: component.state.systemIdentifier
              , systemUser: component.state.systemUser
              }}
              >
            {this.state.isSmallScreen ? this.renderMobile() : this.renderDesktop()}
            {this.notificationsAndClipboard()}
            {this.leftPanel()}
          </AppContext.Provider>);
    }
}

export default WWWComponent;

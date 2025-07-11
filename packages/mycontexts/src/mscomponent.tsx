/**
 * Props for the MSComponent.
 * 
 * @interface MSComponentProps
 * @property {boolean} isMobile - Indicates if the component is being viewed on a mobile device. If true, either the main content or the sliding content will be displayed.
 * @property {React.ReactElement<MainContentProps>[]} children - An array containing exactly two children: 
 *   the main content and the sliding panel content.
 * @property {string} [className] - Optional additional class name(s) to apply to the component.
 */
/*
  This component renders a single TableFormDef screen element.
 */

import * as React from "react";
const { Component } = React;
import { CloseButton, Col, Row } from "react-bootstrap";
import "./styles/slidingPanels.css";
import { RoleInstanceT, RoleType } from "perspectives-proxy";

type RoleInstanceSelectionEvent = Event & { detail: { roleInstance: RoleInstanceT, roleType: RoleType } };

interface MSComponentState {
  selectedRoleInstance: RoleInstanceT | undefined;
  selectedRoleType?: RoleType;
  isFormVisible: boolean;
  isSliding: boolean;
  isTabbable: boolean;
}

export interface MainContentProps {
  className?: string;
}

export interface SlidingPanelContentProps {
  className?: string;
  selectedRoleInstance?: RoleInstanceT;
  selectedRoleType?: RoleType;
}

interface MSComponentProps {
  isMobile: boolean;
  doubleclickOpensDetails: boolean;
  children: [React.ReactElement<MainContentProps>, React.ReactElement<SlidingPanelContentProps>];
  className?: string;
}

class MSComponent extends Component<MSComponentProps, MSComponentState> {
  private containerRef: React.RefObject<HTMLDivElement | null>;
  private slidingPanelRef: React.RefObject<HTMLDivElement | null>;
  private mainPanelRef: React.RefObject<HTMLDivElement | null>;

  constructor(props: MSComponentProps) {
    super(props);
    this.state = { selectedRoleInstance: undefined, isFormVisible: false, isSliding: false, isTabbable: false };
    this.showDetails = this.showDetails.bind(this);
    this.conditionallyShowDetails = this.conditionallyShowDetails.bind(this);
    this.containerRef = React.createRef();
    this.slidingPanelRef = React.createRef();
    this.mainPanelRef = React.createRef();
  }

  showDetails = (event: RoleInstanceSelectionEvent) => {
    // First set isFormVisible to true to render the panel (but it's still off-screen)
    this.setState({
      selectedRoleInstance: event.detail.roleInstance,
      selectedRoleType: event.detail.roleType,
      isFormVisible: true
    });

    // Use requestAnimationFrame to ensure the panel is rendered before we start animating
    requestAnimationFrame(() => {
      // Now trigger the animation by setting isSliding to true
      this.setState({ isSliding: true });

      // Set tabindex after animation completes
      setTimeout(() => {
        this.setState({ isTabbable: true });
      }, 400); // Match this to your transition duration
    });
  };

  conditionallyShowDetails (event: RoleInstanceSelectionEvent) {
    if (this.props.doubleclickOpensDetails)
      {
        this.showDetails(event);
      }
  }
  componentDidMount(): void {
    const component = this;
    if (component.containerRef.current) { 
      component.containerRef.current.addEventListener('OpenDetails', this.conditionallyShowDetails, true );
      component.containerRef.current.addEventListener('OpenWhereDetails', this.showDetails, true );
    }
    component.mainPanelRef.current?.focus();
  }

  componentDidUpdate(prevProps: MSComponentProps): void {
    if (prevProps.isMobile !== this.props.isMobile && this.containerRef.current) {
      // We have to do this because the reference to the containerRef changes when the component is re-rendered for mobile (and vv).
      this.containerRef.current.removeEventListener('OpenWhereDetails', this.showDetails as EventListener, true);
      this.containerRef.current.removeEventListener('OpenDetails', this.conditionallyShowDetails, true );
      this.containerRef.current.addEventListener('OpenDetails', this.conditionallyShowDetails as EventListener, true);
      this.containerRef.current.addEventListener('OpenWhereDetails', this.showDetails, true );
    }
    if (this.props.isMobile && this.state.isFormVisible && this.slidingPanelRef.current) {
      this.slidingPanelRef.current.focus();
    }
  }

  componentWillUnmount(): void {
    const component = this;
    if (component.containerRef.current) {
      component.containerRef.current.removeEventListener('OpenDetails', this.conditionallyShowDetails as EventListener, true);
      component.containerRef.current.removeEventListener('OpenWhereDetails', this.showDetails as EventListener, true);
    }
  }

  handleKeyDown = (event: React.KeyboardEvent<HTMLDivElement>) => {
    if (event.key === "Escape") {
      this.handleClose();
    }
  };

  handleClose = () => {
    // First stop the sliding
    this.setState({ isSliding: false, isTabbable: false });
    
    // After the animation completes, hide the panel
    setTimeout(() => {
      this.setState({ isFormVisible: false });
    }, 400); // Match this to your transition duration

    this.mainPanelRef.current?.focus();
  };

  render() {
    const component = this;
    const { children } = this.props;

    if (!children || children.length !== 2) {
      console.error("MSComponent requires exactly two children: <MainContent /> and <SlidingPanelContent />");
      return null;
    }

    const [mainContent, slidingContent] = children;

    if (component.props.isMobile) {
      return (
        <div 
          className={`sliding-panels-container pb-2 ${this.state.isSliding ? 'has-open-panel' : ''}`} 
          ref={this.containerRef}
        >
          {/* Main Panel */}
          <div className="main-panel" ref={this.mainPanelRef}>
            {React.cloneElement(mainContent as React.ReactElement<MainContentProps>, { className: this.props.className })}
          </div>
          {/* Sliding Panel - Always render it but control visibility with CSS */}
          <div
            className={`cover-panel ${this.state.isSliding ? 'open' : ''} bg-light-subtle p-2`}
            onKeyDown={(e) => component.handleKeyDown(e)}
            ref={this.slidingPanelRef}
            tabIndex={this.state.isTabbable ? 0 : undefined}
            style={{ display: this.state.isFormVisible ? 'flex' : 'none' }} // Control display separately
          >
            <CloseButton onClick={this.handleClose} />
            <div className="sliding-panel-content">
              {React.cloneElement(slidingContent as React.ReactElement<SlidingPanelContentProps>, {
                className: this.props.className,
                selectedRoleInstance: this.state.selectedRoleInstance,
                selectedRoleType: this.state.selectedRoleType,
              })}
            </div>
          </div>
        </div>
      );
    } else {
      return (
        <Row className="px-1" ref={this.containerRef}>
          <Col>{React.cloneElement(mainContent as React.ReactElement<MainContentProps>, { className: this.props.className })}</Col>
          <Col>{React.cloneElement(slidingContent as React.ReactElement<SlidingPanelContentProps>, {
            className: this.props.className,
            selectedRoleInstance: this.state.selectedRoleInstance,
            selectedRoleType: this.state.selectedRoleType,
          })}</Col>
        </Row>
      );
    }
  }
}

export default MSComponent;
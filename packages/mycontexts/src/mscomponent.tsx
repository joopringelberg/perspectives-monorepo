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
  children: [React.ReactElement<MainContentProps>, React.ReactElement<SlidingPanelContentProps>];
  className?: string;
}

class MSComponent extends Component<MSComponentProps, MSComponentState> {
  private containerRef: React.RefObject<HTMLDivElement | null>;
  private slidingPanelRef: React.RefObject<HTMLDivElement | null>;
  private mainPanelRef: React.RefObject<HTMLDivElement | null>;
  // Tracks whether a form field is being edited, without going
  // through React state to avoid re-rendering the sliding panel.
  private isEditingDom: boolean = false;
  // Observes the sliding panel's size so the container can grow to fit.
  private panelResizeObserver: ResizeObserver | null = null;

  constructor(props: MSComponentProps) {
    super(props);
    this.state = { selectedRoleInstance: undefined, isFormVisible: false, isSliding: false, isTabbable: false };
    this.showDetails = this.showDetails.bind(this);
    this.containerRef = React.createRef();
    this.slidingPanelRef = React.createRef();
    this.mainPanelRef = React.createRef();
  }

  private handleFieldEditStarted = (_event: Event) => {
    this.isEditingDom = true;
    // Hide or disable the navigation button directly in the DOM to
    // avoid triggering a React re-render of the sliding panel.
    if (this.slidingPanelRef.current) {
      const closeButton = this.slidingPanelRef.current.querySelector('button[aria-label="Sluit formulier"]') as HTMLButtonElement | null;
      if (closeButton) {
        closeButton.disabled = true;
        closeButton.style.visibility = "hidden";
      }
    }
  };

  private handleFieldEditEnded = (_event: Event) => {
    this.isEditingDom = false;
    if (this.slidingPanelRef.current) {
      const closeButton = this.slidingPanelRef.current.querySelector('button[aria-label="Sluit formulier"]') as HTMLButtonElement | null;
      if (closeButton) {
        closeButton.disabled = false;
        closeButton.style.visibility = "";
      }
    }
  };

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

  componentDidMount(): void {
    const component = this;
    if (component.containerRef.current) { 
      component.containerRef.current.addEventListener('ShowDetails', this.showDetails, true );
      component.containerRef.current.addEventListener('FormFieldEditStarted', this.handleFieldEditStarted as EventListener, true);
      component.containerRef.current.addEventListener('FormFieldEditEnded', this.handleFieldEditEnded as EventListener, true);
    }
    component.mainPanelRef.current?.focus();
  }

  componentDidUpdate(prevProps: MSComponentProps, prevState: MSComponentState): void {
    if (prevProps.isMobile !== this.props.isMobile && this.containerRef.current) {
      // We have to do this because the reference to the containerRef changes when the component is re-rendered for mobile (and vv).
      this.containerRef.current.removeEventListener('ShowDetails', this.showDetails as EventListener, true);
      this.containerRef.current.removeEventListener('FormFieldEditStarted', this.handleFieldEditStarted as EventListener, true);
      this.containerRef.current.removeEventListener('FormFieldEditEnded', this.handleFieldEditEnded as EventListener, true);
      this.containerRef.current.addEventListener('ShowDetails', this.showDetails as EventListener, true);
      this.containerRef.current.addEventListener('FormFieldEditStarted', this.handleFieldEditStarted as EventListener, true);
      this.containerRef.current.addEventListener('FormFieldEditEnded', this.handleFieldEditEnded as EventListener, true);
    }
    // When switching from side-by-side to mobile, close the form panel.
    if (this.props.isMobile && !prevProps.isMobile && this.state.isFormVisible) {
      this.stopObservingPanel();
      this.setState({ isFormVisible: false, isSliding: false, isTabbable: false });
    }
    else if (this.props.isMobile && this.state.isFormVisible && this.slidingPanelRef.current) {
      // Start observing the panel if it just became visible.
      if (!prevState.isFormVisible) {
        this.startObservingPanel();
      }
      this.slidingPanelRef.current.focus();
    }
  }

  componentWillUnmount(): void {
    const component = this;
    this.stopObservingPanel();
    if (component.containerRef.current) {
      component.containerRef.current.removeEventListener('ShowDetails', this.showDetails as EventListener, true);
      component.containerRef.current.removeEventListener('FormFieldEditStarted', this.handleFieldEditStarted as EventListener, true);
      component.containerRef.current.removeEventListener('FormFieldEditEnded', this.handleFieldEditEnded as EventListener, true);
    }
  }

  /** Start observing the sliding panel so the container grows to fit the form. */
  private startObservingPanel() {
    if (this.panelResizeObserver || !this.slidingPanelRef.current) return;
    this.panelResizeObserver = new ResizeObserver(() => {
      if (this.slidingPanelRef.current && this.containerRef.current) {
        const panelHeight = this.slidingPanelRef.current.scrollHeight;
        this.containerRef.current.style.minHeight = `${panelHeight}px`;
      }
    });
    this.panelResizeObserver.observe(this.slidingPanelRef.current);
    // Also set initial min-height immediately.
    if (this.containerRef.current) {
      this.containerRef.current.style.minHeight = `${this.slidingPanelRef.current.scrollHeight}px`;
    }
  }

  /** Stop observing and reset container min-height. */
  private stopObservingPanel() {
    if (this.panelResizeObserver) {
      this.panelResizeObserver.disconnect();
      this.panelResizeObserver = null;
    }
    if (this.containerRef.current) {
      this.containerRef.current.style.minHeight = '';
    }
  }

  handleKeyDown = (event: React.KeyboardEvent<HTMLDivElement>) => {
    if (this.isEditingDom) {
      return;
    }
    if (event.key === "Escape") {
      this.handleClose();
    }
  };

  handleClose = () => {
    // Stop observing the panel before closing.
    this.stopObservingPanel();
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
            {/* The visibility/enabled state of this button is adjusted
                directly in the DOM by the field edit handlers to
                avoid rerendering this panel while editing. */}
            {(
              <button
                type="button"
                className="btn btn-link p-0 me-2"
                onClick={this.handleClose}
                aria-label="Sluit formulier"
              >
                <i className="bi bi-arrow-right" aria-hidden="true" />
              </button>
            )}
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
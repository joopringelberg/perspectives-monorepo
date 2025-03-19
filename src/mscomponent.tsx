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

import { Component, ReactNode } from "react";
import { CloseButton, Col, Row } from "react-bootstrap";
import "./slidingPanels.css";
import * as React from "react";
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

  constructor(props: MSComponentProps) {
    super(props);
    this.state = { selectedRoleInstance: undefined, isFormVisible: false, isSliding: false, isTabbable: false };
    this.setSelectRow = this.setSelectRow.bind(this);
    this.containerRef = React.createRef();
    this.slidingPanelRef = React.createRef();
    this.mainPanelRef = React.createRef();
  }

  setSelectRow = (event: RoleInstanceSelectionEvent) => {
    this.setState(
      { selectedRoleInstance: event.detail.roleInstance
      , selectedRoleType: event.detail.roleType
      , isFormVisible: this.props.isMobile
    });
    // Without this construct, the panel will be displayed before the sliding animation is complete.
    setTimeout(() => {
      this.setState({ isSliding: true });
    }
    , 100);
    // If we do not delay setting the tabIndex, the animation will not be completed and the panel is visible immidiately.
    setTimeout(() => {
      this.setState({ isTabbable: true });
    }
    , 600);
  };

  componentDidMount(): void {
    const component = this;
    if (component.containerRef.current) {
      component.containerRef.current.addEventListener('SetRow', this.setSelectRow as EventListener, true);
    }
    component.mainPanelRef.current?.focus();
  }

  componentDidUpdate(prevProps: MSComponentProps): void {
    if (prevProps.isMobile !== this.props.isMobile && this.containerRef.current) {
      this.containerRef.current.removeEventListener('SetRow', this.setSelectRow as EventListener, true);
      this.containerRef.current.addEventListener('SetRow', this.setSelectRow as EventListener, true);
    }
    if (this.props.isMobile && this.state.isFormVisible && this.slidingPanelRef.current) {
      this.slidingPanelRef.current.focus();
    }
  }

  componentWillUnmount(): void {
    const component = this;
    if (component.containerRef.current) {
      component.containerRef.current.removeEventListener('SetRow', this.setSelectRow as EventListener, true);
    }
  }

  handleKeyDown = (event: React.KeyboardEvent<HTMLDivElement>) => {
    if (event.key === "Escape") {
      this.handleClose();
    }
  };

  handleClose = () => {
    this.setState({ isFormVisible: false, isSliding: false, isTabbable: false });
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
        <div className="sliding-panels-container" ref={this.containerRef}>
          {/* Main Panel */}
          <div className="main-panel" ref={this.mainPanelRef} tabIndex={1}>
            {React.cloneElement(mainContent as React.ReactElement<MainContentProps>, { className: this.props.className })}
          </div>
          {/* Sliding Panel */}
          {this.state.isFormVisible && (
            <div
              className={`cover-panel ${this.state.isSliding ? 'open' : ''} bg-info`}
              onKeyDown={(e) => component.handleKeyDown(e)}
              ref={this.slidingPanelRef}
              tabIndex={this.state.isTabbable ? 0 : undefined}
            >
              <CloseButton onClick={this.handleClose} />
              {React.cloneElement(slidingContent as React.ReactElement<SlidingPanelContentProps>, {
                className: this.props.className,
                selectedRoleInstance: this.state.selectedRoleInstance,
                selectedRoleType: this.state.selectedRoleType,
              })}
            </div>
          )}
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
import { PropertyType, RoleInstanceT, RoleType } from "perspectives-proxy";
import React, { createRef } from "react";
import { PSRol, PSRolType } from "./reactcontexts";
import { BehaviourAdder, BehaviourComponentProps, CardProperties, InnerCardProperties } from "./cardbehaviour";
import { ComponentProps } from "react";

/*
  Do not use this component to add behaviour to a Card.
  Instead, use the AdorningComponentWrapper component.
  This component is used by the AdorningComponentWrapper component.
*/
// Extend HTMLElement to include optional __swipeCleanup property
interface HTMLElementWithSwipeCleanup extends HTMLElement {
    __swipeCleanup?: () => void;
  }

export interface AdornWithBehaviourProps extends CardProperties, BehaviourComponentProps {
  addedBehaviour: BehaviourAdder[];
  Card: React.ComponentType<InnerCardProperties>;
  externalRef?: React.RefObject<HTMLElement>;
}

export class AdornWithBehaviour extends React.Component<AdornWithBehaviourProps>
{
  prevRef?: HTMLElement | null;
  addedBehaviour: string[];
  declare context: PSRolType;
  static contextType = PSRol
  // This reference will point to the div element that wraps the Card component. 
  // It will have all behaviour handlers.
  // It will also receive focus.
  ref: React.RefObject<HTMLElementWithSwipeCleanup | null>;
  
  constructor(props: AdornWithBehaviourProps) {
    super(props);
    this.addedBehaviour = [];
    if (props.externalRef){
      this.ref = props.externalRef;
    }
    else {
      this.ref = createRef();
    }
  }

  componentDidMount() {
    const component = this;
    const domEl = this.ref.current as HTMLElement;
    // Save for componentDidUpdate, so we can see if we need to re-establish behaviour.
    component.prevRef = domEl;
    // Add behaviour to the div DOM element.
    if (domEl) 
      {
        this.props.addedBehaviour.forEach(behaviour => behaviour(domEl!, this, component.props.title));
      }
  }

  componentWillUnmount(): void {
    if (this.ref.current?.__swipeCleanup) {
      // If the element has a swipe cleanup function, call it.
      this.ref.current.__swipeCleanup();
    }
  }

  componentDidUpdate() {
    const component = this;
    const domEl = this.ref.current;
    if (component.prevRef !== domEl) {
      // Do behaviour again.
      if (domEl)
      {
        this.props.addedBehaviour.forEach(behaviour => behaviour(domEl!, this, component.props.title));
      }
    }
  }

  render() {
    const component = this;
    return (
      <div ref={component.ref as React.RefObject<HTMLDivElement>}
        // This might be a handler added in the context of a perspectives table.
        onClick={this.props.onClick}
        tabIndex={this.props.tabIndex}
      >
        <this.props.Card 
          title={this.props.title} 
          aria-label={this.props["aria-label"]} 
          className={this.props.className}
          type={this.props.type}
          />
      </div>
    );
  }
}
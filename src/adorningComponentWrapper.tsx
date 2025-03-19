import { PropertyType } from "perspectives-proxy";
import React from "react";
import { AppContext, PSContext } from "./reactcontexts";
import { AdornWithBehaviour } from "./adornwithbehaviour";
import { CardProperties, getBehaviourAdder, InnerCardProperties } from "./cardbehaviour";

/*
  Use this component to wrap a component that implements the CardProperties interface.
  This component provides the AppContext and PSContext to the wrapped component.
  It also adds behaviour to the wrapped component.
  The behaviour is determined by the behaviourNames prop.
*/

interface AdorningComponentWrapperProps extends CardProperties {
  behaviourNames: string[];
  Card: React.ComponentType<InnerCardProperties>;
  externalRef?: React.RefObject<HTMLElement>;
}

export class AdorningComponentWrapper extends React.Component<AdorningComponentWrapperProps>
{
  constructor(props : AdorningComponentWrapperProps){
    super(props as AdorningComponentWrapperProps);
  }
  render() {
    const addedBehaviour = this.props.behaviourNames.map(getBehaviourAdder);
    // Remove behaviourNames from the props.
    const { behaviourNames, ...rest } = this.props;

    return (
      <PSContext.Consumer>
        {pscontext => (
          <AppContext.Consumer>
            {appcontext => (
                <AdornWithBehaviour 
                  {...rest}
                  systemExternalRole={appcontext.systemExternalRole}
                  myroletype={pscontext.myroletype}
                  addedBehaviour={addedBehaviour}
                />
            )}
          </AppContext.Consumer>
        )}
      </PSContext.Consumer>
    );

}
}

////////////////////////////////////////////
//// A HOC to add behaviours to a card component
////////////////////////////////////////////
export interface WithBehavioursProps extends CardProperties {
  behaviourNames: string[];
  externalRef?: React.RefObject<HTMLElement>;
}

// Wraps a component with CardProperties and produces a component with CardProperties and an extra prop behaviourNames.
export const cardWithConfigurableBehaviour = (Card: React.ComponentType<InnerCardProperties>) => {
  return ({ behaviourNames, ...props }: WithBehavioursProps) => {
    return (
      <AdorningComponentWrapper
        behaviourNames={behaviourNames}
        Card={Card}
        {...props}
      />
    );
  };
};

export interface WithOutBehavioursProps extends CardProperties {
  externalRef?: React.RefObject<HTMLElement>;
}

// Takes behaviourNames as argument and a component with CardProperties and produces a component with CardProperties.
export const CardWithFixedBehaviour = (Card: React.ComponentType<CardProperties>, behaviourNames: string[]) : React.ComponentType<CardProperties> => {
  return (props: WithOutBehavioursProps) => {
    return (
      <AdorningComponentWrapper
        behaviourNames={behaviourNames}
        Card={Card}
        {...props}
      />
    );
  };
}
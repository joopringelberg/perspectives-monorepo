import { Component, PureComponent } from "react";
import {PDRproxy, Unsubscriber} from "perspectives-proxy";

interface PerspectivesComponentState {
  [key: string]: any;
}

interface PerspectivesComponentProps {
  [key: string]: any;
}

export default class PerspectivesComponent<P = PerspectivesComponentProps, S = PerspectivesComponentState> extends Component<P, S>
{
  unsubscribers: Unsubscriber[];
  __mounted__: boolean;

  constructor(props: PerspectivesComponentProps) {
    super(props as Readonly<P>);
    this.state = {} as Readonly<S>;
    this.unsubscribers = [] as Unsubscriber[];
    this.__mounted__ = true;
  }

  componentDidMount ()
  {
    this.__mounted__ = true;
    if (super.componentDidMount)
    {
      super.componentDidMount();
    }
  }

  componentWillUnmount ()
  {
    this.unsubscribeAll();
    this.__mounted__ = false;
  }

  // Returns a promise for when all subscribers have been unsubscribed.
  unsubscribeAll()
  {
    const component = this as PerspectivesComponent;
    return PDRproxy.then(
      function(pproxy)
      {
        component.unsubscribers.forEach(
          function(unsubscriber)
          {
            // unsubscriber = {subject: req.subject, corrId: req.corrId}
            pproxy.send(unsubscriber, function(){});
          });
        component.unsubscribers = [];
      });
  }

  setState<K extends keyof S>(state: S | ((prevState: Readonly<S>, props: Readonly<P>) => S | Pick<S, K> | null) | Pick<S, K> | null, callback?: () => void): void
  {
    // if (this.__mounted__)
    // {
      super.setState(state, callback);
    // }
  }

  // A single component may perform multiple calls through the API. All of these may connect a callback
  // to the dependency network in the core. When the component unmounts, it should inform the core that
  // these callbacks can be unsubscribed. This is what we use the unsubscriber for.
  addUnsubscriber(unsubscriberPromise: Promise<Unsubscriber|void>): Promise<void>
  {
    if (unsubscriberPromise)
    {
      // unsubscriber = {subject: req.subject, corrId: req.corrId}
      return unsubscriberPromise.then((unsubscriber: Unsubscriber|void) => {
        if (unsubscriber)
        {
          this.unsubscribers.push(unsubscriber);
        }
        return Promise.resolve();
      });
    }
    return Promise.resolve();
  }

 // Returns true if all state members have a value, unless they are a boolean, or an array, or are excluded. 
  stateIsComplete (excludedProps: string[] = [])
  {
    const component = this as PerspectivesComponent;
    let isComplete = true;
    Object.keys(component.state).forEach(
      function (prop: string)
      {
        const state = component.state as PerspectivesComponentState;
        if (!state[prop]
            && (typeof state[prop]) != "boolean"
            && (!Array.isArray(state[prop])
            && (excludedProps.indexOf(prop) < 0))
          )
        // if ( !component.state.hasOwnProperty(prop) )
        {isComplete = false;}
      });

    return isComplete;
  }

  stateIsEmpty ()
  {
    return Object.keys(this.state).length === 0;
  }

}

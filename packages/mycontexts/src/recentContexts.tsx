import React, { } from "react";
import { PDRproxy, RoleInstanceT, ContextInstanceT, Perspective, ValueT, FIREANDFORGET } from "perspectives-proxy";
import { deconstructContext, ModelDependencies, PerspectivesComponent, PerspectiveTable } from "perspectives-react";

interface RecentContextsProps {
  systemuser: RoleInstanceT;
  systemIdentifier: ContextInstanceT;
  openContext: RoleInstanceT | undefined;
}

interface RecentContextsState {
  perspective: Perspective | undefined;
  recentContexts: RoleInstanceT[];
}

export class RecentContexts extends PerspectivesComponent<RecentContextsProps, RecentContextsState> {
  constructor(props: {}) {
    super(props);
    this.state = { perspective: undefined, recentContexts: [] };
  }

  componentDidMount() {
    const component = this;
    PDRproxy.then((PDRproxy) => {
      this.addUnsubscriber(
        PDRproxy.getPerspective( this.props.systemuser, ModelDependencies.recentContexts , (perspectives: Perspective[]) =>
          component.setState({ perspective: perspectives[0] })
        ));
      this.addUnsubscriber(
        PDRproxy.getRol( this.props.systemIdentifier, ModelDependencies.actualRecentContexts, recentContexts => 
          component.setState({ recentContexts: recentContexts })
        ));
      });
  }

  componentDidUpdate(prevProps: RecentContextsProps) {
    const component = this;
    if (this.props.openContext && this.props.openContext !== prevProps.openContext) {
      // If the openContext changes, we must check whether it is in the RecentContexts.
      // TODO: je moet de bindingen van de RecentContexts ophalen en kijken of de openContext er al in zit.
      if ( this.state.recentContexts.find( context => context === this.props.openContext ) === undefined ) {
        // The openContext is not in the RecentContexts, add it.
        PDRproxy.then( pproxy => {
          pproxy.bind( deconstructContext( component.props.systemIdentifier as string ) as ContextInstanceT,
            ModelDependencies.recentContexts,
            ModelDependencies.system,
            { properties: { [ModelDependencies.lastShownOnScreen] : [new Date().valueOf().toString() as ValueT] } 
            , binding: this.props.openContext
            },
            ModelDependencies.sysUser,
          )});
        }      
      else {
        // It is in the RecentContexts, update its time stamp.
        PDRproxy.then( pproxy => {
          pproxy.getRoleBinders( this.props.openContext!, ModelDependencies.system, ModelDependencies.recentContexts, (binders: RoleInstanceT[]) => {
            const recentContext = binders[0];
            pproxy.setProperty( recentContext, ModelDependencies.lastShownOnScreen, new Date().valueOf().toString() as ValueT, ModelDependencies.sysUser );
          }, FIREANDFORGET);
        })
      }
    }
  }

  render() {
    if (this.state.perspective === undefined) {
      return null;
    }
    else {
      // We have a perspective, produce a form.
      return <PerspectiveTable perspective={this.state.perspective} showcontrolsandcaption={false}/>;
    }
  }
}
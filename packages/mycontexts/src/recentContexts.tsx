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
}

export class RecentContexts extends PerspectivesComponent<RecentContextsProps, RecentContextsState> {
  constructor(props: {}) {
    super(props);
    this.state = { perspective: undefined};
  }

  componentDidMount() {
    const component = this;
    PDRproxy.then((PDRproxy) => {
      this.addUnsubscriber(
        PDRproxy.getPerspectiveForUser( this.props.systemuser, ModelDependencies.recentContexts, ModelDependencies.WWWUser
          ,(perspectives: Perspective[]) =>
              component.setState({ perspective: perspectives[0] })
          , false
          , (error: String) => {
            // The most likely error is that some recent context can no longer be found.
            // We mitigate this problem by removing the recent contexts.
            PDRproxy.deleteRole(
              component.props.systemIdentifier
              , ModelDependencies.recentContexts
              , ModelDependencies.WWWUser)
          }));
      });
  }

  componentDidUpdate(prevProps: RecentContextsProps) {
    const component = this;
    if (this.props.openContext && this.props.openContext !== prevProps.openContext && this.state.perspective) {
      // If the openContext changes, we must check whether it is in the RecentContexts.
      // TODO: je moet de bindingen van de RecentContexts ophalen en kijken of de openContext er al in zit.
      if ( Object.values( this.state.perspective!.roleInstances ).find( ({filler}) => filler ? filler === this.props.openContext : false ) === undefined ) {
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
            if ( recentContext ) {
            pproxy.setProperty( recentContext, ModelDependencies.lastShownOnScreen, new Date().valueOf().toString() as ValueT, ModelDependencies.sysUser );
            }
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
      // We have a perspective, produce a table.
      return <PerspectiveTable 
              perspective={this.state.perspective} 
              showcontrolsandcaption={false}
              showAsAccordionItem={true}
              sortOnHiddenProperty={ModelDependencies.lastShownOnScreen}
              sortAscending={false}
              />;
    }
  }
}
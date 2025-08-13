import React, { } from "react";
import { PDRproxy, RoleInstanceT, ContextInstanceT, Perspective, ValueT } from "perspectives-proxy";
import type { PerspectivesProxy, Unsubscriber } from "perspectives-proxy";
import { deconstructContext, externalRole, ModelDependencies, PerspectivesComponent, PerspectiveTable } from "perspectives-react";

interface RecentContextsProps {
  systemuser: RoleInstanceT;
  systemIdentifier: ContextInstanceT;
  openContext: RoleInstanceT | undefined;
}

interface RecentContextsState {
  perspective: Perspective | undefined;
}

export class RecentContexts extends PerspectivesComponent<RecentContextsProps, RecentContextsState> {
  constructor(props: RecentContextsProps) {
    super(props);
    this.state = { perspective: undefined };
  }

  // Maximum number of recent contexts to keep and show.
  private static readonly MAX_RECENT = 8;

  private HistoryUnsubscriber : Promise<Unsubscriber> | undefined;

  // Returns roleIds sorted by lastShownOnScreen (desc), missing treated as 0.
  private sortRoleIdsByLastShown(p: Perspective): RoleInstanceT[] {
    const prop = ModelDependencies.lastShownOnScreen;
    return Object.values(p.roleInstances)
      .sort((a, b) => {
        const va = parseInt(a.propertyValues[prop]?.values?.[0] ?? "0", 10);
        const vb = parseInt(b.propertyValues[prop]?.values?.[0] ?? "0", 10);
        return vb - va; // descending
      })
      .map(ri => ri.roleId as RoleInstanceT);
  }

  // Build a new perspective with only the top N roleInstances; also return overflow ids.
  private trimPerspective(p: Perspective, maxHistoryLength: number): { trimmed: Perspective; overflow: RoleInstanceT[] } {
    const orderedIds = this.sortRoleIdsByLastShown(p);
    const keepIds = new Set(orderedIds.slice(0, maxHistoryLength));
    const overflow = orderedIds.slice(maxHistoryLength);
    const trimmedRoleInstances = Object.fromEntries(
      Object.entries(p.roleInstances).filter(([rid]) => keepIds.has(rid as RoleInstanceT))
    ) as Perspective["roleInstances"];
    const trimmed: Perspective = { ...p, roleInstances: trimmedRoleInstances };
    return { trimmed, overflow };
  }

  componentDidMount() {
    const component = this;
    PDRproxy.then((pproxy: PerspectivesProxy) => {
      this.addUnsubscriber(
        pproxy.getProperty( 
          externalRole (component.props.systemIdentifier as string),
          ModelDependencies.maxHistoryItems,
          ModelDependencies.systemExternal,
          (maxItems_: string[]) => {
            if (maxItems_[0]) {
              if (component.HistoryUnsubscriber) {
                component.HistoryUnsubscriber.then( unsub => pproxy.send(unsub, function(){}));
                component.subscribeToHistory(parseInt(maxItems_[0]));
              }
              else {
                component.subscribeToHistory(parseInt(maxItems_[0]))
              }}
              else {
                component.subscribeToHistory(RecentContexts.MAX_RECENT)
              }
            }))
          }
        );
      };

  subscribeToHistory(maxItems: number) {
    const component = this;
    PDRproxy.then((pproxy: PerspectivesProxy) => {
      component.HistoryUnsubscriber = 
        pproxy.getPerspectiveForUser(this.props.systemuser, ModelDependencies.recentContexts, ModelDependencies.WWWUser
          , (perspectives: Perspective[]) => {
            const p = perspectives[0];
              if (!p) { component.setState({ perspective: undefined }); return; }
              // Trim to max and prune overflow from the primary store.
              const { trimmed, overflow } = component.trimPerspective(p, maxItems);
              component.setState({ perspective: trimmed });
              if (overflow.length > 0) {
                // Fire-and-forget deletions; any errors will surface via the subscription.
                overflow.forEach(rid => {
                  pproxy.removeRole( ModelDependencies.recentContexts, rid, ModelDependencies.sysUser );
                });
              }
          }
        );
    });
  }

  componentDidUpdate(prevProps: RecentContextsProps) {
    const component = this;
    if (this.props.openContext && this.props.openContext !== prevProps.openContext && this.state.perspective) {
      // If the openContext changes, we must check whether it is in the RecentContexts.
      const roleWithContext = Object.values( this.state.perspective!.roleInstances ).find( ({filler}) => filler ? filler === this.props.openContext : false );
      if ( roleWithContext === undefined ) {
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
          pproxy.setProperty( roleWithContext.roleId, ModelDependencies.lastShownOnScreen, new Date().valueOf().toString() as ValueT, ModelDependencies.sysUser );
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
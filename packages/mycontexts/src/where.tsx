import * as React from "react";
const { Component } = React;
import { ContextType, RoleInstanceT, ContextInstanceT, WhereTo, PDRproxy, ContextAndName } from "perspectives-proxy";
import type { ModelContextGraph } from "perspectives-proxy";
import { TableForms } from "./tableForms";
import { PinnedContexts } from "./pinnedContexts";
import { RecentContexts } from "./recentContexts";
import { Accordion } from "react-bootstrap";
import { buildMarkDown, PSContext, PerspectivesComponent } from "perspectives-react";
import { WiderContexts } from "./widerContexts";
import { NavigationGraphView } from "./NavigationGraphView";
import type { WiderContext } from "./navigationGraph";

interface WhereProps {
  screenelements: WhereTo;
  showTablesAndForm: boolean;
  systemUser: RoleInstanceT;
  systemIdentifier: ContextInstanceT;
  openContext: RoleInstanceT | undefined;
  currentContextType?: ContextType;
  currentContextTitle?: string;
}

interface WhereState {
  accordionOpen: string[];
  modelGraph: ModelContextGraph | undefined;
  widerContexts: WiderContext[];
}

export class Where extends PerspectivesComponent<WhereProps, WhereState> {
  ref: React.RefObject<HTMLDivElement | null>;
  private subscriptionGeneration = 0;
  private subscriptionRestart: Promise<void> = Promise.resolve();

  constructor(props: WhereProps) {
    super(props);
    this.ref = React.createRef();
    this.state = {
      accordionOpen: [],
      modelGraph: undefined,
      widerContexts: [],
    };
  }

  componentDidMount() {
    const component = this;
    if (this.ref.current) {
      this.ref.current.addEventListener(
        'OpenContext',
        (_e: CustomEvent) => {
          component.setState({ accordionOpen: [] });
        },
        false
      );
      this.ref.current.addEventListener(
        'OpenAccordionItem',
        (e: CustomEvent) => {
          if (component.state.accordionOpen.indexOf(e.detail) === -1) {
            component.setState({ accordionOpen: [e.detail] });
          } else {
            component.setState({ accordionOpen: [] });
          }
        },
        false
      );
    }
    this.restartSubscriptions();
  }

  componentDidUpdate(prevProps: Readonly<WhereProps>) {
    if (
      this.props.currentContextType !== prevProps.currentContextType ||
      this.props.openContext !== prevProps.openContext
    ) {
      this.restartSubscriptions();
    }
  }

  componentWillUnmount() {
    this.subscriptionGeneration += 1;
    super.componentWillUnmount();
  }

  restartSubscriptions() {
    const generation = ++this.subscriptionGeneration;
    this.setState({ modelGraph: undefined, widerContexts: [] });
    this.subscriptionRestart = this.subscriptionRestart
      .then(() => this.unsubscribeAll())
      .then(() => {
        if (!this.__mounted__ || generation !== this.subscriptionGeneration) {
          return;
        }
        return this.subscribeAll(generation);
      });
  }

  subscribeAll(generation: number): Promise<void> {
    return Promise.all([
      this.subscribeToGraph(generation),
      this.subscribeToWiderContexts(generation),
    ]).then(() => undefined);
  }

  subscribeToGraph(generation: number): Promise<void> {
    const component = this;
    if (!this.props.currentContextType) {
      this.setState({ modelGraph: undefined });
      return Promise.resolve();
    }
    const contextType = this.props.currentContextType;
    return PDRproxy.then((pdr) =>
      pdr.getModelContextGraph(contextType).then((graph) => {
        if (generation === component.subscriptionGeneration) {
          component.setState({ modelGraph: graph });
        }
      })
    );
  }

  subscribeToWiderContexts(generation: number): Promise<void> {
    const component = this;
    if (!this.props.openContext) {
      this.setState({ widerContexts: [] });
      return Promise.resolve();
    }
    const openContext = this.props.openContext;
    return PDRproxy.then((pdr) =>
      component.addUnsubscriber(
        pdr.getWiderContexts(
          openContext,
          (contextAndNames: ContextAndName[]) => {
            if (generation !== component.subscriptionGeneration) return;
            // Initialise with unresolved types, then resolve each asynchronously.
            const wcs: WiderContext[] = contextAndNames.map((ca) => ({
              externalRole: ca.externalRole,
              readableName: ca.readableName,
              contextType: undefined,
            }));
            component.setState({ widerContexts: wcs });
            wcs.forEach((wc, idx) => {
              // Derive context instance ID from external role instance ID.
              const contextId = (wc.externalRole as string).replace(/\$External$/, "");
              pdr.getContextType(contextId).then((ctxType) => {
                if (generation !== component.subscriptionGeneration) return;
                component.setState((prev) => {
                  const updated = [...prev.widerContexts];
                  if (
                    updated[idx] &&
                    updated[idx].externalRole === wc.externalRole
                  ) {
                    updated[idx] = { ...updated[idx], contextType: ctxType };
                  }
                  return { widerContexts: updated };
                });
              }).catch(() => {
                // Leave contextType undefined if resolution fails.
              });
            });
          }
        )
      )
    );
  }

  render() {
    const component = this;
    const { modelGraph, widerContexts } = this.state;
    return (
      <PSContext.Consumer>
        {(value) => (
          <div className="content-top-aligned px-0" ref={this.ref}>
            {this.props.screenelements.markdown.map((markdown, index) => (
              <div key={index} className="markdown">
                {buildMarkDown(value.contextinstance, value.myroletype, markdown)}
              </div>
            ))}
            <div className="markdown">
              <WiderContexts externalrole={component.props.openContext} />
              <Accordion activeKey={this.state.accordionOpen} flush className="pb-3">
                <PinnedContexts systemuser={this.props.systemUser} />
                <RecentContexts
                  systemuser={this.props.systemUser}
                  openContext={this.props.openContext}
                  systemIdentifier={this.props.systemIdentifier}
                />
              </Accordion>
            </div>
            {this.props.screenelements.contextRoles.length > 0 ? (
              <div>
                <TableForms
                  screenelements={this.props.screenelements.contextRoles}
                  showTablesAndForm={this.props.showTablesAndForm}
                />
              </div>
            ) : null}
            {this.props.currentContextType ? (
              <NavigationGraphView
                modelGraph={modelGraph}
                currentContextType={this.props.currentContextType}
                currentContextTitle={this.props.currentContextTitle}
                contextRoles={this.props.screenelements.contextRoles}
                widerContexts={widerContexts}
                hostRef={this.ref}
              />
            ) : null}
          </div>
        )}
      </PSContext.Consumer>
    );
  }
}

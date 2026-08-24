import * as React from "react";
const { Component } = React;
import { ContextType, RoleInstanceT, ContextInstanceT, WhereTo, PDRproxy } from "perspectives-proxy";
import type { ModelContextGraph, WiderContext } from "perspectives-proxy";
import { TableForms } from "./tableForms";
import { PinnedContexts } from "./pinnedContexts";
import { RecentContexts } from "./recentContexts";
import { Accordion } from "react-bootstrap";
import { buildMarkDown, PSContext, PerspectivesComponent } from "perspectives-react";
import { WiderContexts } from "./widerContexts";
import { NavigationGraphView } from "./NavigationGraphView";

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
  private graphGeneration = 0;
  private widerContextsGeneration = 0;
  private widerContextsRestart: Promise<void> = Promise.resolve();

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
    this.refreshModelGraph();
    this.restartWiderContextsSubscription();
  }

  componentDidUpdate(prevProps: Readonly<WhereProps>) {
    if (this.props.currentContextType !== prevProps.currentContextType) {
      this.refreshModelGraph();
    }
    if (this.props.openContext !== prevProps.openContext) {
      this.restartWiderContextsSubscription();
    }
  }

  componentWillUnmount() {
    this.graphGeneration += 1;
    this.widerContextsGeneration += 1;
    super.componentWillUnmount();
  }

  restartWiderContextsSubscription() {
    const generation = ++this.widerContextsGeneration;
    this.widerContextsRestart = this.widerContextsRestart
      .catch(() => undefined)
      .then(() => this.unsubscribeAll())
      .then(() => {
        if (!this.__mounted__ || generation !== this.widerContextsGeneration) {
          return;
        }
        return this.subscribeToWiderContexts(generation);
      })
      .catch(() => undefined);
  }

  refreshModelGraph(): Promise<void> {
    const generation = ++this.graphGeneration;
    const component = this;
    if (!this.props.currentContextType) {
      this.setState((previousState) =>
        previousState.modelGraph === undefined
          ? null
          : { modelGraph: undefined }
      );
      return Promise.resolve();
    }
    const contextType = this.props.currentContextType;
    return PDRproxy.then((pdr) =>
      pdr.getModelContextGraph(contextType).then((graph) => {
        if (generation === component.graphGeneration) {
          component.setState({ modelGraph: graph });
        }
      })
    );
  }

  subscribeToWiderContexts(generation: number): Promise<void> {
    const component = this;
    if (!this.props.openContext) {
      this.setState((previousState) =>
        previousState.widerContexts.length === 0
          ? null
          : { widerContexts: [] }
      );
      return Promise.resolve();
    }
    const openContext = this.props.openContext;
    return PDRproxy.then((pdr) =>
      component.addUnsubscriber(
        pdr.getWiderContexts(
          openContext,
          (contextAndNames: WiderContext[]) => {
            if (generation !== component.widerContextsGeneration) {
              return;
            }
            component.setState((previousState) => {
              const unchanged =
                previousState.widerContexts.length === contextAndNames.length &&
                contextAndNames.every((widerContext, index) => {
                  const previous = previousState.widerContexts[index];
                  return (
                    previous?.externalRole === widerContext.externalRole &&
                    previous.readableName === widerContext.readableName &&
                    previous.contextType === widerContext.contextType
                  );
                });
              return unchanged ? null : { widerContexts: contextAndNames };
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

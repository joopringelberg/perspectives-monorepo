import * as React from "react";
const { Component } = React;
import { ContextType, RoleInstanceT, ContextInstanceT, WhereTo, WhoWhatWhereScreenDef } from "perspectives-proxy";
import { TableForms } from "./tableForms";
import { PinnedContexts } from "./pinnedContexts";
import { RecentContexts } from "./recentContexts";
import { Accordion } from "react-bootstrap";
import { buildMarkDown, PSContext } from "perspectives-react";
import { WiderContexts } from "./widerContexts";
import { NavigationGraphView } from "./NavigationGraphView";

interface WhereProps {
  screenelements: WhereTo;
  showTablesAndForm: boolean;
  systemUser: RoleInstanceT;
  systemIdentifier: ContextInstanceT;
  openContext: RoleInstanceT | undefined;
  /** Full screen definition used to populate the navigation graph. */
  whoWhatWhereScreen?: WhoWhatWhereScreenDef;
  /** The context type currently open (graph highlight). */
  currentContextType?: ContextType;
  /** Human-readable label for the current context type. */
  currentContextLabel?: string;
}

interface WhereState {
  accordionOpen: string[];
}

export class Where extends Component<WhereProps, WhereState> {
  ref: React.RefObject<HTMLDivElement | null>;

  constructor(props: WhereProps) {
    super(props);
    this.ref = React.createRef();
    this.state = { accordionOpen: [] };
  }

  componentDidMount() {
    const component = this;
    if (this.ref.current) {
      this.ref.current.addEventListener(
        'OpenContext', 
        (e : CustomEvent) => {
          component.setState({accordionOpen: []});  
        }, 
        false);
      this.ref.current.addEventListener(
        'OpenAccordionItem',
        (e : CustomEvent) => {
          if (component.state.accordionOpen.indexOf( e.detail ) === -1) {
            component.setState({accordionOpen: [e.detail]});
          }
          else {
            component.setState({accordionOpen: []});
          }
        },
        false)
    }
  }

  render() {
    const component = this;
    const { whoWhatWhereScreen, currentContextType, currentContextLabel } = this.props;
    return (<PSContext.Consumer>{ value => 
    (<div className="content-top-aligned px-0" ref={this.ref}>
      {this.props.screenelements.markdown.map((markdown, index) => 
          <div key={index} className="markdown">{ buildMarkDown(value.contextinstance, value.myroletype, markdown) }</div>
        )}
      <div className="markdown">
        <WiderContexts externalrole={component.props.openContext}/>
        <Accordion activeKey={this.state.accordionOpen} flush className="pb-3">
          <PinnedContexts systemuser={this.props.systemUser} />
          <RecentContexts systemuser={this.props.systemUser} openContext={this.props.openContext} systemIdentifier={this.props.systemIdentifier}/>
        </Accordion>
      </div>
      {
        this.props.screenelements.contextRoles.length > 0 ?
        <div>
          <TableForms screenelements={this.props.screenelements.contextRoles} showTablesAndForm={this.props.showTablesAndForm} />
        </div>
        : null
      }
      { whoWhatWhereScreen && currentContextType ? (
        <NavigationGraphView
          currentContextType={currentContextType}
          currentContextLabel={currentContextLabel ?? ""}
          currentScreen={whoWhatWhereScreen}
          hostRef={this.ref}
        />
      ) : null }
    </div>)
    }</PSContext.Consumer>);
  }
}

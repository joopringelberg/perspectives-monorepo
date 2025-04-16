import React, { } from "react";
import { PDRproxy, RoleInstanceT, Perspective } from "perspectives-proxy";
import { ModelDependencies, PerspectivesComponent, PerspectiveTable } from "perspectives-react";

interface PinnedContextsProps {
  systemuser: RoleInstanceT;
}

interface PinnedContextsState {
  perspective: Perspective | undefined;
}

export class PinnedContexts extends PerspectivesComponent<PinnedContextsProps, PinnedContextsState> {
  constructor(props: {}) {
    super(props);
    this.state = { perspective: undefined };
  }

  componentDidMount() {
    const component = this;
    PDRproxy.then((PDRproxy) => {
      PDRproxy.getPerspective( this.props.systemuser, ModelDependencies.pinnedContexts , (perspectives: Perspective[]) => {
        component.setState({ perspective: perspectives[0] });
      });
    })
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
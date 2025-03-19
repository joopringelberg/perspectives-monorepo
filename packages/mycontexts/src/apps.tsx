import React, { } from "react";
import { PDRproxy, RoleInstanceT, Perspective, PropertyType } from "perspectives-proxy";
import { ModelDependencies, PerspectiveBasedForm, PerspectivesComponent, PerspectiveTable } from "perspectives-react";

interface AppsProps {
  systemuser: RoleInstanceT;
}

interface AppsState {
  perspective: Perspective | undefined;
}

export class Apps extends PerspectivesComponent<AppsProps, AppsState> {
  constructor(props: {}) {
    super(props);
    this.state = { perspective: undefined
    };
  }

  componentDidMount() {
    const component = this;
    PDRproxy.then((PDRproxy) => {
      PDRproxy.getPerspective( this.props.systemuser, ModelDependencies.startContexts , (perspectives: Perspective[]) => {
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
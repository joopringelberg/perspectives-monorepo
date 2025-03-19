import React, { } from "react";
import { PDRproxy, RoleInstanceT, Perspective, PropertyType } from "perspectives-proxy";
import { ModelDependencies, PerspectiveBasedForm, PerspectivesComponent } from "perspectives-react";

interface MeProps {
  systemuser: RoleInstanceT;
}

interface MeState {
  perspective: Perspective | undefined;
}

export class Me extends PerspectivesComponent<MeProps, MeState> {
  constructor(props: {}) {
    super(props);
    this.state = { perspective: undefined
    };
  }

  componentDidMount() {
    const component = this;
    PDRproxy.then((PDRproxy) => {
      PDRproxy.getPerspective( this.props.systemuser, ModelDependencies.sysUser, (perspectives: Perspective[]) => {
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
      return <PerspectiveBasedForm perspective={this.state.perspective} cardtitle={"" as PropertyType} showControls={false}/>;
    }
  }
}
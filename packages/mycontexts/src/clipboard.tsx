import React from "react";
import { ModelDependencies, PerspectivesComponent, PerspectiveTable } from "perspectives-react";
import { PDRproxy, RoleInstanceT, Perspective, PropertyType } from "perspectives-proxy";

interface ClipboardProps {
  systemuser: RoleInstanceT;
}

interface ClipboardState {
  perspective: Perspective | undefined;
}
export class Clipboard extends PerspectivesComponent<ClipboardProps, ClipboardState> {
  constructor(props: {}) {
    super(props);
    this.state = { perspective: undefined };
  }

  componentDidMount(): void {
    const component = this;
    PDRproxy.then( pproxy => {
      pproxy.getPerspective( this.props.systemuser, ModelDependencies.itemsOnClipboard , (perspectives: Perspective[]) => {
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
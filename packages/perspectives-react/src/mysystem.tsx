import { Component, JSX } from "react";
import {ContextInstanceT, PDRproxy, ContextType, RoleType} from "perspectives-proxy";

import PerspectivesComponent from "./perspectivesComponent";
import {PSContext} from "./reactcontexts.js";
import { default as ModelDependencies } from "./modelDependencies.js";

interface MySystemState {
  contextinstance: ContextInstanceT;
  contexttype: ContextType;
  myroletype: RoleType;
}

interface MySystemProps {
  children: React.ReactNode;
}

export default class MySystem extends Component<MySystemProps, MySystemState>
{
  constructor (props: MySystemProps)
  {
    super(props);
    this.state = {
      contextinstance: "" as ContextInstanceT,
      contexttype: ModelDependencies.system,
      myroletype:  ModelDependencies.sysUser
    };
  }

  componentDidMount ()
  {
    const component = this;
    PDRproxy.then(
      function (pproxy)
      {
        pproxy.getSystemIdentifier()
          .then(
            function(sysId)
            {
              component.setState({contextinstance: "def:#" + sysId as ContextInstanceT});
            });
      });
  }

  render () : JSX.Element
  {
    const component = this;
    if (component.state.contextinstance !== "")
    {
      return (<PSContext.Provider value={component.state}>
        {component.props.children}
      </PSContext.Provider>);
    }
    else {
      return <div/>;
    }
  }
}

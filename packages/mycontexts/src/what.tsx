import * as React from "react";
import { MainScreenElements, MarkDownElementDef, TableFormDef, What as WhatDef } from "perspectives-proxy";
import { buildMarkDown, FreeFormScreen, PerspectivesComponent, PSContext } from "perspectives-react";
import { TableForms } from "./tableForms";

interface WhatProps {
  screenelements: WhatDef;
  showTablesAndForm: boolean;
}

export class What extends PerspectivesComponent<WhatProps>{
  
  constructor(props: WhatProps) {
    super(props);
  }

  render() {
    const component = this;
    let element;
    return (<PSContext.Consumer>{
      context => {
        switch (this.props.screenelements.tag) {
          case "TableForms":
            element = this.props.screenelements.elements as unknown as {markdown: MarkDownElementDef[], tableForms: TableFormDef[]};
            return <div className="content-top-aligned px-0">
                {element.markdown.map((markdown, index) => 
                  <div key={index} className="markdown">{ buildMarkDown(context.contextinstance, context.myroletype, markdown) }</div>
                  )}
                <TableForms screenelements={element.tableForms} showTablesAndForm={component.props.showTablesAndForm} doubleclickOpensDetails={false}/>
              </div>;
          case "FreeFormScreen":
            return  <div className="content-top-aligned">
                      <FreeFormScreen 
                        screen={component.props.screenelements.elements as MainScreenElements}
                        contextinstance={context.contextinstance}
                        contexttype={context.contexttype}
                        myroletype={context.myroletype}
                      />
                    </div>;
        }}
    
      }
    </PSContext.Consumer>);
  }
}

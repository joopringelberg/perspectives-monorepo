import * as React from "react";
const { Component } = React;
import { FormElementDef, TableFormDef } from "perspectives-proxy";
import { buildForm, buildMarkDown, buildTable } from "perspectives-react";
import { Accordion } from "react-bootstrap";
import MSComponent, { SlidingPanelContentProps } from "./mscomponent";

interface TableFormsProps {
  screenelements: TableFormDef[];
  showTablesAndForm: boolean;
  doubleclickOpensDetails: boolean;

}

export class TableForms extends Component<TableFormsProps> {
  render() {
    const forms = this.props.screenelements.map(({ form }) => form);
    if (forms.length > 0) {
      return (
        <MSComponent 
          isMobile={!this.props.showTablesAndForm} 
          className='bg-light-subtle' 
          doubleclickOpensDetails={this.props.doubleclickOpensDetails}
        >
          <Accordion defaultActiveKey="0" flush>
            {
            this.props.screenelements.map(({markdown, table, form}, index) => {
              const contextinstance = table.widgetCommonFields.perspective.contextInstance;
              const myroletype = table.widgetCommonFields.perspective.userRoleType;
              // No TableControls, show as Accordion item.
              return (
                <div key={index} className="markdown">
                  { markdown.map( (md, index) => <div key={index}>{ buildMarkDown( contextinstance, myroletype, md) }</div>) }
                  { buildTable(table, false, true, true) }
                </div>);
              })
            }
          </Accordion>
          <SelectedForm forms={forms} />
      </MSComponent>)
    }
  }
}

interface SelectedFormProps extends SlidingPanelContentProps {
  forms: FormElementDef[];
}

const SelectedForm: React.FC<SelectedFormProps> = ({ forms, selectedRoleInstance, selectedRoleType }) => {
  if (selectedRoleType && selectedRoleInstance)
  {
    const theForm = forms.find(form => form.widgetCommonFields.perspective.roleType === selectedRoleType);
    if (theForm === undefined) {
      // console.error(`No form found for role type ${selectedRoleType}`);
      return null;
    } else {
      return <>
        <h3 className="column-heading mt-2">{theForm.widgetCommonFields.perspective.displayName}</h3>
        { buildForm(theForm, false, selectedRoleInstance) }
        </>;
    }
  }
};

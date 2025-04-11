import React from "react";
import { FormElementDef, RoleInstanceT, TableFormDef } from "perspectives-proxy";
import { buildForm, buildTable } from "perspectives-react";
import { Component } from "react";
import { Accordion, Container, Row } from "react-bootstrap";
import MSComponent, { SlidingPanelContentProps } from "./mscomponent";

interface TableFormsProps {
  screenelements: TableFormDef[];
  showTablesAndForm: boolean;
  doubleclickOpensDetails: boolean;

}

export class TableForms extends Component<TableFormsProps> {
  render() {
    const forms = this.props.screenelements.map(({ form }) => form);
    return (
      <MSComponent isMobile={!this.props.showTablesAndForm} className='bg-light-subtle' doubleclickOpensDetails={this.props.doubleclickOpensDetails}>
        <Accordion defaultActiveKey="0" flush>
          {
          this.props.screenelements.map(({table, form}, index) => {
            // No TableControls, show as Accordion item.
            return buildTable(table, false, true);
            })
          }
        </Accordion>
        <SelectedForm forms={forms} />
    </MSComponent>)
  }
}

interface SelectedFormProps extends SlidingPanelContentProps {
  forms: FormElementDef[];
}

const SelectedForm: React.FC<SelectedFormProps> = ({ forms, selectedRoleInstance, selectedRoleType }) => {
  if (selectedRoleType && selectedRoleInstance)
  {
    const theForm = forms.find(form => form.fields.perspective.roleType === selectedRoleType);
    if (theForm === undefined) {
      // console.error(`No form found for role type ${selectedRoleType}`);
      return null;
    } else {
      return buildForm(theForm, false, selectedRoleInstance);
    }
  }
};

import React from "react";
import { FormElementDef, RoleInstanceT, TableFormDef } from "perspectives-proxy";
import { buildForm, buildTable } from "perspectives-react";
import { Component } from "react";
import { Container, Row } from "react-bootstrap";
import MSComponent, { SlidingPanelContentProps } from "./mscomponent";

interface WhoProps {
  screenelements: TableFormDef[];
  showTablesAndForm: boolean;
}

interface WhoState {
  selectedRoleInstance: RoleInstanceT | undefined;
}

export class Who extends Component<WhoProps> {
  render() {
    const forms = this.props.screenelements.map(({ form }) => form);
    return (
      <MSComponent isMobile={!this.props.showTablesAndForm} className='bg-light-subtle'>
        <Container fluid>
          {this.props.screenelements.map(({table, form}, index) => {
            return <Row className="pb-3" key={table.fields.perspective.id}>{buildTable(table, false)}</Row>;
          }
          )}
        </Container>
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
      console.error(`No form found for role type ${selectedRoleType}`);
      return null;
    } else {
      return buildForm(theForm, false, selectedRoleInstance);
    }
  }
};
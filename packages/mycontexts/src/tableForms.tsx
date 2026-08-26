import * as React from "react";
const { Component } = React;
import { FormElementDef, Perspective, TableFormDef } from "perspectives-proxy";
import { buildForm, buildMarkDown, buildTable } from "perspectives-react";
import { Accordion } from "react-bootstrap";
import MSComponent, { SlidingPanelContentProps } from "./mscomponent";
import { HelpModeContext } from "./helpTypes";

interface TableFormsProps {
  screenelements: TableFormDef[];
  showTablesAndForm: boolean;
}

export class TableForms extends Component<TableFormsProps> {
  render() {
    const forms = this.props.screenelements.map(({ form }) => form);
    if (forms.length > 0) {
      return (
        <MSComponent 
          isMobile={!this.props.showTablesAndForm} 
          className='bg-light-subtle'
        >
          <HelpModeContext.Consumer>
            {help => (
              <Accordion defaultActiveKey="0" flush>
                {this.props.screenelements.map(({markdown, table}, index) => {
                  const perspective = table.widgetCommonFields.perspective;
                  const openRoleHelp = (targetPerspective: Perspective, anchor: HTMLElement) => help.openHelp({
                    target: {
                      kind: 'role',
                      contextInstance: targetPerspective.contextInstance,
                      contextType: targetPerspective.contextType,
                      userRoleType: targetPerspective.userRoleType,
                      roleType: targetPerspective.roleType,
                      perspectiveId: targetPerspective.id,
                      label: targetPerspective.displayName,
                    },
                    anchorRect: anchor.getBoundingClientRect(),
                    triggerElement: anchor,
                  });

                  return (
                    <div key={index} className="markdown">
                      {markdown.map((md, markdownIndex) => <div key={markdownIndex}>{buildMarkDown(perspective.contextInstance, perspective.userRoleType, md)}</div>)}
                      {buildTable(table, false, true, this.props.showTablesAndForm, help.active, openRoleHelp)}
                    </div>
                  );
                })}
              </Accordion>
            )}
          </HelpModeContext.Consumer>
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

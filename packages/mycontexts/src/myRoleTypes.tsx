// BEGIN LICENSE
// Perspectives Distributed Runtime
// Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//
// Full text of this license can be found in the LICENSE file in the projects root.
// END LICENSE


import { PDRproxy, RoleInstanceT, UserRoleType, RoleType } from 'perspectives-proxy';
import {deconstructLocalName, AppContext, UserMessagingPromise, PerspectivesComponent} from "perspectives-react";
import i18next from "i18next";
import { Table } from "react-bootstrap"; 

interface MyRoleTypesProps
{
  externalRoleId: RoleInstanceT;
  title: string;
  currentroletype: RoleType
}

interface MyRoleTypesState
{
  myRoleTypes: Record<UserRoleType, string>;
}

export class MyRoleTypes extends PerspectivesComponent<MyRoleTypesProps, MyRoleTypesState>
{
  constructor( props: MyRoleTypesProps )
  {
    super( props);
    this.state = {myRoleTypes: {}};
  }

  componentDidMount()
  {
    const component = this;
    PDRproxy.then(
      function( pproxy )
      {
        pproxy.getAllMyRoleTypes( component.props.externalRoleId)
          .then( myRoleTypes => component.setState({myRoleTypes}))
          .catch(e => UserMessagingPromise.then( um => 
            um.addMessageForEndUser(
              { title: i18next.t("app_myroletypes_title", { ns: 'mycontexts' }) 
              , message: i18next.t("app_myroletypes_message", {context: component.props.title, ns: 'mycontexts'})
              , error: e.toString()
            })));
      }
    );
  }

  handleSelect(event: React.MouseEvent<HTMLTableRowElement>, userRoleType: UserRoleType)
  {
    const component = this;

      PDRproxy
        .then( pproxy => pproxy.setPreferredUserRoleType( component.props.externalRoleId, userRoleType) )
        .catch(e => UserMessagingPromise.then( um => 
          um.addMessageForEndUser(
            { title: i18next.t("setMyRoleTypes_title", { ns: 'mycontexts' }) 
            , message: i18next.t("setMyRoleTypes_message", {preferredType: userRoleType, ns: 'mycontexts'})
            , error: e.toString()
          })));
  }

  // Reset the user role types when we've got another role.
  componentDidUpdate(prevProps : MyRoleTypesProps )
  {
    const component = this;
    if (component.props.externalRoleId !== prevProps.externalRoleId )
    {
      PDRproxy.then(
        function( pproxy )
        {
          pproxy.getAllMyRoleTypes( component.props.externalRoleId)
            .then( myRoleTypes => component.setState({myRoleTypes}))
            .catch(e => UserMessagingPromise.then( um => 
              um.addMessageForEndUser(
                { title: i18next.t("app_myroletypes_title", { ns: 'mycontexts' }) 
                , message: i18next.t("app_myroletypes_message", {context: component.props.title, ns: 'mycontexts'})
                , error: e.toString()
              })));
        }
      );
    }
  }

  render()
  {
    const component = this;
    // build a table.
    // Add an onclickhandler to the table. Handle it to set the current user role type.
    return  <Table striped bordered hover>
              <thead>
                <tr>
                  <th>Choose role type</th>
                </tr>
              </thead>
              <tbody>
                {
                  Object.keys( component.state.myRoleTypes ).map(
                    function(r: string)
                    {
                      const rt = r as UserRoleType;
                      return  <tr key={rt} onClick={e => component.handleSelect(e, rt)}>
                                <td>{ component.state.myRoleTypes[rt] } { rt === component.props.currentroletype ? <i className="bi bi-check text-success"></i> : null }</td>
                              </tr>;
                    })
                }
              </tbody>
            </Table>
  }
}

MyRoleTypes.contextType = AppContext;


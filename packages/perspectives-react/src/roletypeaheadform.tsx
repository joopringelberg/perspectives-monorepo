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

import React from 'react';
import { FilterValueEntry, Perspective, RoleInstanceT } from 'perspectives-proxy';
import PerspectivesComponent from './perspectivesComponent';
import i18next from 'i18next';
import { Form, ListGroup } from 'react-bootstrap';
import PerspectiveBasedForm from './perspectivebasedform.js';

////////////////////////////////////////////////////////////////////////////////
// ROLETYPEAHEADFORM
// A typeahead search widget that lets the user pick a candidate role instance
// (from the FilterValue view) and then presents that instance in a form.
// Designed for inspecting a single instance from a large candidate list.
//
// Props:
//  - perspective : Perspective – the serialised perspective on the role to present
//  - candidates  : FilterValueEntry[] – pre-fetched {filterValue, roleId} entries
//  - title       : string | undefined – optional label for the widget
//  - fieldConstraints: any[] – per-property display constraints
////////////////////////////////////////////////////////////////////////////////

interface RoleTypeAheadFormProps {
  perspective: Perspective;
  candidates: FilterValueEntry[];
  title?: string;
  fieldConstraints?: any[];
}

interface RoleTypeAheadFormState {
  query: string;
  isOpen: boolean;
  selectedRoleId: RoleInstanceT | null;
}

const MAX_VISIBLE = 20;
// Delay (ms) before closing the dropdown after blur, so that a click on a
// list item registers before the list is removed from the DOM.
const DROPDOWN_CLOSE_DELAY = 150;

export default class RoleTypeAheadForm extends PerspectivesComponent<RoleTypeAheadFormProps, RoleTypeAheadFormState>
{
  constructor(props: RoleTypeAheadFormProps) {
    super(props);
    this.state = { query: '', isOpen: false, selectedRoleId: null };
    this.handleInputChange = this.handleInputChange.bind(this);
    this.handleSelect = this.handleSelect.bind(this);
    this.handleBlur = this.handleBlur.bind(this);
    this.handleFocus = this.handleFocus.bind(this);
  }

  // --------------------------------------------------------------------
  // Filtering
  // --------------------------------------------------------------------
  filteredCandidates(): FilterValueEntry[] {
    const { query } = this.state;
    const { candidates } = this.props;
    // If the total number of candidates is below MAX_VISIBLE, show all of them
    // immediately (even without a query) so the user sees the full list on focus.
    if (!query) {
      return candidates.length < MAX_VISIBLE ? candidates : [];
    }
    const lower = query.toLowerCase();
    return candidates
      .filter(c => c.filterValue.toLowerCase().includes(lower))
      .slice(0, MAX_VISIBLE);
  }

  // --------------------------------------------------------------------
  // Handlers
  // --------------------------------------------------------------------
  handleInputChange(e: React.ChangeEvent<HTMLInputElement>) {
    this.setState({ query: e.target.value, isOpen: true, selectedRoleId: null });
  }

  handleFocus() {
    // Always open on focus — candidates below threshold will be shown immediately.
    this.setState({ isOpen: true });
  }

  handleBlur() {
    // Delay closing so click on list item registers first.
    setTimeout(() => this.setState({ isOpen: false }), DROPDOWN_CLOSE_DELAY);
  }

  handleSelect(candidate: FilterValueEntry) {
    this.setState({
      query: candidate.filterValue,
      isOpen: false,
      selectedRoleId: candidate.roleId as RoleInstanceT,
    });
  }

  // --------------------------------------------------------------------
  // Render
  // --------------------------------------------------------------------
  render() {
    const component = this;
    const { title, perspective, fieldConstraints } = component.props;
    const { query, isOpen, selectedRoleId } = component.state;
    const matches = component.filteredCandidates();

    return (
      <div>
        { title && <Form.Label>{title}</Form.Label> }
        <div style={{ position: 'relative' }}>
          <Form.Control
            type="text"
            value={query}
            placeholder={i18next.t('typeAheadForm_placeholder', { ns: 'preact' })}
            onChange={component.handleInputChange}
            onFocus={component.handleFocus}
            onBlur={component.handleBlur}
            aria-label={title || (perspective.displayName && perspective.displayName.trim()) || i18next.t('typeAheadForm_ariaLabel', { ns: 'preact' })}
          />
          { isOpen && matches.length > 0 && (
            <ListGroup
              style={{
                position: 'absolute',
                zIndex: 1000,
                width: '100%',
                maxHeight: '300px',
                overflowY: 'auto',
                boxShadow: '0 4px 8px rgba(0,0,0,0.15)',
              }}
            >
              { matches.map((candidate, idx) => (
                <ListGroup.Item
                  key={idx}
                  action
                  onMouseDown={() => component.handleSelect(candidate)}
                >
                  {candidate.filterValue}
                </ListGroup.Item>
              ))}
            </ListGroup>
          )}
        </div>
        { selectedRoleId && (
          <div className="mt-3">
            <PerspectiveBasedForm
              perspective={perspective}
              roleinstance={selectedRoleId}
              cardtitle={perspective.identifyingProperty}
              showControls={false}
              fieldConstraints={fieldConstraints}
            />
          </div>
        )}
      </div>
    );
  }
}

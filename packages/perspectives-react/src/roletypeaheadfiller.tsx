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
import { PDRproxy, Perspective, FilterValueEntry, RoleInstanceT } from 'perspectives-proxy';
import PerspectivesComponent from './perspectivesComponent';
import { UserMessagingPromise } from './userMessaging.js';
import i18next from 'i18next';
import { Form, ListGroup } from 'react-bootstrap';

////////////////////////////////////////////////////////////////////////////////
// ROLETYPEAHEADFILLER
// A typeahead search widget that lets the user pick a candidate role instance
// (from the FilterValue view) and fill a target role in the current context
// with it. Designed for situations where the number of candidates is too large
// for a simple dropdown list.
//
// Props:
//  - perspective : Perspective – the serialised perspective on the role to fill
//  - candidates  : FilterValueEntry[] – pre-fetched {filterValue, roleId} entries
//  - title       : string | undefined – optional label for the widget
////////////////////////////////////////////////////////////////////////////////

interface RoleTypeAheadFillerProps {
  perspective: Perspective;
  candidates: FilterValueEntry[];
  title?: string;
}

interface RoleTypeAheadFillerState {
  query: string;
  isOpen: boolean;
}

const MAX_VISIBLE = 20;
// Delay (ms) before closing the dropdown after blur, so that a click on a
// list item registers before the list is removed from the DOM.
const DROPDOWN_CLOSE_DELAY = 150;

export default class RoleTypeAheadFiller extends PerspectivesComponent<RoleTypeAheadFillerProps, RoleTypeAheadFillerState>
{
  constructor(props: RoleTypeAheadFillerProps) {
    super(props);
    this.state = { query: '', isOpen: false };
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
    this.setState({ query: e.target.value, isOpen: true });
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
    const component = this;
    const { perspective } = component.props;
    // Use the first role instance in the perspective as the role to be filled.
    // The TypeAheadFiller is intended for roles where there is at most one
    // (unfilled or empty) instance that the user wants to assign a filler to.
    // Multi-instance scenarios should use a table with per-row fill controls.
    const firstInstance = Object.values(perspective.roleInstances)[0];
    const filledRoleInstance: RoleInstanceT | undefined = firstInstance?.roleId as RoleInstanceT | undefined;

    if (!filledRoleInstance) {
      UserMessagingPromise.then(um =>
        um.addMessageForEndUser({
          title: i18next.t('fillRole_title', { ns: 'preact' }),
          message: i18next.t('typeAheadFiller_noInstance', { ns: 'preact' }),
          error: '',
        })
      );
      return;
    }

    PDRproxy.then(pproxy =>
      pproxy
        .bind_(
          filledRoleInstance,
          candidate.roleId as RoleInstanceT,
          perspective.userRoleType
        )
        .then(uniqueFiller => {
          if (!uniqueFiller) {
            UserMessagingPromise.then(um =>
              um.addMessageForEndUser({
                title: i18next.t('fillRole_title', { ns: 'preact' }),
                message: i18next.t('fillRole_filler_has_been_used', { ns: 'preact' }),
                error: i18next.t('fillRole_error', { ns: 'preact' }),
              })
            );
          }
        })
        .catch(e =>
          UserMessagingPromise.then(um =>
            um.addMessageForEndUser({
              title: i18next.t('fillRole_title', { ns: 'preact' }),
              message: i18next.t('fillRole_message', { ns: 'preact' }),
              error: e.toString(),
            })
          )
        )
    );

    // Close the dropdown and reset the query after selection.
    component.setState({ query: candidate.filterValue, isOpen: false });
  }

  // --------------------------------------------------------------------
  // Render
  // --------------------------------------------------------------------
  render() {
    const component = this;
    const { title, perspective } = component.props;
    const { query, isOpen } = component.state;
    const matches = component.filteredCandidates();
    const canFill = perspective.verbs && perspective.verbs.includes('Fill');

    return (
      <div style={{ position: 'relative' }}>
        { title && <Form.Label>{title}</Form.Label> }
        <Form.Control
          type="text"
          value={query}
          placeholder={i18next.t('typeAheadFiller_placeholder', { ns: 'preact' })}
          onChange={component.handleInputChange}
          onFocus={component.handleFocus}
          onBlur={component.handleBlur}
          disabled={!canFill}
          aria-label={title || perspective.displayName || i18next.t('typeAheadFiller_ariaLabel', { ns: 'preact' })}
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
    );
  }
}

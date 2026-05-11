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

// Returns the filterValue label for the current filler of the first role
// instance in the perspective, or '' if the role is not yet filled or if the
// filler is not found among the candidates.
function currentFillerLabel(perspective: Perspective, candidates: FilterValueEntry[]): string {
  const firstInstance = Object.values(perspective.roleInstances)[0];
  const existingFiller = firstInstance?.filler;
  if (existingFiller) {
    const match = candidates.find(c => c.roleId === existingFiller);
    return match ? match.filterValue : '';
  }
  return '';
}

export default class RoleTypeAheadFiller extends PerspectivesComponent<RoleTypeAheadFillerProps, RoleTypeAheadFillerState>
{
  // Tracks whether a selection was just made so that handleContainerBlur can
  // distinguish "user selected a candidate" from "user clicked elsewhere".
  // A selection closes the dropdown immediately (onClick → handleSelect →
  // setState({isOpen:false})); the flag prevents handleContainerBlur from
  // then overwriting the newly selected label with the old filler label.
  private _selectionJustMade = false;

  constructor(props: RoleTypeAheadFillerProps) {
    super(props);
    this.state = {
      query: currentFillerLabel(props.perspective, props.candidates),
      isOpen: false,
    };
    this.handleInputChange = this.handleInputChange.bind(this);
    this.handleSelect = this.handleSelect.bind(this);
    this.handleContainerBlur = this.handleContainerBlur.bind(this);
    this.handleFocus = this.handleFocus.bind(this);
  }

  // When the filler changes externally (e.g. another user fills the role),
  // update the input label — but only while the dropdown is not open so we
  // don't interrupt an active search.
  componentDidUpdate(prevProps: RoleTypeAheadFillerProps) {
    const prevFiller = Object.values(prevProps.perspective.roleInstances)[0]?.filler;
    const currFiller = Object.values(this.props.perspective.roleInstances)[0]?.filler;
    if (prevFiller !== currFiller && !this.state.isOpen) {
      this.setState({
        query: currentFillerLabel(this.props.perspective, this.props.candidates),
      });
    }
  }

  // --------------------------------------------------------------------
  // Filtering
  // --------------------------------------------------------------------
  filteredCandidates(): FilterValueEntry[] {
    const { query } = this.state;
    const { candidates, perspective } = this.props;
    const fillerLabel = currentFillerLabel(perspective, candidates);

    // "Display mode": query is empty OR the query still shows the filler label
    // (the user has not started a new search yet).  Show all candidates up to
    // MAX_VISIBLE so the user can browse the full list on focus.
    if (!query || query === fillerLabel) {
      return candidates.slice(0, MAX_VISIBLE);
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
    this.setState({ isOpen: true });
  }

  // Close the dropdown only when focus leaves the entire widget (input + list).
  // When the user clicks a list item the input loses focus first, but
  // relatedTarget is the list item (inside the container) so we do NOT close.
  // Only when focus moves outside (click elsewhere) do we close and restore
  // the input to the current filler label.
  handleContainerBlur(e: React.FocusEvent<HTMLDivElement>) {
    if (e.currentTarget.contains(e.relatedTarget as Node | null)) {
      return; // Focus still within widget — keep dropdown open
    }
    // Focus left the widget.
    if (this._selectionJustMade) {
      // handleSelect already closed the dropdown and set the label — just
      // clear the flag and leave state as-is.
      this._selectionJustMade = false;
      return;
    }
    // No selection was made; restore the input to the current filler label.
    this.setState({
      isOpen: false,
      query: currentFillerLabel(this.props.perspective, this.props.candidates),
    });
  }

  handleSelect(candidate: FilterValueEntry) {
    const component = this;
    const { perspective } = component.props;
    // Use the first role instance in the perspective as the role to be filled.
    // The TypeAheadFiller is intended for roles where there is at most one
    // (unfilled or empty) instance that the user wants to assign a filler to.
    // Multi-instance scenarios should use a table with per-row fill controls.
    const firstInstance = Object.values(perspective.roleInstances)[0];
    const filledRoleInstance: RoleInstanceT | undefined = firstInstance?.roleId;

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
          candidate.roleId,
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

    // Mark that a selection was just made (checked in handleContainerBlur) and show the
    // chosen label immediately while the PDR processes the bind_ call.
    component._selectionJustMade = true;
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
    const fillerLabel = currentFillerLabel(perspective, component.props.candidates);

    return (
      <div style={{ position: 'relative' }} onBlur={component.handleContainerBlur}>
        { title && <Form.Label>{title}</Form.Label> }
        <Form.Control
          type="text"
          value={query}
          placeholder={fillerLabel || i18next.t('typeAheadFiller_placeholder', { ns: 'preact' })}
          onChange={component.handleInputChange}
          onFocus={component.handleFocus}
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
                onClick={() => component.handleSelect(candidate)}
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

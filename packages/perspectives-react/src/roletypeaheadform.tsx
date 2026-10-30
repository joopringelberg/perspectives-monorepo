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
import { PDRproxy, CONTINUOUS, FilterValueEntry, Perspective, RoleInstanceT, Unsubscriber } from 'perspectives-proxy';
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
//  - displayName : string – human-readable role-type name used as aria-label
//  - candidates  : FilterValueEntry[] – pre-fetched {filterValue, roleId} entries
//  - title       : string | undefined – optional label for the widget
//  - fieldConstraints: any[] – per-property display constraints
//
// After the user selects a candidate the component calls getPerspective() to
// fetch the perspective for that single instance on demand.  No full role-
// instance list is ever sent at screen-build time.
////////////////////////////////////////////////////////////////////////////////

interface RoleTypeAheadFormProps {
  displayName: string;
  candidates: FilterValueEntry[];
  title?: string;
  fieldConstraints?: any[];
}

interface RoleTypeAheadFormState {
  query: string;
  isOpen: boolean;
  selectedRoleId: RoleInstanceT | null;
  selectedPerspective: Perspective | null;
  highlightIndex: number;
}

const MAX_VISIBLE = 20;

export default class RoleTypeAheadForm extends PerspectivesComponent<RoleTypeAheadFormProps, RoleTypeAheadFormState>
{
  // Tracks the unsubscriber for the active getPerspective subscription so we
  // can cancel it before starting a new one when the user re-selects.
  private _perspectiveUnsub: Unsubscriber | null = null;

  constructor(props: RoleTypeAheadFormProps) {
    super(props);
    this.state = { query: '', isOpen: false, selectedRoleId: null, selectedPerspective: null, highlightIndex: -1 };
    this.handleInputChange = this.handleInputChange.bind(this);
    this.handleSelect = this.handleSelect.bind(this);
    this.handleContainerBlur = this.handleContainerBlur.bind(this);
    this.handleFocus = this.handleFocus.bind(this);
    this.handleKeyDown = this.handleKeyDown.bind(this);
  }

  componentWillUnmount() {
    const component = this;
    // Cancel the active perspective subscription before the base-class cleanup.
    if (component._perspectiveUnsub) {
      const unsub = component._perspectiveUnsub;
      PDRproxy.then(function(pproxy) {
        pproxy.send(unsub, function() {});
      });
      component._perspectiveUnsub = null;
    }
    super.componentWillUnmount();
  }

  // --------------------------------------------------------------------
  // Filtering
  // --------------------------------------------------------------------
  filteredCandidates(): FilterValueEntry[] {
    const { query, selectedRoleId } = this.state;
    const { candidates } = this.props;

    // "Display mode": query is empty OR the query still equals the selected
    // item's label (i.e. the user has not started a new search yet).
    // In display mode show all candidates up to MAX_VISIBLE so the user can
    // browse the full list and re-select without having to erase the label.
    if (!query) {
      return candidates.slice(0, MAX_VISIBLE);
    }
    if (selectedRoleId) {
      const selectedEntry = candidates.find(c => c.roleId === selectedRoleId);
      if (selectedEntry && selectedEntry.filterValue === query) {
        return candidates.slice(0, MAX_VISIBLE);
      }
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
    // Changing the query always exits "display mode" and starts a real search.
    // Clearing selectedRoleId ensures filteredCandidates filters normally.
    this.setState({ query: e.target.value, isOpen: true, selectedRoleId: null, selectedPerspective: null, highlightIndex: -1 });
  }

  handleFocus() {
    this.setState({ isOpen: true });
  }

  // Close the dropdown only when focus leaves the entire widget (input + list).
  // When focus moves from the input to a list item, relatedTarget is inside
  // the container and we must NOT close — otherwise the list disappears before
  // the user's click registers.
  handleContainerBlur(e: React.FocusEvent<HTMLDivElement>) {
    if (e.currentTarget.contains(e.relatedTarget as Node | null)) {
      return; // Focus still within widget — keep dropdown open
    }
    // Focus left the widget; close the dropdown and restore the query to the
    // selected item's label so the user still sees which item is chosen.
    const { selectedRoleId } = this.state;
    const { candidates } = this.props;
    if (selectedRoleId) {
      const match = candidates.find(c => c.roleId === selectedRoleId);
      this.setState({ isOpen: false, query: match ? match.filterValue : '' });
    } else {
      this.setState({ isOpen: false });
    }
  }

  handleSelect(candidate: FilterValueEntry) {
    const component = this;

    // Cancel the previous perspective subscription before starting a new one.
    if (component._perspectiveUnsub) {
      const prevUnsub = component._perspectiveUnsub;
      PDRproxy.then(function(pproxy) {
        pproxy.send(prevUnsub, function() {});
      });
      component._perspectiveUnsub = null;
    }

    component.setState({
      query: candidate.filterValue,
      isOpen: false,
      selectedRoleId: candidate.roleId,
      selectedPerspective: null,
      highlightIndex: -1,
    });

    // Fetch the perspective for the selected role instance on demand.
    PDRproxy.then(function(pproxy) {
      const unsub = pproxy.getPerspective(
        candidate.roleId,
        undefined,
        function(perspectives: Perspective[]) {
          if (perspectives.length > 0) {
            component.setState({ selectedPerspective: perspectives[0] });
          }
        },
        CONTINUOUS
      );
      // Store unsubscriber for manual cleanup on re-selection.
      unsub.then(function(u) { component._perspectiveUnsub = u; });
    });
  }

  handleKeyDown(e: React.KeyboardEvent<HTMLInputElement>) {
    const component = this;
    const { isOpen, highlightIndex } = component.state;
    const matches = component.filteredCandidates();

    if (!isOpen || matches.length === 0) {
      if (e.key === 'ArrowDown') {
        e.preventDefault();
        component.setState({ isOpen: true, highlightIndex: 0 });
      }
      return;
    }

    if (e.key === 'ArrowDown') {
      e.preventDefault();
      component.setState({ highlightIndex: Math.min(highlightIndex + 1, matches.length - 1) });
    } else if (e.key === 'ArrowUp') {
      e.preventDefault();
      component.setState({ highlightIndex: Math.max(highlightIndex - 1, -1) });
    } else if (e.key === 'Enter') {
      e.preventDefault();
      if (highlightIndex >= 0 && highlightIndex < matches.length) {
        component.handleSelect(matches[highlightIndex]);
      }
    } else if (e.key === 'Escape') {
      e.preventDefault();
      component.setState({ isOpen: false, highlightIndex: -1 });
    }
  }

  // --------------------------------------------------------------------
  // Render
  // --------------------------------------------------------------------
  render() {
    const component = this;
    const { title, displayName, fieldConstraints } = component.props;
    const { query, isOpen, selectedRoleId, selectedPerspective, highlightIndex } = component.state;
    const matches = component.filteredCandidates();

    return (
      <div>
        { title && <Form.Label>{title}</Form.Label> }
        <div style={{ position: 'relative' }} onBlur={component.handleContainerBlur}>
          <Form.Control
            type="text"
            value={query}
            placeholder={i18next.t('typeAheadForm_placeholder', { ns: 'preact' })}
            onChange={component.handleInputChange}
            onFocus={component.handleFocus}
            onKeyDown={component.handleKeyDown}
            aria-label={title || (displayName && displayName.trim()) || i18next.t('typeAheadForm_ariaLabel', { ns: 'preact' })}
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
                  active={idx === highlightIndex}
                  onClick={() => component.handleSelect(candidate)}
                >
                  {candidate.filterValue}
                </ListGroup.Item>
              ))}
            </ListGroup>
          )}
        </div>
        { selectedRoleId && selectedPerspective && (
          <div className="mt-3">
            <PerspectiveBasedForm
              perspective={selectedPerspective}
              roleinstance={selectedRoleId}
              cardtitle={selectedPerspective.identifyingProperty}
              showControls={false}
              fieldConstraints={fieldConstraints}
            />
          </div>
        )}
      </div>
    );
  }
}


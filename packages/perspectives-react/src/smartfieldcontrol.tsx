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

// This Component is built upon the data sent from the PDR for a single property.

import React, { PureComponent } from "react";
const Component = PureComponent;
import {ContextInstanceT, PDRproxy, RoleInstanceT, RoleType, ValueT, PropertyValues, SerialisedProperty, InputType } from "perspectives-proxy";
import { mapRange } from "perspectives-proxy";
import { Form } from "react-bootstrap";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";
import {PerspectivesFile} from "./perspectivesFile.js";
import {MarkDownWidget} from "./markdownWidget.js";
import { formatPropertyValue } from "./utilities.js";

////////////////////////////////////////////////////////////////////////////////
// TABINDEX VALUES
////////////////////////////////////////////////////////////////////////////////
// A negative value means that the element should be focusable,
// but should not be reachable via sequential keyboard navigation;
// Presumably this means clicking on it or setting it from Javascript.
const focusable = -1;

// 0 means that the element should be focusable and reachable via sequential keyboard navigation,
// but its relative order is defined by the platform convention;
const receiveFocusByKeyboard = 0;

// Assumed line height in em units, used for computing max-height of textareas from maxLines.
const LINE_HEIGHT_EM = 1.5;

interface SmartFieldControlProps
{
  serialisedProperty: SerialisedProperty;
  propertyValues?: PropertyValues;
  roleId: RoleInstanceT | undefined;
  myroletype: RoleType;
  inputRef?: React.RefObject<HTMLElement> | null;
  disabled: boolean;
  isselected: boolean;
  contextinstance: ContextInstanceT;
  minLines?: number;
  maxLines?: number;
}

interface SmartFieldControlState
{
  value: string;
  // True while the user has made local edits that have not
  // been cancelled (undo/Escape) or finished (blur).
  hasLocalEdits: boolean;
}

export default class SmartFieldControl extends Component<SmartFieldControlProps, SmartFieldControlState>
{
  private inputType: InputType;
  private controlType: string | undefined;
  private ref = React.createRef<HTMLElement>();
  // Tracks whether the default text/date/time control is in an
  // editing session, without involving React state.
  private defaultEditingActive: boolean = false;
  // Undo button for the default control path; controlled via DOM
  // to avoid React re-renders while editing.
  private undoRef = React.createRef<HTMLButtonElement>();

  constructor(props : SmartFieldControlProps)
  {
    super(props);
    this.inputType = mapRange( this.props.serialisedProperty.range );
    // If the range is PDateTime, `value` is a string that represents an Epoch. We convert it to a DateTime format that the input control accepts.
    // before storing it, we convert it back to an Epoch.
    this.state = { value: this.valueOnProps(), hasLocalEdits: false };
    this.leaveControl = this.leaveControl.bind(this);
    this.controlType = this.htmlControlType();
    this.ref = props.inputRef ? props.inputRef : React.createRef<HTMLElement>();
  }

  // True when the user has local edits pending for this control.
  private isDirty(): boolean {
    return this.state.hasLocalEdits;
  }

  // Only text and date/time-like input types get an inline undo button.
  private isUndoRelevantType(): boolean {
    return ["text", "datetime-local", "date", "time"].indexOf(this.inputType) >= 0;
  }

  private dispatchEditingStarted() {
    if (this.ref.current instanceof HTMLElement) {
      this.ref.current.dispatchEvent(
        new CustomEvent("FormFieldEditStarted", { bubbles: true })
      );
    }
  }

  private dispatchEditingEnded() {
    if (this.ref.current instanceof HTMLElement) {
      this.ref.current.dispatchEvent(
        new CustomEvent("FormFieldEditEnded", { bubbles: true })
      );
    }
  }

  componentDidMount(): void {
    if (this.ref.current instanceof HTMLInputElement || this.ref.current instanceof HTMLTextAreaElement)
    {
      this.ref.current.value = this.state.value;
    }
    if (this.undoRef.current)
    {
      this.undoRef.current.disabled = true;
      this.undoRef.current.classList.remove("bg-danger-subtle", "text-danger-emphasis", "border-0");
    }
    if (this.props.isselected && this.ref.current)
    {
      this.ref.current.focus();
    }
  }

  componentDidUpdate(prevProps : SmartFieldControlProps)
  {
    if (!prevProps.propertyValues || 
        (this.props.propertyValues
        && prevProps.propertyValues.values[0] != this.props.propertyValues.values[0]))
    {
      // Only sync from props when there are no local edits in progress,
      // to avoid fighting the user's typing and avoid flicker.
      if (!this.state.hasLocalEdits)
      {
        const newValue = this.valueOnProps();
        this.setState({ value: newValue });
        if (this.ref.current instanceof HTMLInputElement || this.ref.current instanceof HTMLTextAreaElement)
        {
          this.ref.current.value = newValue;
        }
      }
    }
    if (this.props.isselected && this.ref.current)
    {
      this.ref.current.focus();
    }
   }

  htmlControlType ()
  {
    const controlType = mapRange(this.props.serialisedProperty.range );
    const maxLength = this.maxLength(this.inputType);
    if (controlType == "checkbox")
    {
      return "checkbox";
    }
    if (controlType == "text")
    {
      if (this.props.minLines || (maxLength && maxLength > 80) || this.minLength(this.inputType) > 80 || this.state.value.length > 80)
      {
        return "textarea";
      }
      else if (this.enumeration().length > 0)
      {
        return "select";
      }
      else
      {
        return "input";
      }
    }
    if (controlType == "file")
    {
      return "PerspectivesFile";
    }
    if (controlType == "markdown")
      {
        return "MarkDownWidget";
      }
  }

  // Returns the first value in the `propertyValues` prop, or the empty string.
  valueOnProps() : string
  {
    if (this.props.propertyValues)
    {
      return formatPropertyValue(this.props.propertyValues.values, this.inputType);
    }
    else
    {
      return "";
    }
  }

  // `val` is a string.
  changeValue (val : string)
  {
    function inputVal_to_value() : ValueT
    {
      switch (component.inputType) {
        case "datetime-local":
          // return the timestamp (milliseconds since the epoch).
          return new Date(val).valueOf() + "" as ValueT;
        case "date":
          // return the timestamp (milliseconds since the epoch).
          return new Date(val).valueOf() + "" as ValueT;
        case "time":
          // val is a string in the format "hh:mm" or " hh:mm:ss"
          // We return the corresponding milliseconds.
          return (new Date("1970-01-01T" + val).valueOf() - new Date('1970-01-01T00:00').valueOf() ) + "" as ValueT;
        default:
          return val as ValueT;
      }
    }
    const component = this;
    const oldValue = component.valueOnProps();
    if (component.props.roleId !== undefined)
    {
      if (val == "" && oldValue != "")
      {
        PDRproxy.then(
          function(pproxy)
          {
            pproxy.deleteProperty(
              component.props.roleId!,
              component.props.serialisedProperty.id,
              component.props.myroletype )
              .catch(e => UserMessagingPromise.then( um => 
                um.addMessageForEndUser(
                  { title: i18next.t("setProperty_title", { ns: 'preact' }) 
                  , message: i18next.t("deleteProperty_message", {ns: 'preact', property: component.props.serialisedProperty.id})
                  , error: e.toString()
                  })));
            });
      }
      else if ( oldValue != val )
      {
        PDRproxy.then(
          function(pproxy)
          {
            pproxy.setProperty(
              component.props.roleId!,
              component.props.serialisedProperty.id,
              inputVal_to_value(),
              component.props.myroletype )
              .catch(e => UserMessagingPromise.then( um => 
                um.addMessageForEndUser(
                  { title: i18next.t("setProperty_title", { ns: 'preact' }) 
                  , message: i18next.t("setProperty_message", {ns: 'preact', property: component.props.serialisedProperty.id})
                  , error: e.toString()
                  })));
            });
      }}
  }

  // newvalue should be a string.
  handleKeyDown (event : React.KeyboardEvent, newvalue : string)
  {
    const component = this;
    if (!component.props.disabled)
    {
      switch( event.key )
      {
        // Assuming this code only runs when the field control is active,
        // let it handle the navigation keys locally but do not bubble.
        case "ArrowLeft": // left arrow.
        case "ArrowRight": // right arrow.
        case "ArrowUp": // Up arrow
        case "ArrowDown": // Down arrow
        case "Control": // Vertical Tab.
        case "Space": // Space
          event.stopPropagation();
          break;
        case "Enter": // Return
          if ( !event.shiftKey )
            {
              // Safe changes, allow event to bubble.
              if (component.reportValidity(event))
                {
                  component.changeValue(newvalue);
                }
                else
                {
                  event.stopPropagation();
                }  
            }
          else
            {
              event.stopPropagation();
            }
          break;
        case "Escape": // Escape
          // Discard changes, allow event to bubble.
          {
            if (component.controlType === "input" || component.controlType === "textarea")
            {
              // Default text-like control: reset DOM value and end
              // the editing session without touching React state.
              if (component.ref.current instanceof HTMLInputElement || component.ref.current instanceof HTMLTextAreaElement)
              {
                component.ref.current.value = component.valueOnProps();
              }
              if (component.defaultEditingActive)
              {
                component.defaultEditingActive = false;
                component.dispatchEditingEnded();
                if (component.undoRef.current)
                {
                  component.undoRef.current.disabled = true;
                  component.undoRef.current.classList.remove("bg-danger-subtle", "text-danger-emphasis", "border-0");
                }
              }
            }
            else
            {
              // Other controls still use state-based tracking.
              const hadLocalEdits = component.state.hasLocalEdits;
              if (component.ref.current instanceof HTMLInputElement || component.ref.current instanceof HTMLTextAreaElement)
              {
                component.ref.current.value = component.valueOnProps();
              }
              component.setState({ hasLocalEdits: false });
              if (hadLocalEdits)
              {
                component.dispatchEditingEnded();
              }
            }
          }
          event.preventDefault();
          break;
      }
    }
  }

  // This method is called onBlur, i.e. when the user navigates away
  // from the SmartFieldControl.
  // Event target is the input control.
  leaveControl(e : React.FocusEvent ) : boolean
  {
    const hadLocalEdits = this.state.hasLocalEdits;
    this.changeValue((e.target as HTMLInputElement).value);
    if (hadLocalEdits)
    {
      this.setState({ hasLocalEdits: false });
      this.dispatchEditingEnded();
    }
    return this.reportValidity(e);
  }

  // Handle blur at the wrapper level: only when focus actually leaves
  // the form control + undo button group do we trigger leaveControl.
  handleGroupBlur(e: React.FocusEvent<HTMLElement>) {
    const currentTarget = e.currentTarget;
    const relatedTarget = e.relatedTarget as (HTMLElement | null);
    const target = e.target as (HTMLElement | null);

    // We only care about blur events where the input itself loses
    // focus. Blurs from other descendants (e.g. the undo button)
    // are ignored.
    if (target !== this.ref.current)
    {
      return;
    }

    // If focus moves to another element inside the same wrapper
    // (e.g. from the input to the undo button), do not treat this
    // as leaving the control.
    if (relatedTarget && currentTarget.contains(relatedTarget))
    {
      return;
    }

    // Now the input lost focus to an element outside the group:
    // perform the normal leaveControl logic.
    this.leaveControl(e as unknown as React.FocusEvent);

    // For the default text-like control, also end the editing
    // session here without relying on React state.
    if (this.controlType === "input" || this.controlType === "textarea")
    {
      if (this.defaultEditingActive)
      {
        this.defaultEditingActive = false;
        this.dispatchEditingEnded();
        if (this.undoRef.current)
        {
          this.undoRef.current.disabled = true;
          this.undoRef.current.classList.remove("bg-danger-subtle", "text-danger-emphasis", "border-0");
        }
      }
    }
  }

  // The event should have the input element as target. Returns true iff no constraints are violated.
  reportValidity(event : React.SyntheticEvent ) : boolean  
  {
    // A ValidityState object. See: https://developer.mozilla.org/en-US/docs/Web/API/ValidityState
    const validity = (event.target as HTMLInputElement).validity;
    // We have no validity check for MarkDown.
    if (validity && validity.patternMismatch)
    {
      // We now expect a pattern in the perspective.
      const label = this.pattern()!.label;
      (event.target as HTMLInputElement).setCustomValidity( label );
    }
    else
    {
      (event.target as HTMLInputElement).setCustomValidity( "" );
    }
    return (event.target as HTMLInputElement).reportValidity();
  }

  // Returns an integer. It will be zero (0) if the property has no minimum length.
  minLength( inputType : InputType) : number
  {
    const component = this;
    if (["text", "search", "url", "tel", "email", "password", "markdown"].indexOf(inputType) >= 0)
    {
      return component.props.serialisedProperty.constrainingFacets.minLength || 0;
    }
    else
    {
      return 0;
    }
  }

  // Returns an integer. It will be zero (0) if the property has no maximum length.
  maxLength(inputType : InputType) : number | undefined
  {
    const component = this;
    if (["text", "search", "url", "tel", "email", "password", "markdown"].indexOf(inputType) >= 0)
    {
      return component.props.serialisedProperty.constrainingFacets.maxLength || undefined;
    }
    else
    {
      return undefined;
    }
  }

  // Returns an integer or undefined.
  minInclusive()
  {
    const component = this;
    if (["range", "number", "date", "month", "week", "datetime", "datetime-local", "time"].indexOf(this.inputType) >= 0)
    {
      return component.props.serialisedProperty.constrainingFacets.minInclusive;
    }
  }

  // Returns an integer or undefined.
  maxInclusive()
  {
    const component = this;
    if (["range", "number", "date", "month", "week", "datetime", "datetime-local", "time"].indexOf(this.inputType) >= 0)
    {
      return component.props.serialisedProperty.constrainingFacets.maxInclusive;
    }
  }

  // Returns an array of strings.
  enumeration()
  {
    return this.props.serialisedProperty.constrainingFacets.enumeration || [];
  }

  // Returns object of this shape:
  // { regex: string.isRequired
  // , label: string.isRequired}
  pattern() : { regex: string, label: string } | undefined
  {
    const component = this;
    if (["text", "search", "url", "tel", "email", "password"].indexOf(this.inputType) >= 0)
    {
      return component.props.serialisedProperty.constrainingFacets.pattern;
    }
  }

  render()
  {
    function toggleValue()
    {
      const newvalue = (component.state.value != "true").toString();
      if ( !component.props.disabled )
      {
        component.changeValue(newvalue);
      }
    }
    // Expects object of this shape:
    // { regex: string.isRequired
    // , label: string.isRequired}
    // Returns the string that represents just the regex, no flags.
    // label has the shape /regex/flags.
    // flags will be ignored.
    function patternToSource(p : { regex: string, label: string })
    {
      const r = /\/(.*)\//;
      const match = p.regex.match(r);
      return match ? match[1] : "";
    }

    const component = this;
    const mandatory = component.props.serialisedProperty.isMandatory;
    const pattern = component.pattern();
    const maxLength = component.maxLength(component.inputType);
    let showUndo: boolean, selectedClass: string, combinedClassName: string, handleUndoClick: (e: React.MouseEvent<HTMLButtonElement>) => void;

    switch ( this.controlType ){
      case "checkbox":
        return (
          <div onKeyDown={e => component.handleKeyDown(e, component.state.value)}>
            <Form.Check
              id={component.props.serialisedProperty.id + "_" + component.props.roleId}
              ref={component.ref as React.RefObject<HTMLInputElement>}
              tabIndex={component.props.isselected ? receiveFocusByKeyboard : focusable}
              aria-label={ component.props.serialisedProperty.displayName }
              readOnly={ component.props.disabled }
              checked={ component.state.value == "true" }
              // onChange={ toggleValue }
              onClick={ toggleValue }
              required={mandatory}
            />
          </div>);
      case "select":
        return (
          <div onKeyDown={e => component.handleKeyDown(e, (e.target as HTMLInputElement).value)}>
            <Form.Control
              id={component.props.serialisedProperty.id + "_" + component.props.roleId}
              as="select"
              ref={component.ref as React.RefObject<HTMLSelectElement>}
              tabIndex={component.props.isselected ? receiveFocusByKeyboard : focusable}
              aria-label={ component.state.value }
              readOnly={ component.props.disabled }
              disabled={ component.props.disabled }
              value={ component.state.value }
              onChange={e => {
                const wasEditing = component.state.hasLocalEdits;
                const newVal = e.target.value;
                component.setState({ value: newVal, hasLocalEdits: true });
                if (!wasEditing)
                {
                  component.dispatchEditingStarted();
                }
              }}
              onBlur={component.leaveControl}
              required={mandatory}
            >
            {
              component.enumeration().map( value => <option key={value}>{value}</option>)
            }
            </Form.Control>
          </div>);
      case "PerspectivesFile":
        return (
          <PerspectivesFile
            id={component.props.serialisedProperty.id + "_" + component.props.roleId}
            serialisedProperty={component.props.serialisedProperty}
            propertyValues={component.props.propertyValues}
            roleId={component.props.roleId}
            myRoletype={component.props.myroletype}>
          </PerspectivesFile>);
      case "MarkDownWidget":
          if (component.props.disabled)
            {
              // Just render the html for a read-only perspective on this property.
              return <MarkDownWidget markdown={component.state.value} contextid={component.props.contextinstance} myroletype={component.props.myroletype}/>;
            }
          else
          {
            // Create an editor. Currently, this is just an html input or a textarea, depdending on minInclusive.
            const as = component.minLength("markdown") > 80 ? "textarea" : "input";
            return (
              <div onKeyDown={e => component.handleKeyDown(e, (e.target as HTMLInputElement).value)}>
                <Form.Control
                  id={component.props.serialisedProperty.id + "_" + component.props.roleId}
                  as={as}
                  ref={component.ref as React.RefObject<HTMLInputElement>}
                  tabIndex={component.props.isselected ? receiveFocusByKeyboard : focusable}
                  aria-label={ pattern? pattern.label : component.state.value }
                  readOnly={ component.props.disabled }
                  value={ component.state.value }
                  onChange={e => {
                    const wasEditing = component.state.hasLocalEdits;
                    const newVal = e.target.value;
                    component.setState({ value: newVal, hasLocalEdits: true });
                    if (!wasEditing)
                    {
                      component.dispatchEditingStarted();
                    }
                  }}
                  onBlur={component.leaveControl}
                  type="text"
                  required={mandatory}
                  minLength={component.minLength(component.inputType)}
                  {... maxLength ? { maxLength: maxLength } : {}}
                  min={component.minInclusive()}
                  max={component.maxInclusive()}
                  {...(pattern ? { pattern: patternToSource(pattern) } : {})}
                />
              </div>);
              }
      default:
        // For the default text/date/time-like control we avoid using
        // React state in onChange. The undo button is always shown
        // for relevant types; its handler restores the previous
        // value both in the DOM and in the PDR.
        showUndo = component.isUndoRelevantType() && component.props.disabled === false;
        selectedClass = component.props.isselected ? "text-light bg-secondary" : "";
        combinedClassName = (selectedClass ? selectedClass + " " : "") + "flex-grow-1";
        handleUndoClick = (e: React.MouseEvent<HTMLButtonElement>) => {
          e.preventDefault();
          e.stopPropagation();
          if (!showUndo) {
            return;
          }
          if (component.ref.current instanceof HTMLInputElement)
          {
            const originalValue = component.valueOnProps();
            component.ref.current.value = originalValue;
            component.changeValue(originalValue);
          }
          // For the default control path, end the editing session
          // when undo is pressed, without involving React state.
          if (component.controlType === "input" || component.controlType === "textarea")
          {
            if (component.defaultEditingActive)
            {
              component.defaultEditingActive = false;
              component.dispatchEditingEnded();
              if (component.undoRef.current)
              {
                component.undoRef.current.disabled = true;
                component.undoRef.current.classList.remove("bg-danger-subtle", "text-danger-emphasis", "border-0");
              }
            }
          }
        };
        // Compute textarea-specific props (rows and maxHeight) from minLines/maxLines constraints.
        const textareaConstraintProps = component.controlType === "textarea"
          ? {
              ...(component.props.minLines ? { rows: component.props.minLines } : {}),
              ...(component.props.maxLines ? { style: { maxHeight: `${component.props.maxLines * LINE_HEIGHT_EM}em`, overflowY: 'auto' as const } } : {})
            }
          : {};
        return (
          <div
            className="d-flex align-items-center"
            onBlur={e => component.handleGroupBlur(e)}
            onKeyDown={e => component.handleKeyDown(e, (e.target as HTMLInputElement).value)}
          >
            <Form.Control
              id={component.props.serialisedProperty.id + "_" + component.props.roleId}
              as={ (component.controlType as React.ElementType) || "input" }
              ref={component.ref as React.RefObject<HTMLInputElement>}
              tabIndex={component.props.isselected ? receiveFocusByKeyboard : focusable}
              aria-label={ pattern? pattern.label : component.state.value }
              readOnly={ component.props.disabled }
              onChange={e => {
                // Start an editing session the first time the user
                // changes the value. We avoid setState here to keep
                // the input from re-rendering on each keypress.
                if (!component.defaultEditingActive)
                {
                  component.defaultEditingActive = true;
                  component.dispatchEditingStarted();
                  if (component.undoRef.current)
                  {
                    component.undoRef.current.disabled = false;
                    component.undoRef.current.classList.add("bg-danger-subtle", "text-danger-emphasis", "border-0");
                  }
                }
              }}
              type={component.inputType}
              required={mandatory}
              minLength={component.minLength(component.inputType)}
              {... maxLength ? { maxLength: maxLength } : {}}
              min={component.minInclusive()}
              max={component.maxInclusive()}
              {...(pattern ? { pattern: patternToSource(pattern) } : {})}
              {...textareaConstraintProps}
              className={combinedClassName}
            />
            {showUndo && (
              <button
                type="button"
                className="btn btn-sm ms-2 px-2 py-0 d-flex align-items-center justify-content-center"
                onMouseDown={(e) => e.preventDefault()}
                onClick={handleUndoClick}
                ref={component.undoRef}
                aria-label={i18next.t("smartfield_undo", { ns: "preact" })}
              >
                ↺
              </button>
            )}
          </div>);
    }
  }
}

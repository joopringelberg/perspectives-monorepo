import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { createRequire } from 'node:module';
import { test } from 'node:test';
import { URL } from 'node:url';
import { runInNewContext } from 'node:vm';
import React from 'react';
import ts from 'typescript';

const require = createRequire(import.meta.url);

// Shallow rendering isolates the form from PDR subscriptions and browser controls.
function loadForm() {
  const source = readFileSync(new URL('../src/perspectivebasedform.tsx', import.meta.url), 'utf8');
  const code = ts.transpileModule(source, {
    compilerOptions: { module: ts.ModuleKind.CommonJS, jsx: ts.JsxEmit.React, esModuleInterop: true }
  }).outputText;
  const component = name => ({ default: name, __esModule: true });
  const mocks = {
    'react': React,
    'perspectives-proxy': {},
    './perspectivesComponent': { default: React.Component, __esModule: true },
    './maproleverbstobehaviours.js': { mapRoleVerbsToBehaviourNames: () => [] },
    './roleDropzone.js': component('RoleDropZone'),
    './roleinstance.js': component('RoleInstance'),
    './smartfieldcontrolgroup.js': component('SmartFieldControlGroup'),
    './formcontrols.js': component('FormControls'),
    './utilities': { orderPropertiesByPerspective: perspective => Object.values(perspective.properties) },
    'react-bootstrap': { Card: 'Card', Form: 'Form' },
    './userMessaging.js': {},
    'i18next': {},
    './adorningComponentWrapper.js': { cardWithConfigurableBehaviour: card => card },
    'lodash': require('lodash'),
    './urifunctions.js': { isInPublicScheme: () => false }
  };
  const exports = {};
  runInNewContext(code, {
    exports,
    require(name) {
      assert.ok(Object.hasOwn(mocks, name), `Unexpected form dependency: ${name}`);
      return mocks[name];
    }
  });
  return exports.default;
}

const PerspectiveBasedForm = loadForm();
const identifyingProperty = 'Name';

function renderForm(properties, showControls, suppressIdentifyingProperty = false) {
  return new PerspectiveBasedForm({
    perspective: { properties, identifyingProperty, roleInstances: {}, roleType: 'Role' },
    cardtitle: identifyingProperty,
    showControls,
    suppressIdentifyingProperty
  }).render();
}

test('forms with fields show controls only when enabled', () => {
  const properties = { Name: { id: 'Name' }, Remarks: { id: 'Remarks' } };
  assert.equal(renderForm(properties, true).props.children[1].type.name, 'Controls');
  assert.equal(renderForm(properties, false).props.children[1], null);
});

test('propertyless forms also respect disabled controls', () => {
  assert.equal(renderForm({}, true).type.name, 'Controls');
  assert.equal(renderForm({}, false), null);
});

test('identifier suppression removes only the identifying field', () => {
  const properties = { Name: { id: 'Name' }, Remarks: { id: 'Remarks' } };
  const fieldIds = form => Array.from(form.props.children[0], field => field.props.children.props.serialisedProperty.id);
  assert.deepEqual(fieldIds(renderForm(properties, true)), ['Name', 'Remarks']);
  assert.deepEqual(fieldIds(renderForm(properties, true, true)), ['Remarks']);
  assert.deepEqual(fieldIds(renderForm(properties, false, true)), ['Remarks']);
});

test('suppressing the only field does not restore disabled controls', () => {
  const form = renderForm({ Name: { id: 'Name' } }, false, true);
  assert.equal(form.props.children[0].length, 0);
  assert.equal(form.props.children[1], null);
});

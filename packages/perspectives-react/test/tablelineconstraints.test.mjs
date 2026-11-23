import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { runInNewContext } from 'node:vm';
import React from 'react';
import ts from 'typescript';

const modules = {
  react: React,
  './perspectivesComponent': { default: React.Component, __esModule: true },
  'perspectives-proxy': { mapRange: () => 'text' },
  'react-bootstrap': { Table: 'table', Form: { Control: 'control' } },
  '././styles/components.css': {},
  './reactcontexts': { PSRol: React.createContext({}) },
  './maproleverbstobehaviours.js': { mapRoleVerbsToBehaviourNames: () => [] },
  './tablecontrols.js': { default: 'TableControls', __esModule: true },
  './tableItemContextMenu': { default: 'TableItemContextMenu', __esModule: true },
  './roleinstance.js': { default: 'RoleInstance', __esModule: true },
  './urifunctions.js': { isInPublicScheme: () => false },
  './modelDependencies': { default: {}, __esModule: true },
  './userMessaging.js': {},
  './perspectivesFile.js': {},
  './markdownWidget.js': {},
  i18next: { t: key => key },
  './utilities': {
    formatPropertyValue: values => values.join(', '),
    orderPropertiesByPerspective: p => p.propertyOrder.map(id => p.properties[id])
  }
};
modules['./utilities.js'] = modules['./utilities'];

function load(filename) {
  const source = readFileSync(new URL(`../src/${filename}`, import.meta.url), 'utf8');
  const code = ts.transpileModule(source, {
    compilerOptions: { module: ts.ModuleKind.CommonJS, jsx: ts.JsxEmit.React, esModuleInterop: true }
  }).outputText;
  const exports = {};
  runInNewContext(code, {
    exports,
    require(name) {
      assert.ok(Object.hasOwn(modules, name), `Unexpected ${filename} dependency: ${name}`);
      return modules[name];
    }
  });
  return exports;
}

modules['./smartfieldcontrol.js'] = load('smartfieldcontrol.tsx');
const cellModule = load('tablecell.tsx');
modules['./tablecell.js'] = cellModule;
modules['./tablerow.js'] = load('tablerow.tsx');
const { AdornWithBehaviour } = load('adornwithbehaviour.tsx');
modules['./adorningComponentWrapper'] = {
  CardWithFixedBehaviour: Card => props => React.createElement(AdornWithBehaviour, { ...props, Card, addedBehaviour: [] })
};
const { default: PerspectiveTable } = load('perspectivetable.tsx');

function controls(element, editing) {
  if (!React.isValidElement(element)) return [];
  if (typeof element.type === 'function') {
    if (element.type.prototype?.render) {
      const instance = new element.type(element.props);
      if (instance instanceof cellModule.default) instance.state.editable = editing;
      return controls(instance.render(), editing);
    }
    return controls(element.type(element.props), editing);
  }
  if (element.type === 'control') return [element.props];
  return React.Children.toArray(element.props.children).flatMap(child => controls(child, editing));
}

function renderControls(fieldConstraints, editing = false) {
  const propertyOrder = ['Title', 'Description'];
  const row = {
    roleId: 'item',
    propertyValues: Object.fromEntries(propertyOrder.map(id => [id, {
      values: [id], propertyVerbs: ['Consult', 'SetPropertyValue']
    }]))
  };
  const table = new PerspectiveTable({
    perspective: {
      id: 'table', identifyingProperty: 'Title', displayName: 'Items',
      propertyOrder, verbs: [],
      properties: Object.fromEntries(propertyOrder.map(id => [id, { id, range: 'PString', constrainingFacets: {} }])),
      roleInstances: { item: row }
    },
    fieldConstraints
  });
  return controls(table.constructTable(), editing);
}

for (const editing of [false, true]) {
  test(`table line constraints reach ordinary and identifying controls (editing: ${editing})`, () => {
    const fields = [
      { propertyType: { tag: 'ENP', value: 'Title' }, minLines: 2, maxLines: 4 },
      { propertyType: { tag: 'CP', value: 'Description' }, minLines: 3, maxLines: 6 }
    ];
    const rendered = renderControls(fields, editing);
    assert.equal(rendered.length, 2);
    for (const [index, control] of rendered.entries()) {
      assert.equal(control.as, 'textarea');
      assert.equal(control.rows, fields[index].minLines);
      assert.equal(control.style.maxHeight, `${fields[index].maxLines * 1.5}em`);
      assert.equal(control.style.overflowY, 'auto');
      assert.equal(control.readOnly, !editing);
    }
  });
}

test('table fields without line constraints retain their single-line controls', () => {
  for (const control of renderControls(undefined)) {
    assert.equal(control.as, 'input');
    assert.equal(control.rows, undefined);
  }
});

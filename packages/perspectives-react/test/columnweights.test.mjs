import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { runInNewContext } from 'node:vm';
import React from 'react';
import ts from 'typescript';

function loadTable() {
  const source = readFileSync(new URL('../src/perspectivetable.tsx', import.meta.url), 'utf8');
  const code = ts.transpileModule(source, {
    compilerOptions: { module: ts.ModuleKind.CommonJS, jsx: ts.JsxEmit.React, esModuleInterop: true }
  }).outputText;
  const component = name => ({ default: name, __esModule: true });
  const mocks = {
    'react': React,
    './perspectivesComponent': { default: React.Component, __esModule: true },
    './maproleverbstobehaviours.js': {},
    './tablerow.js': component('TableRow'),
    './tablecontrols.js': component('TableControls'),
    'react-bootstrap': { Table: 'table', Form: { Control: 'Form.Control' } },
    '././styles/components.css': {},
    './adorningComponentWrapper': {},
    './reactcontexts': {},
    './tableItemContextMenu': component('TableItemContextMenu'),
    './utilities': {
      orderPropertiesByPerspective: p => p.propertyOrder.map(id => p.properties[id]).filter(Boolean)
    }
  };
  const exports = {};
  runInNewContext(code, {
    exports,
    require(name) {
      assert.ok(Object.hasOwn(mocks, name), `Unexpected table dependency: ${name}`);
      return mocks[name];
    }
  });
  return exports.default;
}

const PerspectiveTable = loadTable();
const constraint = (property, columnWeight) => ({
  propertyType: { tag: 'ENP', value: property },
  columnWeight
});

function renderTable(fieldConstraints, propertyOrder = ['Status', 'Title', 'Description'], sortOnHiddenProperty) {
  return new PerspectiveTable({
    perspective: {
      id: 'table', roleInstances: {}, identifyingProperty: 'Title',
      propertyOrder,
      properties: Object.fromEntries(propertyOrder.map(id => [id, { id, displayName: id }]))
    },
    fieldConstraints,
    sortOnHiddenProperty
  }).constructTable();
}

function columns(table) {
  const group = React.Children.toArray(table.props.children).find(child => child.type === 'colgroup');
  return React.Children.toArray(group.props.children);
}

test('table columns receive normalized 1:2:3 widths in display order', () => {
  const table = renderTable([constraint('Description', 3), constraint('Status', 1), constraint('Title', 2)]);
  assert.match(table.props.className, /weighted-columns/);
  assert.deepEqual(columns(table).map(col => Number.parseFloat(col.props.style.width)), [100 / 6, 200 / 6, 50]);
});

test('unspecified columns default to weight one and hidden properties consume no width', () => {
  const table = renderTable(
    [constraint('Title', 2), constraint('Hidden', 100), constraint('SortKey', 100)],
    ['Description', 'Title', 'Status', 'SortKey'],
    'SortKey'
  );
  assert.deepEqual(columns(table).map(col => col.props.style.width), ['25%', '50%', '25%']);
});

test('weights do not activate fixed layout when no visible column specifies one', () => {
  for (const constraints of [undefined, [], [constraint('Hidden', 3)], [{ ...constraint('Title'), minLines: 2 }]]) {
    const table = renderTable(constraints);
    assert.equal(table.props.className, 'mb-0');
    assert.equal(React.Children.toArray(table.props.children).some(child => child.type === 'colgroup'), false);
  }
});

test('a single weighted column occupies the full table width', () => {
  assert.deepEqual(columns(renderTable([constraint('Title', 3)], ['Title'])).map(col => col.props.style.width), ['100%']);
});

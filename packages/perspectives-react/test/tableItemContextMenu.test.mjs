import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { createRequire } from 'node:module';
import { test } from 'node:test';
import { runInNewContext } from 'node:vm';
import React from 'react';
import i18next from 'i18next';
import ts from 'typescript';

const require = createRequire(import.meta.url);
const source = readFileSync(new URL('../src/tableItemContextMenu.tsx', import.meta.url), 'utf8');
const code = ts.transpileModule(source, {
  compilerOptions: { module: ts.ModuleKind.CommonJS, jsx: ts.JsxEmit.React, esModuleInterop: true }
}).outputText;

async function loadMenu({ providedTypeCompatible = true, requiredRole = 'copied', providedName = 'Person', requiredName = 'Person', language = 'en' } = {}) {
  const i18n = i18next.createInstance();
  await i18n.init({
    lng: language,
    resources: {
      [language]: {
        preact: JSON.parse(readFileSync(new URL(`../src/lang/${language}/preact.json`, import.meta.url), 'utf8'))
      }
    }
  });
  const proxy = {
    checkBindingP: async () => providedTypeCompatible,
    getRoleNameP: async () => providedName,
    getMostGeneralAllowedBindingType: async () => requiredRole === undefined ? undefined : ({
      roleType: 'PersonType', readableName: requiredName, roleInstance: requiredRole
    }),
    bind_: async (...args) => { proxy.lastBinding = args; return true; }
  };
  const mocks = {
    'react': React,
    'react-bootstrap': { Dropdown: { Item: 'Dropdown.Item' } },
    'i18next': i18n,
    'perspectives-proxy': { PDRproxy: Promise.resolve(proxy) },
    './userMessaging': {},
    './reactcontexts': {},
    './urifunctions': {},
    'lodash': require('lodash'),
    './cardbehaviour': {}
  };
  const exports = {};
  runInNewContext(code, {
    exports,
    require(name) {
      assert.ok(Object.hasOwn(mocks, name), `Unexpected menu dependency: ${name}`);
      return mocks[name];
    }
  });
  const menu = new exports.default({
    perspective: {
      roleType: 'TargetType', userRoleType: 'Author', verbs: ['Fill'],
      roleInstances: { target: {} }
    },
    roleinstance: 'target',
    roleOnClipboard: { roleData: { rolinstance: 'copied', cardTitle: 'Alice' } },
    isOpen: true
  });
  await new Promise(resolve => {
    menu.setState = state => { Object.assign(menu.state, state); resolve(); };
    menu.refreshClipboardCompatibility();
  });
  return { menu, proxy };
}

for (const replacing of [false, true]) {
  const operation = replacing ? 'replacing a filler' : 'filling an empty role';
  const itemsFor = menu => {
    if (replacing) menu.props.perspective.roleInstances.target.filler = 'previous';
    return replacing ? menu.computeReplaceFillerItem() : menu.computeFillItem();
  };

  test(`${operation}: equivalent targets appear once even with different type labels`, async () => {
    const { menu } = await loadMenu({ providedName: 'Persoon', requiredName: 'Person' });
    const items = itemsFor(menu);
    assert.equal(items.length, 1);
    assert.equal(items[0].props.children, 'Fill with Alice (Persoon)');
  });

  test(`${operation}: distinct targets retain both commands even with identical type names`, async () => {
    const { menu, proxy } = await loadMenu({ requiredRole: 'underlying' });
    const items = itemsFor(menu);
    assert.equal(items.length, 2);
    assert.equal(items[0].props.children, 'Fill with Alice (Person)');
    assert.equal(items[1].props.children, 'Fill with underlying filler of Alice (Person)');
    items[0].props.onClick();
    await Promise.resolve();
    assert.deepEqual(proxy.lastBinding, ['target', 'copied', 'Author', 'provided']);
    items[1].props.onClick();
    await Promise.resolve();
    assert.deepEqual(proxy.lastBinding, ['target', 'copied', 'Author', 'required']);
  });

  test(`${operation}: an underlying filler remains available when the copied role is incompatible`, async () => {
    const { menu } = await loadMenu({
      providedTypeCompatible: false, requiredRole: 'underlying',
      requiredName: 'Persoon', language: 'nl'
    });
    const items = itemsFor(menu);
    assert.equal(items.length, 1);
    assert.equal(items[0].props.children, 'Vul met de onderliggende vuller van Alice (Persoon)');
  });
}

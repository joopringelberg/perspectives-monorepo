import assert from 'node:assert/strict';
import { readFile, readdir, stat } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { runInNewContext } from 'node:vm';
import { test } from 'node:test';

const output = new URL('../output/', import.meta.url);

test('multipage HTML includes the manual and all local assets and page targets', async () => {
  const pages = (await readdir(output)).filter(name => name.endsWith('.html'));
  assert.ok(pages.includes('overview.html'));
  assert.ok(pages.includes('concept-screen.html'));
  assert.ok(pages.length > 20, `Expected multipage output, got ${pages.length} pages`);

  for (const page of pages) {
    const html = await readFile(new URL(page, output), 'utf8');
    assert.doesNotMatch(html, /Unresolved directive/);
    assert.match(html, /src="scripts\/highlight\.js"/);
    assert.doesNotMatch(html, /cdnjs.*highlight/);

    for (const [, target] of html.matchAll(/(?:src|href)="([^"]+)"/g)) {
      if (/^(?:[a-z][a-z0-9+.-]*:|\/\/|#)/i.test(target)) continue;
      const path = new URL(target.split('#')[0], output);
      assert.ok((await stat(path)).isFile(), `${page} references missing file ${target}`);
    }
  }

  const overview = await readFile(new URL('overview.html', output), 'utf8');
  assert.match(overview, /The Perspectives Language Reference Manual/);
  assert.match(overview, /adoc-colony/);
});

test('browser highlighting bundle runs without external scripts or module loading', async () => {
  const script = await readFile(new URL('scripts/highlight.js', output), 'utf8');
  let highlighted = false;
  runInNewContext(script, {
    document: {
      readyState: 'complete',
      querySelectorAll(selector) {
        assert.equal(selector, 'pre code');
        highlighted = true;
        return [];
      }
    }
  }, { filename: fileURLToPath(new URL('scripts/highlight.js', output)) });
  assert.ok(highlighted);
});

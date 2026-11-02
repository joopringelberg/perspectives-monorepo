import { spawnSync } from 'node:child_process';
import { cp, mkdir, rm } from 'node:fs/promises';
import { createRequire } from 'node:module';
import { dirname, join } from 'node:path';
import process from 'node:process';
import { fileURLToPath } from 'node:url';
import commonjs from '@rollup/plugin-commonjs';
import { nodeResolve } from '@rollup/plugin-node-resolve';
import { rollup } from 'rollup';

const root = fileURLToPath(new URL('../', import.meta.url));
const output = join(root, 'output');
const require = createRequire(import.meta.url);

await rm(output, { recursive: true, force: true });

const result = spawnSync('bundle', [
  'exec', 'asciidoctor-multipage',
  '--failure-level', 'WARN', 'overview.adoc', '-D', 'output'
], { cwd: root, stdio: 'inherit' });

if (result.error) {
  throw new Error('Cannot run Bundler. Install Ruby and Bundler, then run pnpm --filter perspectives-reference setup.', { cause: result.error });
}
if (result.status !== 0) {
  throw new Error(`Asciidoctor failed (${result.signal ?? result.status}). Run pnpm --filter perspectives-reference setup to install the locked Ruby dependencies.`);
}

const bundle = await rollup({
  input: join(root, 'scripts/highlight.js'),
  plugins: [nodeResolve(), commonjs()]
});
try {
  await bundle.write({
    file: join(output, 'scripts/highlight.js'),
    format: 'iife'
  });
} finally {
  await bundle.close();
}

await mkdir(join(output, 'styles'), { recursive: true });
await Promise.all([
  cp(join(root, 'images'), join(output, 'images'), { recursive: true }),
  cp(join(root, 'extrastyles.css'), join(output, 'styles/extrastyles.css')),
  cp(join(dirname(require.resolve('highlight.js/package.json')), 'styles/github.css'),
    join(output, 'styles/github.css'))
]);

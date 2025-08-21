// build-module-map.mjs
import { readdirSync, readFileSync } from 'fs'
import { join } from 'path'

const skip = p => /(^|\/)(output|\.spago|node_modules|dist|test)\//.test(p)
function* walk(dir) {
  for (const e of readdirSync(dir, { withFileTypes: true })) {
    const p = join(dir, e.name)
    if (skip(p)) continue
    if (e.isDirectory()) yield* walk(p)
    else if (e.isFile() && p.endsWith('.purs')) yield p
  }
}

const re = /^\s*module\s+([A-Za-z0-9_.]+)\s+where/m
const map = {}
for (const file of walk(process.cwd())) {
  const m = re.exec(readFileSync(file, 'utf8'))
  if (m) map[m[1]] = file
}
process.stdout.write(JSON.stringify(map, null, 2))
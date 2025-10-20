#!/usr/bin/env node
import fs from 'node:fs/promises'
import path from 'node:path'
import url from 'node:url'
import https from 'node:https'

const __dirname = path.dirname(url.fileURLToPath(import.meta.url))
const root = path.resolve(__dirname, '..')
// Source and generated output live in scripts/ and are out of Spago's scope.
const defaultInFile = path.join(__dirname, 'modelDependencies_source.purs')
let outFile = path.join(__dirname, 'modelDependencies.purs')

// CLI args (support both --flag=value and --flag value)
const argv = process.argv.slice(2)
function parseArgs(argv) {
  const out = { dryRun: false, cacheDir: null, inFile: null, outFile: null, refresh: false, preferDoc: false }
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i]
    if (a === '--dry-run') { out.dryRun = true; continue }
    if (a === '--refresh') { out.refresh = true; continue }
    if (a === '--prefer-doc') { out.preferDoc = true; continue }
    if (a === '--cache-dir') { out.cacheDir = argv[i + 1]; i++; continue }
    if (a === '--in-file') { out.inFile = argv[i + 1]; i++; continue }
    if (a === '--out-file') { out.outFile = argv[i + 1]; i++; continue }
    if (a.startsWith('--cache-dir=')) { out.cacheDir = a.split('=')[1]; continue }
    if (a.startsWith('--in-file=')) { out.inFile = a.split('=')[1]; continue }
    if (a.startsWith('--out-file=')) { out.outFile = a.split('=')[1]; continue }
  }
  return out
}
const parsed = parseArgs(argv)
const dryRun = parsed.dryRun
const srcFile = parsed.inFile ? path.resolve(parsed.inFile) : defaultInFile
if (parsed.outFile) outFile = path.resolve(parsed.outFile)
const cacheDir = parsed.cacheDir ? path.resolve(parsed.cacheDir) : path.join(root, '.cache', 'sidecars')
const refresh = parsed.refresh || process.env.PDR_REFRESH_SIDECARS === '1' || process.env.PDR_REFRESH_SIDECARS === 'true'
const preferDoc = parsed.preferDoc || process.env.PDR_PREFER_DOC === '1' || process.env.PDR_PREFER_DOC === 'true'

// Config: versions per stable model uri (optional, can be omitted for now)
const configPath = path.join(__dirname, 'transpile.config.json')
let config = { models: {} }
try {
  const raw = await fs.readFile(configPath, 'utf8')
  config = JSON.parse(raw)
} catch {
  // optional
}

// Regexes (translated from identifiers.purs)
const typePattern = /^(model:\/\/[^/]+#[^$]+)\$?(.*)$/
const localNameRegex = /^model:\/\/[^/]+#[^#$\/]+\$([^$\/]+)$/

// Local helpers re-implementing small parts used by transitionaryTypeSwitching
const splitTypeUri = (s) => {
  const m = s.match(typePattern)
  if (!m) return null
  const modelUri = m[1]
  const localName = m[2] ?? ''
  return { modelUri, localName }
}

const newModelPattern = /^model:\/\/([^/]+)#([^$\/]+)$/
const modelUri2ModelUrl = (s) => {
  const m = s.match(newModelPattern)
  if (!m) throw new Error(`modelUri2ModelUrl: not a model uri: ${s}`)
  const authority = m[1]
  const localModelName = m[2]
  const parts = authority.split('.')
  if (parts.length < 2) throw new Error('authority needs at least two parts')
  const toplevel = parts[parts.length - 1]
  const secondLevel = parts[parts.length - 2]
  const ns = parts.join('_')
  return {
    repositoryUrl: `https://${secondLevel}.${toplevel}/models_${ns}`,
    documentName: `${ns}-${localModelName}.json`
  }
}

// Load modelReadableToStable from src/core/readableStableMappings.purs
async function loadReadableToStable() {
  const mappingPath = path.join(root, 'src', 'core', 'readableStableMappings.purs')
  try {
    const file = await fs.readFile(mappingPath, 'utf8')
    const start = file.indexOf('modelReadableToStable = fromFoldable')
    if (start === -1) return {}
    const chunk = file.slice(start)
    const arrayStart = chunk.indexOf('[')
    const arrayEnd = chunk.indexOf(']\n', arrayStart)
    const body = chunk.slice(arrayStart + 1, arrayEnd === -1 ? undefined : arrayEnd)
    const entries = [...body.matchAll(/\(Tuple\s+"([^"]+)"\s+"([^"]+)"\)/g)].map(([, k, v]) => [k, v])
    return Object.fromEntries(entries)
  } catch (e) {
    console.warn(`[transpile] Could not read mappings at ${path.relative(root, mappingPath)}: ${e.message}`)
    return {}
  }
}

// Simple fetch with cache and optional refresh; adds cache-busting
async function fetchWithCache(urlStr, cachePath, { refresh = false } = {}) {
  await fs.mkdir(path.dirname(cachePath), { recursive: true })
  if (!refresh) {
    try {
      const data = await fs.readFile(cachePath, 'utf8')
      return data
    } catch {}
  }
  const u = new URL(urlStr)
  if (refresh) {
    u.searchParams.set('_ts', Date.now().toString())
  }
  const data = await new Promise((resolve, reject) => {
    const options = {
      headers: refresh ? { 'Cache-Control': 'no-cache', 'Pragma': 'no-cache' } : undefined
    }
    https.get(u, options, (res) => {
      if (res.statusCode !== 200) {
        reject(new Error(`HTTP ${res.statusCode} for ${u.toString()}`))
        res.resume()
        return
      }
      let chunks = ''
      res.setEncoding('utf8')
      res.on('data', (d) => (chunks += d))
      res.on('end', () => resolve(chunks))
    }).on('error', reject)
  })
  await fs.writeFile(cachePath, data, 'utf8')
  return data
}

// Build stable id for a readable FQN using a StableIdMapping JSON
function idUriForContext(m, ctxFqn) {
  const ns = typeUri2typeNameSpace_(ctxFqn)
  // Root context base-case: only if mapping recognizes this FQN as a context
  if (ns === ctxFqn) {
    const cuid = lookupContextCuid(m, ctxFqn)
    return cuid ? m.modelIdentifier : null
  }
  const nsTid = idUriForContext(m, ns)
  const cuid = lookupContextCuid(m, ctxFqn)
  return nsTid && cuid ? `${nsTid}$${cuid}` : null
}
function idUriForRole(m, roleFqn) {
  if (!roleFqn.includes('$')) return null
  const cuid = lookup(m.roleCuids, m.roles, roleFqn)
  if (!cuid) return null
  const ctxFqn = typeUri2typeNameSpace_(roleFqn)
  const ctxTid = idUriForContext(m, ctxFqn)
  return ctxTid && cuid ? `${ctxTid}$${cuid}` : null
}
function idUriForProperty(m, propFqn) {
  const cuid = lookup(m.propertyCuids, m.properties, propFqn)
  if (!cuid) return null
  const roleFqn = typeUri2typeNameSpace_(propFqn)
  const roleTid = idUriForRole(m, roleFqn)
  return roleTid && cuid ? `${roleTid}$${cuid}` : null
}
function idUriForView(m, viewFqn) {
  const cuid = lookup(m.viewCuids, m.views, viewFqn)
  if (!cuid) return null
  const roleFqn = typeUri2typeNameSpace_(viewFqn)
  const roleTid = idUriForRole(m, roleFqn)
  return roleTid && cuid ? `${roleTid}$${cuid}` : null
}
function idUriForState(m, stateFqn) {
  if (!stateFqn.includes('$')) return null
  const nsFqn = typeUri2typeNameSpace_(stateFqn)
  // Root state of role or context: if the FQN is exactly a role or context, reuse their TIDs.
  const roleCuid = lookup(m.roleCuids, m.roles, stateFqn)
  if (roleCuid) return idUriForRole(m, stateFqn)
  const ctxCuid = lookupContextCuid(m, stateFqn)
  if (ctxCuid) return idUriForContext(m, stateFqn)
  // Non-root nested state
  const stTid = lookup(m.stateCuids, m.states, stateFqn)
  if (!stTid) return null
  if (nsFqn === stateFqn) return null
  const parent = idUriForState(m, nsFqn)
  return parent ? `${parent}$${stTid}` : null
}
function idUriForAction(m, actionFqn) {
  // Only proceed if this FQN is recognized as an action
  const actTid = lookup(m.actionCuids, m.actions, actionFqn)
  if (!actTid) return null
  const stateFqn = typeUri2typeNameSpace_(actionFqn)
  const stateTid = idUriForState(m, stateFqn)
  return stateTid && actTid ? `${stateTid}$${actTid}` : null
}
function typeUri2typeNameSpace_(s) {
  const idx = s.lastIndexOf('$')
  return idx === -1 ? s : s.slice(0, idx)
}
function typeUri2LocalName(s) {
  const idx = s.lastIndexOf('$')
  return idx === -1 ? null : s.slice(idx + 1)
}
function lookup(cuids, aliases, fqn) {
  if (cuids && cuids[fqn]) return cuids[fqn]
  if (aliases && aliases[fqn] && cuids[aliases[fqn]]) return cuids[aliases[fqn]]
  return null
}
// Context lookup is similar to generic lookup but also treats the model root as present
// when a modelIdentifier exists. This mirrors that the root context resolves to the
// modelIdentifier without needing a specific context cuid.
function lookupContextCuid(m, fqn) {
  if (m && m.contextCuids && m.contextCuids[fqn]) return m.contextCuids[fqn]
  if (m && m.contexts && m.contextCuids) {
    const canonical = m.contexts[fqn]
    if (canonical && m.contextCuids[canonical]) return m.contextCuids[canonical]
  }
  return null
}
function escapeRegex(s) {
  return s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
}

// Individuals: modelUri + single $local segment; lookup by last segment in contextIndividuals/roleIndividuals
function hasSingleLocalSegment(s) {
  const count = (s.match(/\$/g) || []).length
  return count === 1
}

// Main
;(async () => {
  // Map readable model URIs to stable ones (fallback to given URI if unmapped)
  const readableToStable = await loadReadableToStable()
  const text = await fs.readFile(srcFile, 'utf8')

  // Find all string literals containing model:// URIs
  const stringLitRe = /"model:\/\/[^"]+"/g
  const matches = [...text.matchAll(stringLitRe)].map(m => ({ match: m[0], index: m.index }))

  // Deduplicate by string value
  const unique = new Map()
  for (const m of matches) unique.set(m.match, m)

  const replacements = new Map()
  for (const [str] of unique) {
    const raw = str.slice(1, -1) // remove quotes
    const parts = splitTypeUri(raw)
    if (!parts) continue
  const { modelUri } = parts
  const stableModelUri = readableToStable[modelUri] || modelUri
  // Versioned name for sidecar fetch (no version in final URIs). Prefer version keyed by stable model URI.
  const version = (config.models?.[stableModelUri]) ?? (config.models?.[modelUri])
  const uriForFetch = version ? `${stableModelUri}@${version}` : stableModelUri
  const { repositoryUrl, documentName } = modelUri2ModelUrl(uriForFetch)
    // Build URLs and caches
    const attachmentUrl = `${repositoryUrl}/${documentName}/stableIdMapping.json`
    const attachmentCache = path.join(cacheDir, `${documentName}-stableIdMapping.json`)
    const docUrl = `${repositoryUrl}/${documentName}`
    const docCache = path.join(cacheDir, `${documentName}.json`)

    let mapping
    const tryAttachment = async () => {
      const text = await fetchWithCache(attachmentUrl, attachmentCache, { refresh })
      return JSON.parse(text)
    }
    const tryDocument = async () => {
      const docText = await fetchWithCache(docUrl, docCache, { refresh })
      const mappingDoc = JSON.parse(docText)
      if (mappingDoc && mappingDoc._attachments && mappingDoc._attachments['stableIdMapping.json'] && mappingDoc._attachments['stableIdMapping.json'].data) {
        const b64 = mappingDoc._attachments['stableIdMapping.json'].data
        const buf = Buffer.from(b64, 'base64')
        return JSON.parse(buf.toString('utf8'))
      } else if (mappingDoc && mappingDoc.stableIdMapping) {
        return mappingDoc.stableIdMapping
      } else if (mappingDoc && mappingDoc.version && mappingDoc.modelIdentifier) {
        return mappingDoc
      } else {
        throw new Error('No stableIdMapping in document')
      }
    }

    try {
      if (preferDoc) {
        mapping = await tryDocument()
      } else {
        mapping = await tryAttachment()
      }
    } catch (e) {
      const primary = preferDoc ? 'document' : 'attachment'
      const secondary = preferDoc ? 'attachment' : 'document'
      console.warn(`[transpile] Sidecar ${primary} fetch failed for ${uriForFetch} (${e.message}). Trying ${secondary}…`)
      try {
        mapping = preferDoc ? await tryAttachment() : await tryDocument()
      } catch (e2) {
        // If we tried versioned and failed, try unversioned stable as best-effort.
        if (version) {
          try {
            const { repositoryUrl: repo2, documentName: doc2 } = modelUri2ModelUrl(stableModelUri)
            const att2 = `${repo2}/${doc2}/stableIdMapping.json`
            const cache2 = path.join(cacheDir, `${doc2}-stableIdMapping.json`)
            const txt2 = await fetchWithCache(att2, cache2, { refresh })
            mapping = JSON.parse(txt2)
          } catch (e3) {
            console.warn(`[transpile] Failed to fetch mapping for ${uriForFetch}: ${e2.message}`)
            continue
          }
        } else {
          console.warn(`[transpile] Failed to fetch mapping for ${uriForFetch}: ${e2.message}`)
          continue
        }
      }
    }

    // Build stable id: capability-driven order avoids fragile $-segment counting
    const fqn = raw
    let stable = null

    // Indexed individuals (context or role) live as modelUri + one segment; last segment is unique within the model
    if (hasSingleLocalSegment(fqn)) {
      if (mapping.contextIndividuals && mapping.contextIndividuals[fqn]) {
        stable = mapping.contextIndividuals[fqn]
      } else if (mapping.roleIndividuals && mapping.roleIndividuals[fqn]) {
        stable = mapping.roleIndividuals[fqn]
      }
    }
    const isAction = !!lookup(mapping.actionCuids, mapping.actions, fqn)
    const isProperty = !!lookup(mapping.propertyCuids, mapping.properties, fqn)
    const isView = !!lookup(mapping.viewCuids, mapping.views, fqn)
    const isState = !!lookup(mapping.stateCuids, mapping.states, fqn)
    const isRole = !!lookup(mapping.roleCuids, mapping.roles, fqn)
    const isContext = !!lookupContextCuid(mapping, fqn)


    if (!stable && isAction) stable = idUriForAction(mapping, fqn)
    else if (!stable && isProperty) stable = idUriForProperty(mapping, fqn)
    else if (!stable && isView) stable = idUriForView(mapping, fqn)
    else if (!stable && isState) stable = idUriForState(mapping, fqn)
    else if (!stable && isRole) stable = idUriForRole(mapping, fqn)
    else if (!stable && isContext) stable = idUriForContext(mapping, fqn)

    if (!stable) {
      console.warn(`[transpile] No mapping found for ${fqn}`)
      continue
    }

    // Ensure no version in final stable (as requested)
    if (/@/.test(stable)) {
      console.warn(`[transpile] Unexpected version in stable URI; stripping: ${stable}`)
      stable = stable.replace(/@[^$]+/, '')
    }

    replacements.set(str, `"${stable}"`)
  }

  if (replacements.size === 0) {
    console.log('[transpile] No changes needed.')
    process.exit(0)
  }

  // Apply replacements conservatively
  let out = text
  for (const [from, to] of replacements) {
    const re = new RegExp(escapeRegex(from), 'g')
    out = out.replace(re, to)
  }

  if (dryRun) {
    console.log(`[transpile] Would replace ${replacements.size} string(s). Use without --dry-run to apply.`)
    process.exit(0)
  }

  await fs.writeFile(outFile, out, 'utf8')
  console.log(`[transpile] Wrote ${replacements.size} replacement(s) to ${path.relative(root, outFile)} from ${path.relative(root, srcFile)}`) 
})().catch((e) => {
  console.error('[transpile] Error:', e)
  process.exit(1)
})

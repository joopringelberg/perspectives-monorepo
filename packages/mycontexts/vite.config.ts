import { defineConfig } from 'vite'
// import react from '@vitejs/plugin-react-swc'
import del from 'rollup-plugin-delete'
import glob from 'fast-glob'
import fs from 'fs'
import { resolve } from 'path'
import { nodeResolve } from '@rollup/plugin-node-resolve';

import { default as thepackage } from './package.json'

// Get path to monorepo root
const monorepoRoot = resolve(__dirname, '../..');

const pageDispatcherVersion = "1";

// https://vite.dev/config/
export default defineConfig(({ mode }) => {
  const isDev = mode === 'development'
  const devBase = '/'
  const { build, buildPath } = JSON.parse(fs.readFileSync("./build.json", { encoding: "utf-8" }))

  // module-scope guards
  let devSwDone = false

  const generateServiceWorkerDev = {
    name: 'generate-service-worker-dev',
    apply: 'serve' as const,            // dev only
    async buildStart() {
      if (devSwDone) return
      devSwDone = true
      console.log(`Generating development service worker (webroot: ${buildPath})...`)
      let swContent = fs.readFileSync('./src/perspectives-serviceworker.js', 'utf8')
      const devFiles = [
        `${buildPath}index.html`,
        `${buildPath}manage.html`,
        `${buildPath}assets/main.js`,
        `${buildPath}assets/main.css`,
      ]
      const fileListStr = devFiles.map(f => `"${f}"`).join(',\n  ')
      if (swContent.includes('const appFiles = [')) {
        swContent = swContent.replace(/const appFiles = \[\s*[\s\S]*?\];/m, `const appFiles = [\n  ${fileListStr}\n];`)
      } else {
        swContent = swContent.replace(/(const cacheName = .*)/, `$1\n\nconst appFiles = [\n  ${fileListStr}\n];`)
      }
      swContent = swContent.replace(/__MYCONTEXTS_VERSION__/g, JSON.stringify(thepackage.version))
      swContent = swContent.replace(/__BUILD__/g, JSON.stringify(build))
      fs.writeFileSync('./public/perspectives-serviceworker.js', swContent)
      console.log('Development service worker generated at ./public/perspectives-serviceworker.js')
    },
  }

  const generateServiceWorkerBuild = {
    name: 'generate-service-worker-build',
    apply: 'build' as const,            // build only
    async writeBundle() {
      let swContent = fs.readFileSync('./src/perspectives-serviceworker.js', 'utf8')
      const files = await glob('**/*', { cwd: resolve(__dirname, 'dist'), onlyFiles: true })
      const fileListStr = files.map(f => `"/${f}"`).join(',\n  ')
      if (swContent.includes('const appFiles = [')) {
        swContent = swContent.replace(/const appFiles = \[\s*[\s\S]*?\];/m, `const appFiles = [\n  ${fileListStr}\n];`)
      } else {
        swContent = swContent.replace(/(const cacheName = .*)/, `$1\n\nconst appFiles = [\n  ${fileListStr}\n];`)
      }
      swContent = swContent.replace(/__MYCONTEXTS_VERSION__/g, JSON.stringify(thepackage.version))
      swContent = swContent.replace(/__BUILD__/g, JSON.stringify(build))
      fs.writeFileSync('./dist/perspectives-serviceworker.js', swContent)
      console.log('Service worker generated with', files.length, 'files in cache list')
    },
  }

  // Plugin fallback: strip top-level "use client" from react-bootstrap ESM
  const silenceUseClient = {
    name: 'silence-use-client',
    enforce: 'pre' as const,
    transform(code: string, id: string) {
      if (!id.includes('node_modules/react-bootstrap/esm/')) return null
      const out = code.replace(/^\s*['"]use client['"];?\s*/,'')
      return out === code ? null : { code: out, map: null }
    }
  }

  return {
    // Suppress esbuild’s “ignored directive” warnings globally (build transforms)
    esbuild: {
      logOverride: {
        'ignored-directive': 'silent',
        'unsupported-directive': 'silent',
      },
    },
    base: isDev ? devBase : buildPath,
    server: {
      port: 5177,
      host: '0.0.0.0',
      https: {
        key: fs.readFileSync(resolve(monorepoRoot, 'certificates/key.pem')),
        cert: fs.readFileSync(resolve(monorepoRoot, 'certificates/cert.pem')),
      },
      fs: {
        allow: [
          '..', 
          '/Users/joopringelberg/Code/perspectives-monorepo/node_modules',
          '/Users/joopringelberg/Code/perspectives-monorepo/packages/perspectives-core/output'
        ]
      },
      headers: {
        // Critical headers for service worker development
        'Service-Worker-Allowed': '/',
        'Cache-Control': 'no-store, no-cache, must-revalidate, proxy-revalidate, max-age=0',
        'Pragma': 'no-cache',
        'Surrogate-Control': 'no-store',
        'Expires': '0',
        'Access-Control-Allow-Origin': '*',
        'Content-Security-Policy': "default-src * 'self' 'unsafe-inline' 'unsafe-eval' data: gap: https://ssl.gstatic.com; style-src * 'self' 'unsafe-inline'; script-src * 'self' 'unsafe-inline' 'unsafe-eval';"
      },
    },
    plugins: [
      silenceUseClient,               // fallback (keeps tree clean if logOverride isn’t honored)
      generateServiceWorkerDev,
      generateServiceWorkerBuild,
    ],
    resolve: {
      alias: {
        'perspectives-core': resolve( __dirname, '../perspectives-core/dist/perspectives-core.js'),
        'perspectives-sharedworker': resolve( __dirname, '../perspectives-sharedworker/dist/perspectives-sharedworker.js'),
        'perspectives-pageworker': resolve(__dirname, '../perspectives-pageworker/dist/perspectives-pageworker.js'),
        '@perspectives/core': resolve( __dirname, '../perspectives-core/src'),
        '@perspectives/proxy': resolve( __dirname, '../perspectives-proxy/src'),
        '@perspectives/react': resolve( __dirname, '../perspectives-react/src'),
        
        // Force a single React instance across all packages
        'react': resolve(__dirname, 'node_modules/react'),
        'react-dom': resolve(__dirname, 'node_modules/react-dom'),
        'react/jsx-runtime': resolve(__dirname, 'node_modules/react/jsx-runtime'),
        'react/jsx-dev-runtime': resolve(__dirname, 'node_modules/react/jsx-dev-runtime'),
        'react-is': resolve(monorepoRoot, 'node_modules/.pnpm/react-is@16.13.1/node_modules/react-is'),
      },
      dedupe: ['react', 'react-dom', 'react-is']
    },
    optimizeDeps: {
      include: [
        'react', 
        'react-dom', 
        'react/jsx-runtime',
        'react/jsx-dev-runtime',
      ],
      exclude: [        
        'react/cjs/react.production.min.js',
        'react-dom/cjs/react-dom.production.min.js'
      ],
      esbuildOptions: {
        mainFields: ['module', 'main'],
        resolveExtensions: ['.mjs', '.js', '.ts', '.jsx', '.tsx', '.json'],
        format: 'esm',
        // Suppress during dep pre-bundling as well
        logOverride: {
          'ignored-directive': 'silent',
          'unsupported-directive': 'silent',
        },
      },
    },
    build: {
      target: 'es2023',
      sourcemap: true,
      rollupOptions: {
        output: {
          entryFileNames: 'assets/[name]-[hash].js',
          chunkFileNames: 'assets/[name]-[hash].js',
          assetFileNames: 'assets/[name]-[hash].[ext]',
        },
        input: { main: './index.html', manage: './manage.html' },
        plugins: [
          del({ targets: 'dist/*' }),
          // removed generate-service-worker from here; handled by top-level plugins
        ],
      },
    },
    define: {
      __MYCONTEXTS_VERSION__: JSON.stringify(thepackage.version),
      __STARTPAGE__: JSON.stringify("pub:https://perspectives.domains/cw_ro6a1vrf9y/#wxl4tmx54i$External"),
      __MyContextsContainer__: JSON.stringify("root"),
      __PAGEDISPATCHER_VERSION__: pageDispatcherVersion,
      __BUILD__: JSON.stringify(build),
    },
  }
})

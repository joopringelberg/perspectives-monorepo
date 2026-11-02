import { defineConfig } from 'vite'
import fs from 'fs'
import { resolve } from 'path'

// The editor is served from `<MyContexts base path>arceditor/` on the same origin as MyContexts,
// so it can attach to MyContexts' SharedWorker. The base path is read from MyContexts' build.json.
const mycontextsBuild = JSON.parse(fs.readFileSync(resolve(__dirname, '../mycontexts/build.json'), 'utf8'))
const buildPath: string = process.env.BUILD_PATH || mycontextsBuild.buildPath || '/www/'

export default defineConfig({
  base: `${buildPath}arceditor/`,
  resolve: {
    alias: { 'perspectives-core': resolve(__dirname, 'src/noPageHostedCore.ts') },
  },
  build: {
    target: 'es2023',
    outDir: 'dist',
    emptyOutDir: true,
  },
})

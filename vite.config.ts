import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react-swc'
import tsconfigPaths from 'vite-tsconfig-paths'
import commonjs from 'vite-plugin-commonjs'
import { visualizer } from 'rollup-plugin-visualizer'
import del from 'rollup-plugin-delete'
import postcss from 'rollup-plugin-postcss';
import json from '@rollup/plugin-json';import { default as thepackage } from './package.json'

// https://vite.dev/config/
export default defineConfig({
  base: "/www/",
  server: {
    port: 5177, // Set a fixed port
    fs: {
      allow: [
        // Allow serving files from these directories
        '/Users/joopringelberg/Code/mycontexts',
        '/Users/joopringelberg/Code/perspectives-core',
        '/Users/joopringelberg/Code/perspectives-proxy',
        '/Users/joopringelberg/Code/perspectives-react',
        '/Users/joopringelberg/Code/perspectives-pageworker',
        '/Users/joopringelberg/Code/perspectives-sharedworker'
      ]
    }
  },
  plugins: [
    react(),
    commonjs(),
    tsconfigPaths()
  ],
  // resolve: {
  //   alias: {
  //     'perspectives-core': '../perspectives-core/dist/perspectives-core.js',
  //     '/perspectives-core.js': '../perspectives-core/dist/perspectives-core.js',
  //     'perspectives-pageworker': '../perspectives-pageworker/dist/perspectives-pageworker.js',
  //     'perspectives-sharedworker': '../perspectives-sharedworker/dist/perspectives-sharedworker.js'
  //   }
  // },
  optimizeDeps: {
    include: ['react', 'react-dom']
  },
  build: {
    target: 'es2023',
    sourcemap: true,
    rollupOptions: {
      input: {
        main: './index.html',
        manage: './manage.html'
      },
      // external: [
      //   '/Users/joopringelberg/Code/perspectives-core/dist/perspectives-core.js',
      //   '/Users/joopringelberg/Code/perspectives-pageworker/dist/perspectives-pageworker.js',
      //   '/Users/joopringelberg/Code/perspectives-sharedworker/dist/perspectives-sharedworker.js'
      // ],
      plugins: [
        del({ targets: 'dist/*' }), // Add this line to clear the dist directory
        // postcss({
        //   extract: true, // Extract CSS to a separate file
        //   minimize: true, // Minimize the CSS
        //   sourceMap: true // Generate source maps for the CSS
        //   }),
        // json(),
        visualizer({
          filename: './dist/stats.html',
          open: true
        })
      ]
    }
  },
  define: {
    __MYCONTEXTS_VERSION__: JSON.stringify(thepackage.version),
    __PPSTORAGELIMIT__: JSON.stringify(10),
    __PPWARNINGLEVEL__: JSON.stringify(5),
    __PPSTORAGEURL__: JSON.stringify("https://mycontexts.com/ppsfs/uploadfile")
  }
})

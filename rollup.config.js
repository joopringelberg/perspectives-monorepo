import typescript from '@rollup/plugin-typescript';
import resolve from '@rollup/plugin-node-resolve';
import del from 'rollup-plugin-delete';

export default {
  input: 'src/languages/perspectives-arc.ts',
  output: {
    file: 'dist/perspectives-arc.js',
    format: 'es'
  },
  plugins: [
    del({ targets: 'dist/*' }),
    resolve(),
    typescript({ tsconfig: './tsconfig.json' })
  ]
};
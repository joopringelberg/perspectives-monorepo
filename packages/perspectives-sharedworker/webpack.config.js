
import { CleanWebpackPlugin } from "clean-webpack-plugin";

export default {
  entry: {
    sharedworker: "./src/perspectives-sharedworker.js"
  },
  output: {
    library: {
      type: "module",
    },
    filename: 'perspectives-sharedworker.js',
    path: new URL("dist", import.meta.url).pathname,
    chunkFormat: "module",
  },
  experiments: {
    outputModule: true
  },
  watch: false,
  mode: "development",
  target: "webworker",
  plugins: [
    new CleanWebpackPlugin(), // Plugin to clear out the output directory
  ],
  externals: {
    "perspectives-core": "perspectives-core"
  }
};

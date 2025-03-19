import path from "path";
import { fileURLToPath } from "url";
import CopyPlugin from "copy-webpack-plugin";
import { CleanWebpackPlugin } from 'clean-webpack-plugin';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);


export default {
  entry: path.join(__dirname, "src/perspectives-proxy.js" ),
  output: {
    library: {
      type: "module" // Change this line to use the esm format
    },
    filename: "perspectives-proxy.js",
    path: path.join(__dirname, "dist"),
    globalObject: "this",
    chunkFormat: "module"
  },
  experiments: {
    outputModule: true // Enable output as a module
  },
  watch: false,
  mode: "development",
  target: "webworker",
  // core is imported dynamically, so we need to tell Webpack to not bundle it
  // sharedworker is used to create a SharedWorker context. Again, tell Webpack to not bundle it
  externals: 
    { "perspectives-core": "perspectives-core",
      "perspectives-sharedworker": "perspectives-sharedworker"
    },
  module: {
    rules: [
      {
        test: /perspectives-sharedworker\.js$/, // Match only the perspectives-sharedworker.js file
        use: { loader: 'worker-loader' } // Use worker-loader to handle this file
      }
    ]
  },
  plugins: [ new CleanWebpackPlugin(),
    new CopyPlugin({
      patterns: [
        {
          from: path.join(__dirname, "src/perspectives-proxy.d.ts"),
          to: path.join(__dirname, "dist/perspectives-proxy.d.ts")
        }
      ],
    }) ,
  ]
  };

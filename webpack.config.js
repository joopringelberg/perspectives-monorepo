import CopyPlugin from "copy-webpack-plugin";
import { CleanWebpackPlugin } from "clean-webpack-plugin";

export default {
  entry: {
    pageworker: "./src/perspectives-pageworker.js"
  },
  output: {
    library: {
      type: "module",
    },
    filename: 'perspectives-pageworker.js',
    path: new URL("dist", import.meta.url).pathname,
    chunkFormat: "module",
  },
  experiments: {
    outputModule: true
  },
  watch: false,
  mode: "development",
  target: "webworker",
  module: {
    rules: [{
        test: /.js?$/,
        exclude: /node_modules/,
        use: [
          {
            loader: "babel-loader",
            options: {
              presets: [
                '@babel/preset-env'
              ]
            }
          }
        ]
      }]
  },
  plugins: [ 
    new CopyPlugin({
      patterns: [
        {
          from: new URL("src/perspectives-pageworker.d.ts", import.meta.url).pathname,
          to: new URL("dist/perspectives-pageworker.d.ts", import.meta.url).pathname
        }
      ],
      }),
    new CleanWebpackPlugin(), // Plugin to clear out the output directory  
  ]
};

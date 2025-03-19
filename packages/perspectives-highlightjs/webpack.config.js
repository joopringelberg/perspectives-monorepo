

export default  {
  entry: new URL("src/languages/perspectives-arc.js", import.meta.url).pathname,
  output: {
    filename: "perspectives-arc.jsm",
    path: new URL("dist", import.meta.url).pathname,
    library: {
      type: 'module'
    }
  },
  experiments: {
    outputModule: true
  },
  watch: false,
  mode: "development",
  target: "web",
  module: {
    rules: []
  }
};

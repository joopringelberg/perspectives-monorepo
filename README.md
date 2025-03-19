# perspectives-arc Highlightjs

This is the README for the contributed (3rd party) language definition "perspectives-arc" for [highlightjs](https://github.com/highlightjs/highlight.js). This module makes it possible to highlight Perspectives ARC code in your html pages.

## Getting it

Install with npm:

```
npm install git+https://github.com/joopringelberg/perspectives-highlightjs.git
```

## Using it

### In a HTML page
The `dist` file contains an umd module. This you can include in your page as follows:

```
  <script src="path/to/highlight.js"></script>
  <script src="another/path/to/perspectives-arc.js"></script>
  <script>
    hljs.registerLanguage("perspectives-arc", perspectivesarc.default); 
    hljs.highlightAll();
  </script>
```

The last script line registers the module default export with `highlightjs` (bound to a global variable by the `highlight.js` script). In your html file, include an element that contains the ARC code and set the language property on it:

```
<textarea class="perspectives-arc">domain MyModel</textarea>
```

Obviously, you'll need at least one base16 stylesheet:

```
<link rel="stylesheet" href="/path/to/styles/default.min.css">
```

### Using React
Assuming some bundler (e.g. Webpack), install the following (note this installs highlight with all standard languages!):

```
npm install react, react-dom, highlight.js
npm install git+https://github.com/joopringelberg/perspectives-highlightjs.git
```

Here is a complete code example:

```
import React from "react";
import {render} from 'react-dom';

// Import highlight library (NOTE: this is the complete module, can be done with smaller footprint!)
// https://highlightjs.readthedocs.io/en/latest/readme.html#es6-modules-import
import hljs from 'highlight.js';

// Import perspectives-arc as a third party language
import perspectivesarc from 'perspectives-highlightjs';

// Import a stylesheet
import "highlight.js/styles/base16/solar-flare.css";

// Register the language, so it can be used as a value for the language prop.
hljs.registerLanguage("perspectives-arc", perspectivesarc); 

const arcSource = "-- Copyright Joop Ringelberg and Cor Baars 2019, 2020, 2021\ndomain SimpleChat\n  use sys for model:System\n\n  case Model\n    aspect sys:Model\n    external\n      aspect sys:Model$External\n";

class App extends React.Component
{
    componentDidMount()
    {
        hljs.highlightAll();
    }
    componentDidChange()
    {
        hljs.highlightAll();
    }

    render()
    {
        return <pre><code className="perspectives-arc">{arcSource}</code></pre>
    }

}

render(<App />, document.getElementById('root'))
```

## Release Notes

### 1.0.0
Initial release.

### 1.1.0
Added line comments and block comments.

### 1.2.0
Technical improvement: removed lookbehind for Safari.

### 1.3.0
Extension for timing facets of automatic actions and notifications:

* The keywords after, until, every, maximally and times get the same color as the other keywords in automatic actions.
* The time constants Milliseconds, Seconds, Minutes, Hours and Days get the same color as simple values.


## Requirements
This package depends on [highlightjs](https://github.com/highlightjs/highlight.js).

## Known Issues

* filledBy is not tokenized
* Improvement: make `relational` etc the same color as Verbs; use the freed up color for User identifiers.

# Syntax coloring for Perspectives
We have syntax coloring for vscode and for Atom. Both use the TextMate approach to colorize source files. The TextMate approach is based on rules that are a combination of named regular expressions to select a part of the source text (the `scope`) and a string that identifies a particular style to format it. We follow a scope naming regime that makes [Base16](http://chriskempson.com/projects/base16/) themes applicable.

Highlightjs uses a different meta-format to specify a tokenizer. We try to implement a tokenization and scope naming regime that corresponds to the TextMate based approach. See the repository [language-perspectives-arcII](https://github.com/joopringelberg/language-perspectives-arcII.git) for an explanation of Base16 and TextMate rules.


## The Highlightjs-base16 mapping

The table below has been derived from the template file [default.moustache](https://github.com/highlightjs/base16-highlightjs/blob/main/templates/default.mustache) in the Highlightjs Base16 repository on Github.

We choose a single highlight scope name (style) to represent a particular base16 color.

|base16 color variable|Highlightjs scope name|
|---|---|
|base03|comment|
|base04|tag|
|base05|operator|
|base06||
|base07||
|base08|variable|
|base09|number|
|base0A|title|
|base0B|string|
|base0C|regexp|
|base0D|title.function|
|base0E|keyword|
|base0F|meta|

## ARC categories
The table below gives a grouping of ARC keywords. The TOKENIZER RULE defines semantically appropriate groups of keywords as they are lumped together by our tokenizer for ARC. These are lumped together in even larger groups (under COLOR GROUP), where a Base64 color is assigned to each COLOR GROUP. In other words, the table gives the mapping from tokenizer rules to Base16 colors.

|COLOR GROUP|TOKENIZER RULE|ARC KEYWORDS|BASE64|
|---|---|---|---|
|Contexts|Context kinds|domain, case, party, activity|base0A|
|Roles|Role kinds|external, thing, context|base0B|
|||View||
|||filledBy||
|User role|User role|user|base03|
|Property|Property|property|base08|
||Property facet|mandatory, relational, unlinked, minLength, maxLength, enumeration, pattern, maxInclusive, minInclusive||
|Perspective|Perspective|view, verbs, props, only, except, defaults, all roleverbs|base0D|
|Control|State|state|base0E|
||State transition|on entry [of <statekind>], on exit [of <statekind>]||
||Notification|notify [user]||
||Automatic action|do [for <user>]||
||Assignment|remove, delete, create, etc.||
|||action||
||Timing|after, until, every, maximally, times||
|Simple values|Boolean|true, false|base09|
||date|||
||number|||
||regular expression|||
||Property Range|String, Boolean, DateTime, Number||
||Time constants|Milliseconds, Seconds, Minutes, Hours, Days||
|Expressions|Operators|either, both, binds, matches, and, or, not, exists, available, boundBy, binder, context, extern|base0C|
|||filter…with||
|||>>=, >>, *, /, +, -, ==, >=, <, >=, >||
||Let|letA, letE||
|Variables|Standard variables|currentcontext, nofifieduser, origin, currentactor|base06|
|Verbs|Role verbs|Remove, Delete, Create, CreateAndFill, Fill, Unbind, RemoveFiller, Move|base0F|
||Property verbs|RemovePropertyValue, DeleteProperty, AddPropertyValue, SetPropertyValue, Consult||
|Meta|Meta|aspect, use, indexed|base05|

## Tokenizer scope names
Instead of using the higlightjs scope names directly, we've defined variables in the (javascript) `perspectives-arc.js` file for each of the base16 colors. These variable names are used to bind colors to tokenizer rules, e.g.:

```
const contexts = {
    scope: base0A,
    match: /\b(domain|case|party|activity)\b/,
};
```

# Development
Adapt `perspectives-arc.js`. Then run 

```
npm run build
```

or run 

```
npm run watch
```

to have your changes incorporated automatically while you develop. 

Test by using the page `developer.html` from `highlight.js` (see next paragraph). 

## developer.html
This is a file in the highlight.js package (tools directory) that you can open in the browser. It shows a code box to enter ARC code in, a language selector and a theme selector. Customize this file as follows to be able to test ARC models:

* Add `perspectives-arc` as a third party language by including this script:
```
<script src="../extra/perspectives-arc/dist/perspectives-arc.js"></script>
```
* When the document is ready, register `perspectivs-arc`:
```
$(document).ready(function() {
      hljs.registerLanguage("perspectives-arc", perspectivesarc.default);
```
* For convenience, add the class "perspectives-arc" to the textarea that forms the code box:
```
<textarea class="perspectives-arc">domain MyModel</textarea>
```

## Packaging, Releasing
There is no explicit packaging. Follow these steps to publish a new version:
* set the new version number in the package file.
* push all changes to github.
* create a new tag, use the semantic version number preceded by 'v', e.g. `v0.1.0`.
* push the tag on the command line (vscode won't do it):

```
git push origin <tagname>
```



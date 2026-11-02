// Monaco language definition for the Perspectives Language (ARC).
// Token classes are adapted from the TextMate grammar in packages/language-perspectives-arcII/syntaxes/arc.tmLanguage.yaml
// and the bracket/comment configuration from packages/language-perspectives-arcII/language-configuration.json.
// Kept free of monaco imports so it can be unit tested in Node.

export const ARC_LANGUAGE_ID = "arc";

export const declarationKeywords = ["domain", "case", "party", "activity", "external", "thing", "context", "user", "property", "state", "use", "aspect", "indexed"];
export const perspectiveKeywords = ["perspective", "on", "of", "view", "verbs", "props", "only", "except", "defaults", "all", "roleverbs", "action", "screen", "who", "what", "where", "tab", "row", "column", "form", "table", "markdown", "master", "detail", "with", "without", "when", "chat", "readableName"];
export const statementKeywords = ["entry", "exit", "notify", "do", "for", "remove", "role", "create", "delete", "from", "in", "letA", "letE", "callEffect", "callExternal", "returns", "bind", "unbind", "move", "file", "as", "after", "until", "every", "maximally", "times", "once", "settled", "filledBy", "mandatory", "relational", "unlinked", "minLength", "maxLength", "enumeration", "pattern", "maxInclusive", "minInclusive", "functional", "calculated"];
export const operatorKeywords = ["either", "both", "binds", "matches", "regexp", "and", "or", "not", "exists", "available", "boundBy", "binder", "extern", "selectFrom", "just", "runContextAction", "runRoleAction", "publicrole", "publiccontext", "translate", "filter", "first", "count", "sum", "minimum", "maximum"];
export const builtinVariables = ["currentcontext", "notifieduser", "origin", "currentactor"];
export const typeKeywords = ["String", "Number", "Boolean", "DateTime", "File", "Email", "Date", "Time", "Duration"];
export const durationKeywords = ["Milliseconds", "Seconds", "Minutes", "Hours", "Days", "Weeks", "Months", "Years"];
export const verbKeywords = ["Remove", "Delete", "Create", "CreateAndFill", "Fill", "Unbind", "RemoveFiller", "Move", "RemovePropertyValue", "DeletePropertyValue", "DeleteProperty", "AddPropertyValue", "SetPropertyValue", "Consult"];

// Language configuration (monaco.languages.LanguageConfiguration).
export const arcLanguageConfiguration = {
  comments: { lineComment: "--", blockComment: ["{-", "-}"] as [string, string] },
  brackets: [["(", ")"]] as [string, string][],
  autoClosingPairs: [
    { open: "(", close: ")" },
    { open: "\"", close: "\"", notIn: ["string"] },
    { open: "'", close: "'", notIn: ["string", "comment"] },
  ],
  surroundingPairs: [
    { open: "(", close: ")" },
    { open: "\"", close: "\"" },
    { open: "'", close: "'" },
  ],
  // ARC is indentation sensitive: indent after block-opening lines, never auto-dedent.
  indentationRules: {
    increaseIndentPattern: /^\s*(domain|case|party|activity|external|thing|context|user|state|on\s+(entry|exit)|do(\s+for\s+\w+)?|perspective\s+(on|of)|screen|who|what|where|tab|row|column|letA|letE|in)\b.*$/,
  },
};

// Monarch tokenizer (monaco.languages.IMonarchLanguage).
export const arcMonarchLanguage = {
  defaultToken: "",
  tokenPostfix: ".arc",
  declarationKeywords,
  perspectiveKeywords,
  statementKeywords,
  operatorKeywords,
  builtinVariables,
  typeKeywords,
  durationKeywords,
  verbKeywords,
  tokenizer: {
    root: [
      [/--.*$/, "comment"],
      [/\{-/, "comment", "@blockComment"],
      [/(markdown)(\s*)(<)/, ["keyword.perspective", "", { token: "string.markdown", next: "@markdown" }]],
      [/"([^"\\]|\\.)*"/, "string"],
      [/'[\d-]+'/, "number.date"],
      [/\b(true|false)\b/, "constant.language"],
      [/\b\d+(\.\d+)?\b/, "number"],
      // Qualified names: prefix:Local, model://..., Context$Role$Property
      [/model:\/\/[\w.]+#[\w$@.]*/, "type.identifier"],
      [/\b[a-z]\w*:[A-Z][\w$]*/, "type.identifier"],
      [/[A-Za-z_][\w]*(?:\$[\w]+)*/, {
        cases: {
          "@declarationKeywords": "keyword",
          "@perspectiveKeywords": "keyword.perspective",
          "@statementKeywords": "keyword.statement",
          "@operatorKeywords": "keyword.operator",
          "@builtinVariables": "variable.predefined",
          "@typeKeywords": "type",
          "@durationKeywords": "type",
          "@verbKeywords": "constant.verb",
          "@default": "identifier",
        },
      }],
      [/>>=|>>|==|>=|<=|<-|[*/+\-<>=]/, "operator"],
      [/[(),]/, "delimiter"],
    ],
    blockComment: [
      [/[^{-]+/, "comment"],
      [/-\}/, "comment", "@pop"],
      [/[{-]/, "comment"],
    ],
    markdown: [
      [/[^>]+/, "string.markdown"],
      [/>/, "string.markdown", "@pop"],
    ],
  },
};

export const ARC_TAB_SIZE = 2;

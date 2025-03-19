/*
Language: Perspectives ARC
Author: Joop Ringelberg <joopringelberg@perspect.it>
Contributors: Cor Baars <corbaars@perspect.it>
Description: A modelling language for co-operation.
Website: https://joopringelberg.github.io/perspectives-documentation/
*/
///// COLOR VARIABLES
const // base01
// base02
base03 = "comment", // base05
// base06
base05 = "operator", base08 = "variable", base09 = "number", base0A = "title", base0B = "string", base0C = "regexp", base0D = "title.function", base0E = "keyword", base0F = "meta";
const whitespace = /\s+/;
const identifier = /\w+/;
function lexeme(w) {
    return new RegExp("\\b" + w + "\\b");
}
const lex = lexeme;
// CONTEXT KINDS (base0A)
const contexts = {
    scope: base0A,
    match: /\b(domain|case|party|activity)\b/,
};
// ROLE KINDS (NOT USER) (base0B)
const roles = {
    begin: /(\n\s*)\b(external|thing|context)\b/,
    beginScope: {
        2: base08
    }
};
// USER ROLE IS SPECIAL (role name gets base03)
const user = {
    begin: [lex("user"), whitespace, identifier],
    beginScope: {
        1: base0B,
        3: base03
    }
};
const property = {
    scope: base08,
    begin: lexeme("property")
};
const propertyFacets = {
    scope: base08,
    begin: /\b(mandatory|relational|unlinked|minLength|maxLength|enumeration|pattern|maxInclusive|minInclusive)\b/
};
const perspective = {
    scope: base0D,
    begin: /\bperspective(\s+(on|of))?\b/,
};
const perspectiveParts = {
    scope: base0D,
    begin: /\b(view(?=\s+[\w|:|\$]+\s+verbs)|verbs|props|only|except|defaults|all\s+roleverbs|action)\b/,
};
const state = {
    scope: base0E,
    begin: lexeme("state")
};
// STATE TRANSITION (ON ENTRY, ON EXIT)
const stateTransition = {
    scope: base0E,
    begin: /\bon\s+(entry|exit)\b/
};
// NOTIFICATION
const notification = {
    begin: [lexeme("notify"), whitespace, identifier],
    beginScope: {
        1: base0E,
        3: base03
    }
};
// AUTOMATIC ACTION
// User in `do for user` gets the role coloring.
const automaticAction = {
    begin: [lexeme("do"), whitespace, lexeme("for"), whitespace, identifier],
    beginScope: {
        1: base0E,
        3: base0E,
        4: base03
    }
};
const timing = {
    scope: base0E,
    begin: /\b(after|until|every|maximally|times)\b/
};
// ASSIGNMENT
const remove = {
    begin: [lexeme("remove"), whitespace, /\b(role|context)\b/],
    beginScope: {
        1: base0E,
        2: base0E
    }
};
const create = {
    begin: [lexeme("create"), whitespace, lexeme("role"), whitespace, identifier, whitespace, lexeme("in")],
    beginScope: {
        1: base0E,
        3: base0E,
        7: base0E
    }
};
const deleteStatement = {
    begin: [lexeme("delete"), whitespace, lexeme("role"), whitespace, identifier, whitespace, lexeme("from")],
    beginScope: {
        1: base0E,
        3: base0E,
        7: base0E
    }
};
// SIMPLE VALUES
const boolean = {
    begin: /true|false/,
    scope: base09
};
const date = {
    begin: /\'[\d|\-]+\'/,
    scope: base09
};
const number = {
    begin: /\b\d+\b/,
    scope: base09
};
const regexp = {
    begin: /\/.*\//,
    scope: base09
};
const timeConstants = {
    begin: /Milliseconds|Seconds|Minutes|Hours|Days/,
    scope: base09
};
// OPERATORS
const alphabeticOperator = {
    begin: /\b(either|both|binds|matches|and|or|not|exists|available|boundBy|binder|context|extern)\b/,
    scope: base0C
};
const filter = {
    begin: [lexeme("filter"), /.*/, lexeme("with")],
    beginScope: {
        1: base0C,
        3: base0C
    }
};
const nonAlphabeticOperator = {
    begin: /\>\>\=|\>\>|\*|\/|\+|\-|\=\=|\>\=|\<|>\=|>/,
    scope: base0C
};
// STANDARD VARIABLES
const standardVariables = {
    begin: /\b(currentcontext|nofifieduser|origin|currentactor)\b/,
    scope: base03
};
// PROPERTY RANGE
const propertyRange = {
    begin: /\b(String|Number|Boolean|DateTime)\b/,
    scope: base09
};
// ROLE VERBS
const roleVerbs = {
    begin: /\b(Remove|Delete|Create|CreateAndFill|Fill|Unbind|RemoveFiller|Move)\b/,
    scope: base0F
};
// PROPERTY VERBS
const propertyVerbs = {
    begin: /\b(RemovePropertyValue|DeleteProperty|AddPropertyValue|SetPropertyValue|Consult)\b/,
    scope: base0F
};
// META: ASPECT, INDEXED
const meta = {
    begin: / \b(aspect|indexed)\b/,
    scope: base05
};
const use = {
    begin: [lexeme("use"), /.*/, lexeme("for")],
    beginScope: {
        1: base05,
        3: base05
    }
};
const blockComment = {
    scope: base03,
    begin: /\{-/,
    end: /-\}/
};
const perspectivesArc = function (hljs) {
    // COMMENT
    const simpleComment = hljs.COMMENT('--', '$');
    return {
        name: "Perspectives ARC",
        case_insensitive: false,
        contains: [hljs.QUOTE_STRING_MODE,
            hljs.APOS_STRING_MODE,
            simpleComment,
            blockComment,
            contexts,
            roles,
            user,
            property,
            propertyFacets,
            perspective,
            perspectiveParts,
            state,
            stateTransition,
            notification,
            automaticAction,
            timing,
            remove,
            create,
            deleteStatement,
            boolean, date, number, regexp, timeConstants,
            alphabeticOperator,
            filter,
            nonAlphabeticOperator,
            standardVariables,
            propertyRange,
            roleVerbs,
            propertyVerbs,
            meta,
            use
        ]
    };
};

export { perspectivesArc as default };

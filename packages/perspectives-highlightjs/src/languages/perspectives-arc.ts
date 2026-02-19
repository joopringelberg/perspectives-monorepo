import { LanguageFn, HLJSApi, Mode, Language } from 'highlight.js';

/*
Language: Perspectives ARC
Author: Joop Ringelberg <joopringelberg@perspect.it>
Contributors: Cor Baars <corbaars@perspect.it>
Description: A modelling language for co-operation.
Website: https://joopringelberg.github.io/perspectives-documentation/
*/

///// COLOR VARIABLES
const
    // base01
    // base02
    base03 = "comment",
    base04 = "tag",
    // base05
    // base06
    base05 = "operator",
    base08 = "variable",
    base09 = "number",
    base0A = "title",
    base0B = "string",
    base0C = "regexp",
    base0D = "title.function",
    base0E = "keyword",
    base0F = "meta"
    ;

const whitespace: RegExp = /\s+/;

const identifier: RegExp = /\w+/;

function lexeme(w: string): RegExp {
    return new RegExp("\\b" + w + "\\b");
}
const lex = lexeme;
const l = lexeme;

// CONTEXT KINDS (base0A)
const contexts: Mode = {
    scope: base0A,
    match: /\b(domain|case|party|activity|public)\b/,
};

// ROLE KINDS (NOT USER) (base0B)
const roles: Mode = {
    begin: /(\n\s*)\b(external|thing|context)\b/,
    beginScope: {
        2: base08
    }
};

// USER ROLE IS SPECIAL (role name gets base03)
const user: Mode = {
    begin: [lex("user"), whitespace, identifier],
    beginScope: {
        1: base0B,
        3: base03
    }
};

const property: Mode = {
    scope: base08,
    begin: lexeme("property")
};

const propertyFacets: Mode = {
    scope: base08,
    begin: /\b(mandatory|relational|functional|unlinked|selfonly|authoronly|minLength|maxLength|enumeration|pattern|maxInclusive|minInclusive|maxExclusive|minExclusive|whiteSpace|totalDigits|fractionDigits|messageProperty|mediaProperty|readableName|regexp|setting)\b/
};

const perspective: Mode = {
    scope: base0D,
    begin: /\bperspective(\s+(on|of))?\b/,
}

const perspectiveParts: Mode = {
    scope: base0D,
    begin: /\b(view(?=\s+[\w|:$]+\s+verbs)|verbs|without|props|only|except|defaults|default|all\s+roleverbs|no\s+roleverbs|action)\b/,
}

const state: Mode = {
    scope: base0E,
    begin: lexeme("state")
};

// STATE TRANSITION (ON ENTRY, ON EXIT)
const stateTransition: Mode = {
    scope: base0E,
    begin: /\bon\s+(entry|exit)\b/
};

// IN STATE, SUBJECT/OBJECT/CONTEXT STATE
const inState: Mode = {
    scope: base0E,
    begin: /\b(in\s+state|subject\s+state|object\s+state|context\s+state)\b/
};

// NOTIFICATION
const notification: Mode = {
    begin: [lexeme("notify"), whitespace, identifier],
    beginScope: {
        1: base0E,
        3: base03
    }
};

// AUTOMATIC ACTION
// User in `do for user` gets the role coloring.
const automaticAction: Mode = {
    begin: [lexeme("do"), whitespace, lexeme("for"), whitespace, identifier],
    beginScope: {
        1: base0E,
        3: base0E,
        4: base03
    }
};

const timing: Mode = {
    scope: base0E,
    begin: /\b(after|until|every|maximally|times)\b/
}

// ASSIGNMENT
const remove: Mode = {
    begin: [lexeme("remove"), whitespace, /\b(role|context)\b/],
    beginScope: {
        1: base0E,
        2: base0E
    }
};

const create: Mode = {
    begin: [lexeme("create"), whitespace, lexeme("role"), whitespace, identifier, whitespace, lexeme("in")],
    beginScope: {
        1: base0E,
        3: base0E,
        7: base0E
    }
};

const deleteStatement: Mode = {
    begin: [lexeme("delete"), whitespace, lexeme("role"), whitespace, identifier, whitespace, lexeme("from")],
    beginScope: {
        1: base0E,
        3: base0E,
        7: base0E
    }
};

// BIND, UNBIND, MOVE, CALL ASSIGNMENTS
const bindStatement: Mode = {
    begin: [lexeme("bind"), whitespace, identifier],
    beginScope: {
        1: base0E
    }
};

const unbindStatement: Mode = {
    scope: base0E,
    begin: /\b(unbind|unbind_|bind_)\b/
};

const moveStatement: Mode = {
    begin: [lexeme("move"), whitespace, identifier],
    beginScope: {
        1: base0E
    }
};

const createContext_: Mode = {
    begin: [lexeme("create_"), whitespace, lexeme("context"), whitespace, identifier, whitespace, lexeme("bound")],
    beginScope: {
        1: base0E,
        3: base0E,
        7: base0E
    }
};

const callStatements: Mode = {
    scope: base0E,
    begin: /\b(callEffect|callDestructiveEffect|callExternal)\b/
};

// SIMPLE VALUES
const boolean: Mode = {
    begin: /true|false/,
    scope: base09
};

const date: Mode = {
    begin: /'[\d|-]+'/,
    scope: base09
};

const number: Mode = {
    begin: /\b\d+\b/,
    scope: base09
};

const regexp: Mode = {
    begin: /\/.*\//,
    scope: base09
};

const timeConstants: Mode = {
    begin: /\b(Milliseconds|Seconds|Minutes|Hours|Days|MilliSecond|Millisecond|Year|Month|Week|Day|Hour|Minute|Second|milliseconds|seconds|minutes|hours|days|weeks|months|years|millisecond|second|minute|hour|day|week|month|year)\b/,
    scope: base09
}

// OPERATORS
const alphabeticOperator: Mode = {
    begin: /\b(either|both|binds|matches|and|or|not|exists|available|boundBy|binder|context|extern|fills|filledBy|letE|letA|union|intersection|orElse|binding|this|me|indexedName|publicrole|publiccontext|isInState|contextinstance|roleinstance|specialisesRoleType|roleTypes|contextType|roleType|modelname|returns|translate)\b/,
    scope: base0C
};

const filter: Mode = {
    begin: [lexeme("filter"), /.*/, lexeme("with")],
    beginScope: {
        1: base0C,
        3: base0C
    }
};

const nonAlphabeticOperator: Mode = {
    begin: />>=|>>|\*|\/|\+|-|==|>=|<|>=|>/,
    scope: base0C
};

// STANDARD VARIABLES
const standardVariables: Mode = {
    begin: /\b(currentcontext|nofifieduser|origin|currentactor)\b/,
    scope: base03
};

// PROPERTY RANGE
const propertyRange: Mode = {
    begin: /\b(String|Number|Boolean|DateTime|Date|Time|File|Email|MarkDown)\b/,
    scope: base09
};

// ROLE VERBS
const roleVerbs: Mode = {
    begin: /\b(Remove|Delete|Create|CreateAndFill|Fill|Unbind|RemoveFiller|Move)\b/,
    scope: base0F
};

// PROPERTY VERBS
const propertyVerbs: Mode = {
    begin: /\b(RemovePropertyValue|DeleteProperty|AddPropertyValue|SetPropertyValue|Consult)\b/,
    scope: base0F
};

// META: ASPECT, INDEXED
const meta: Mode = {
    begin: / \b(aspect|indexed)\b/,
    scope: base05
};

// SCREEN ELEMENTS (base04)
const screenKeywords: Mode = {
    scope: base04,
    begin: /\b(screen|tab|row|column|form|markdown|table|chat|messages|media|who|what|where|master|detail|when|fillfrom)\b/
};

const use: Mode = {
    begin: [lexeme("use"), /.*/, lexeme("for")],
    beginScope: {
        1: base05,
        3: base05
    }
};

const blockComment: Mode = {
    scope: base03,
    begin: /\{-/,
    end: /-\}/
};

const perspectivesArc: LanguageFn = function(hljs: HLJSApi): Language {
    // COMMENT
    const simpleComment = hljs.COMMENT('--', '$');

    return {
      name: "Perspectives ARC",
      case_insensitive: false,
      contains: 
        [ hljs.QUOTE_STRING_MODE
        , hljs.APOS_STRING_MODE
        , simpleComment
        , blockComment
        , contexts
        , roles
        , user
        , property
        , propertyFacets
        , perspective
        , perspectiveParts 
        , state
        , stateTransition
        , inState
        , notification
        , automaticAction
        , timing
        , remove
        , create
        , deleteStatement
        , bindStatement
        , unbindStatement
        , moveStatement
        , createContext_
        , callStatements
        , boolean, date, number, regexp, timeConstants
        , alphabeticOperator
        , filter
        , nonAlphabeticOperator
        , standardVariables
        , propertyRange
        , roleVerbs
        , propertyVerbs
        , meta
        , use
        , screenKeywords
        ]
    }
};

export default perspectivesArc;
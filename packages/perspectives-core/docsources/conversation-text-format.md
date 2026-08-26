# Conversation Text Format and Parser

This document specifies the compact plain-text format for one reusable conversation body and describes its PureScript parser. The format is intended to be easier to author than the equivalent YAML conversation structure while preserving statements, questions, answers, nested branches and reusable fragments.

> **Primary source module**
> | Concern | Module | File |
> |---|---|---|
> | Grammar, AST and parser | `Perspectives.Conversations.Parser` | `src/core/conversations/parser.purs` |
> | Context YAML renderer | `Perspectives.Conversations.Renderer` | `src/core/conversations/renderer.purs` |
> | Parser tests | `Test.Conversations.Parser` | `test/conversations/parser.purs` |

---

## 1. Overview

A document consists of named sections. Exactly one section is named `conversation`; it is the entry point. Every other section is a reusable conversation fragment.

Within a section:

- an ordinary line is a bot statement;
- a line beginning with `?` is a bot question;
- a line beginning with `-` is a human answer to the directly enclosing question;
- content indented beneath an answer is that answer's continuation;
- `ref: fragment-name` in an answer continuation refers to a named fragment.

Indentation expresses nesting. Each nesting level uses exactly two spaces. Tabs are not allowed.

A terminal answer needs no special syntax: it is simply an answer without an indented continuation.

---

## 2. Grammar

The following EBNF describes the format. Layout constraints following the grammar are significant and are enforced by the parser.

```ebnf
document           = section*, conversation-section, section* ;
section            = identifier, ":", newline, sequence ;
conversation-section
                   = "conversation:", newline, sequence ;

sequence           = element, { element } ;
element            = statement | question ;
statement          = text-line, { text-line } ;
question           = "?", question-text, newline,
                     answer, { answer } ;
answer             = indent, "-", answer-text, newline,
                     [ continuation ] ;
continuation       = indent, sequence
                   | indent, reference ;
reference          = "ref:", identifier ;

identifier         = letter, { letter | digit | "-" | "_" } ;
```

The parser applies these layout and semantic rules:

1. Section headers start at column one and contain only an identifier followed immediately by `:`.
2. A document contains exactly one `conversation:` section.
3. Section identifiers are unique. `conversation` is reserved for the entry section.
4. An identifier starts with a Unicode letter. Remaining characters are Unicode letters or digits, `-`, or `_`.
5. A question starts with `?`. The question text does not have to end in `?`.
6. A question must have at least one non-empty answer exactly two spaces deeper.
7. The entire text following the leading `-` is the answer text. Hyphens within it have no structural meaning.
8. An answer continuation is exactly two spaces deeper than the answer. It may contain statements, questions and references.
9. `ref:` is only allowed in an answer continuation. Its target may be declared before or after the reference.
10. Every reference must name a section in the same document. The parser allows cyclic references.
11. Tabs are rejected. Spaces are the only indentation characters.
12. Blank lines may separate sections and statement blocks. Consecutive ordinary lines at the same indentation are combined into one statement, joined with newline characters.

The `indent` symbol in the EBNF is contextual: it means exactly two spaces more than the indentation of the construct that owns the nested content.

---

## 3. Elements

### Statements

An ordinary line is a statement:

```text
Welcome to the Interactive Cooking Guide.
```

Consecutive ordinary lines at the same indentation form one multi-line statement:

```text
Welcome to the Interactive Cooking Guide.
I will help you find the right recipe.
```

This becomes `Statement "Welcome ...\nI will help ..."` in the AST. A blank line ends the statement block.

### Questions and answers

A leading `?` marks a question. Answers immediately follow it, indented by two spaces:

```text
? What would you like to cook today?
  - Pasta
  - Risotto
  - Soup
```

The marker, rather than final punctuation, determines whether a line is a question. This is therefore also valid:

```text
? Choose a dish
  - Pasta
```

An answer with no nested content is terminal. No additional terminal marker is required.

### Nested continuations

Content nested beneath an answer is the sequence that follows selection of that answer:

```text
? Which pasta dish would you like to prepare?
  - Penne Arrabbiata
    Penne Arrabbiata is a spicy tomato-based pasta.
    ? How spicy would you like it?
      - Mild - use half a chilli pepper.
      - Hot - use two chilli peppers
```

`Mild - use half a chilli pepper.` is one complete answer. The second hyphen is ordinary answer text.

### Named fragments and references

Every section other than `conversation` defines a reusable fragment:

```text
pasta-guide:
Pasta dishes require durum wheat pasta and a suitable sauce.
? Which pasta dish would you like to prepare?
  - Spaghetti Carbonara
  - Lasagna al Forno
```

A reference can occur in an answer continuation:

```text
conversation:
? What would you like to cook today?
  - Pasta
    ref: pasta-guide
```

Forward references are supported, so `pasta-guide` may also appear later in the document. Reference cycles are accepted by this body parser. A consumer that expands references must handle cycles explicitly rather than recurse without a bound.

---

## 4. Complete food preparation example

```text
pasta-guide:
Pasta dishes require durum wheat pasta and a suitable sauce.
? Which pasta dish would you like to prepare?
  - Spaghetti Carbonara
  - Penne Arabbiata
    Penne Arrabiata is a spicy tomato-based pasta.
    ? How spicy would you like it?
      - Mild - use half a chilli pepper.
      - Hot - use two chilli peppers
  - Lasagna al Forno

risotto-guide:
Risotto requires Arborio or Carnaroli rice and warm stock.
? Which risotto would you like to prepare?
  - Risotto ai Funghi
  - Risotto al Limone
  - Risotto Milanese

soup-guide:
Soups are versatile and can be made from almost any vegetable.
? Which soup would you like to make?
  - Minestrone
  - French Onion Soup
  - Pumpkin Soup

conversation:
Welcome to the Interactive Cooking Guide.
I will help you find the right recipe.
? What would you like to cook today?
  - Pasta
    ref: pasta-guide
  - Risotto
    ref: risotto-guide
  - Soup
    ref: soup-guide
```

The entry sequence contains one two-line statement, one question and three answers. Each answer continues with a reference to a named guide. The `Penne Arabbiata` answer demonstrates a nested statement, question and answer set. `Spaghetti Carbonara`, `Lasagna al Forno` and the other answers without continuations are terminal answers.

---

## 5. PureScript representation

The parser returns a typed conversation body:

```purescript
type Answer =
  { text :: String
  , sequence :: Maybe (Array ConversationElement)
  }

data ConversationElement
  = Statement String
  | Question String
  | Answer Answer
  | Sequence (Array ConversationElement)
  | Ref String

type ConversationBody =
  { elements :: Object ConversationElement
  , conversation :: Array ConversationElement
  }
```

Named sections become entries in `elements`, each wrapped in `Sequence`. The `conversation` section becomes the root `conversation` array. An answer without nested content has `sequence: Nothing`; an answer with a continuation has `sequence: Just elements`.

For example:

```text
conversation:
? Choose one
  - First
  - Second
```

produces the following shape:

```purescript
{ elements: Object.empty
, conversation:
    [ Question "Choose one"
    , Answer { text: "First", sequence: Nothing }
    , Answer { text: "Second", sequence: Nothing }
    ]
}
```

---

## 6. Parser API and phases

The public entry point is:

```purescript
parseConversation
  :: String
  -> Either ConversationParseError ConversationBody
```

The implementation uses `purescript-parsing` and proceeds in four stages:

1. **Line parsing** reads spaces, text and Unix, Windows or classic Mac line endings into numbered `SourceLine` values.
2. **Source validation** rejects tabs.
3. **Section parsing and validation** recognises headers, checks reference targets and ensures section names are valid.
4. **Recursive indentation parsing** constructs statements, questions, answers, continuations and references, then assembles the single root conversation and named element map.

The parser separates low-level and structural failures:

```purescript
data ConversationParseError
  = LexicalError ParseError
  | StructuralError Int String
```

`LexicalError` wraps an error from `purescript-parsing`. `StructuralError` includes the one-based source line and a focused message, for example:

```text
Line 3: A question must be followed by at least one answer indented by two spaces.
```

The parser validates the conversation body only. Model metadata, context and audience bindings, storage, and conversion to the compiled `conversations.json` attachment remain concerns of the surrounding conversation-document and runtime compilation layers.

---

## 7. Equivalent YAML concepts

The text AST maps directly to the existing YAML conversation-body concepts:

| Text form | AST constructor | YAML concept |
|---|---|---|
| Ordinary text | `Statement` | `statement: <text>` |
| `? <text>` | `Question` | `question: <text>` |
| `- <text>` | `Answer` with `sequence: Nothing` | `answer: <text>` |
| Answer with indented content | `Answer` with `sequence: Just ...` | `answer: { text, sequence }` |
| Named section | `Sequence` in `elements` | Named entry under `elements:` |
| `ref: <identifier>` | `Ref` | `ref: <identifier>` |
| `conversation:` section | `conversation` field | Root `conversation:` sequence |

This correspondence allows the text format to act as an authoring syntax for the established conversation-body structure without changing the runtime conversation representation.

---

## 8. Rendering from a context YAML document

The renderer can select one conversation from a complete context conversation document and produce canonical surface text:

```purescript
renderConversationFromContextYaml
  :: String
  -> String
  -> Effect (Either ConversationRenderError String)
```

The first argument is the context YAML source and the second is the document-local conversation identifier under `conversations`. The renderer:

1. parses the YAML with `js-yaml`;
2. looks up the requested conversation;
3. decodes its `elements` map and root `conversation` sequence;
4. renders named elements followed by the `conversation:` section;
5. preserves `ref` nodes and nested answer sequences;
6. inserts a blank line between adjacent statement elements so the text parser does not merge them;
7. reparses the generated text with `parseConversation` before returning it.

The lower-level pure entry point renders an already decoded body:

```purescript
renderConversationBody
  :: ConversationBody
  -> Either ConversationRenderError String
```

The output is canonical rather than a reproduction of YAML layout. YAML comments, scalar styles, quoting, wrapping and blank-line choices are not part of the decoded document and cannot be recovered.

The renderer does not extend the surface grammar. It returns `UnrepresentableConversation` when the selected YAML body uses a currently unsupported construct, including:

- a multi-line question or answer;
- an empty line inside one statement value;
- leading or trailing whitespace that the text parser would discard;
- a statement line beginning with `?`, `-` or `ref:`;
- a `ref` outside an answer continuation.

Other errors distinguish malformed YAML, a missing conversation identifier and an invalid conversation-body shape. Consequently, successful output has the following guarantee:

```text
context YAML + conversation identifier
  -> renderConversationFromContextYaml
  -> canonical surface text
  -> parseConversation succeeds
```

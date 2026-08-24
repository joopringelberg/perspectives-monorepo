# Conversational Structure — YAML Specification

## 1. Introduction

This document specifies a YAML representation for **conversational structures**. A conversation is modelled as role-taking between two participants:

- **Bot** — the participant who drives the conversation, asks questions, and may introduce branching.
- **Human** — the participant who selects among available answers, but does not branch independently.

A typical use-case is a nested Frequently Asked Questions (FAQ) tree, where the bot presents a question and the human chooses one of several prepared answers, each of which may lead to a further sub-conversation.

---

## 2. Formal Syntax (BNF)

```bnf
<document>    ::= <element>+

<element>     ::= <labeled-element> | <unlabeled-element>

<labeled-element>   ::= <label> ":" <unlabeled-element>
                      | <label> ":" <ref>

<unlabeled-element> ::= <statement>
                      | <question>
                      | <answer>
                      | <sequence>

<statement>   ::= "statement:" <text>
<question>    ::= "question:" <text>
<answer>      ::= "answer:" (<text> | <sequence>)

<sequence>    ::= "sequence:" <element>+

<text>        ::= <single-line-string> | <multi-line-string>

<label>       ::= <identifier>
<ref>         ::= "ref:" <identifier>

<identifier>  ::= [a-zA-Z][a-zA-Z0-9_-]*
```

### Semantic constraints (not enforced by syntax)

- A `question` must be followed in its enclosing sequence by at least one `answer`.
- An `answer` must be preceded in its enclosing sequence by a `question`.
- Multiple `statement` elements may appear consecutively.
- A `ref` resolves to a previously defined `<labeled-element>` elsewhere in the same document.

---

## 3. Mapping to YAML

### 3.1 Primitive elements

| Concept     | YAML key     | Value type                            |
|-------------|--------------|---------------------------------------|
| Statement   | `statement`  | string (single- or multi-line)        |
| Question    | `question`   | string (single- or multi-line)        |
| Answer      | `answer`     | string **or** mapping with `sequence` |
| Sequence    | `sequence`   | YAML list of elements                 |
| Label       | top-level key name under `elements` (or as anchor `&label`) |
| Reference   | `ref: <label>` |

### 3.2 Document structure

A document is a YAML mapping with an optional `elements` map (for named/labeled elements) and a mandatory `conversation` entry that is the top-level sequence.

```yaml
# conversational-document.yaml
elements:                   # optional section holding labeled/reusable elements
  <label>:
    <element>

conversation:               # the root sequence of the document
  - <element>
  - <element>
  ...
```

### 3.3 Element forms

**Statement** (single line)
```yaml
- statement: "This is a statement."
```

**Statement** (multi-line, using YAML block scalar)
```yaml
- statement: |
    This is the first line of a statement.
    This is the second line.
```

**Question**
```yaml
- question: "What would you like to do?"
```

**Answer with inline text**
```yaml
- answer: "I would like to start over."
```

**Answer with a nested sequence (introduces branching)**
```yaml
- answer:
    sequence:
      - statement: "Great choice!"
      - question: "Which step do you want to repeat?"
      - answer: "Step 1"
      - answer: "Step 2"
```

**Sequence**
```yaml
- sequence:
    - statement: "Welcome."
    - question: "How can I help you?"
    - answer: "Tell me about pasta."
    - answer: "Tell me about risotto."
```

**Labeled element** (defined under `elements`)
```yaml
elements:
  pasta-intro:
    sequence:
      - statement: "Pasta is made from durum wheat semolina and water."
      - question: "Which pasta dish interests you?"
      - answer: "Spaghetti carbonara"
      - answer: "Penne arrabbiata"
```

**Reference to a labeled element**
```yaml
- ref: pasta-intro
```

---

## 4. Examples — Food Preparation

### Example 1: Simple statement sequence

```yaml
conversation:
  - statement: "Welcome to the Cooking Guide."
  - statement: "Here you will find instructions for preparing various dishes."
```

### Example 2: A single question with answers

```yaml
conversation:
  - question: "What would you like to cook today?"
  - answer: "Pasta"
  - answer: "Risotto"
  - answer: "Soup"
```

### Example 3: Branching FAQ tree — food preparation

This example shows a two-level FAQ: the top level lets the human choose a cuisine category; each choice leads to a deeper sub-conversation.

```yaml
elements:

  pasta-guide:
    sequence:
      - statement: "Pasta dishes require durum wheat pasta and a suitable sauce."
      - question: "Which pasta dish would you like to prepare?"
      - answer: "Spaghetti Carbonara"
      - answer:
          sequence:
            - statement: "Penne Arrabbiata is a spicy tomato-based pasta."
            - question: "How spicy would you like it?"
            - answer: "Mild — use half a chilli pepper."
            - answer: "Hot — use two chilli peppers."
      - answer: "Lasagne al Forno"

  risotto-guide:
    sequence:
      - statement: "Risotto requires Arborio or Carnaroli rice and warm stock."
      - question: "Which risotto would you like to prepare?"
      - answer: "Risotto ai Funghi"
      - answer: "Risotto al Limone"
      - answer: "Risotto Milanese"

  soup-guide:
    sequence:
      - statement: "Soups are versatile and can be made from almost any vegetable."
      - question: "Which soup would you like to make?"
      - answer: "Minestrone"
      - answer: "French Onion Soup"
      - answer: "Pumpkin Soup"

conversation:
  - statement: "Welcome to the Interactive Cooking Guide."
  - statement: "I will help you find the right recipe."
  - question: "What would you like to cook today?"
  - answer:
      sequence:
        - ref: pasta-guide
  - answer:
      sequence:
        - ref: risotto-guide
  - answer:
      sequence:
        - ref: soup-guide
```

### Example 4: Multi-line statement and labeled reuse

```yaml
elements:

  knife-safety:
    statement: |
      Always keep your knives sharp — a dull knife requires more force
      and is more likely to slip.
      Keep fingers curled inward (the "bear claw" grip) when cutting.

  basic-stock:
    sequence:
      - statement: |
          A good stock is the foundation of many dishes.
          Simmer bones or vegetables with aromatics for at least one hour.
      - question: "Which stock do you want to prepare?"
      - answer: "Chicken stock"
      - answer: "Vegetable stock"
      - answer: "Beef stock"

conversation:
  - statement: "Welcome to the Kitchen Fundamentals Guide."
  - question: "What fundamental skill would you like to learn about?"
  - answer:
      sequence:
        - statement: "Knife skills are essential in any kitchen."
        - ref: knife-safety
  - answer:
      sequence:
        - ref: basic-stock
  - answer:
      sequence:
        - statement: "Heat management is crucial: too high and food burns, too low and it steams."
        - question: "Which heat technique interests you?"
        - answer: "Sautéing"
        - answer: "Braising"
        - answer: "Roasting"
```

---

## 5. Summary

| Element     | YAML representation                              | Role       |
|-------------|--------------------------------------------------|------------|
| `statement` | `statement: <text>`                              | Bot        |
| `question`  | `question: <text>`                               | Bot        |
| `answer`    | `answer: <text>` or `answer: sequence: [...]`    | Human      |
| `sequence`  | `sequence: [...]` (list of elements)             | Structural |
| Label       | Named key under `elements:`                      | Both       |
| Reference   | `ref: <label>`                                   | Both       |

A `sequence` nested inside an `answer` introduces a **branch** in the conversation. Labels and `ref` allow conversation fragments to be defined once and reused in multiple places without repetition.

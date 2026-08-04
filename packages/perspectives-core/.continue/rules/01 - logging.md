---
name: Apply logging function
invokable: true
---

NOTE: the sources are written in Purescript.

* apply the convenience logging function I used in the prompt
* apply it to the snippet I have shared
* replace the simple Purescript log statement
* If the snippet is an error situation, use a `PerspectivesError` constructor
* Otherwise use a `PerspectivesWarning` constructor.
* Use `humanizePerspectivesError` or `humanizePerspectivesWarning` if the constructor is defined on Purescript types instead of just simple values like String.

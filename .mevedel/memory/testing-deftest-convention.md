---
name: One zenit-deftest per symbol
description: Testing convention for grouping Zenit ERT coverage by function or variable.
type: feedback
---

Keep tests for a function or variable inside a single `zenit-deftest` declaration for that symbol.

**Why:** The user wants test files organized with one `zenit-deftest` per function or variable, e.g. one `(zenit-deftest +popup--delete-window ...)` rather than additional suffixed declarations like `(zenit-deftest +popup--delete-window/another-test ...)`.

**How to apply:** When adding more cases for an already-tested symbol, extend the existing `zenit-deftest` form and use `:doc` entries or grouped assertions to clarify individual behaviors instead of creating another `zenit-deftest` with a slash suffix.

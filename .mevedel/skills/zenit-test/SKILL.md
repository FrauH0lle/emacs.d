---
name: zenit-test
description: Run and interpret Zenit Emacs tests through the project CLI.
disable-model-invocation: true
---

Use this skill when the user asks to test Zenit changes, when an Elisp change
needs validation, or when choosing between focused and broad checks.

## Workflow

1. Identify the smallest useful test target.
   - Core library tests usually live in `lisp/core/lib/test/`.
   - Core integration tests usually live in `lisp/core/test/`.
   - Module tests live under the module's `test/*.el` directory.
2. Run a focused file first when one exists:
   - `bin/emacs-config test -f path/to/test.el`
3. Run the full CI-equivalent suite when the change affects startup, module
   resolution, package management, CLI behavior, or shared core helpers:
   - `bin/emacs-config test`
4. Report failures by naming the failed test file/test names and the first
   actionable condition. Do not fix unrelated failures unless the user asks.

## Notes

- The test runner starts clean `emacs -Q --batch --init-directory=...`
  subprocesses and calls ERT.
- Do not use `clean --straight`, `clean --all`, `sync`, or `freeze` as part of
  ordinary test validation.
- If package or autoload state changed, run `bin/emacs-config refresh` before
  tests only when generated state is needed for the behavior under test.

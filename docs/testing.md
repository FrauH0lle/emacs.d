# Testing

Zenit tests use ERT through the project CLI.

## Commands

- Full CI-equivalent suite: `bin/emacs-config test`
- Focused test file: `bin/emacs-config test -f path/to/test.el`

## Conventions

- Use ERT for unit tests.
- Prefer `zenit-deftest` for new tests.
- Comparison assertions should consistently compare expected to actual. This makes failed output easier to read:

  ```elisp
  (should (eq EXPECTED ACTUAL))
  ```

## Minimal coverage

If no better behavior-level test is available, minimal tests should check:

- Variables: the variable is bound via `boundp`.
- Functions/macros: the function or macro is bound via `fboundp`.
- Hooks: the hook variable is bound and the function was added to it.
- Advice: the function was advised via `advice-member-p`.

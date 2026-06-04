# Repository Guidelines

## Project Structure

Zenit Emacs is a Doom-inspired personal Emacs configuration. Runtime startup flows through `early-init.el`, which loads `lisp/core/zenit-core.el` and then `zenit-cli` for batch commands or `zenit-start` for interactive startup.

- `lisp/core/`: core runtime, CLI, libraries, and core tests.
- `lisp/modules/`: module tree. Common module files are `init.el`, `config.el`, `packages.el`, `autoload.el` or `autoload/*.el`, `patches.el`, `control.el`, and `test/*.el`.
- `site-lisp/`: tracked local configuration and package overrides.
- `straight/versions/`: package lockfiles managed by the Zenit CLI.
- `docs/`: conventions and commit-message guidance.

Do not hand-edit generated or runtime-local artifacts: `init.el`, `init.elc`, `.local/`, `auto-save-list/`, `straight/build*`, `straight/repos/`, or `.mevedel/sessions/`. Edit source files under `lisp/`, tracked `site-lisp/` files, docs, and approved `.mevedel/skills` or hook config instead.

## Documentation map

This file is the entry point. Detail docs live in `docs/`; read them when planning work in the relevant area.

- [`docs/code-style.md`](docs/code-style.md) — docstrings, comments, Emacs Lisp style references, and macro debug specs.
- [`docs/testing.md`](docs/testing.md) — ERT conventions, `zenit-deftest`, comparison assertion order, and minimal coverage checks.
- [`docs/byte-compilation.md`](docs/byte-compilation.md) — async byte-compilation ordering, compile-time evaluation, and `declare-function` caveats.
- [`docs/module-conventions.md`](docs/module-conventions.md) — `core/lib` requires, bootstrap-safe module checks, and `use-package` `:after` avoidance.
- [`docs/commit-messages.md`](docs/commit-messages.md) — full Conventional Commit plus GNU-style file-bullet message format.

Each core or module `.el` file should carry its own header and docstrings for local behavior details.

## Build/Test/Dev Commands

Use the custom CLI as executable truth; there is no Make/Eask/Cask/package.json workflow.

- Fresh deploy: `bin/emacs-config --yes deploy`
- CI-equivalent test suite: `bin/emacs-config test`
- Focused test file: `bin/emacs-config test -f path/to/test.el`
- Refresh generated init/autoload/package state after module or package changes: `bin/emacs-config refresh`
- Update packages intentionally: `bin/emacs-config sync -u`, then `bin/emacs-config freeze` when lockfiles should change
- Show CLI help: `bin/emacs-config help`

Treat `bin/emacs-config clean --straight` and `bin/emacs-config clean --all` as destructive package-cache operations; do not run them unless the user explicitly asks.

CI runs Emacs 30.2, `bin/emacs-config --yes deploy`, then `bin/emacs-config test` in `.github/workflows/ci.yml`.

## Style

- Use `lexical-binding: t` in Emacs Lisp source.
- Follow existing file headers. `packages.el`, `control.el`, and test files often also use `no-byte-compile: t`.
- Prefer spaces; general defaults are `tab-width` 4 and `fill-column` 80. Emacs Lisp mode intentionally manages its own Lisp indentation.
- Add docstrings for functions and variables. Macro doc/debug behavior matters: macros should declare a debug spec, commonly `(declare (debug t))` when all args evaluate normally.
- Do not define functions or macros inside `after!`; its body is not byte-compiled.
- Use `compile-along!` for patch/autoload companion files that need to compile with their parent.
- For code compiled before generated `init.el` is processed, prefer `zenit-module-p` over `modulep!`.
- Avoid the `:after` keyword in `use-package` forms; see `docs/module-conventions.md`.

## Packages and Modules

- Declare packages with `package!` in `packages.el`; use existing `:lockfile` naming patterns.
- After package declarations or module graph changes, run `bin/emacs-config refresh` before relying on generated autoload/init state.
- For package revision updates, use `bin/emacs-config sync -u` deliberately and `bin/emacs-config freeze` when the lockfiles should be updated.
- Files under `core/lib` are loaded as subfeatures of `zenit-lib`; require them with `zenit-require`, but use the concrete file name in `declare-function` declarations.

## Testing

Tests are ERT-based and run in clean `emacs -Q --batch --init-directory=...` subprocesses through `bin/emacs-config test`.

- Put core tests under `lisp/core/test/` or `lisp/core/lib/test/` as existing files do.
- Put module tests under the module’s `test/*.el` directory.
- Use `zenit-deftest` for new tests; it supports parameterized cases and tags.
- Comparison assertions should read expected then actual, e.g. `(should (equal EXPECTED ACTUAL))`.
- Minimal coverage for new symbols should check `boundp` for variables, `fboundp` for functions/macros, hook membership for hooks, and `advice-member-p` for advice.

## Commits

Follow `docs/commit-messages.md` and `.gitmessage`.

- Summary format: `type(component): Capitalized active-verb summary`.
- Use Conventional Commit types from the docs; this repo also uses `pkg` for package/library/version changes and `tweak` for user-facing default changes.
- Body entries use GNU-style file bullets, e.g. `* file.el (symbol): Describe change.`
- Include rationale for non-obvious changes and mention experimental limitations when relevant.

## Agent Notes

- Prefer Emacs Lisp introspection tools for loaded state when available; static reads can miss advice, generated state, or buffer-local overrides.
- Use `bin/emacs-config test -f ...` before the full test suite when a focused test file covers the change.
- Do not invent lint/format commands; interactive Flycheck/package-lint config exists, but CI’s reliable check is the Zenit test command.
- Project hooks live in `.mevedel/hooks.json` and `.mevedel/hooks/`. They are ignored until reviewed and trusted with `M-x mevedel-hooks-trust-project`.

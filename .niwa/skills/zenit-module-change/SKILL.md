---
name: zenit-module-change
description: Safely modify Zenit modules, autoloads, patches, and core/module boundaries.
disable-model-invocation: true
---

Use this skill for changes under `lisp/modules/`, module declarations, autoload
files, patches, or module/core interactions.

## Inspect

- Locate the module directory and existing files: `init.el`, `config.el`,
  `packages.el`, `autoload.el` or `autoload/*.el`, `patches.el`, `control.el`,
  and `test/*.el`.
- Check whether code runs during bootstrap/compile time or after normal startup.
- For loaded Emacs Lisp behavior, prefer introspection tools over static reads
  when available.

## Change Rules

- Keep module responsibilities local; avoid moving behavior into core unless
  shared startup infrastructure needs it.
- Use `zenit-module-p` instead of `modulep!` for code compiled before generated
  `init.el` is processed.
- Do not define functions or macros inside `after!`; its body is not
  byte-compiled.
- Use `compile-along!` when a patch/autoload companion must compile with its
  parent.
- Put autoloadable public entry points in `autoload.el` or `autoload/*.el`
  following existing patterns.
- Do not hand-edit generated `init.el`, `init.elc`, `.local/`, or
  `straight/build*` artifacts.

## Validate

- Run the focused module test file when one exists: `bin/emacs-config test -f
  lisp/modules/<category>/<module>/test/<file>.el`
- Run `bin/emacs-config refresh` when module/package changes need regenerated
  init/autoload state.
- Run `bin/emacs-config test` when startup, module discovery, or shared core
  behavior may be affected.

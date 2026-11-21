---
name: zenit-package-change
description: Handle Zenit package declarations, package updates, and lockfiles.
disable-model-invocation: true
---

Use this skill when editing `packages.el`, adding/removing `package!`
declarations, changing straight recipes, or updating `straight/versions/*.el`.

## Inspect

- Read the relevant module or `site-lisp/packages.el` package declarations.
- Check existing `:lockfile` naming conventions before adding a package.
- Determine whether the task is a declaration/config change or an intentional
  package revision update.

## Change Rules

- Declare packages with `package!` only in `packages.el` contexts.
- Prefer existing `:lockfile` naming patterns for module packages.
- Do not edit `straight/build*`, `straight/repos/`, `.local/`, or generated
  `init.el`/`init.elc`.
- Do not update lockfiles casually as a side effect of unrelated code changes.

## Commands

- After package declarations or recipes change: `bin/emacs-config refresh`
- For intentional package updates: `bin/emacs-config sync -u`
- To record intended package revisions after an update: `bin/emacs-config
  freeze`
- CI-equivalent validation: `bin/emacs-config test`

Treat `bin/emacs-config clean --straight` and `bin/emacs-config clean --all` as
destructive package-cache operations; ask before running them.

---
name: zenit-commit-message
description: Draft commit summaries and bodies using this repo's Conventional Commit plus GNU-style file bullet format.
disable-model-invocation: true
---

Use this skill when the user asks for a commit summary, commit message, or
review of a proposed commit message.

## Sources

- `docs/commit-messages.md` for full messages.
- `.gitmessage` for the concrete template.

## Format

Summary:

```text
type(component): Capitalized active-verb summary
```

Use the repo's Conventional Commit types. In addition to the usual types, this
repo uses:

- `pkg` for package/library/version changes.
- `tweak` for user-facing default changes that are not major features.

Body entries use GNU-style bullets:

```text
* file.el (symbol1, symbol2): Describe the change.
  Add rationale or limitations when useful.
```

## Rules

- Keep the summary under 72 characters when practical.
- Start the summary description with a capitalized active verb.
- Capture intent, not just touched files.
- Include rationale for non-obvious changes.
- Use `Ditto` for repeated similar file changes.
- For summary-only requests, output only the summary line.

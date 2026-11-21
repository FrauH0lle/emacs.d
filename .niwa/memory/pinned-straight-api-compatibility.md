---
name: "Target the supported pinned Straight API"
description: "Avoid obsolete-format compatibility for Straight unless older versions are intentionally supported."
type: feedback
---

For Zenit's pinned Straight dependency, target the pinned API rather than adding speculative support for obsolete formats.

**Why:** On 2026-11-01, the user questioned dual-format support because the old format was obsolete, then explicitly confirmed: “Then no backwards compat.” Keeping compatibility code limited to supported versions avoids unnecessary branches and maintenance.

**When to apply:** Use this rule when changing integration with the pinned Straight version. Compatibility handling remains appropriate if older pins or independent downgrades are intentionally supported; establish that requirement rather than assuming it. Do not generalize this correction into a ban on backward compatibility for other dependencies.

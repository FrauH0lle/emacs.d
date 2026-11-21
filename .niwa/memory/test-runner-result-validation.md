---
name: "Validate ERT execution, not just CLI exit status"
description: "Check test-load errors and actual ERT counts before reporting a successful test run."
type: feedback
---

When validating Zenit CLI test runs, inspect test-load errors and the actual ERT execution counts and results. A successful process exit alone does not establish that the intended tests ran or passed.

**Why:** The 2026-10-17 Markdown investigation note reports that the CLI initially returned success despite test-load errors and zero executed tests. The eventual four-test pass was established from ERT counts and output, not exit status alone.

**When to apply:** Before reporting focused or full-suite test success, confirm that the intended tests actually executed and that their results support the claim. This observation does not establish that every version or invocation of the runner has the same behavior.

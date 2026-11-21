---
name: "Validate mouse behavior through the command lifecycle"
description: "Mouse regressions need replay and post-command processing; text-position changes alone do not prove dragging."
type: feedback
---

When testing mouse-command behavior involving selection or mode transitions, exercise the complete relevant command lifecycle. Dispatch queued `unread-command-events` and run `post-command-hook` when those participate in the behavior; do not stop at the state immediately returned by the handler.

For click-versus-drag classification in dynamically redisplayed buffers, text-position changes alone are not reliable evidence of physical movement. Compare physical pointer movement as well.

**Why:** On 2026-10-26, the user reported persistent Visual activation despite an initial 12-test pass. Those tests omitted delayed mark activation and mouse-up replay. A captured gesture also had different press/release text positions at identical pixel coordinates, with only one-pixel intervening movement. Redisplay-induced position remapping was the model's explanation, not a directly established cause. Revised lifecycle tests passed 20/20, and the user subsequently confirmed that the replacement worked on graphical retry.

**Scope:** Apply this lesson to mouse behavior affected by replay, post-command hooks, or redisplay. Not every handler-only unit test needs a full event-loop simulation. Passing synthetic tests establishes coverage of those sequences, not proof of live graphical behavior; distinguish test results from user confirmation.

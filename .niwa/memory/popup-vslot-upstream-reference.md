---
name: "Upstream popup vslot stacking fix"
description: "Doom issue and fix for comparison when maintaining Zenit's independent popup stacking implementation."
type: reference
---

For upstream context when maintaining Zenit's popup `:vslot` stacking behavior:

- Issue: https://github.com/doomemacs/core/issues/8452
- Fix: https://github.com/doomemacs/modules/commit/cab83f8ae497d07bd94a04f4c0d8f29ba50df897

On 2026-11-17, the user identified this commit's `+popup--side-window-next-to` as upstream's fix for the same issue addressed independently by Zenit's `+popup--find-vslot-split-ref` in `lisp/modules/ui/popup/+popup-display-func.el`.

Use the issue and commit as comparison sources when investigating stacking regressions or considering upstream alignment. This attribution does not establish equivalence between the implementations; inspect their current behavior before adopting changes. The external pages were not independently inspected in this review.

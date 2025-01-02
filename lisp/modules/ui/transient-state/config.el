;; ui/transient-state/config.el -*- lexical-binding: t; -*-

(defvar-local +symbol-overlay-scope-range nil
  "Cons cell whos car is the start and the cdr is the end of the
current `symbol-overlay-scope'.")

;; PATCH 2024-12-12: `symbol-overlay'
(compile-along! "patches")
(el-patch-feature symbol-overlay)

(use-package! symbol-overlay
  :defer t
  :config
  (load! "patches"))

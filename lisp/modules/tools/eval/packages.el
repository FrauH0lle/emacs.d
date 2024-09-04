;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; tools/eval/packages.el

(package! quickrun :lockfile tools_eval)
(when (modulep! +overlay)
  (package! eros :lockfile tools_eval_overlay))

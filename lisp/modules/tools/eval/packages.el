;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; tools/eval/packages.el

(package! quickrun :lockfile tools-eval)
(when (modulep! +overlay)
  (package! eros :lockfile tools-eval-overlay))

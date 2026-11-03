;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lang/javascript/packages.el

;; Major modes
(unless (modulep! +tree-sitter)
  (package! typescript-mode :lockfile lang_javascript))

;; Extensions
(package! nodejs-repl :lockfile lang_javascript)

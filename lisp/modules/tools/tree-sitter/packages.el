;; -*- no-byte-compile: t; -*-
;; tools/tree-sitter/packages.el

(package! tree-sitter
  :lockfile tools-tree-sitter)

(package! tree-sitter-langs
  :lockfile tools-tree-sitter)

(when (modulep! :editor evil)
  (package! evil-textobj-tree-sitter
    :lockfile tools-tree-sitter))

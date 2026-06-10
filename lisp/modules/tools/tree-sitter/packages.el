;; -*- no-byte-compile: t; -*-
;; tools/tree-sitter/packages.el

(when (modulep! :editor evil)
  (package! evil-textobj-tree-sitter
    :lockfile tools_tree-sitter))

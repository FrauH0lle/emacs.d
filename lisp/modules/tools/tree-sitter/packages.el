;; -*- no-byte-compile: t; -*-
;; tools/tree-sitter/packages.el

(package! tree-sitter
  :lockfile tools_tree-sitter)

(package! tree-sitter-langs
  :lockfile tools_tree-sitter)

(when (modulep! :editor evil)
  (package! evil-textobj-tree-sitter
    :lockfile tools_tree-sitter))

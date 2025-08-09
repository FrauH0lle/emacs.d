;; -*- no-byte-compile: t; -*-
;; tools/tree-sitter/packages.el

(package! tree-sitter
  :lockfile tools_tree-sitter)

(package! treesit-langs
  :recipe
  (:host github :repo "kiennq/treesit-langs" :files ("treesit-*.el")))

;; NOTE 2025-08-08: Only used for queries
(package! tree-sitter-langs
  :lockfile tools_tree-sitter)

(when (modulep! :editor evil)
  (package! evil-textobj-tree-sitter
    :lockfile tools_tree-sitter))

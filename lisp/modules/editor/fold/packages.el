;; -*- no-byte-compile: t; -*-
;; editor/fold/packages.el

(package! hideshow :built-in t)

(package! vimish-fold :lockfile editor_fold)
(when (modulep! :editor evil)
  (package! evil-vimish-fold :lockfile editor_fold_evil))
(when (modulep! :tools tree-sitter)
  (package! treesit-fold :lockfile editor_fold_tree-sitter
    :recipe (:host github :repo "emacs-tree-sitter/treesit-fold")))

(package! outline-minor-faces :lockfile editor_fold)
(package! backline :lockfile editor_fold)

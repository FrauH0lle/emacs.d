;; -*- no-byte-compile: t; -*-
;; editor/fold/packages.el

(package! hideshow :built-in t)

(package! vimish-fold :lockfile editor-fold)
(when (modulep! :editor evil)
  (package! evil-vimish-fold :lockfile editor-fold-evil))
(when (modulep! :tools tree-sitter)
  (package! ts-fold :lockfile editor-fold-tree-sitter
    :recipe (:host github :repo "emacs-tree-sitter/ts-fold")))

(package! outline-minor-faces :lockfile editor-fold)

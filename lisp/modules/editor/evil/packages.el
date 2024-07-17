;; -*- no-byte-compile: t; -*-
;; editor/evil/packages.el

(package! evil :lockfile editor-evil)
(package! evil-args :lockfile editor-evil)
(package! evil-easymotion :lockfile editor-evil)
(package! evil-embrace :lockfile editor-evil)
(package! evil-escape
  :recipe (:host github :repo "hlissner/evil-escape")
  :lockfile editor-evil)
(package! evil-exchange :lockfile editor-evil)
(package! evil-indent-plus :lockfile editor-evil)
(package! evil-lion :lockfile editor-evil)
(package! evil-nerd-commenter :lockfile editor-evil)
(package! evil-numbers :lockfile editor-evil)
(package! evil-snipe :lockfile editor-evil)
(package! evil-surround :lockfile editor-evil)
(package! evil-textobj-anyblock
  :recipe (:host github
           :repo "willghatch/evil-textobj-anyblock"
           :branch "fix-inner-block")
  :lockfile editor-evil)
(package! evil-traces :lockfile editor-evil)
(package! evil-visualstar :lockfile editor-evil)
(package! exato :lockfile editor-evil)
(package! evil-quick-diff
  :recipe (:host github :repo "rgrinberg/evil-quick-diff")
  :lockfile editor-evil)
(package! evil-collection :lockfile editor-evil)

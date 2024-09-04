;; -*- no-byte-compile: t; -*-
;; editor/evil/packages.el

(package! evil :lockfile editor_evil)
(package! evil-args :lockfile editor_evil)
(package! evil-easymotion :lockfile editor_evil)
(package! evil-embrace :lockfile editor_evil)
(package! evil-escape
  :recipe (:host github :repo "hlissner/evil-escape")
  :lockfile editor_evil)
(package! evil-exchange :lockfile editor_evil)
(package! evil-indent-plus :lockfile editor_evil)
(package! evil-lion :lockfile editor_evil)
(package! evil-nerd-commenter :lockfile editor_evil)
(package! evil-numbers :lockfile editor_evil)
(package! evil-snipe :lockfile editor_evil)
(package! evil-surround :lockfile editor_evil)
(package! evil-textobj-anyblock
  :recipe (:host github
           :repo "willghatch/evil-textobj-anyblock"
           :branch "fix-inner-block")
  :lockfile editor_evil)
(package! evil-traces :lockfile editor_evil)
(package! evil-visualstar :lockfile editor_evil)
(package! exato :lockfile editor_evil)
(package! evil-quick-diff
  :recipe (:host github :repo "rgrinberg/evil-quick-diff")
  :lockfile editor_evil)
(package! evil-collection :lockfile editor_evil)

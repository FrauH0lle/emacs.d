;; -*- no-byte-compile: t; -*-
;; editor/evil/packages.el

(package! evil :lockfile editor-evil)
(package! evil-args :lockfile editor-evil)
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
(package! evil-collection :lockfile editor-evil)

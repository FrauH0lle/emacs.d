;; -*- no-byte-compile: t; -*-
;; editor/multiple-cursors/packages.el

(when (modulep! :editor evil)
    (package! evil-mc :lockfile editor-multiple-cursors-evil)
    (package! evil-multiedit :lockfile editor-multiple-cursors-evil))

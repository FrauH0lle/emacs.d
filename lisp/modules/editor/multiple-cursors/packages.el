;; -*- no-byte-compile: t; -*-
;; editor/multiple-cursors/packages.el

(when (modulep! :editor evil)
    (package! evil-mc :lockfile editor_multiple-cursors)
    (package! evil-multiedit :lockfile editor_multiple-cursors))

;; -*- no-byte-compile: t; -*-
;; ui/modeline/packages.el

(package! doom-modeline :lockfile ui_modeline)
(package! anzu :lockfile ui_modeline)
(when (modulep! :editor evil)
  (package! evil-anzu :lockfile ui_modeline_evil))

;; -*- no-byte-compile: t; -*-
;; ui/modeline/packages.el

(package! doom-modeline :lockfile ui-modeline)
(package! anzu :lockfile ui-modeline)
(when (modulep! :editor evil)
  (package! evil-anzu :lockfile ui-modeline-evil))

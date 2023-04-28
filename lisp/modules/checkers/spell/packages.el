;; -*- no-byte-compile: t; -*-
;; checkers/spell/packages.el

(package! flyspell-correct :lockfile checkers-spell)
(cond ((not (modulep! :completion vertico))
       (package! flyspell-correct-popup)))
(package! flyspell-lazy :lockfile checkers-spell)

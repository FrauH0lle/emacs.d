;; -*- no-byte-compile: t; -*-
;; checkers/spell/packages.el

(if (executable-find "enchant-2")
    (package! jinx :lockfile checkers-spell)
  (package! flyspell-correct :lockfile checkers-spell)
  (cond ((not (modulep! :completion vertico))
         (package! flyspell-correct-popup :lockfile checkers-spell)))
  (package! flyspell-lazy :lockfile checkers-spell))

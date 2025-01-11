;; -*- no-byte-compile: t; -*-
;; checkers/spell/packages.el

(if (executable-find "enchant-2")
    (package! jinx :lockfile checkers_spell)
  (package! flyspell-correct :lockfile checkers_spell)
  (cond ((not (modulep! :completion vertico))
         (package! flyspell-correct-popup :lockfile checkers_spell)))
  (package! flyspell-lazy :lockfile checkers_spell))

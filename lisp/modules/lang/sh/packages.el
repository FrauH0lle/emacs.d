;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lang/sh/packages.el

(when (modulep! +fish)
  (package! fish-mode :lockfile lang_sh))

(when (modulep! +powershell)
  (package! powershell :lockfile lang_sh))

;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; emacs/eshell/packages.el

(package! eshell-up :lockfile emacs_eshell)
(package! eshell-z :lockfile emacs_eshell)
(package! shrink-path :lockfile emacs_eshell)
(package! esh-help :lockfile emacs_eshell)
(package! eshell-did-you-mean :lockfile emacs_eshell)
(package! eshell-syntax-highlighting :lockfile emacs_eshell)

(unless zenit--system-windows-p
  (package! fish-completion :lockfile emacs_eshell)
  (package! bash-completion :lockfile emacs_eshell))

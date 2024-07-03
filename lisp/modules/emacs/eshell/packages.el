;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; emacs/eshell/packages.el

(package! eshell-up :lockfile emacs-eshell)
(package! eshell-z :lockfile emacs-eshell)
(package! shrink-path :lockfile emacs-eshell)
(package! esh-help :lockfile emacs-eshell)
(package! eshell-did-you-mean :lockfile emacs-eshell)
(package! eshell-syntax-highlighting :lockfile emacs-eshell)

(unless zenit--system-windows-p
  (package! fish-completion :lockfile emacs-eshell)
  (package! bash-completion :lockfile emacs-eshell))

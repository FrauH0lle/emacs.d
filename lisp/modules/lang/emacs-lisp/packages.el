;; -*- no-byte-compile: t; -*-
;; lang/emacs-lisp/packages.el

(package! elisp-mode :built-in t)

;; Fontification plugins
(package! highlight-quoted :lockfile lang_emacs-lisp)

;; Tools
(package! helpful :lockfile lang_emacs-lisp)
(package! macrostep :lockfile lang_emacs-lisp)
(package! elisp-def :lockfile lang_emacs-lisp)
(package! elisp-demos :lockfile lang_emacs-lisp)
(when (modulep! :checkers syntax -flymake)
  (package! flycheck-package :lockfile lang_emacs-lisp))
(when (modulep! :checkers syntax +flymake)
  (package! package-lint-flymake :lockfile lang_emacs-lisp))

(package! buttercup :lockfile lang_emacs-lisp)

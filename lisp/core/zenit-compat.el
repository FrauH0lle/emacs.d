;; lisp/core/zenit-compat.el -*- lexical-binding: t; -*-

;; Doesn't exist in terminal Emacs, but some Emacs packages (internal and
;; external) use it anyway, leading to a void-function error, so define a no-op
;; substitute to suppress them.
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))
(unless (fboundp 'set-fontset-font)
  (fset 'set-fontset-font #'ignore))

(provide 'zenit-compat)

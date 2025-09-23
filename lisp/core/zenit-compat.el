;;; lisp/core/zenit-compat.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Doesn't exist in terminal Emacs, but some Emacs packages (internal and
;; external) use it anyway, leading to a void-function error, so define a no-op
;; substitute to suppress them.
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))
(unless (fboundp 'set-fontset-font)
  (fset 'set-fontset-font #'ignore))

;; Introduced in 31.1
(unless (fboundp 'static-when)
  (defmacro static-when (condition &rest body)
    "A conditional compilation macro.
Evaluate CONDITION at macro-expansion time.  If it is non-nil,
expand the macro to evaluate all BODY forms sequentially and return
the value of the last one, or nil if there are none."
    (declare (indent 1) (debug t))
    (if body
        (if (eval condition lexical-binding)
            (cons 'progn body)
          nil)
      (macroexp-warn-and-return (format-message "`static-when' with empty body")
                                nil '(empty-body static-when) t
                                condition))))

;; Introduced in 31.1
(unless (fboundp 'static-unless)
  (defmacro static-unless (condition &rest body)
    "A conditional compilation macro.
Evaluate CONDITION at macro-expansion time.  If it is nil,
expand the macro to evaluate all BODY forms sequentially and return
the value of the last one, or nil if there are none."
    (declare (indent 1) (debug t))
    (if body
        (if (eval condition lexical-binding)
            nil
          (cons 'progn body))
      (macroexp-warn-and-return (format-message "`static-unless' with empty body")
                                (list 'progn nil nil) '(empty-body static-unless) t))))

(provide 'zenit-compat)

;;; zenit-compat.el ends here.

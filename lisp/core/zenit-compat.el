;;; lisp/core/zenit-compat.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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

;; From Emacs 31+
(declare-function mode-line-invisible-mode nil)
(unless (fboundp 'mode-line-invisible-mode)
  (defvar-local mode-line-invisible--buf-state nil)
  (define-minor-mode mode-line-invisible-mode
    "Toggle the mode-line visibility of the current buffer.
Hide the mode line if it is shown, and show it if it's hidden."
    :global nil
    :group 'mode-line
    (if mode-line-invisible-mode
        (progn
          (add-hook 'after-change-major-mode-hook #'mode-line-invisible-mode nil t)
          (setq mode-line-invisible--buf-state
                (buffer-local-set-state mode-line-format nil)))
      (remove-hook 'after-change-major-mode-hook #'mode-line-invisible-mode t)
      (when mode-line-invisible--buf-state
        (setq mode-line-invisible--buf-state
              (buffer-local-restore-state mode-line-invisible--buf-state)))
      (unless mode-line-format
        (setq-local mode-line-format (default-value 'mode-line-format)))
      (when (called-interactively-p 'any)
        (force-mode-line-update))))
  (put 'mode-line-invisible--buf-state 'permanent-local t)
  (put 'mode-line-invisible-mode 'permanent-local-hook t))

(provide 'zenit-compat)

;;; zenit-compat.el ends here.

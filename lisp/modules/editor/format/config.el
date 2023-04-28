;;; editor/format/config.el -*- lexical-binding: t; -*-

(defvar +format-preserve-indentation t
  "If non-nil, the leading indentation is preserved when formatting
the whole
buffer. This is particularly useful for partials.

Indentation is always preserved when formatting regions.")

(defvar-local +format-with nil
  "Set this to explicitly use a certain formatter for the current
buffer.")

(defvar +format-with-lsp t
  "If non-nil, format with LSP formatter if it's available.

This can be set buffer-locally with `setq-hook!' to disable LSP
formatting in select buffers.")


;;
;;; Bootstrap

(add-to-list 'zenit-debug-variables 'format-all-debug)


;;
;;; Hacks

;; Allow a specific formatter to be used by setting `+format-with', either
;; buffer-locally or let-bound.
(advice-add #'format-all--probe :around #'+format-probe-a)

;; We use a modded `format-all-buffer', which
;;   1. Enables partial reformatting (while preserving leading indentation),
;;   2. Applies changes via RCS patch, line by line, to protect buffer markers
;;      and avoid any jarring cursor+window scrolling.
(advice-add #'format-all-buffer--with :override #'+format-buffer-a)

;; format-all-mode "helpfully" raises an error when it doesn't know how to
;; format a buffer.
(add-to-list 'debug-ignored-errors "^Don't know how to format ")

;; Don't pop up imposing warnings about missing formatters, but still log it in
;; to *Messages*.
(defadvice! +format--all-buffer-from-hook-a (fn &rest args)
  :around #'format-all-buffer--from-hook
  (letf! (defun format-all-buffer--with (formatter mode-result)
           (when (or (eq formatter 'lsp)
                     (eq formatter 'eglot)
                     (condition-case-unless-debug e
                         (format-all--formatter-executable formatter)
                       (error
                        (message "Warning: cannot reformat buffer because %S isn't installed"
                                 (gethash formatter format-all--executable-table))
                        nil)))
             (funcall format-all-buffer--with formatter mode-result)))
    (apply fn args)))

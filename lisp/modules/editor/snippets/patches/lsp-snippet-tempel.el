;; editor/snippets/patches/lsp-snippet-tempel.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'el-patch)
  (require 'lsp-snippet-tempel))

;; PATCH `lsp-mode' now checks if `yas-minor-mode' is bound
(el-patch-defun lsp-snippet-tempel-lsp-mode-init ()
  (lsp-snippet-tempel--init)
  (advice-add 'lsp--expand-snippet :override #'lsp-snippet-tempel--lsp-mode-expand-snippet)
  ;; HACK `lsp-mode' enables snippet based on `(feature 'yasnippet)'
  (el-patch-remove
    (provide 'yasnippet)))

;; PATCH Return custom 'lsp-choice element for `tempel--element'
(el-patch-defun lsp-snippet-tempel--choice-fn (number choices)
  (el-patch-swap
    (lsp-snippet-tempel--placeholder-fn number (string-join choices ","))
    (let ((sym (intern (format "tabstop-%d" number))))
      `((lsp-choice ,choices ,sym)))))

;; editor/snippets/patches/lsp-snippet.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'el-patch)
  (require 'lsp-snippet))

;; PATCH Clumsy approach to fix
;;   https://github.com/svaante/lsp-snippet/issues/5
(el-patch-defun lsp-snippet--escaped-string-to (stop-at)
  (let ((start (lsp-snippet--index))
        (continue t)
        (el-patch-add
          (result "")))
    (while continue
      ;; Skip next token if `backslash'
      (when (lsp-snippet--take '(backslash))
        ;; Skip next token
        (lsp-snippet--ignore '(eof))
        (el-patch-add
          (setq result (concat result (substring lsp-snippet--str (1+ start) (lsp-snippet--index))))
          (setq start (lsp-snippet--index))))
      (setq continue (lsp-snippet--ignore
                      `(eof . ,stop-at)))
      (el-patch-add
        (setq result (concat result (substring lsp-snippet--str start (lsp-snippet--index))))
        (setq start (lsp-snippet--index))))
    (el-patch-swap
      (unless (eq start (lsp-snippet--index))
        (substring lsp-snippet--str start (lsp-snippet--index)))
      (when (length> result 0)
        result))))

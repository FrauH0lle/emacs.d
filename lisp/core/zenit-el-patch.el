;;; lisp/core/zenit-el-patch.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;
;;; el-patch definitions

;; `el-patch' is great! We add extra `el-patch-deftype's early, so we can use
;; them.

(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'el-patch))

(defvar el-patch-variant nil)

(with-eval-after-load 'el-patch
  ;; Add `cl-defmacro'
  (el-patch-deftype cl-defmacro
    :classify el-patch-classify-function
    :locate el-patch-locate-function
    :font-lock el-patch-fontify-as-defun
    :declare ((doc-string 3)
              (indent defun))))

(provide 'zenit-el-patch)

;;; zenit-el-patch.el ends here.

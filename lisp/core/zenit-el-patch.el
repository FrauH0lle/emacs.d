;; lisp/core/zenit-el-patch.el -*- lexical-binding: t; -*-


;;
;;; el-patch definitions

;; `el-patch' is great! We add extra `el-patch-deftype's early, so we can use
;; them.

(use-package! el-patch
  :defer t
  :config
  ;; Add `cl-defmacro'
  (el-patch-deftype cl-defmacro
    :classify el-patch-classify-function
    :locate el-patch-locate-function
    :font-lock el-patch-fontify-as-defun
    :declare ((doc-string 3)
              (indent defun))))

(provide 'zenit-el-patch)

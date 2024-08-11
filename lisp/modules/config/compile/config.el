;; config/compilation/config.el -*- lexical-binding: t; -*-

;; A bit of a rough hack, however, the async native compiler does its work in a
;; pristine emacs process. This is good for packages, but not for our config
;; files. Thus, we patch this function so it loads our config environment
;; beforehand in case the file to be compiled is part of `zenit-core-dir'.

;; PATCH 2024-08-02: `comp'
;; (el-patch-feature comp)
;; (compile-along! "patches")

;; (after! comp
;;   (load! "patches")
;;   ;; HACK 2024-08-10: ~/.emacs.d/init.el gets compiled before our patch actually
;;   ;;   applies thus we add it manually to the native-comp queue again
;;   (setq! native-comp-jit-compilation-deny-list
;;          (delete "/emacs.d/init\\.el\\'" native-comp-jit-compilation-deny-list))
;;   (let ((fname (file-name-concat user-emacs-directory "init.el")))
;;     (when (file-newer-than-file-p fname (comp-el-to-eln-filename fname))
;;       (appendq! comp-files-queue `((,fname . late))))))

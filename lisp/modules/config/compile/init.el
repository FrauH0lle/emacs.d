;; config/compile/init.el -*- lexical-binding: t; -*-

;; PATCH 2025-04-23: `comp-run'
(el-patch-feature comp-run)
(compile-along! "patches")

(after! comp-run
  (load! "patches")
  ;; HACK 2024-08-10: Add ~/.emacs.d/init.el manually to the native-comp queue
  ;;   again.
  (add-hook! 'zenit-first-buffer-hook
    (defun +compile-reset-deny-list-h ()
      (setq! native-comp-jit-compilation-deny-list
             (delete "/\\.emacs\\.d/init\\.el\\'" native-comp-jit-compilation-deny-list))
      (let ((fname (file-name-concat user-emacs-directory "init.el")))
        (when (file-newer-than-file-p fname (comp-el-to-eln-filename fname))
          (appendq! comp-files-queue `((,fname . late))))))))

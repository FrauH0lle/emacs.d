;; config/compile/init.el -*- lexical-binding: t; -*-


;; PATCH 2024-08-02: `comp'
(el-patch-feature comp)
(compile-along! "patches")

(after! comp
  (load! "patches")
  ;; HACK 2024-08-10: ~/.emacs.d/init.el gets compiled before our patch actually
  ;;   applies thus we add it manually to the native-comp queue again
  (add-hook! 'zenit-after-init-hook
    (defun +compile-reset-deny-list-h ()
      (setq! native-comp-jit-compilation-deny-list
             (delete "/\\.emacs\\.d/init\\.el\\'" native-comp-jit-compilation-deny-list))
      (let ((fname (file-name-concat user-emacs-directory "init.el")))
        (when (file-newer-than-file-p fname (comp-el-to-eln-filename fname))
          (appendq! comp-files-queue `((,fname . late)))))))
  )

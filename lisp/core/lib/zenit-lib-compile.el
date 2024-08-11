;; lisp/core/lib/zenit-lib-compile.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Compile order
;; clean:
;; - zenit-lib
;; - zenit-core
;; req zenit-core:
;; - lib/*
;;
;;

(autoload #'async-inject-variables "async")
(autoload #'async-get "async")

;;;###autoload
(defun zenit-compile-generate-args (target)
  (message "target is: %s "(expand-file-name target))
  (message "assumed init.el is: %s "(expand-file-name (file-name-concat user-emacs-directory "init.el")))
  (cond ((string-suffix-p "zenit-lib.el" target)
         nil)
        ((string-suffix-p "zenit-core.el" target)
         nil)
        ((file-in-directory-p target (file-name-concat zenit-core-dir "lib/"))
         '(:req-core t))
        ((string-suffix-p "zenit-modules.el" target)
         '(:req-core t))
        ((string-suffix-p "zenit-start.el" target)
         '(:req-core t))
        ((string-suffix-p "zenit-use-package.el" target)
         '(:req-core t))
        ((string-suffix-p "zenit-el-patch.el" target)
         '(:req-core t))
        ((string-suffix-p "zenit-keybinds.el" target)
         '(:req-core t :req-extra (zenit-modules zenit-use-package zenit-el-patch)))
        ((string-suffix-p "zenit-ui.el" target)
         '(:req-core t :req-extra (zenit-use-package)))
        ((string-suffix-p "zenit-projects.el" target)
         '(:req-core t :req-extra (zenit-use-package)))
        ((string-suffix-p "zenit-editor.el" target)
         '(:req-core t :req-extra (zenit-use-package)))
        ((or (file-in-directory-p target (file-name-concat user-emacs-directory "lisp" "core/"))
             (file-in-directory-p target (file-name-concat user-emacs-directory "lisp" "modules/"))
             (file-in-directory-p target (file-name-concat user-emacs-directory "site-lisp"))
             (equal (expand-file-name target) (expand-file-name (file-name-concat user-emacs-directory "init.el"))))
         '(:req-core-lib t :req-core t :req-core-libs all
           :req-extra (cl-lib zenit-modules zenit-use-package zenit-el-patch
                              zenit-keybinds zenit-projects zenit-editor)
           :modulep t))
        (t
         nil)))

(defun zenit-compile--generate-modules ()
  (let ((config-modules-list (zenit-module-list)))
    ;; Cache module state and flags in symbol plists for quick lookup by
    ;; `modulep!' later.
    `(,@(cl-loop
         for (category . modules) in (seq-group-by #'car config-modules-list)
         collect
         `(setplist ',category
           (quote ,(cl-loop for (_ . module) in modules
                            nconc `(,module ,(get category module)))))))))

;;;###autoload
(cl-defun zenit-compile-setup-env (&key
                                   (req-core-lib nil)
                                   (req-core nil)
                                   (req-core-libs nil)
                                   (req-extra nil)
                                   (modulep nil))
  `(progn
     ,(when req-core-lib
        `(require 'zenit-lib))
     ,(when (or req-core req-core-libs)
        `(require 'zenit-core))
     ,(when req-core-libs
        (cond ((eq req-core-libs 'all)
               `(progn ,@(cl-loop for lib in (zenit-files-in (file-name-concat zenit-core-dir "lib") :match ".el$")
                                  collect `(zenit-require 'zenit-lib ',(intern (string-remove-prefix "zenit-lib-" (file-name-base lib)))))))
              (t
               `(progn ,@(cl-loop for lib in (ensure-list req-core-libs)
                                  collect `(zenit-require 'zenit-lib ',lib))))))
     ,(when req-extra
        `(progn ,@(cl-loop for lib in (ensure-list req-extra)
                           collect `(require ',lib))))
     ,@(when modulep
         (zenit-compile--generate-modules))
     (when (featurep 'zenit-core)
       (zenit-context-push 'compile))

     ;; Prevent packages from being loaded at compile time if they
     ;; don't meet their own predicates.
     (with-eval-after-load 'use-package
       (push (list :no-require t
                   (lambda (_name args)
                     (or (when-let (pred (or (plist-get args :if)
                                             (plist-get args :when)))
                           (not (eval pred t)))
                         (when-let (pred (plist-get args :unless))
                           (eval pred t)))))
             use-package-defaults))))

;;;###autoload
(cl-defun zenit-async-byte-compile-file
    (file &key
          (req-core-lib nil)
          (req-core nil)
          (req-core-libs nil)
          (req-extra nil)
          (modulep nil))
  "Byte compile Lisp code FILE asynchronously.

By setting the following keyword arguments, you can control which
libraries will be loaded before compilation.

  REQ-CORE-LIB (bool): `zenit-lib'
  REQ-CORE (bool): `zenit-core'
  REQ-CORE-LIBS (list): list of libraries in \\='core/lib'
    without the \\='zenit-lib-' prefix
  REQ-EXTRA (list): list of libraries

Note, that `zenit-core' requires `zenit-lib', thus REQ-CORE is
usually enough."
  (require 'async-bytecomp)
  (async-start
   `(lambda ()
      (require 'bytecomp)
      ,(async-inject-variables async-bytecomp-load-variable-regexp)
      ,(async-inject-variables "\\`zenit-modules\\'")
      ,(async-inject-variables "\\`zenit-disabled-packages\\'")
      (setq load-prefer-newer t)
      (setq byte-compile-warnings t)
      ,(zenit-compile-setup-env
        :req-core-lib req-core-lib :req-core req-core
        :req-core-libs req-core-libs :req-extra req-extra :modulep modulep)
(message "for file %s (zenit-context-p 'compile) is: %s" ,file (when (fboundp 'zenit-context-p) (zenit-context-p 'compile)))
      (let ((default-directory ,default-directory)
            error-data status compiler-log)
        (pcase (byte-compile-file ,file)
          (`no-byte-compile (setq status 'no-byte-compile))
          (`nil (setq status nil))
          (_ (setq status t)))
        (when (get-buffer byte-compile-log-buffer)
          (setq error-data (with-current-buffer byte-compile-log-buffer
                             (buffer-substring-no-properties (point-min) (point-max))))
          (unless (string= error-data "")
            (with-temp-buffer
              (insert (format "is noninteractive true? : %s \n" noninteractive))
              (insert (format "for file %s is zenit-context-p definded?: %s\n" ,file (fboundp 'zenit-context-p)))
              (insert (format "for file %s (zenit-context-p 'compile) is: %s\n" ,file (when (fboundp 'zenit-context-p) (zenit-context-p 'compile))))
              (insert error-data ?\n)
              (goto-char (point-min))
              (insert ,file ":\n")
              (setq compiler-log (buffer-string))
              (append-to-file (point-min) (point-max) ,async-byte-compile-log-file))))
        (cons status compiler-log)))
   nil))

(provide 'zenit-lib '(compile))

;; (let ((filename "~/.emacs.d/lisp/core/lib/zenit-lib-config.el"))
;;   (let ((async-byte-compile-log-file (concat (file-name-sans-extension filename) "-async-bytecomp.log")))
;;     (when (file-exists-p async-byte-compile-log-file)
;;       (delete-file async-byte-compile-log-file))
;;     (message "comp: %s" (async-get (zenit-async-byte-compile-file filename :req-core t)))))

;; (pcase-let ((`(,status . ,msg)
             ;; (async-get (zenit-async-byte-compile-file "~/.emacs.d/lisp/core/zenit-core.el" :req-core-libs '(files)))))
;;   (when status (message "all good!"))
;;   (when msg (message "%s" msg))
;;   )

;; (message "comp: %s" (async-get (zenit-async-byte-compile-file "~/.emacs.d/lisp/core/zenit-core.el")))

;; (mapc #'delete-file (zenit-files-in (dir!) :match ".log$"))

;; (let ((filename "~/.emacs.d/lisp/core/zenit-core.el"))
;;   (let ((async-byte-compile-log-file (concat (file-name-sans-extension filename) "-async-bytecomp.log")))
;;     (when (file-exists-p async-byte-compile-log-file)
;;       (delete-file async-byte-compile-log-file))
;;     (message "comp: %s" (async-get (zenit-async-byte-compile-file filename)))))

;; (let ((filename "~/.emacs.d/lisp/core/zenit-lib.el"))
;;   (let ((async-byte-compile-log-file (concat (file-name-sans-extension filename) "-async-bytecomp.log")))
;;     (when (file-exists-p async-byte-compile-log-file)
;;       (delete-file async-byte-compile-log-file))
;;     (message "comp: %s" (async-get (zenit-async-byte-compile-file filename)))))

;; (dolist (filename (zenit-files-in "~/.emacs.d/lisp/core/lib/" :match ".el$"))
;;   (let ((async-byte-compile-log-file (concat (file-name-sans-extension filename) "-async-bytecomp.log")))
;;     (when (file-exists-p async-byte-compile-log-file)
;;       (delete-file async-byte-compile-log-file))
;;     (message "comp: %s" (async-get (zenit-async-byte-compile-file filename :req-core t)))))

;; (let ((filename "~/.emacs.d/lisp/core/zenit-modules.el"))
;;   (let ((async-byte-compile-log-file (concat (file-name-sans-extension filename) "-async-bytecomp.log")))
;;     (when (file-exists-p async-byte-compile-log-file)
;;       (delete-file async-byte-compile-log-file))
;;     (message "comp: %s" (async-get (zenit-async-byte-compile-file filename :req-core t)))))

;; (let ((filename "~/.emacs.d/lisp/core/zenit-start.el"))
;;   (let ((async-byte-compile-log-file (concat (file-name-sans-extension filename) "-async-bytecomp.log")))
;;     (when (file-exists-p async-byte-compile-log-file)
;;       (delete-file async-byte-compile-log-file))
;;     (message "comp: %s" (async-get (zenit-async-byte-compile-file filename :req-core t)))))

;; (let ((filename "~/.emacs.d/lisp/core/zenit-use-package.el"))
;;   (let ((async-byte-compile-log-file (concat (file-name-sans-extension filename) "-async-bytecomp.log")))
;;     (when (file-exists-p async-byte-compile-log-file)
;;       (delete-file async-byte-compile-log-file))
;;     (message "comp: %s" (async-get (zenit-async-byte-compile-file filename :req-core t)))))

;; (let ((filename "~/.emacs.d/lisp/core/zenit-el-patch.el"))
;;   (let ((async-byte-compile-log-file (concat (file-name-sans-extension filename) "-async-bytecomp.log")))
;;     (when (file-exists-p async-byte-compile-log-file)
;;       (delete-file async-byte-compile-log-file))
;;     (message "comp: %s" (async-get (zenit-async-byte-compile-file filename :req-core t :req-extra '(zenit-use-package))))))

;; (let ((filename "~/.emacs.d/lisp/core/zenit-keybinds.el"))
;;   (let ((async-byte-compile-log-file (concat (file-name-sans-extension filename) "-async-bytecomp.log")))
;;     (when (file-exists-p async-byte-compile-log-file)
;;       (delete-file async-byte-compile-log-file))
;;     (message "comp: %s" (async-get
;;                          (zenit-async-byte-compile-file
;;                           filename
;;                           :req-core t :req-extra '(zenit-modules zenit-use-package zenit-el-patch))))))

;; (let ((filename "~/.emacs.d/lisp/core/zenit-ui.el"))
;;   (let ((async-byte-compile-log-file (concat (file-name-sans-extension filename) "-async-bytecomp.log")))
;;     (when (file-exists-p async-byte-compile-log-file)
;;       (delete-file async-byte-compile-log-file))
;;     (message "comp: %s" (async-get
;;                          (zenit-async-byte-compile-file
;;                           filename
;;                           :req-core t :req-extra '(zenit-use-package))))))

;; (let ((filename "~/.emacs.d/lisp/core/zenit-projects.el"))
;;   (let ((async-byte-compile-log-file (concat (file-name-sans-extension filename) "-async-bytecomp.log")))
;;     (when (file-exists-p async-byte-compile-log-file)
;;       (delete-file async-byte-compile-log-file))
;;     (message "comp: %s" (async-get
;;                          (zenit-async-byte-compile-file
;;                           filename
;;                           :req-core t :req-extra '(zenit-use-package))))))

;; (let ((filename "~/.emacs.d/lisp/core/zenit-editor.el"))
;;   (let ((async-byte-compile-log-file (concat (file-name-sans-extension filename) "-async-bytecomp.log")))
;;     (when (file-exists-p async-byte-compile-log-file)
;;       (delete-file async-byte-compile-log-file))
;;     (message "comp: %s" (async-get
;;                          (zenit-async-byte-compile-file
;;                           filename
;;                           :req-core t :req-extra '(zenit-use-package))))))

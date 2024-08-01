;; lisp/core/zenit-cli.el -*- lexical-binding: t; -*-


(defvar zenit-auto-accept (getenv "YES")
  "If non-nil, Emacs will auto-accept any confirmation prompts
during batch commands.")

(defvar zenit-auto-discard (getenv "FORCE")
  "If non-nil, discard all local changes while updating.")


;;
;;; Bootstrap

(when noninteractive
  (zenit-context-push 'cli)

  (setq gc-cons-threshold 134217728  ; 128mb
        gc-cons-percentage 1.0)

  ;; Create all our core directories to quell file errors.
  (mapc (zenit-rpartial #'make-directory 'parents)
        (list zenit-local-dir
              zenit-data-dir
              zenit-cache-dir))

  (require 'cl-lib)

  ;; (quiet!
   (require 'cl nil t)
   (unless site-run-file
     (let ((site-run-file "site-start")
           (tail load-path)
           (lispdir (expand-file-name "../lisp" data-directory))
           dir)
       (while tail
         (setq dir (car tail))
         (let ((default-directory dir))
           (load (expand-file-name "subdirs.el") t inhibit-message t))
         (unless (string-prefix-p lispdir dir)
           (let ((default-directory dir))
             (load (expand-file-name "leim-list.el") t inhibit-message t)))
         (setq tail (cdr tail)))
       (load site-run-file t inhibit-message)))
   ;; )

  (setq-default
   ;; Don't generate superfluous files when writing temp buffers.
   make-backup-files nil
   ;; Stop user configuration from interfering with package management.
   enable-dir-local-variables nil
   ;; Reduce ambiguity, embrace specificity, enjoy predictability.
   case-fold-search nil
   ;; Don't clog the user's trash with our CLI refuse.
   delete-by-moving-to-trash nil)

  (require 'seq)
  (require 'map)

  ;; Suppress any possible coding system prompts during CLI sessions.
  (set-language-environment "UTF-8")

  ;; Eagerly load these libraries
  (mapc (zenit-rpartial #'load nil (not init-file-debug) 'nosuffix)
        (append (file-expand-wildcards (concat zenit-core-dir "lib/*.el"))
                (file-expand-wildcards (concat zenit-core-dir "cli/*.el"))))

  (if init-file-debug (zenit-debug-mode +1))

  ;; Ensure package management is ready
  (require 'zenit-modules)
  (require 'zenit-packages)

  ;; Last minute initialization at the end of loading this file.
  (with-eval-after-load 'zenit-cli
    (zenit-run-hooks 'zenit-before-init-hook))

  ;; Load site-lisp/init.el, which defines the modules to use.
  (load! (string-remove-suffix ".el" zenit-module-init-file) zenit-local-conf-dir t))


;;
;;; Errors
(define-error 'zenit-cli-error "There was an unexpected error" 'zenit-error)

(provide 'zenit-cli)

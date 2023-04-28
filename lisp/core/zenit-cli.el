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
  ;; Native compilation support
  ;; (when (boundp 'native-comp-eln-load-path)
  ;;   ;; Don't store eln files in ~/.emacs.d/eln-cache. We need to set this very
  ;;   ;; early so more or less all files are caught. We also set this again in
  ;;   ;; zenit-core.el.
  ;;   ;; REVIEW Use `startup-redirect-eln-cache' when 28 support is dropped
  ;;   (add-to-list 'native-comp-eln-load-path (expand-file-name ".local/cache/eln/" user-emacs-directory)))

  (setq gc-cons-threshold 134217728  ; 128mb
        gc-cons-percentage 1.0)

  ;; Create all our core directories to quell file errors.
  (mapc (zenit-rpartial #'make-directory 'parents)
        (list zenit-local-dir
              zenit-data-dir
              zenit-cache-dir))

  (setq-default
   ;; Don't generate superfluous files when writing temp buffers.
   make-backup-files nil
   ;; Stop user configuration from interfering with package management.
   enable-dir-local-variables nil
   ;; Reduce ambiguity, embrace specificity, enjoy predictability.
   case-fold-search nil
   ;; Don't clog the user's trash with our CLI refuse.
   delete-by-moving-to-trash nil)

  (require 'cl-lib)
  (require 'seq)
  (require 'map)

  ;; Eagerly load these libraries
  (mapc (zenit-rpartial #'load nil (not init-file-debug) 'nosuffix)
        (append (file-expand-wildcards (concat zenit-core-dir "lib/*.el"))
                (file-expand-wildcards (concat zenit-core-dir "cli/*.el"))))

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

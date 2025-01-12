;; early-init.el -*- lexical-binding: t; -*-

;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens. This means, that this file is loaded
;; first, not matter what.

;; Unsetting `file-name-handler-alist' offers a reduction of startup time.
(let (file-name-handler-alist)
  ;; Defer garbage collection further back in the startup process.
  (if noninteractive
      ;; 128mb for non-interactive sessions
      (setq gc-cons-threshold 134217728
            gc-cons-percentage 1.0)
    (setq gc-cons-threshold most-positive-fixnum))


  ;; Not checking if bytecode is up-to-date saves some time. This should be
  ;; ensured when the configuration is built.
  (setq load-prefer-newer noninteractive)

  ;; DEBUG envvar as an alternative to --debug-init.
  (when (getenv-internal "DEBUG")
    (setq init-file-debug t
          debug-on-error t))

  ;; Native compilation support
  (when (featurep 'native-compile)
    ;; Don't store eln files in ~/.emacs.d/eln-cache. We need to set this very
    ;; early so more or less all files are caught. We also set this again in
    ;; zenit-core.el.
    (startup-redirect-eln-cache ".local/cache/eln/")
    ;; HACK 2024-08-10: ~/.emacs.d/init.el gets compiled before our patch actually
    ;;   applies. Thus, we add it to `native-comp-jit-compilation-deny-list' to
    ;;   prevent any compilation attempt. Otherwise there can be a race condition
    ;;   between the first compilation attempt of .emacs.d/init.el and any later
    ;;   one.
    (defvar native-comp-jit-compilation-deny-list '("/\\.emacs\\.d/init\\.el\\'")))

  (when (member "--benchmark-init" command-line-args)
    (delete "--benchmark-init" command-line-args)

    (defvar zenit-tracked-loads nil
      "Alist of feature to `load-file-name' when it was first
 required. Start Emacs with --benchmark-init to populate.")

    (defun +zenit-require-advice (feature &optional filename &rest _)
      "For every `require', record the current `load-file-name'."
      (unless (cond (feature
                     (featurep feature))
                    (filename
                     (load-history-filename-element
                      (purecopy (load-history-regexp filename))))
                    (t t))
        (setf (alist-get (or feature filename) zenit-tracked-loads) load-file-name)))

    (defun +zenit-load-advice (file &rest _)
      "For every `load', record the current `load-file-name' if not
already recorded."
      (unless (alist-get file zenit-tracked-loads)
        (setf (alist-get file zenit-tracked-loads) load-file-name)))

    (advice-add #'require :before #'+zenit-require-advice)
    (advice-add #'load :before #'+zenit-load-advice)

    (defun zenit-show-tracked-loads ()
      (interactive)
      (let ((buffer (get-buffer-create "*zenit:tracked-loads*")))
        (with-current-buffer buffer
          (erase-buffer)
          (print (nreverse zenit-tracked-loads) (current-buffer))
          (pp-buffer))
        (switch-to-buffer buffer)))

    (require 'benchmark-init
             ;; Point to the file manually as `load-path' is not initialized yet
             (expand-file-name
              "benchmark-init"
              (file-name-concat user-emacs-directory "straight" "build" "benchmark-init")))
    (add-hook 'zenit-after-init-hook #'benchmark-init/deactivate 109))

  ;; `load' and `require' use `load-suffixes' to locate a file. Startup time can
  ;; be reduced by limiting them.
  (if (let ((load-suffixes '(".elc" ".el"))
            (zenit-core-file (expand-file-name "lisp/core/zenit-core" user-emacs-directory)))
        (message "user-emacs-directory : %s" (expand-file-name "lisp/core/zenit-core" user-emacs-directory))
        (message "zenit-core-file : %s" zenit-core-file)
        (message "does the file exist? : %s" (file-exists-p (concat zenit-core-file ".el")))
        ;; The following keeps non-config related errors visible.
        (if (file-exists-p (concat zenit-core-file ".el"))
            ;; Load the core of the configuration.
            (load zenit-core-file nil (not init-file-debug) nil 'must-suffix)
          ;; If this fails, something is wrong in the lisp/core directory.
          (signal 'error
                  (list "Could not find lisp/core/zenit-core"
                        "make sure this file exists."))))
      ;; Otherwise, proceed the startup.
      (require (if noninteractive 'zenit-cli 'zenit-start))))

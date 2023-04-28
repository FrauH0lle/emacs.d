;; early-init.el -*- lexical-binding: t; -*-

;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens. This means, that this file is loaded
;; first, not matter what. This also means, that this file replaces `init.el'
;; because anything lower than Emacs 27 is not supported.

;; Defer garbage collection further back in the startup process.
(setq gc-cons-threshold most-positive-fixnum)

;; Not checking if bytecode is up-to-date saves some time. This should be
;; ensured when the configuration is built.
(setq load-prefer-newer noninteractive)

;; DEBUG envvar as an alternative to --debug-init.
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))

;; Native compilation support
;; Don't store eln files in ~/.emacs.d/eln-cache. We need to set this very early
;; so more or less all files are caught. We also set this again in
;; zenit-core.el.
(when (featurep 'native-compile)
  (startup-redirect-eln-cache ".local/cache/eln/"))

(or
 ;; Unsetting `file-name-handler-alist' offers a reduction of startup time. Will
 ;; be used in lisp/core/zenit-core.el again.
 (let (file-name-handler-alist)
   (let* (;; Unset `command-line-args' in noninteractive sessions, to ensure
          ;; upstream switches aren't misinterpreted.
          (command-line-args (unless noninteractive command-line-args)))
     (let ((init-dir (or (cadr (member "--init-directory" command-line-args))
                         (getenv-internal "EMACSDIR"))))
       (if (null init-dir)
           ;; If this file is loaded directly (via 'emacs -batch -l
           ;; early-init.el'), then `user-emacs-directory' might be wrong.
           (when noninteractive
             (setq user-emacs-directory
                   (file-name-directory (file-truename load-file-name))))
         ;; Prevent "invalid option" errors later.
         (push (cons "--init-directory" (lambda (_) (pop argv))) command-switch-alist)
         (setq user-emacs-directory (expand-file-name init-dir)))))

   ;; `load' and `require' use `load-suffixes' to locate a file. Startup time can
   ;; be reduced by limiting them.
   (if (let ((load-suffixes '(".elc" ".el")))
         ;; The following keeps non-config related errors visible.
         (condition-case _
             ;; Load the core of the configuration.
             (load (expand-file-name "lisp/core/zenit-core" user-emacs-directory)
                   nil (not init-file-debug) nil 'must-suffix)
           ;; If this fails, something is wrong in the directory.
           (file-missing
            (signal 'error
                    (list "Could not find lisp/core/zenit-core"
                          "make sure this file exists.")))))
       ;; Otherwise, proceed the startup.
       (require (if noninteractive 'zenit-cli 'zenit-start))))

 ;; Then continue on to the config we want to load.
 (load user-init-file 'noerror (not init-file-debug) nil 'must-suffix))

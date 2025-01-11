;; lisp/core/cli/sync.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Two ways to sync the emacs config:
;;
;; - refreshing it, thus taking care that all packages are built and autoloads
;;   are generated
;;
;; - syncing it, thus taking care that all packages are on the same version as
;;   specified in the lockfiles and then refreshing plus cleaning up the
;;   repositories

(defun zenit-cli-refresh (&optional no-envvar-p debug-p force-p)
  "Refresh Emacs config."
  (run-hooks 'zenit-refresh-pre-hook)
  (when (or debug-p (getenv "DEBUG"))
    (zenit-debug-mode +1))
  (when (or force-p (getenv "FORCE"))
    (setq force-p t))

  (print! (start "Refreshing your Emacs config..."))
  (unwind-protect
      (print-group!
       (let ((files (list zenit-config-init-file)))
         (pcase-dolist (`(,file . ,fn) zenit-cache-generators)
           (push (file-name-concat zenit-local-dir file) files))
         (mapc #'zenit-autoloads--delete-file
               files))
       (when (and (not no-envvar-p)
                  (file-exists-p zenit-env-file))
         (zenit-cli-reload-env-file 'force))
       (zenit-cli-clean-compiled-files)
       (zenit-cli-packages-install)
       (zenit-cli-packages-build force-p)
       (zenit-packages-purge t t t)
       (zenit--remove-wrong-eln-cache)
       (run-hooks 'zenit-refresh-post-hook)
       (when (zenit-module-p :config 'compile)
         (zenit-cli-compile))
       (when (zenit-autoloads-reload)
         (print! (item "Restart Emacs for changes to take effect")))
       t)))

(defun zenit-cli-sync (&optional no-envvar-p debug-p force-p)
  "Refresh Emacs config."
  (run-hooks 'zenit-sync-pre-hook)
  (when (or debug-p (getenv "DEBUG"))
    (zenit-debug-mode +1))
  (when (or force-p (getenv "FORCE"))
    (setq force-p t))

  (print! (start "Synchronizing your Emacs config..."))
  (unwind-protect
      (print-group!
       (let ((files (list zenit-config-init-file)))
         (pcase-dolist (`(,file . ,fn) zenit-cache-generators)
           (push  (file-name-concat zenit-local-dir file) files))
         (mapc #'zenit-autoloads--delete-file
               (list zenit-config-init-file)))
       (when (and (not no-envvar-p)
                  (file-exists-p zenit-env-file))
         (zenit-cli-reload-env-file 'force))
       (zenit-cli-clean-compiled-files)
       (zenit-cli-packages-install)
       (if force-p
           (zenit-cli-packages-update force-p)
         (zenit-cli-packages-build)
         (zenit-cli-packages-update))
       (zenit-packages-purge t t t)
       (zenit--remove-wrong-eln-cache)
       (run-hooks 'zenit-sync-post-hook)
       (when (zenit-module-p :config 'compile)
         (zenit-cli-compile))
       (when (zenit-autoloads-reload)
         (print! (item "Restart Emacs for changes to take effect")))
       t)))

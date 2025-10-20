;; lisp/core/cli/sync.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Two ways to sync the emacs config:
;;
;; - refreshing it, thus taking care that all packages are built and autoloads
;;   are generated
;;
;; - syncing it, thus taking care that all packages are on the same version as
;;   specified in the lockfiles and then refreshing plus cleaning up the
;;   repositories

(defun zenit-cli-refresh (&optional noenvvar-p rebuild-p nobuild-p)
  "Refresh Emacs config."
  (run-hooks 'zenit-refresh-pre-hook)
  (print! (start "Refreshing your Emacs config..."))
  (unwind-protect
      (print-group!
       (let ((files (list zenit-config-init-file)))
         (pcase-dolist (`(,file . ,fn) zenit-init-generators)
           (push (file-name-concat zenit-local-dir file) files))
         (mapc #'zenit-autoloads--delete-file
               files))
       (when (and (not noenvvar-p)
                  (file-exists-p zenit-env-file))
         (zenit-cli-reload-env-file))
       (zenit-cli-clean-compiled-files)
       (unless nobuild-p
         (zenit-cli-packages-ensure rebuild-p))
       (zenit-packages-purge t t t)
       (zenit--remove-wrong-eln-cache)
       (run-hooks 'zenit-refresh-post-hook)
       (when (zenit-module-p :config 'compile)
         (zenit-cli-compile))
       (when (zenit-autoloads-reload)
         (print! (item "Restart Emacs for changes to take effect")))
       t)))

(defun zenit-cli-sync (&optional noenvvar-p update-p noupdate-p reclone-p rebuild-p nobuild-p)
  "Refresh Emacs config."
  (run-hooks 'zenit-sync-pre-hook)
  (print! (start "Synchronizing your Emacs config..."))
  (unwind-protect
      (print-group!
       (let ((files (list zenit-config-init-file)))
         (pcase-dolist (`(,file . ,fn) zenit-init-generators)
           (push  (file-name-concat zenit-local-dir file) files))
         (mapc #'zenit-autoloads--delete-file
               (list zenit-config-init-file)))
       (when (and (not noenvvar-p)
                  (file-exists-p zenit-env-file))
         (zenit-cli-reload-env-file))
       (zenit-cli-clean-compiled-files)
       (unless nobuild-p
         (zenit-cli-packages-ensure rebuild-p))
       (unless noupdate-p
         (zenit-cli-packages-update reclone-p (not update-p)))
       (zenit-packages-purge t t t)
       (zenit--remove-wrong-eln-cache)
       (run-hooks 'zenit-sync-post-hook)
       (when (zenit-module-p :config 'compile)
         (zenit-cli-compile))
       (when (zenit-autoloads-reload)
         (print! (item "Restart Emacs for changes to take effect")))
       t)))

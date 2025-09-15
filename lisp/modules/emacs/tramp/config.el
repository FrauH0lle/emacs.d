;; emacs/tramp/config.el -*- lexical-binding: t; -*-

;; Prefix tramp autosaves to prevent conflicts with local ones
(cl-pushnew (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            auto-save-file-name-transforms
            :test #'equal)


(after! tramp
  (setq remote-file-name-inhibit-cache 60
        remote-file-name-inhibit-locks t
        tramp-copy-size-limit (* 1024 1024) ; 1mb
        tramp-use-scp-direct-remote-copying t
        tramp-completion-reread-directory-timeout 60
        tramp-backup-directory-alist backup-directory-alist
        tramp-auto-save-directory  (concat zenit-cache-dir "tramp-autosave/"))

  ;; The ssh method is faster than the default ssh on Windows
  (eval-unless! (featurep :system 'windows)
    (setq tramp-default-method "ssh")))


;; When creating a new process in Emacs, you have two options: synchronous or
;; asynchronous. Async processes have historically been really slow over TRAMP,
;; because it has to create a new connection for every async process. However
;; recent version of TRAMP have added a feature called direct async process that
;; makes this significantly faster. This feature alone will take many packages
;; (like magit or git-gutter) from completely unusable to bearable over TRAMP.
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 `(:application tramp :protocol "scp")
 'remote-direct-async-process)


;; See magit/magit#5220
(after! magit
  (setq magit-tramp-pipe-stty-setings 'pty))


;; Newer versions of TRAMP will use SSH connection sharing for much faster
;; connections. These don’t require you to reenter your password each time you
;; connect. The compile command disables this feature, so we want to turn it
;; back on.
(after! (tramp compile)
  (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options))

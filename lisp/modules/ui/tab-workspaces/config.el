;; ui/tab-workspaces/config.el -*- lexical-binding: t; -*-

(defvar +workspaces-main "main"
  "The name of the primary and initial workspace, which cannot be
deleted.")

(defvar +workspaces-switch-project-function #'zenit-project-find-file
  "The function to run after `projectile-switch-project' or
`counsel-projectile-switch-project'. This function must take one
argument: the new project directory.")

;;
;;; Packages

(use-package! tabspaces
  :unless noninteractive
  :hook (zenit-init-ui . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :config
  (setq tabspaces-default-tab +workspaces-main
        ;; REVIEW Is this as expected?
        tabspaces-remove-to-default t
        tabspaces-include-buffers '("*scratch*")
        tabspaces-initialize-project-with-todo nil
        ;; sessions
        tabspaces-session t
        tabspaces-session-auto-restore nil
        ;; Use `projectile'
        tabspaces-project-switch-commands 'zenit-project-find-file)

  ;; HACK Projectile integration
  (defadvice! +workspaces-use-projectile (fn &rest args)
    "TODO"
    :around #'tabspaces-open-or-create-project-and-workspace
    (letf! ((#'project-prompt-project-dir #'s))))
)

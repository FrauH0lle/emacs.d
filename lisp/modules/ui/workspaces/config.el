;; ui/workspaces/config.el -*- lexical-binding: t; -*-

(defvar +workspaces-main "main"
  "The name of the primary and initial workspace, which cannot be
deleted.")

(defvar +workspaces-switch-project-function #'zenit-project-find-file
  "The function to run after `projectile-switch-project' or
`counsel-projectile-switch-project'. This function must take one
argument: the new project directory.")

(defvar +workspaces-on-switch-project-behavior 'non-empty
  "Controls the behavior of workspaces when switching to a new
project.

Can be one of the following:

t          Always create a new workspace for the project.
'non-empty Only create a new workspace if the current one already
           has buffers associated with it.
nil        Never create a new workspace on project switch.")

(defvar +workspaces-save-directory (file-name-concat zenit-data-dir "workspaces/")
  "The directory to store workspaces in.")

(defvar +workspaces-data-file "_workspaces"
  "The file to store workspaces in.")

(defvar +workspaces-autosave-file "autosave"
  "The file to store autosaved workspaces.")

(defvar +workspaces-autosave t
  "Controls if the session is autosaved:

nil Do not auto save.
t   Save on Emacs shutdown.")

(defvar +workspaces-autosave-num-of-backups 3
  "How many autosave file backups to keep.")

;; REVIEW 2024-06-23: Could this be removed?
(defvar +workspaces-bookmark-alist nil)


;;
;;; Packages

(use-package! bufferlo
  :unless noninteractive
  :hook (zenit-init-ui . bufferlo-mode)
  :init
  (defvar bufferlo-mode-map (make-sparse-keymap))
  :config/el-patch
  ;; PATCH 2024-06-24: Use own functions to determine project root.
  (defun bufferlo-isolate-project (&optional file-buffers-only)
    "Isolate a project in the frame or tab.
Remove all buffers that do not belong to the current project from
the local buffer list.  When FILE-BUFFERS-ONLY is non-nil or the
prefix argument is given, remove only buffers that visit a file.
Buffers matching `bufferlo-include-buffer-filters' are not removed."
    (interactive "P")
    (bufferlo--warn)
    (let ((curr-project (el-patch-swap (project-current) (zenit-project-root)))
          (include (bufferlo--merge-regexp-list
                    (append '("a^") bufferlo-include-buffer-filters))))
      (if curr-project
          (dolist (buffer (bufferlo-buffer-list))
            (when (and (not (string-match-p include (buffer-name buffer)))
                       (not (equal curr-project
                                   (with-current-buffer buffer (el-patch-swap (project-current) (zenit-project-root)))))
                       (or (not file-buffers-only) (buffer-file-name buffer)))
              (bufferlo-remove buffer)))
        (message "Current buffer is not part of a project"))))
  :config
  ;; Buffer to always include
  (setq! bufferlo-include-buffer-filters '("^\\*\\Messages" "^\\*Warnings"))

  ;; Toggle `tab-bar-mode'
  (defhook! +workspaces-toggle-tab-bar-mode-h ()
    "Toggle `tab-bar-mode' together with `bufferlo-moder'."
    'bufferlo-mode-hook
    (cond
     (bufferlo-mode
      ;; We include all present buffers in the first workspace
      (let ((bufferlo-exclude-buffer-filters nil)
            (bufferlo-include-buffer-filters '(".*")))
        (bufferlo--include-exclude-buffers nil)
        (tab-bar-mode +1)))
     (t (tab-bar-mode -1))))

  ;; Give the first tab the name specified in `+workspaces-main'
  (after! tab-bar
    (tab-bar-rename-tab +workspaces-main))

  ;; `consult' integration
  (eval-when! (modulep! :completion vertico)
    (after! consult
      ;; Hide default buffer source
      (consult-customize
       consult--source-buffer
       :hidden t
       :default nil)

      ;; Add workspace buffers source
      (defvar +consult--source-workspace
        `(:name     "Workspace Buffers"
          :narrow   ?w
          :history  buffer-name-history
          :category buffer
          :state    ,#'consult--buffer-state
          :default  t
          :items
          ,(lambda () (consult--buffer-query
                       :predicate (lambda (x)
                                    (and (bufferlo-local-buffer-p x)
                                         (if (modulep! :ui popup)
                                             (not (popper-popup-p x))
                                           t)))
                       :sort 'visibility
                       :as #'buffer-name))))

      (spliceq! consult-buffer-sources '+consult--source-workspace 'consult--source-buffer)))

  ;; Filter popups by workspace
  (eval-when! (modulep! :ui popup)
    (defun +popper-group-by-workspace ()
      (let ((current-tab (alist-get 'current-tab (funcall tab-bar-tabs-function))))
        (when (bufferlo-local-buffer-p (current-buffer))
          (alist-get 'name current-tab))))

    (setq popper-group-function #'+popper-group-by-workspace))

  ;; Delete the current workspace if closing the last open window
  (define-key! bufferlo-mode-map
    [remap delete-window] #'+workspace/close-window-or-workspace
    [remap evil-window-delete] #'+workspace/close-window-or-workspace)

  ;; Projectile
  (setq projectile-switch-project-action (lambda () (+workspaces-set-project-action-fn) (+workspaces-switch-to-project-h)))

  ;; Autosave
  (add-hook 'kill-emacs-hook #'+workspaces-kill-emacs-h))

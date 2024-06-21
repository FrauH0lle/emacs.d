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

t          Always create a new workspace for the project
'non-empty Only create a new workspace if the current one already
           has buffers associated with it.
nil        Never create a new workspace on project switch.")

(defvar +workspaces-save-directory (file-name-concat zenit-data-dir "workspaces/")
  "The directory to store workspaces in.")

(defvar +workspaces-data-file "_workspaces"
  "The file to store workspaces in.")

(defvar +workspaces-autosave-file "autosave"
  "The file to store autosaved workspaces.")

(defvar +workspaces-bookmark-alist nil)


;;
;;; Packages

(use-package! tab-bar
  :defer t
  :config/el-patch
  (defun tab-bar-tab-name-format-default (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat (if tab-bar-tab-hints (format (el-patch-swap "%d " "  #%d: ") i) "")
               (alist-get 'name tab)
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   ""))
       'face (funcall tab-bar-tab-face-function tab))))
  :config
  (setq! tab-bar-close-button-show nil
         tab-bar-new-button-show nil
         tab-bar-history-limit 25
         tab-bar-show 1
         tab-bar-tab-hints t)

  (protect-macros!
    (custom-set-faces!
      ;; The tab bar's appearance
      `(tab-bar
        :background ,(face-attribute 'mode-line-inactive :background)
        :foreground ,(face-attribute 'mode-line-inactive :foreground)
        :box
        (:line-width 6 :color ,(face-attribute 'mode-line-inactive :background) :style nil))
      ;; Inactive tabs
      `(tab-bar-tab-inactive
        :background ,(face-attribute 'mode-line-inactive :background)
        :foreground ,(face-attribute 'mode-line-inactive :foreground)
        :box
        (:line-width 3 :color ,(face-attribute 'mode-line-inactive :background) :style nil))
      ;; Active tab
      `(tab-bar-tab
        :background ,(face-attribute 'default :background)
        :foreground ,(face-attribute 'font-lock-keyword-face :foreground nil t)
        :box
        (:line-width 3 :color ,(face-attribute 'default :background) :style nil))

      ;; The tab bar's appearance
      `(tab-bar-tab-ungrouped
        :background ,(face-attribute 'mode-line-inactive :background)
        :foreground ,(face-attribute 'mode-line-inactive :foreground)
        :box
        (:line-width 3 :color ,(face-attribute 'mode-line-inactive :background) :style nil))

      ;; Inactive tabs
      `(tab-bar-tab-group-inactive
        :background ,(face-attribute 'mode-line-inactive :background)
        :foreground ,(face-attribute 'mode-line-inactive :foreground)
        :box
        (:line-width 3 :color ,(face-attribute 'mode-line-inactive :background) :style nil))

      ;; Active tab
      `(tab-bar-tab-group-current
        :background ,(face-attribute 'mode-line-inactive :background) :foreground ,(face-attribute 'default :foreground)
        :box (:line-width 3
              :color ,(face-attribute 'mode-line-inactive :background)
              :style nil)))))


(use-package! bufferlo
  :unless noninteractive
  :hook (zenit-init-ui . bufferlo-mode)
  :config/el-patch
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

  ;;Projectile
  (setq projectile-switch-project-action (lambda () (+workspaces-set-project-action-fn) (+workspaces-switch-to-project-h))))

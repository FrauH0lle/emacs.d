;; ui/workspaces/autoload/tabs.el -*- lexical-binding: t; -*-

(defvar +workspace-tabs--saved-settings nil
  "Settings saved from before `+workspace-tabs-mode' was activated.
Used to restore them when the mode is disabled.")

(defun +workspace-tab-bar-format ()
  "Produce workspace group labels and real tabs for the tab bar.
Non-current workspaces appear as collapsed clickable group labels. The
current workspace shows an active group label followed by its real tabs."
  (let* ((all-tabs (funcall tab-bar-tabs-function))
         (current-ws (+workspace-current-name))
         (ws-names (or (frame-parameter nil 'workspaces)
                       (list current-ws)))
         (group-offset 1000)
         (group-i 0)
         (tab-i 0)
         result)
    (dolist (ws-name ws-names)
      (cl-incf group-i)
      (let ((current-p (string= ws-name current-ws)))
        ;; Group label
        (setq result
              (nconc result
                     (tab-bar--format-tab-group
                      `(,(if current-p 'current-tab 'tab)
                        (name . ,ws-name)
                        (group . ,ws-name)
                        ,@(unless current-p
                            `((binding
                               . ,(let ((name ws-name))
                                    (lambda ()
                                      (interactive)
                                      (+workspace-switch name)))))))
                      (+ group-offset group-i)
                      current-p)))
        ;; Real tabs for current workspace
        (when current-p
          (dolist (tab all-tabs)
            (cl-incf tab-i)
            (setq result
                  (nconc result
                         (tab-bar--format-tab tab tab-i)))))))
    result))

(defun +workspace--current-group-name ()
  "Return the current workspace name, for use as `tab-bar-new-tab-group'."
  (+workspace-current-name))

;;;###autoload
(define-minor-mode +workspace-tabs-mode
  "Use tabs to show and manage perspectives as tab groups.
Each workspace appears as a tab group in the tab bar. The current
workspace's real tabs are shown; other workspaces appear as collapsed
clickable group labels."
  :group 'persp-mode
  :global t
  (if +workspace-tabs-mode
      ;; Activate
      (progn
        (unless (bound-and-true-p persp-mode)
          (user-error "`+workspace-tabs-mode' requires `persp-mode' to be active"))
        ;; Save settings
        (setq +workspace-tabs--saved-settings
              `((tab-bar-format . ,tab-bar-format)
                (tab-bar-new-tab-group . ,tab-bar-new-tab-group)
                (tab-bar-tabs-function . ,tab-bar-tabs-function)
                (tab-bar-close-button-show . ,tab-bar-close-button-show)))
        ;; Replace tab-bar-format-tabs (or tab-bar-format-tabs-groups)
        ;; with our format function
        (setq! tab-bar-format
               (seq-uniq
                (mapcar (lambda (f)
                          (if (memq f '(tab-bar-format-tabs
                                        tab-bar-format-tabs-groups))
                              '+workspace-tab-bar-format
                            f))
                        tab-bar-format)))
        ;; New tabs auto-join the current workspace's group
        (setq tab-bar-new-tab-group #'+workspace--current-group-name)
        ;; Use real tabs (restore default if previously overridden)
        (setq tab-bar-tabs-function #'tab-bar-tabs)
        (tab-bar-mode +1))
    ;; Deactivate
    (cl-loop for (symbol . value) in +workspace-tabs--saved-settings
             do (set symbol value))
    (setq +workspace-tabs--saved-settings nil)
    (tab-bar-mode -1))
  (force-mode-line-update 'all))

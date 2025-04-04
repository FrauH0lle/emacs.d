;; ui/workspaces/autoload/persp-tabs.el -*- lexical-binding: t; -*-

;; Based on https://git.sr.ht/~woozong/perspective-tabs

(defvar perspective-tabs-saved-settings
  `((tab-bar-separator . ,tab-bar-separator)
    (tab-bar-close-button-show . ,tab-bar-close-button-show)
    (tab-bar-tabs-function . ,tab-bar-tabs-function))
  "Settings saved from before `perspective-tabs-mode' was activated.
Used to restore them when the mode is disabled.")

(defun perspective-tabs-function (&optional _frame)
  "Return a list of perspective tabs in FRAME.
FRAME defaults to the current frame."
  (let ((perspectives *persp-hash*)
        (persp-current-name (+workspace-current-name))
        (persp-names (if-let* ((assoc-ws (frame-parameter nil 'workspace)))
                         (ensure-list assoc-ws)
                      (cl-remove-if
                       (lambda (x)
                         (member x (mapcar #'car +workspaces-frames-alist)))
                       (+workspace-list-names)))))
    (mapcar (lambda (persp-name)
              (list
               (if (string= persp-name persp-current-name) 'current-tab 'tab)
               (cons 'name persp-name)
               (cons 'perspective (gethash persp-name perspectives))))
            persp-names)))

(defun perspective-tabs-select-tab (&optional arg)
  "Set the frame's perspective to the selected tab's perspective.
ARG is the position of the perspective in the tab bar."
  ;; modeled on/copied from `bufler-workspace-tabs--tab-bar-select-tab'.
  (interactive "P")
  (unless (integerp arg)
    (let ((key (event-basic-type last-command-event)))
      (setq arg (if (and (characterp key) (>= key ?1) (<= key ?9))
                    (- key ?0)
                  1))))
  (let* ((tabs (funcall tab-bar-tabs-function))
         (from-index (tab-bar--current-tab-index tabs))
         (to-index (1- (max 1 (min arg (length tabs))))))
    (unless (eq from-index to-index)
      (let* ((_from-tab (tab-bar--tab))
             (to-tab (nth to-index tabs))
             (perspective (alist-get 'perspective to-tab)))
        (persp-activate perspective)
        (force-mode-line-update 'all)))))

(defun perspective-tabs-close-tab (&optional arg)
  "Close a perspective. Called from tab-bar code and icons.
ARG is the position of the perspective in the tab bar."
  ;; riffing on :) from `bufler-workspace-tabs--tab-bar-select-tab'.
  (interactive "P")
  (unless (integerp arg)
    (let ((key (event-basic-type last-command-event)))
      (setq arg (if (and (characterp key) (>= key ?1) (<= key ?9))
                    (- key ?0)
                  1))))
  (let* ((tabs (funcall tab-bar-tabs-function))
         (kill-index (1- (max 1 (min arg (length tabs)))))
         (kill-tab (nth kill-index tabs))
         (perspective-name (alist-get 'name kill-tab)))
    (if (= 1 (length tabs))
        (delete-frame)
      (persp-kill perspective-name))
    (force-mode-line-update 'all)))

(defun perspective-tabs-new (&optional _arg)
  "Create a new perspective.
ARG is just here to be compatible with the function `tab-bar-new-tab-to'."
  (interactive "i")
  (+workspace/new))

;;;###autoload
(define-minor-mode perspective-tabs-mode
  "Use tabs to show and manage perspectives."
  :group 'perspective-tabs
  :global t
  (if perspective-tabs-mode
      ;; activate
      (progn
        (unless (bound-and-true-p persp-mode)
          (user-error "`perspective-tabs-mode' requires perspective (`persp-mode') to be active"))
        ;; Save settings
        ;; (message "settings: %s" perspective-tabs-saved-settings)
        (cl-loop for (symbol . _value) in perspective-tabs-saved-settings
                 do (setf (alist-get symbol perspective-tabs-saved-settings)
                          (symbol-value symbol)))
        (advice-add 'tab-bar-select-tab :override #'perspective-tabs-select-tab)
        (advice-add 'tab-bar-switch-to-tab :override #'persp-switch)
        (advice-add 'tab-bar-new-tab-to :override #'perspective-tabs-new)
        (advice-add 'tab-bar-close-tab :override #'perspective-tabs-close-tab)
        (advice-add 'tab-bar-rename-tab :override #'persp-rename)
        (setf tab-bar-tabs-function #'perspective-tabs-function)
        (tab-bar-mode +1))
    ;;deactivate
    (advice-remove 'tab-bar-select-tab #'perspective-tabs-select-tab)
    (advice-remove 'tab-bar-switch-to-tab #'persp-switch)
    (advice-remove 'tab-bar-new-tab-to #'perspective-tabs-new)
    (advice-remove 'tab-bar-close-tab #'perspective-tabs-close-tab)
    (advice-remove 'tab-bar-rename-tab #'persp-rename)
    ;; Restore settings.
    (cl-loop for (symbol . value) in perspective-tabs-saved-settings
             do (set symbol value)
             do (setf (map-elt perspective-tabs-saved-settings symbol) nil))
    (tab-bar-mode -1))
  (force-mode-line-update 'all))

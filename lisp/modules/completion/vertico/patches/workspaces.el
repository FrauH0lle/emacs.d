;; completion/vertico/patches/workspaces.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'el-patch)
  (require 'consult))


(defun +vertico--workspace-buffer-state ()
  (let ((preview
         ;; Only preview in current window and other window.
         ;; Preview in frames and tabs is not possible since these don't get cleaned up.
         (if (memq consult--buffer-display
                   '(switch-to-buffer switch-to-buffer-other-window))
             (let ((orig-buf (current-buffer))
                   other-win
                   cleanup-buffers)
               (lambda (action cand)
                 (when (eq action 'preview)
                   (when (and (eq consult--buffer-display #'switch-to-buffer-other-window)
                              (not other-win))
                     (switch-to-buffer-other-window orig-buf)
                     (setq other-win (selected-window)))
                   (let ((win (or other-win (selected-window))))
                     (when (window-live-p win)
                       (with-selected-window win
                         (cond
                          ((and cand (get-buffer cand))
                           (unless (+workspace-contains-buffer-p cand)
                             (cl-pushnew cand cleanup-buffers))
                           (switch-to-buffer cand 'norecord))
                          ((buffer-live-p orig-buf)
                           (switch-to-buffer orig-buf 'norecord)
                           (mapc #'persp-remove-buffer cleanup-buffers)))))))))
           #'ignore)))
    (lambda (action cand)
      (funcall preview action cand))))

(defun +vertico--workspace-generate-sources ()
  "Generate list of consult buffer sources for all workspaces"
  (let* ((active-workspace (+workspace-current-name))
         (key-range (append (cl-loop for i from ?1 to ?9 collect i)
                            (cl-loop for i from ?a to ?z collect i)
                            (cl-loop for i from ?A to ?Z collect i)))
         (i 0))
    (mapcar (lambda (name)
              (cl-incf i)
              `(:name     ,(format "Workspace: %s" name)
                :hidden   ,(not (string= active-workspace name))
                :narrow   ,(nth (1- i) key-range)
                :category buffer
                :state    +vertico--workspace-buffer-state
                :items    ,(lambda ()
                             (consult--buffer-query
                              :sort 'visibility
                              :as #'buffer-name
                              :predicate
                              (lambda (buf)
                                (when-let* ((workspace (+workspace-get name t)))
                                  (+workspace-contains-buffer-p buf workspace)))))))
            (+workspace-list-names))))

;; PATCH 2024-09-04: Filter `consult-buffer' by workspace and allow selection of
;;   workspaces via its number.
(el-patch-defun consult-buffer (&optional sources (el-patch-add force-same-workspace))
  (el-patch-concat
    "Enhanced `switch-to-buffer' command with support for virtual buffers.\n\n"

    (el-patch-add
      "Type the workspace's number (starting from 1) followed by a space
to display its buffer list. Selecting a buffer in another
workspace will switch to that workspace instead. If
FORCE-SAME-WORKSPACE (the prefix arg) is non-nil, that buffer
will be opened in the current workspace instead.\n\n")

    "The command supports recent files, bookmarks, views and project files as
virtual buffers.  Buffers are previewed.  Narrowing to buffers (b), files (f),
bookmarks (m) and project files (p) is supported via the corresponding
keys.  In order to determine the project-specific files and buffers, the
`consult-project-function' is used.  The virtual buffer SOURCES
default to `consult-buffer-sources'.  See `consult--multi' for the
configuration of the virtual buffer sources.")
  (el-patch-swap (interactive) (interactive (list nil current-prefix-arg)))
  (let ((selected (consult--multi (or sources (el-patch-swap
                                                consult-buffer-sources
                                                (zenit-splice-into consult-buffer-sources (+vertico--workspace-generate-sources) 'consult-source-buffer)))
                                  :require-match
                                  (confirm-nonexistent-file-or-buffer)
                                  :prompt (el-patch-swap
                                            "Switch to: "
                                            (format "Switch to buffer (%s): "
                                                    (+workspace-current-name)))
                                  :history 'consult--buffer-history
                                  :sort nil)))
    (el-patch-remove
      ;; For non-matching candidates, fall back to buffer creation.
      (unless (plist-get (cdr selected) :match)
        (consult--buffer-action (car selected))))
    (el-patch-add
      (let ((origin-workspace (string-trim-left (plist-get (cdr selected) :name) "^Workspace: ")))
        ;; Switch to the workspace the buffer belongs to, maybe
        (if (or (equal origin-workspace (+workspace-current-name))
                force-same-workspace)
            (consult--buffer-action (car selected))
          (+workspace-switch origin-workspace)
          (+workspace-message (format "Switched to %S workspace" origin-workspace) 'success)
          (if-let* ((window (get-buffer-window (car selected))))
              (select-window window)
            (consult--buffer-action (car selected))))))))

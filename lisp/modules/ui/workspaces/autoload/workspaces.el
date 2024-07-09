;; ui/workspaces/autoload/workspaces.el -*- lexical-binding: t; -*-

(defvar +workspace--last nil)

;;;###autoload
(defface +workspace-tab-selected-face '((t (:inherit highlight)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'tab-bar-mode)

;;;###autoload
(defface +workspace-tab-face '((t (:inherit default)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'tab-bar-mode)


;;
;;; Library

;; NOTE 2024-06-14: In principle, this is unnecessary.
(defun +workspace--protected-p (name)
  (equal name "none"))

(defun +workspace--generate-id ()
  (or (cl-loop for name in (+workspace-list-names)
               when (string-match-p "^workspace-[0-9]+$" name)
               maximize (string-to-number (substring name 10)) into max
               finally return (if max (1+ max)))
      1))

(defun +workspaces--get-tabnum (name)
  "Get index of tab NAME."
  (let ((tabnum (cl-position-if
                 (lambda (x) (equal (alist-get 'name x) name))
                 (funcall tab-bar-tabs-function))))
    (unless tabnum
      (user-error "No workspace named '%s' exists" name))
    tabnum))


;;
;;; Predicates

;;;###autoload
(defun +workspace-exists-p (name)
  "Returns t if NAME is the name of an existing workspace."
  (member name (+workspace-list-names)))

;;;###autoload
(defalias #'+workspace-contains-buffer-p #'bufferlo-local-buffer-p
  "Return non-nil if BUFFER is in WORKSPACE (defaults to current workspace).")


;;
;;; Getters

;;;###autoload
(defun +workspace-get (name &optional noerror)
  "Return a workspace named NAME. Unless NOERROR is non-nil, this
throws an error if NAME doesn't exist."
  (cl-check-type name string)
  (if-let ((ws (car-safe (+workspace-exists-p name))))
      ws
    (cond ((not noerror)
           (error "No workspace called '%s' was found" name)))))

;;;###autoload
(defun +workspace-current-name ()
  "Get the name of the current workspace."
  (let ((current-tab (alist-get 'current-tab (funcall tab-bar-tabs-function))))
    (alist-get 'name current-tab)))

;;;###autoload
(defun +workspace-list-names ()
  "Return the list of names of open workspaces."
  (cl-loop for tab in (funcall tab-bar-tabs-function)
           if (alist-get 'name tab)
           collect it))

;;;###autoload
(defun +workspace-buffer-list (&optional tab)
  "Return a list of buffers in TAB.

TAB is a string (name of a workspace). If nil or omitted, it
defaults to the current workspace."
  (let* ((tab (or tab (+workspace-current-name)))
         (tabnum (+workspaces--get-tabnum tab)))
    (unless tabnum
      (user-error "Not in a valid workspace (%s)" tab))
    (bufferlo--get-buffers nil tabnum)))

;;;###autoload
(defun +workspace-orphaned-buffer-list ()
  "Return a list of buffers that aren't associated with any
workspace."
  (bufferlo--get-orphan-buffers))

;;
;;; Actions

;;;###autoload
(defun +workspace-load (name)
  "Loads a single workspace (named NAME) into the current session.
Can only retrieve perspectives that were explicitly saved with
`+workspace-save'.

Returns t if successful, nil otherwise."
  (when (+workspace-exists-p name)
    (user-error "A workspace named '%s' already exists." name))
  (let ((bookmark-alist +workspaces-bookmark-alist)
        (bookmark-default-file (expand-file-name +workspaces-data-file +workspaces-save-directory)))
    (unless bookmark-alist
      (bookmark-load (expand-file-name +workspaces-data-file +workspaces-save-directory) t))
    (+workspace-switch name t)
    (bufferlo-bookmark-tab-load name)
    (setq +workspaces-bookmark-alist bookmark-alist))
  (+workspace-exists-p name))

;;;###autoload
(defun +workspace-save (name)
  "Saves a single workspace (NAME) from the current session. Can be
loaded again with `+workspace-load'. NAME can be the string name
of a workspace or its perspective hash table.

Returns t on success, nil otherwise."
  (unless (+workspace-exists-p name)
    (error "'%s' is an invalid workspace" name))
  (let ((bookmark-alist +workspaces-bookmark-alist)
        (bookmark-default-file (expand-file-name +workspaces-data-file +workspaces-save-directory)))
    (letf! ((#'bookmark-maybe-load-default-file #'ignore))
      (bufferlo-bookmark-tab-save name))
    (bookmark-write-file (expand-file-name +workspaces-data-file +workspaces-save-directory))
    (setq +workspaces-bookmark-alist bookmark-alist))
  t)

;;;###autoload
(defun +workspace-save-session (file)
  "Saves all workspaces from the current session to FILE.

Returns t on success, nil otherwise."
  (let ((bookmark-alist nil)
        (bookmark-default-file file)
        (current-ws (+workspace-current-name))
        (frames (visible-frame-list))
        (i 1))
    (dolist (f frames)
      (letf! ((#'bookmark-maybe-load-default-file #'ignore))
        (with-selected-frame f
          (bufferlo-bookmark-frame-save (format "frame-%s" i)))
        (cl-incf i)))
    (bookmark-write-file file)
    (setq +workspaces-bookmark-alist bookmark-alist))
  t)

;;;###autoload
(defun +workspaces-rotate-autosaves (fname)
  "Rotate file FNAME. The number of backups is controlled by
`+workspaces-autosave-num-of-backups'."
  (when (> +workspaces-autosave-num-of-backups 0)
    (cl-do ((cur +workspaces-autosave-num-of-backups (1- cur))
            (prev (1- +workspaces-autosave-num-of-backups) (1- prev)))
        ((> 1 cur) nil)
      (let ((cf (concat fname (number-to-string cur)))
            (pf (concat fname (if (> prev 0)
                                  (number-to-string prev)
                                ""))))
        (when (file-exists-p pf)
          (when (file-exists-p cf)
            (delete-file cf))
          (rename-file pf cf t))))
    (when (file-exists-p fname)
      (rename-file fname (concat fname (number-to-string 1)) t)))
  t)

;;;###autoload
(defun +workspace-new (name &optional clone-p)
  "Create a new workspace named NAME. If one already exists, return
nil.
Otherwise return t on success, nil otherwise."
  (when (+workspace--protected-p name)
    (error "Can't create a new '%s' workspace" name))
  (when (+workspace-exists-p name)
    (error "A workspace named '%s' already exists" name))
  (let ((+popup--inhibit-transient t))
    (if clone-p
        (let ((tab-bar-new-tab-choice 'clone))
          (tab-bar-new-tab (1+ (length (funcall tab-bar-tabs-function)))
                           (1+ (+workspaces--get-tabnum (+workspace-current-name)))))
      (tab-bar-new-tab (1+ (length (funcall tab-bar-tabs-function))))
      (tab-bar-rename-tab name)
      (switch-to-buffer (zenit-fallback-buffer))
      (bufferlo-clear))
    t))

;;;###autoload
(defun +workspace-rename (name new-name)
  "Rename the current workspace named NAME to NEW-NAME. Returns old
name on success, nil otherwise."
  (when (+workspace--protected-p name)
    (error "Can't rename '%s' workspace" name))
  (tab-bar-rename-tab new-name (1+ (+workspaces--get-tabnum name)))
  (when (+workspaces--get-tabnum new-name)
    name))

;;;###autoload
(defun +workspace-kill (workspace &optional inhibit-kill-p)
  "Kill the workspace denoted by WORKSPACE, which is the name
 of a tab. If INHIBIT-KILL-P is non-nil, don't kill this
workspace's buffers."
  (when (+workspace--protected-p workspace)
    (error "Can't delete '%s' workspace" workspace))
  ;; Error checking
  (+workspace-get workspace)
  (delete-other-windows)
  (unless inhibit-kill-p
    (bufferlo-kill-buffers nil nil (+workspaces--get-tabnum workspace)))
  (tab-bar-close-tab (1+ (+workspaces--get-tabnum workspace)))
  ;; A little workaround if we only have one workspace left. Then we should
  ;; simply return t.
  (if (and (length= (+workspace-list-names) 1)
           (string= (car (+workspace-list-names)) +workspaces-main))
      t
    (not (+workspace-exists-p workspace))))

;;;###autoload
(defun +workspace-switch (name &optional auto-create-p)
  "Switch to another workspace named NAME (a string).

If AUTO-CREATE-P is non-nil, create the workspace if it doesn't exist, otherwise
throws an error."
  (unless (+workspace-exists-p name)
    (if auto-create-p
        (+workspace-new name)
      (error "%s is not an available workspace" name)))
  (let ((old-name (+workspace-current-name)))
    (unless (equal old-name name)
      (setq +workspace--last
            (or old-name
                +workspaces-main))
      (tab-bar-switch-to-tab name))
    (equal (+workspace-current-name) name)))


;;
;;; Commands

;;;###autoload
(defalias '+workspace/restore-last-session #'zenit/quickload-session)

;;;###autoload
(defun +workspace/load (name)
  "Load a workspace and switch to it. If called with C-u, try to
reload the current workspace (by name) from session files."
  (interactive
   (list
    (if current-prefix-arg
        (+workspace-current-name)
      (let ((bookmark-alist +workspaces-bookmark-alist)
            (bookmark-default-file (expand-file-name +workspaces-data-file +workspaces-save-directory)))
        (unless bookmark-alist
          (bookmark-load (expand-file-name +workspaces-data-file +workspaces-save-directory) t))
        (completing-read
         "Workspace to load: "
         (bufferlo--bookmark-get-names #'bufferlo--bookmark-tab-handler))))))
  (if (not (+workspace-load name))
      (+workspace-error (format "Couldn't load workspace %s" name))
    (+workspace/switch-to name)
    (+workspace/display)))

;;;###autoload
(defun +workspace/save (name)
  "Save the current workspace. If called with C-u, autosave the current
workspace."
  (interactive
   (list
    (if current-prefix-arg
        (+workspace-current-name)
      (completing-read "Workspace to save: " (+workspace-list-names)))))
  (if (+workspace-save name)
      (+workspace-message (format "'%s' workspace saved" name) 'success)
    (+workspace-error (format "Couldn't save workspace %s" name))))

;;;###autoload
(defun +workspace/rename (new-name)
  "Rename the current workspace."
  (interactive (list (completing-read "New workspace name: " (list (+workspace-current-name)))))
  (condition-case-unless-debug ex
      (let* ((current-name (+workspace-current-name))
             (old-name (+workspace-rename current-name new-name)))
        (unless old-name
          (error "Failed to rename %s" current-name))
        (+workspace-message (format "Renamed '%s'->'%s'" old-name new-name) 'success))
    ('error (+workspace-error ex t))))

;;;###autoload
(defun +workspace/kill (name)
  "Kill this workspace. If called with C-u, prompts you for the
name of the workspace to delete."
  (interactive
   (let ((current-name (+workspace-current-name)))
     (list
      (if current-prefix-arg
          (completing-read (format "Kill workspace (default: %s): " current-name)
                           (+workspace-list-names)
                           nil nil nil nil current-name)
        current-name))))
  (condition-case-unless-debug ex
      (let ((workspaces (+workspace-list-names)))
        (if (not (member name workspaces))
            (+workspace-message (format "'%s' workspace doesn't exist" name) 'warn)
          (cond ((not (equal (+workspace-current-name) name))
                 (+workspace-kill name))
                ((cdr workspaces)
                 (+workspace-kill name)
                 (+workspace-switch
                  (if (+workspace-exists-p +workspace--last)
                      +workspace--last
                    (car (+workspace-list-names))))
                 (unless (zenit-buffer-frame-predicate (window-buffer))
                   (switch-to-buffer (zenit-fallback-buffer))))
                (t
                 (+workspace-kill name)))
          (+workspace-message (format "Deleted '%s' workspace" name) 'success)))
    ('error (+workspace-error ex t))))

;;;###autoload
(defun +workspace/kill-session (&optional interactive)
  "Delete the current session, all workspaces, windows and their buffers."
  (interactive (list t))
  (let ((frames (visible-frame-list))
        (num-frames (length (visible-frame-list)))
        (num-windows 0)
        (num-workspaces 0)
        (num-buffers 0))
    (dolist (f frames)
      (with-selected-frame f
        (setq num-windows (+ num-windows (length (window-list)))
              num-workspaces (+ num-workspaces (length (+workspace-list-names))))
        (unless (cl-every (zenit-rpartial #'+workspace-kill t) (+workspace-list-names))
          (+workspace-error "Could not clear session"))))
    (+workspace-switch +workspaces-main t)
    (setq num-buffers (zenit/kill-all-buffers (buffer-list)))
    (when interactive
      (message "Killed %d workspace(s), %d window(s) & %d buffer(s) in %d frame(s)"
               num-workspaces num-windows num-buffers num-frames))))

;;;###autoload
(defun +workspace/kill-session-and-quit ()
  "Kill emacs without saving anything."
  (interactive)
  (kill-emacs))

;;;###autoload
(defun +workspace/new (&optional name clone-p)
  "Create a new workspace named NAME.
If CLONE-P is non-nil, clone the current workspace, otherwise the
new workspace is blank."
  (interactive (list nil current-prefix-arg))
  (unless name
    (setq name (format "workspace-%s" (+workspace--generate-id))))
  (condition-case e
      (cond ((+workspace-exists-p name)
             (error "%s already exists" name))
            (clone-p (+workspace-new name t))
            (t
             (+workspace-switch name t)
             (+workspace/display)))
    ((debug error) (+workspace-error (cadr e) t))))

;;;###autoload
(defun +workspace/new-named (name)
  "Create a new workspace with a given NAME."
  (interactive "sWorkspace Name: ")
  (+workspace/new name))

;;;###autoload
(defun +workspace/switch-to (index)
  "Switch to a workspace at a given INDEX.
A negative number will start from the end of the workspace list."
  (interactive
   (list (or current-prefix-arg
             (completing-read "Switch to workspace: " (+workspace-list-names)))))
  (when (and (stringp index)
             (string-match-p "^[0-9]+$" index))
    (setq index (string-to-number index)))
  (condition-case-unless-debug ex
      (let ((names (+workspace-list-names))
            (old-name (+workspace-current-name)))
        (cond ((numberp index)
               (let ((dest (nth index names)))
                 (unless dest
                   (error "No workspace at #%s" (1+ index)))
                 (+workspace-switch dest)))
              ((stringp index)
               (+workspace-switch index t))
              (t
               (error "Not a valid index: %s" index)))
        (unless (called-interactively-p 'interactive)
          (if (equal (+workspace-current-name) old-name)
              (+workspace-message (format "Already in %s" old-name) 'warn)
            (+workspace/display))))
    ('error (+workspace-error (cadr ex) t))))

;;;###autoload
(dotimes (i 9)
  (defalias (intern (format "+workspace/switch-to-%d" i))
    (lambda () (interactive) (+workspace/switch-to i))
    (format "Switch to workspace #%d" (1+ i))))

;;;###autoload
(defun +workspace/switch-to-final ()
  "Switch to the final workspace in open workspaces."
  (interactive)
  (+workspace/switch-to (car (last (+workspace-list-names)))))

;;;###autoload
(defun +workspace/other ()
  "Switch to the last activated workspace."
  (interactive)
  (+workspace/switch-to +workspace--last))

;;;###autoload
(defun +workspace/cycle (n)
  "Cycle n workspaces to the right (default) or left."
  (interactive (list 1))
  (let ((current-name (+workspace-current-name)))
    (condition-case-unless-debug ex
        (let* ((persps (+workspace-list-names))
               (perspc (length persps))
               (index (cl-position current-name persps)))
          (when (= perspc 1)
            (user-error "No other workspaces"))
          (+workspace/switch-to (% (+ index n perspc) perspc))
          (unless (called-interactively-p 'interactive)
            (+workspace/display)))
      ('user-error (+workspace-error (cadr ex) t))
      ('error (+workspace-error ex t)))))

;;;###autoload
(defun +workspace/switch-left ()  (interactive) (+workspace/cycle -1))

;;;###autoload
(defun +workspace/switch-right () (interactive) (+workspace/cycle +1))

;;;###autoload
(defun +workspace/close-window-or-workspace ()
  "Close the selected window. If it's the last window in the
workspace, either close the workspace (as well as its associated
frame, if one exists) and move to the next."
  (interactive)
  (let ((delete-window-fn (if (featurep 'evil) #'evil-window-delete #'delete-window)))
    (if (window-dedicated-p)
        (funcall delete-window-fn)
      (let ((current-workspace-name (+workspace-current-name)))
        (cond ((or (+workspace--protected-p current-workspace-name)
                   (cdr (zenit-visible-windows)))
               (funcall delete-window-fn))

              ((cdr (+workspace-list-names))
               (let ((frame-persp (frame-parameter nil 'workspace)))
                 (if (string= frame-persp (+workspace-current-name))
                     (delete-frame)
                   (+workspace/kill current-workspace-name))))

              ((+workspace-error "Can't delete last workspace" t)))))))

;;;###autoload
(defun +workspace/move-left (&optional count)
  "Move the current workspace to the left by n COUNTs."
  (interactive "p")
  (let* ((current-pos (cl-position (+workspace-current-name) (+workspace-list-names)))
         (new-pos (- current-pos (min count (1- (length (+workspace-list-names)))))))
    (tab-bar-move-tab-to (1+ new-pos) (1+ current-pos))))

;;;###autoload
(defun +workspace/move-right (&optional count)
  "Move the current workspace to the right by n COUNTs."
  (interactive "p")
  (let* ((current-pos (cl-position (+workspace-current-name) (+workspace-list-names)))
         (new-pos (+ current-pos (min count (1- (length (+workspace-list-names)))))))
    (tab-bar-move-tab-to (1+ new-pos) (1+ current-pos))))


;;
;;; Tabs display in minibuffer

(defun +workspace--tabline (&optional names)
  (let ((names (or names (+workspace-list-names)))
        (current-name (+workspace-current-name)))
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i to (length names)
              collect
              (propertize (format " [%d] %s " (1+ i) name)
                          'face (if (equal current-name name)
                                    '+workspace-tab-selected-face
                                  '+workspace-tab-face)))
     " ")))

(defun +workspace--message-body (message &optional type)
  (concat (+workspace--tabline)
          (propertize " | " 'face 'font-lock-comment-face)
          (propertize (format "%s" message)
                      'face (pcase type
                              ('error 'error)
                              ('warn 'warning)
                              ('success 'success)
                              ('info 'font-lock-comment-face)))))

;;;###autoload
(defun +workspace-message (message &optional type)
  "Show an 'elegant' message in the echo area next to a listing of workspaces."
  (message "%s" (+workspace--message-body message type)))

;;;###autoload
(defun +workspace-error (message &optional noerror)
  "Show an 'elegant' error in the echo area next to a listing of workspaces."
  (funcall (if noerror #'message #'error)
           "%s" (+workspace--message-body message 'error)))

;;;###autoload
(defun +workspace/display ()
  "Display a list of workspaces (like tabs) in the echo area."
  (interactive)
  (let (message-log-max)
    (message "%s" (+workspace--tabline))))


;;
;;; Hooks

(defvar +workspaces--project-dir nil)
;;;###autoload
(defun +workspaces-set-project-action-fn ()
  "A `projectile-switch-project-action' that sets the project directory for
`+workspaces-switch-to-project-h'."
  (setq +workspaces--project-dir default-directory))

;;;###autoload
(defun +workspaces-switch-to-project-h (&optional dir)
  "Creates a workspace dedicated to a new project. If one already exists, switch
to it. If in the main workspace and it's empty, recycle that workspace, without
renaming it.

Afterwords, runs `+workspaces-switch-project-function'. By default, this prompts
the user to open a file in the new project.

This be hooked to `projectile-after-switch-project-hook'."
  (when dir
    (setq +workspaces--project-dir dir))
  ;; HACK Clear projectile-project-root, otherwise cached roots may interfere
  ;;      with project switch (see #3166)
  (let (projectile-project-root)
    (when (and bufferlo-mode +workspaces--project-dir)
      (when projectile-before-switch-project-hook
        (with-temp-buffer
          ;; Load the project dir-local variables into the switch buffer, so the
          ;; action can make use of them
          (setq default-directory +workspaces--project-dir)
          (hack-dir-local-variables-non-file-buffer)
          (run-hooks 'projectile-before-switch-project-hook)))
      (unwind-protect
          (if (and (not (null +workspaces-on-switch-project-behavior))
                   (or (eq +workspaces-on-switch-project-behavior t)
                       (zenit-real-buffer-list (+workspace-buffer-list))))
              (let* ((project-name (zenit-project-name +workspaces--project-dir))
                     (persp (+workspace-get project-name t)))
                (+workspace-switch project-name (unless persp t))
                (with-current-buffer (zenit-fallback-buffer)
                  (setq default-directory +workspaces--project-dir)
                  (hack-dir-local-variables-non-file-buffer))
                (unless current-prefix-arg
                  (funcall +workspaces-switch-project-function +workspaces--project-dir))
                (+workspace-message
                 (format "Switched to '%s' in new workspace" project-name)
                 'success))
            (with-current-buffer (zenit-fallback-buffer)
              (setq default-directory +workspaces--project-dir)
              (hack-dir-local-variables-non-file-buffer)
              (message "Switched to '%s'" (zenit-project-name +workspaces--project-dir)))
            (with-demoted-errors "Workspace error: %s"
              (+workspace-rename (+workspace-current-name) (zenit-project-name +workspaces--project-dir)))
            (unless current-prefix-arg
              (funcall +workspaces-switch-project-function +workspaces--project-dir)))
        (run-hooks 'projectile-after-switch-project-hook)
        (setq +workspaces--project-dir nil)))))


;;
;;; Advice

;;;###autoload
(defun +workspaces-autosave-real-buffers-a (fn &rest args)
  "Don't autosave if no real buffers are open."
  (when (zenit-real-buffer-list)
    (apply fn args))
  t)

;;
;;; Hooks

;;;###autoload
(defun +workspaces-kill-emacs-h ()
  "Autosave workspace session."
  (when +workspaces-autosave
    (let ((file (zenit-session-file)))
      (+workspaces-rotate-autosaves file)
      (+workspace-save-session file))))

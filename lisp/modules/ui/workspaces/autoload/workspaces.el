;; ui/workspaces/autoload/workspaces.el -*- lexical-binding: t; -*-

(defvar +workspace--last nil)
(defvar +workspace--index 0)

;;;###autoload
(defface +workspace-tab-selected-face '((t (:inherit highlight)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)

;;;###autoload
(defface +workspace-tab-face '((t (:inherit default)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)


;;
;;; Library

(defun +workspace--protected-p (name)
  (equal name persp-nil-name))

(defun +workspace--generate-id ()
  (or (cl-loop for name in (+workspace-list-names)
               when (string-match-p "^workspace-[0-9]+$" name)
               maximize (string-to-number (substring name 10)) into max
               finally return (if max (1+ max)))
      1))


;;
;;; Predicates

;;;###autoload
(defalias #'+workspace-p #'perspective-p
  "Return t if OBJ is a perspective hash table.")

;;;###autoload
(defun +workspace-exists-p (name)
  "Returns t if NAME is the name of an existing workspace."
  (member name (+workspace-list-names)))

;;;###autoload
(defalias #'+workspace-contains-buffer-p #'persp-contain-buffer-p
  "Return non-nil if BUFFER is in WORKSPACE (defaults to current workspace).")


;;
;;; Getters

;;;###autoload
(defalias #'+workspace-current #'get-current-persp
  "Return the currently active workspace.")

;;;###autoload
(defun +workspace-get (name &optional noerror)
  "Return a workspace named NAME. Unless NOERROR is non-nil, this throws an
error if NAME doesn't exist."
  (cl-check-type name string)
  (when-let* ((persp (persp-get-by-name name)))
    (cond ((+workspace-p persp) persp)
          ((not noerror)
           (error "No workspace called '%s' was found" name)))))

;;;###autoload
(defun +workspace-current-name ()
  "Get the name of the current workspace."
  (safe-persp-name (+workspace-current)))

;;;###autoload
(defun +workspace-list-names ()
  "Return the list of names of open workspaces."
  (cl-remove persp-nil-name persp-names-cache :count 1))

;;;###autoload
(defun +workspace-list ()
  "Return a list of workspace structs (satisifes `+workspace-p')."
  ;; We don't use `hash-table-values' because it doesn't ensure order in older
  ;; versions of Emacs
  (cl-loop for name in (+workspace-list-names)
           if (gethash name *persp-hash*)
           collect it))

;;;###autoload
(defun +workspace-buffer-list (&optional persp)
  "Return a list of buffers in PERSP.

PERSP can be a string (name of a workspace) or a workspace (satisfies
`+workspace-p'). If nil or omitted, it defaults to the current workspace."
  (let ((persp (or persp (+workspace-current))))
    (unless (+workspace-p persp)
      (user-error "Not in a valid workspace (%s)" persp))
    (persp-buffers persp)))

;;;###autoload
(defun +workspace-orphaned-buffer-list ()
  "Return a list of buffers that aren't associated with any perspective."
  (cl-remove-if #'persp--buffer-in-persps (buffer-list)))


;;
;;; Actions

;;;###autoload
(defun +workspace-load (name)
  "Loads a single workspace (named NAME) into the current session. Can only
retrieve perspectives that were explicitly saved with `+workspace-save'.

Returns t if successful, nil otherwise."
  (when (+workspace-exists-p name)
    (user-error "A workspace named '%s' already exists." name))
  (persp-load-from-file-by-names
   (expand-file-name +workspaces-data-file persp-save-dir)
   *persp-hash* (list name))
  (+workspace-exists-p name))

;;;###autoload
(defun +workspace-save (name)
  "Saves a single workspace (NAME) from the current session. Can be loaded again
with `+workspace-load'. NAME can be the string name of a workspace or its
perspective hash table.

Returns t on success, nil otherwise."
  (unless (+workspace-exists-p name)
    (error "'%s' is an invalid workspace" name))
  (let ((fname (expand-file-name +workspaces-data-file persp-save-dir)))
    (persp-save-to-file-by-names fname *persp-hash* (list name) t)
    (and (member name (persp-list-persp-names-in-file fname))
         t)))

;;;###autoload
(defun +workspace-delete (workspace)
  "Delete WORKSPACE from the saved workspaces in `persp-save-dir'.

Return t if WORKSPACE was successfully deleted. Throws error if WORKSPACE is not
found or wasn't saved with `+workspace-save'."
  (let* ((fname (expand-file-name +workspaces-data-file persp-save-dir))
         (workspace-name (if (stringp workspace) workspace (persp-name workspace)))
         (workspace-names (persp-list-persp-names-in-file fname))
         (workspace-idx (cl-position workspace-name workspace-names :test #'equal)))
    (unless workspace-idx
      (error "Couldn't find saved workspace '%s'" workspace-name))
    (zenit-file-write
     fname (list (cl-remove-if (lambda (ws) (equal workspace-name (nth 1 ws)))
                               (zenit-file-read fname :by 'read)
                               :count 1)))
    (not (member name (persp-list-persp-names-in-file fname)))))

;;;###autoload
(defun +workspace-new (name)
  "Create a new workspace named NAME. If one already exists, return nil.
Otherwise return t on success, nil otherwise."
  (when (+workspace--protected-p name)
    (error "Can't create a new '%s' workspace" name))
  (when (+workspace-exists-p name)
    (error "A workspace named '%s' already exists" name))
  (when (frame-parameter (selected-frame) 'workspace-exclusive)
    (error "New workspaces are not allowed in this frame"))
  (let ((persp (persp-add-new name))
        (+popup--inhibit-transient t))
    (save-window-excursion
      (let ((ignore-window-parameters t)
            (+popup--inhibit-transient t))
        (persp-delete-other-windows))
      (switch-to-buffer (zenit-fallback-buffer))
      (setf (persp-window-conf persp)
            (funcall persp-window-state-get-function (selected-frame))))
    (+workspaces--add-ws-to-frame name)
    persp))

;;;###autoload
(defun +workspace-rename (name new-name)
  "Rename the current workspace named NAME to NEW-NAME.
Returns old name on success, nil otherwise."
  (when (+workspace--protected-p name)
    (error "Can't rename '%s' workspace" name))
  ;; Update frame parameter
  (+workspaces--remove-ws-from-frame name)
  (+workspaces--add-ws-to-frame new-name)
  (persp-rename new-name (+workspace-get name)))

;;;###autoload
(defun +workspace-kill (workspace &optional inhibit-kill-p)
  "Kill the workspace denoted by WORKSPACE.
WORKSPACE can be the name of a perspective or its hash table.

If INHIBIT-KILL-P is non-nil, don't kill this workspace's
buffers."
  (unless (stringp workspace)
    (setq workspace (persp-name workspace)))
  (when (+workspace--protected-p workspace)
    (error "Can't delete '%s' workspace" workspace))
  (+workspace-get workspace) ; error checking
  (persp-kill workspace inhibit-kill-p)
  (+workspaces--remove-ws-from-frame workspace)
  (not (+workspace-exists-p workspace)))

;;;###autoload
(defun +workspace-switch (name &optional auto-create-p)
  "Switch to another workspace named NAME (a string).

If AUTO-CREATE-P is non-nil, create the workspace if it doesn't
exist, otherwise throws an error."
  (unless (+workspace-exists-p name)
    (if auto-create-p
        (+workspace-new name)
      (error "%s is not an available workspace" name)))
  (let* ((from-name (+workspace-current-name))
         (to-frame (or (car (cl-rassoc-if (lambda (ws) (member name ws)) +workspaces-frames-alist))
                       (selected-frame))))
    (unless (equal from-name name)
      (setq +workspace--last
            (or (and (not (+workspace--protected-p from-name))
                     from-name)
                +workspaces-main))

      ;; Switch workspace
      (persp-frame-switch name to-frame)
      (select-frame-set-input-focus to-frame)
      (+workspaces--update-tab-bar))
    (equal (+workspace-current-name) name)))

;;;###autoload
(defun +workspace-move (workspace &optional frame)
  "Move WORKSPACE to another FRAME.
If FRAME is not specified, prompt to select from existing frames
or create new."
  (when (+workspace--protected-p workspace)
    (error "Cannot move protected workspace"))

  (when-let* ((frame (if (framep frame)
                         frame
                       (make-frame `((width . ,(frame-parameter (selected-frame) 'width))
                                     (height . ,(frame-parameter (selected-frame) 'height))))))
              (old-frame (selected-frame)))

    ;; Remove from old frame
    (+workspaces--remove-ws-from-frame workspace old-frame)
    (let ((frame-workspaces (frame-parameter nil 'workspaces)))
      (when (length= frame-workspaces 0)
        (delete-frame)))

    ;; Add to new frame
    (+workspaces--add-ws-to-frame workspace frame)
    (with-selected-frame frame
      (persp-activate (+workspace-get workspace) frame)
      (select-frame-set-input-focus frame))

    ;; Switch to last recent tab in old frame
    (when (frame-live-p old-frame)
      (with-selected-frame old-frame
        (tab-bar-switch-to-recent-tab)))

    (+workspaces--update-tab-bar)))


;;
;;; Commands

;;;###autoload
(defalias '+workspace/restore-last-session #'zenit/quickload-session)

;;;###autoload
(defun +workspace/load (name)
  "Load a workspace and switch to it. If called with C-u, try to reload the
current workspace (by name) from session files."
  (interactive
   (list
    (if current-prefix-arg
        (+workspace-current-name)
      (completing-read
       "Workspace to load: "
       (persp-list-persp-names-in-file
        (expand-file-name +workspaces-data-file persp-save-dir))))))
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
  "Delete this workspace. If called with C-u, prompts you for the name of the
workspace to delete."
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
          (cond ((delq (selected-frame) (persp-frames-with-persp (get-frame-persp)))
                 (user-error "Can't close workspace, it's visible in another frame"))
                ((not (equal (+workspace-current-name) name))
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
                 (+workspace-switch +workspaces-main t)
                 (unless (string= (car workspaces) +workspaces-main)
                   (+workspace-kill name))
                 (zenit/kill-all-buffers (zenit-buffer-list))))
          (+workspace-message (format "Deleted '%s' workspace" name) 'success)))
    ('error (+workspace-error ex t))))

;;;###autoload
(defun +workspace/delete (name)
  "Delete a saved workspace in `persp-save-dir'.

Can only selete workspaces saved with `+workspace/save' or
`+workspace-save'."
  (interactive
   (list
    (completing-read "Delete saved workspace: "
                     (cl-loop with wsfile = (zenit-path persp-save-dir +workspaces-data-file)
                              for p in (persp-list-persp-names-in-file wsfile)
                              collect p))))
  (and (condition-case-unless-debug ex
           (or (+workspace-delete name)
               (+workspace-error (format "Couldn't delete '%s' workspace" name)))
         ('error (+workspace-error ex t)))
       (+workspace-message (format "Deleted '%s' workspace" name) 'success)))

;;;###autoload
(defun +workspace/kill-session (&optional interactive)
  "Delete the current session, all workspaces, windows and their buffers."
  (interactive (list t))
  (let ((windows (length (window-list)))
        (persps (length (+workspace-list-names)))
        (buffers 0))
    (let ((persp-autokill-buffer-on-remove t))
      (unless (cl-every #'+workspace-kill (+workspace-list-names))
        (+workspace-error "Could not clear session")))
    (+workspace-switch +workspaces-main t)
    (setq buffers (zenit/kill-all-buffers (buffer-list)))
    (when interactive
      (message "Killed %d workspace(s), %d window(s) & %d buffer(s)"
               persps windows buffers))))

;;;###autoload
(defun +workspace/kill-session-and-quit ()
  "Kill emacs without saving anything."
  (interactive)
  (let ((persp-auto-save-opt 0))
    (kill-emacs)))

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
            (clone-p
             (persp-copy name t))
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
    (if (+workspace--protected-p current-name)
        (+workspace-switch +workspaces-main t)
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
        ('error (+workspace-error ex t))))))

;;;###autoload
(defun +workspace/switch-left (&optional n)  (interactive "p") (+workspace/cycle (- n)))

;;;###autoload
(defun +workspace/switch-right (&optional n) (interactive "p") (+workspace/cycle n))

;;;###autoload
(defun +workspace/close-window-or-workspace ()
  "Close the selected window.
If it's the last window in the workspace, either close the
workspace (as well as its associated frame, if one exists) and
move to the next."
  (interactive)
  (let ((delete-window-fn (if (featurep 'evil) #'evil-window-delete #'delete-window)))
    (if (window-dedicated-p)
        (funcall delete-window-fn)
      (let ((current-persp-name (+workspace-current-name)))
        (cond ((or (+workspace--protected-p current-persp-name)
                   (cdr (zenit-visible-windows)))
               (funcall delete-window-fn))

              ((cdr (+workspace-list-names))
               (let ((frame-workspaces (frame-parameter nil 'workspaces)))
                 (if (length= frame-workspaces 1)
                     (delete-frame)
                   (+workspace/kill current-persp-name))))

              ((+workspace-error "Can't delete last workspace" t)))))))

;;;###autoload
(defun +workspace/move (workspace &optional frame)
  "Move WORKSPACE to another FRAME.

If FRAME is not specified, prompt to select from existing frames
or create new."
  (interactive
   (list (+workspace-current-name)
         (let ((frames-alist (+workspaces--unique-frames-alist (mapcar #'car +workspaces-frames-alist))))
           (alist-get (completing-read
                       "Move workspace to frame: "
                       (append '("New frame")
                               frames-alist)
                       nil t)
                      frames-alist nil nil #'string=))))
  (+workspace-move workspace frame))

(defun +workspaces--unique-frames-alist (frames)
  (let (;; Remove frames which contain already the current workspace
        (frames (cl-remove-if
                 (lambda (f)
                   (member (+workspace-current-name) (frame-parameter f 'workspaces)))
                 frames))
        frame-names-alist uniques)
    ;; Build frame name - frame assoc list and uniquify the frame names
    (dolist (f frames)
      (let* ((name (frame-parameter f 'name))
             (name (if-let* ((entry (alist-get name uniques nil nil #'string=)))
                       (let ((new-name (format "%s - %d" name entry)))
                         (setf (alist-get name uniques nil nil #'string=)
                               (1+ entry))
                         new-name)
                     (setf (alist-get name uniques nil nil #'string=) 1)
                     name)))
        (setf (alist-get name frame-names-alist nil nil #'string=) f)))
    frame-names-alist))

;;;###autoload
(defun +workspace/swap-left (&optional count)
  "Swap the current workspace with the COUNTth workspace on its left."
  (interactive "p")
  (let* ((current-name (+workspace-current-name))
         (count (or count 1))
         (persps (+workspace-list-names))
         (frame-ws (frame-parameter (selected-frame) 'workspaces))
         (persp-index (- (cl-position current-name persps :test #'equal)
                         count))
         (persp-names (remove current-name persps))
         (frame-ws-index (- (cl-position current-name frame-ws :test #'equal)
                            count))
         (frame-ws-names (remove current-name frame-ws)))
    (unless persp-names
      (user-error "Only one workspace"))
    (let ((persp-index (min (max 0 persp-index) (length persp-names)))
          (frame-ws-index (min (max 0 frame-ws-index) (length frame-ws-names))))
      (setq persp-names-cache
            (append (cl-subseq persp-names 0 persp-index)
                    (list current-name)
                    (cl-subseq persp-names persp-index)))
      (let ((frame-ws-new
             (append (cl-subseq frame-ws-names 0 frame-ws-index)
                     (list current-name)
                     (cl-subseq frame-ws-names frame-ws-index))))
        (set-frame-parameter (selected-frame) 'workspaces frame-ws-new)
        (setf (alist-get (selected-frame) +workspaces-frames-alist) frame-ws-new)))
    (when (called-interactively-p 'any)
      (+workspace/display))))

;;;###autoload
(defun +workspace/swap-right (&optional count)
  "Swap the current workspace with the COUNTth workspace on its right."
  (interactive "p")
  (funcall-interactively #'+workspace/swap-left (- count)))


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
  "Show an \\='elegant' message in the echo area next to a listing of workspaces."
  (message "%s" (+workspace--message-body message type)))

;;;###autoload
(defun +workspace-error (message &optional noerror)
  "Show an \\='elegant' error in the echo area next to a listing of workspaces."
  (funcall (if noerror #'message #'error)
           "%s" (+workspace--message-body message 'error)))

;;;###autoload
(defun +workspace/display ()
  "Display a list of workspaces (like tabs) in the echo area."
  (interactive)
  (let (message-log-max)
    (message "%s" (+workspace--tabline))))


;;
;;; Frames

(defvar +workspaces-frames-alist nil
  "Alist mapping frames to their associated workspace names.
Format: ((frame . workspace-name) ...)")

(defun +workspaces--add-ws-to-frame (name &optional frame)
  (let* ((frame (or frame (selected-frame)))
         (workspaces (nreverse (frame-parameter frame 'workspaces))))
    (cl-pushnew name workspaces :test #'string=)
    (let ((workspaces (nreverse workspaces)))
      (setf (alist-get frame +workspaces-frames-alist) workspaces)
      (set-frame-parameter frame 'workspaces workspaces))))

(defun +workspaces--remove-ws-from-frame (name &optional frame)
  (let* ((frame (or frame (selected-frame)))
         (workspaces (frame-parameter frame 'workspaces))
         (workspaces (cl-remove name workspaces :test #'string=)))
    (setf (alist-get frame +workspaces-frames-alist) workspaces)
    (set-frame-parameter frame 'workspaces workspaces)))

(defun +workspaces--update-tab-bar ()
  (dolist (f (frame-list))
    (with-selected-frame f
      ;; REVIEW 2025-04-06: This is kinda slow, maybe there is an alternative.
      (tab-bar--update-tab-bar-lines t))))

;;;###autoload
(defun +workspaces-restore-frame-associations-h (&rest _)
  "Restore frame-workspace associations after loading perspectives."
  (let ((frame-map (make-hash-table :test 'equal))
        frames)
    ;; Collect and create all required frames
    (dolist (persp (+workspace-list))
      (dolist (fdata (persp-parameter 'workspace-frames persp))
        (let ((fid (car fdata)))
          (cl-pushnew (cons fid fdata) frames :test #'equal))))
    ;; Restore the associations to the current frame
    (with-selected-frame (selected-frame)
          ;; Associate all loaded workspaces with current frame
      (+workspaces--add-ws-to-frame +workspaces-main)
          (dolist (ws (cons +workspaces-main (+workspace-list-names)))
            (+workspaces--add-ws-to-frame ws)))

    ;; If we need than one frame, we create new frames and move the workspaces
    ;; accordingly
    (when (length> frames 1)
      (cl-loop for (fid . fdata) in (cdr (nreverse frames))
               do (unless (gethash fid frame-map)
                    (puthash fid
                             (make-frame (plist-get (cdr fdata) :geometry))
                             frame-map)))

      ;; Associate workspaces with frames
      (dolist (persp (+workspace-list))
        (let ((name (persp-name persp)))
          (dolist (fdata (persp-parameter 'workspace-frames persp))
            (let* ((fid (car fdata))
                   (frame (gethash fid frame-map)))
              (persp-activate persp)
              (when frame
                (+workspace-move name frame)))))))

    ;; Remove `+workspaces-main' if it was not part of the restored workspaces
    (when (eq (persp-get-by-name +workspaces-main) :nil)
      (dolist (frame (persp-frame-list-without-daemon))
        (+workspaces--remove-ws-from-frame +workspaces-main frame)))

    ;; Finally update UI
    (+workspace-switch (car (+workspace-list-names)))
    (+workspaces--update-tab-bar)))


;; REVIEW 2025-04-07: Maybe move to a better place
(add-hook! '(zenit-switch-frame-hook after-delete-frame-functions)
  (defun +workspaces--cleanup-frame-associations-h (&rest _)
    "Remove dead frames from workspace associations."
    (setq +workspaces-frames-alist
          (cl-loop for (frame . ws) in +workspaces-frames-alist
                   if (frame-live-p frame)
                   collect (cons frame ws)))))

(defun +workspaces--find-workspace-frames (workspace)
  "Return list of frames containing WORKSPACE.

Each element is a cons cell (FRAME . EXCLUSIVE-P) where:
- FRAME is the frame object
- EXCLUSIVE-P is non-nil if WORKSPACE is exclusive to FRAME

Skips daemon frames and frames without valid window
configurations."
  (let (frames)
    (dolist (frame (persp-frame-list-without-daemon) frames)
      (when (and (member workspace (frame-parameter frame 'workspaces)))
        (cl-pushnew `(,frame . ,(frame-parameter frame 'workspace-exclusive)) frames)))
    frames))


;;
;;; Hooks

;;;###autoload
(defun +workspaces-delete-associated-workspace-h (&optional frame)
  "Delete workspace associated with current frame.
A workspace gets associated with a frame when a new frame is
interactively created."
  (when (and persp-mode (not (bound-and-true-p with-editor-mode)))
    (unless frame
      (setq frame (selected-frame)))
    (let ((ws-to-kill (cl-loop for workspace in (frame-parameter frame 'workspaces)
                               for workspace-frames = nil
                               do (cl-loop for (frame . ws) in +workspaces-frames-alist
                                           when (member workspace ws)
                                           do (push frame workspace-frames))
                               if (length= workspace-frames 1)
                               collect workspace into to-kill
                               finally return to-kill)))
      (dolist (ws ws-to-kill)
        (+workspace/kill ws)))
    (+workspaces--update-tab-bar)))

(defun +workspaces--associate-frame (frame &optional workspace)
  "Associate FRAME with WORKSPACE.

If WORKSPACE is omitted, uses the current workspace. Sets up the
frame's workspace list and buffer predicate to filter buffers by
workspace, as well as updates `+workspaces-frames-alist'."
  (let ((workspace (or workspace (+workspace-current-name))))
    (+workspaces--add-ws-to-frame workspace frame)
    ;; Ensure every buffer has a buffer-predicate
    (persp-set-frame-buffer-predicate frame)))

;;;###autoload
(defun +workspaces-make-frame-fn (frame &optional _new-frame-p)
  "Create a new FRAME.
The frame is associated with the same workspace it was spawned
from and displays the last selected buffer."
  (when persp-mode
    (with-selected-frame frame
      ;; Dp not restore the last window configuration
      (let ((persp-init-frame-behaviour nil))
        (persp-activate (frame-parameter (previous-frame) 'persp) frame))
      (+workspaces--associate-frame frame)
      (set-frame-parameter frame 'workspace-exclusive t)
      (+workspaces--update-tab-bar))))

;;;###autoload
(defun +workspaces-associate-frame-fn (frame &optional _new-frame-p)
  "Create a blank, new perspective and associate it with FRAME."
  (when persp-mode
    (if (not (persp-frame-list-without-daemon))
        (+workspace-switch +workspaces-main t)
      (with-selected-frame frame
        (+workspace-switch (format "workspace-%s" (+workspace--generate-id)) t)
        (unless (zenit-real-buffer-p (current-buffer))
          (switch-to-buffer (zenit-fallback-buffer)))
        (+workspaces--associate-frame frame))
      (run-at-time 0.1 nil #'+workspace/display))))

;;;###autoload
(defun +workspaces-switch-to-project-h (&optional dir)
  "Creates a workspace dedicated to a new project.

If one already exists, switch to it. If in the main workspace and
it's empty, recycle that workspace, without renaming it.

Afterwords, runs `+workspaces-switch-project-function'. By
default, this prompts the user to open a file in the new project.

This be hooked to `projectile-after-switch-project-hook'."
  (let* ((default-directory (or dir default-directory))
         (pname (zenit-project-name))
         (proot (file-truename default-directory))
         ;; HACK 2025-04-03: Clear projectile-project-root or cached roots could
         ;;   interfere with project switching.
         projectile-project-root)
    (when persp-mode
      (if (and (not (null +workspaces-on-switch-project-behavior))
               (or (eq +workspaces-on-switch-project-behavior t)
                   (+workspace--protected-p (safe-persp-name (get-current-persp)))
                   ;; NOTE 2025-04-03: Because we associate immediately buffers
                   ;;   to the first workspace, we need to filter here for real
                   ;;   buffers. Otherwise, the condition is never nil.
                   (zenit-real-buffer-list (+workspace-buffer-list))))
          (let* ((ws-param '+workspace-project)
                 (ws (+workspace-get pname t))
                 (ws (if (and ws
                              (ignore-errors
                                (file-equal-p (persp-parameter ws-param ws)
                                              proot)))
                         ws
                       ;; Uniquify the project's name, so we don't clobber a
                       ;; pre-existing workspace with the same name.
                       (let* ((parts (nreverse (split-string proot "/" t)))
                              (pre  (cdr parts))
                              (post (list (car parts))))
                         (while (and pre
                                     (setq ws (+workspace-get (setq pname (string-join post "/")) t))
                                     (not (ignore-errors
                                            (file-equal-p (persp-parameter ws-param ws)
                                                          proot))))
                           (push (pop pre) post))
                         (unless pre ws))))
                 (ws (or ws
                         (+workspace-get pname t)
                         (+workspace-new pname))))
            (set-persp-parameter ws-param proot ws)
            (+workspace-switch pname)
            (with-current-buffer (zenit-fallback-buffer)
              (setq-local default-directory proot)
              (hack-dir-local-variables-non-file-buffer))
            (unless current-prefix-arg
              (funcall +workspaces-switch-project-function proot))
            (+workspace-message
             (format "Switched to '%s' in new workspace" pname)
             'success))
        (with-current-buffer (zenit-fallback-buffer)
          (setq-local default-directory proot)
          (hack-dir-local-variables-non-file-buffer)
          (message "Switched to '%s'" pname))
        (with-demoted-errors "Workspace error: %s"
          (+workspace-rename (+workspace-current-name) pname))
        (unless current-prefix-arg
          (funcall +workspaces-switch-project-function proot))))))

;;;###autoload
(defun +workspaces-save-tab-bar-data-h (_)
  "Save the current workspace's tab bar data."
  (when (get-current-persp)
    (set-persp-parameter
     'tab-bar-tabs (tab-bar-tabs))
    (set-persp-parameter 'tab-bar-closed-tabs tab-bar-closed-tabs)))

;;;###autoload
(defun +workspaces-save-tab-bar-data-to-file-h (&rest _)
  "Save the current workspace's tab bar data to file."
  (when (get-current-persp)
    ;; HACK 2025-04-03: Remove fields (for window-configuration) that cannot be
    ;;   serialized.
    (set-persp-parameter 'tab-bar-tabs
                         (frameset-filter-tabs (tab-bar-tabs) nil nil t))))

;;;###autoload
(defun +workspaces-load-tab-bar-data-h (_)
  "Restores the tab bar data of the workspace we have just switched to."
  (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
  (setq tab-bar-closed-tabs (persp-parameter 'tab-bar-closed-tabs))
  (tab-bar--update-tab-bar-lines t))

;;;###autoload
(defun +workspaces-load-tab-bar-data-from-file-h (&rest _)
  "Restores the tab bar data from file."
  (when-let* ((persp-tab-data (persp-parameter 'tab-bar-tabs)))
    (tab-bar-tabs-set persp-tab-data)
    (tab-bar--update-tab-bar-lines t)))


;;
;;; Advice

;;;###autoload
(defun +workspaces-autosave-real-buffers-a (fn &rest args)
  "Don't autosave if no real buffers are open."
  (when (zenit-real-buffer-list)
    (apply fn args))
  t)

;; lisp/core/lib/zenit-lib-buffers.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

;; `cl-seq'
(declare-function cl-remove-if-not "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `projectile'
(declare-function projectile-project-buffer-p "ext:projectile" (buffer project-root))

;; `subr-x'
(declare-function hash-table-keys "subr-x" (hash-table))
(autoload #'hash-table-keys "subr-x")

;; `zenit-lib-projects'
(declare-function zenit-project-root "zenit-lib-projects" (&optional dir))


;;;###autoload
(defvar zenit-real-buffer-functions '()
  "A list of predicate functions run to determine if a buffer is
real, unlike `zenit-unreal-buffer-functions'. They are passed one
argument: the buffer to be tested.

Should any of its function returns non-nil, the rest of the
functions are ignored and the buffer is considered real.

See `zenit-real-buffer-p' for more information.")

;;;###autoload
(defvar zenit-unreal-buffer-functions
  '(minibufferp zenit-special-buffer-p zenit-non-file-visiting-buffer-p)
  "A list of predicate functions run to determine if a buffer is
*not* real,unlike `zenit-real-buffer-functions'. They are passed
one argument: the buffer to be tested.

Should any of these functions return non-nil, the rest of the
functions are ignored and the buffer is considered unreal.

See `zenit-real-buffer-p' for more information.")

;;;###autoload
(defvar zenit-real-buffer-modes
  '(dired-mode comint-mode term-mode shell-mode eshell-mode vterm-mode)
  "A list of major modes whose buffers are considered real.")

;;;###autoload
(defvar-local zenit-real-buffer-p nil
  "If non-nil, this buffer should be considered real no matter what.
See `zenit-real-buffer-p' for more information.")

;;;###autoload
(defvar zenit-fallback-buffer-name "*scratch*"
  "The name of the buffer to fall back to if no other buffers
exist (will create it if it doesn't exist).")

;;; Functions

;;;###autoload
(defun zenit-buffer-frame-predicate (buf)
  "To be used as the default frame buffer-predicate parameter.
Returns nil if BUF should be skipped over by functions like
`next-buffer' and `other-buffer'."
  (or (zenit-real-buffer-p buf)
      (eq buf (zenit-fallback-buffer))))

;;;###autoload
(defun zenit-fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default
this is the scratch buffer. See `zenit-fallback-buffer-name' to
change this."
  (let (buffer-list-update-hook)
    (get-buffer-create zenit-fallback-buffer-name)))

;;;###autoload
(defalias 'zenit-buffer-list #'buffer-list)

;;;###autoload
(defun zenit-project-buffer-list (&optional project)
  "Return a list of buffers belonging to the specified PROJECT. If
PROJECT is nil, default to the current project."
  (let ((buffers (zenit-buffer-list)))
    (if-let* ((project-root
               (if project (expand-file-name project)
                 (zenit-project-root))))
        (cl-loop for buf in buffers
                 if (projectile-project-buffer-p buf project-root)
                 collect buf)
      buffers)))

;;;###autoload
(defun zenit-open-projects ()
  "Return a list of projects with open buffers."
  (cl-loop with projects = (make-hash-table :test 'equal :size 8)
           for buffer in (zenit-buffer-list)
           if (buffer-live-p buffer)
           if (zenit-real-buffer-p buffer)
           if (with-current-buffer buffer (zenit-project-root))
           do (puthash (abbreviate-file-name it) t projects)
           finally return (hash-table-keys projects)))

;;;###autoload
(defun zenit-special-buffer-p (buf &optional consider-mode-p)
  "Returns non-nil if BUF's name starts with an *.

If CONSIDER-MODE-P is non-nil, returns non-nil if BUF's mode is derived
from `special-mode'."
  (or (char-equal ?* (aref (buffer-name buf) 0))
      (and consider-mode-p
           (provided-mode-derived-p (buffer-local-value 'major-mode buf) 'special-mode))))

;;;###autoload
(defun zenit-temp-buffer-p (buf)
  "Returns non-nil if BUF is temporary."
  (char-equal ?\s (aref (buffer-name buf) 0)))

;;;###autoload
(defun zenit-visible-buffer-p (buf)
  "Return non-nil if BUF is visible."
  (get-buffer-window buf))

;;;###autoload
(defun zenit-buried-buffer-p (buf)
  "Return non-nil if BUF is not visible."
  (not (zenit-visible-buffer-p buf)))

;;;###autoload
(defun zenit-non-file-visiting-buffer-p (buf)
  "Returns non-nil if BUF does not have a value for `buffer-file-name'."
  (not (buffer-file-name (or (buffer-base-buffer buf) buf))))

;;;###autoload
(defun zenit-real-buffer-list (&optional buffer-list)
  "Return a list of buffers that satify `zenit-real-buffer-p'."
  (cl-loop for buf in (or buffer-list (zenit-buffer-list))
           if (zenit-real-buffer-p buf)
           collect buf))

;;;###autoload
(defun zenit-real-buffer-p (buffer-or-name)
  "Returns t if BUFFER-OR-NAME is a \\='real\\=' buffer.
A real buffer is a useful buffer; a first class citizen. Real
ones should get special treatment, because we will be spending
most of our time in them. Unreal ones should be low-profile and
easy to cast aside, so we can focus on real ones.

The exact criteria for a real buffer is:

1. A non-nil value for the buffer-local value of the
`zenit-real-buffer-p' variable

OR

2. Any function in `zenit-real-buffer-functions' returns non-nil

OR

3. None of the functions in `zenit-unreal-buffer-functions' must
return non-nil.

If BUFFER-OR-NAME is omitted or nil, the current buffer is tested."
  (or (bufferp buffer-or-name)
      (stringp buffer-or-name)
      (signal 'wrong-type-argument (list '(bufferp stringp) buffer-or-name)))
  (when-let* ((buf (get-buffer buffer-or-name)))
    (when-let* ((basebuf (buffer-base-buffer buf)))
      (setq buf basebuf))
    (and (buffer-live-p buf)
         (not (zenit-temp-buffer-p buf))
         (or (buffer-local-value 'zenit-real-buffer-p buf)
             (provided-mode-derived-p (buffer-local-value 'major-mode buf)
                                      zenit-real-buffer-modes)
             (run-hook-with-args-until-success 'zenit-real-buffer-functions buf)
             (not (run-hook-with-args-until-success 'zenit-unreal-buffer-functions buf))))))

;;;###autoload
(defun zenit-unreal-buffer-p (buffer-or-name)
  "Return t if BUFFER-OR-NAME is an \\='unreal\\=' buffer.
See `zenit-real-buffer-p' for details on what that means."
  (not (zenit-real-buffer-p buffer-or-name)))

;;;###autoload
(defun zenit-buffers-in-mode (modes &optional buffer-list derived-p)
  "Return a list of buffers whose `major-mode' is `eq' to MODE(S).
If DERIVED-P, test with `derived-mode-p', otherwise use `eq'."
  (cl-loop with modes = (ensure-list modes)
           for buf in (or buffer-list (zenit-buffer-list))
           for mode = (buffer-local-value 'major-mode buf)
           if (if derived-p
                  (apply #'provided-mode-derived-p mode modes)
                (memq mode modes))
           collect buf))

;;;###autoload
(defun zenit-visible-windows (&optional window-list)
  "Return a list of the visible, non-popup (dedicated) windows."
  (cl-loop for window in (or window-list (window-list))
           when (or (window-parameter window 'visible)
                    (not (window-dedicated-p window)))
           collect window))

;;;###autoload
(defun zenit-visible-buffers (&optional buffer-list all-frames)
  "Return a list of visible buffers (i.e. not buried)."
  (let ((buffers
         (delete-dups
          (cl-loop for frame in (if all-frames (visible-frame-list) (list (selected-frame)))
                   if (window-list frame)
                   nconc (mapcar #'window-buffer it)))))
    (if buffer-list
        (cl-loop for buf in buffers
                 unless (memq buf buffer-list)
                 collect buffers)
      buffers)))

;;;###autoload
(defun zenit-buried-buffers (&optional buffer-list)
  "Get a list of buffers that are buried."
  (cl-loop for buf in (or buffer-list (zenit-buffer-list))
           unless (zenit-visible-buffer-p buf)
           collect buf))

;;;###autoload
(defun zenit-matching-buffers (pattern &optional buffer-list)
  "Get a list of all buffers that match the regex PATTERN."
  (cl-loop for buf in (or buffer-list (zenit-buffer-list))
           if (string-match-p pattern (buffer-name buf))
           collect buf))

;;;###autoload
(defun zenit-set-buffer-real (buffer flag)
  "Forcibly mark BUFFER as FLAG (non-nil = real)."
  (with-current-buffer buffer
    (setq zenit-real-buffer-p flag)))

;;;###autoload
(defun zenit-kill-buffer-and-windows (buffer)
  "Kill the buffer and delete all the windows it's displayed in."
  (dolist (window (get-buffer-window-list buffer))
    (unless (one-window-p t)
      (delete-window window)))
  (kill-buffer buffer))

;;;###autoload
(defun zenit-fixup-windows (windows)
  "Ensure that each of WINDOWS is showing a real buffer or the fallback buffer."
  (dolist (window windows)
    (with-selected-window window
      (when (zenit-unreal-buffer-p (window-buffer))
        (previous-buffer)
        (when (zenit-unreal-buffer-p (window-buffer))
          (switch-to-buffer (zenit-fallback-buffer)))))))

;;;###autoload
(defun zenit-kill-buffer-fixup-windows (buffer)
  "Kill the BUFFER and ensure all the windows it was displayed in have switched
to a real buffer or the fallback buffer."
  (let ((windows (get-buffer-window-list buffer)))
    (kill-buffer buffer)
    (zenit-fixup-windows (cl-remove-if-not #'window-live-p windows))))

;;;###autoload
(defun zenit-kill-buffers-fixup-windows (buffers)
  "Kill the BUFFERS and ensure all the windows they were displayed in have
switched to a real buffer or the fallback buffer."
  (let ((seen-windows (make-hash-table :test 'eq :size 8)))
    (dolist (buffer buffers)
      (let ((windows (get-buffer-window-list buffer)))
        (kill-buffer buffer)
        (dolist (window (cl-remove-if-not #'window-live-p windows))
          (puthash window t seen-windows))))
    (zenit-fixup-windows (hash-table-keys seen-windows))))

;;;###autoload
(defun zenit-kill-matching-buffers (pattern &optional buffer-list)
  "Kill all buffers (in current workspace OR in BUFFER-LIST) that match the
regex PATTERN. Returns the number of killed buffers."
  (let ((buffers (zenit-matching-buffers pattern buffer-list)))
    (dolist (buf buffers (length buffers))
      (kill-buffer buf))))

;;; Hooks

;;;###autoload
(defun zenit-mark-buffer-as-real-h ()
  "Hook function that marks the current buffer as real."
  (zenit-set-buffer-real (current-buffer) t))

;;; Interactive commands

;;;###autoload
(defun zenit/save-and-kill-buffer ()
  "Save the current buffer to file, then kill it."
  (interactive)
  (save-buffer)
  (kill-current-buffer))

;;;###autoload
(defun zenit/kill-this-buffer-in-all-windows (buffer &optional dont-save)
  "Kill BUFFER globally and ensure all windows previously showing
this buffer have switched to a real buffer or the fallback
buffer. If DONT-SAVE, don't prompt to save modified
buffers (discarding their changes)."
  (interactive
   (list (current-buffer) current-prefix-arg))
  (cl-assert (bufferp buffer) t)
  (when (and (buffer-modified-p buffer) dont-save)
    (with-current-buffer buffer
      (set-buffer-modified-p nil)))
  (zenit-kill-buffer-fixup-windows buffer))

(defun zenit--message-or-count (interactive message count)
  "Display MESSAGE with COUNT if INTERACTIVE is non-nil.
If INTERACTIVE is nil, only return COUNT."
  (if interactive
      (message message count)
    count))

;;;###autoload
(defun zenit/kill-all-buffers (&optional buffer-list interactive)
  "Kill all buffers and closes their windows.
If the prefix arg is passed, doesn't close windows and only kill
buffers that belong to the current project."
  (interactive
   (list (if current-prefix-arg
             (zenit-project-buffer-list)
           (zenit-buffer-list))
         t))
  (if (null buffer-list)
      (message "No buffers to kill")
    (save-some-buffers)
    (delete-other-windows)
    (when (memq (current-buffer) buffer-list)
      (switch-to-buffer (zenit-fallback-buffer)))
    (mapc #'kill-buffer buffer-list)
    (zenit--message-or-count
     interactive "Killed %d buffers"
     (- (length buffer-list)
        (length (cl-remove-if-not #'buffer-live-p buffer-list))))))

;;;###autoload
(defun zenit/kill-other-buffers (&optional buffer-list interactive)
  "Kill all other buffers (besides the current one).
If the prefix arg is passed, kill only buffers that belong to the
current project."
  (interactive
   (list (delq (current-buffer)
               (if current-prefix-arg
                   (zenit-project-buffer-list)
                 (zenit-buffer-list)))
         t))
  (mapc #'zenit-kill-buffer-and-windows buffer-list)
  (zenit--message-or-count
   interactive "Killed %d other buffers"
   (- (length buffer-list)
      (length (cl-remove-if-not #'buffer-live-p buffer-list)))))

;;;###autoload
(defun zenit/kill-matching-buffers (pattern &optional buffer-list interactive)
  "Kill buffers that match PATTERN in BUFFER-LIST.
If the prefix arg is passed, only kill matching buffers in the
current project."
  (interactive
   (list (read-regexp "Buffer pattern: ")
         (if current-prefix-arg
             (zenit-project-buffer-list)
           (zenit-buffer-list))
         t))
  (zenit-kill-matching-buffers pattern buffer-list)
  (zenit--message-or-count
   interactive "Killed %d matching buffers"
   (- (length buffer-list)
      (length (cl-remove-if-not #'buffer-live-p buffer-list)))))

;;;###autoload
(defun zenit/kill-buried-buffers (&optional buffer-list interactive)
  "Kill buffers that are buried.
If PROJECT-P (universal argument), only kill buried buffers
belonging to the current project."
  (interactive
   (list (zenit-buried-buffers
          (if current-prefix-arg (zenit-project-buffer-list)))
         t))
  (mapc #'kill-buffer buffer-list)
  (when interactive
    (message "Killed %s buried buffers"
             (- (length buffer-list)
                (length (cl-remove-if-not #'buffer-live-p buffer-list))))))

;;;###autoload
(defun zenit/kill-project-buffers (project &optional interactive)
  "Kill buffers for the specified PROJECT."
  (interactive
   (list (if-let* ((open-projects (zenit-open-projects)))
             (completing-read
              "Kill buffers for project: " open-projects
              nil t nil nil
              (if-let* ((project-root (zenit-project-root))
                        (project-root (abbreviate-file-name project-root))
                        ((member project-root open-projects)))
                  project-root))
           (message "No projects are open!")
           nil)
         t))
  (when project
    (let ((buffer-list (zenit-project-buffer-list project)))
      (zenit-kill-buffers-fixup-windows buffer-list)
      (zenit--message-or-count
       interactive "Killed %d project buffers"
       (- (length buffer-list)
          (length (cl-remove-if-not #'buffer-live-p buffer-list)))))))

(provide 'zenit-lib '(buffers))

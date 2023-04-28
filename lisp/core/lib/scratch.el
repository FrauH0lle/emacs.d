;; lisp/core/lib/scratch.el -*- lexical-binding: t; -*-

(defvar zenit-scratch-default-file "__default"
  "The default file name for a project-less scratch buffer.
Will be saved in `zenit-scratch-dir'.")

(defvar zenit-scratch-dir (concat zenit-data-dir "scratch")
  "Where to save persistent scratch buffers.")

(defvar zenit-scratch-initial-major-mode t
  "What major mode to start fresh scratch buffers in.
Scratch buffers preserve their last major mode, however, so this only affects
the first, fresh scratch buffer you create. This accepts:
  t           Inherits the major mode of the last buffer you had selected.
  nil         Uses `fundamental-mode'
  MAJOR-MODE  Any major mode symbol")

(defvar zenit-scratch-buffers nil
  "A list of active scratch buffers.")

(defvar zenit-scratch-current-project nil
  "The name of the project associated with the current scratch buffer.")
(put 'zenit-scratch-current-project 'permanent-local t)

(defvar zenit-scratch-buffer-hook ()
  "The hooks to run after a scratch buffer is created.")


(defun zenit--load-persistent-scratch-buffer (project-name)
  "Load the persistent scratch buffer associated with
 PROJECT-NAME.

If PROJECT-NAME is nil, use `zenit-scratch-default-file' as the
default scratch buffer. This function sets the local variable
`zenit-scratch-current-project' to the value of PROJECT-NAME or
the default file, constructs the path to the scratch buffer file
in `zenit-scratch-dir', and reads the contents, point position,
and mode from the file if it exists and is readable. The buffer
content is then replaced, the mode is set, and the point is
positioned according to the saved data. The function returns
non-nil if the scratch buffer is successfully loaded, and nil
otherwise."
  (setq-local zenit-scratch-current-project
              (or project-name
                  zenit-scratch-default-file))
  (let ((smart-scratch-file
         (expand-file-name (concat zenit-scratch-current-project ".el")
                           zenit-scratch-dir)))
    (make-directory zenit-scratch-dir t)
    (when (file-readable-p smart-scratch-file)
      (message "Reading %s" smart-scratch-file)
      (cl-destructuring-bind (content point mode)
          (with-temp-buffer
            (save-excursion (insert-file-contents smart-scratch-file))
            (read (current-buffer)))
        (erase-buffer)
        (funcall mode)
        (insert content)
        (goto-char point)
        t))))

;;;###autoload
(defun zenit-scratch-buffer (&optional dont-restore-p mode directory project-name)
  "Return a scratchpad buffer in major MODE."
  (let* ((buffer-name (if project-name
                          (format "*zenit:scratch (%s)*" project-name)
                        "*zenit:scratch*"))
         (buffer (get-buffer buffer-name)))
    (with-current-buffer
        (or buffer (get-buffer-create buffer-name))
      (setq default-directory directory)
      (setq-local so-long--inhibited t)
      (if dont-restore-p
          (erase-buffer)
        (unless buffer
          (zenit--load-persistent-scratch-buffer project-name)
          (when (and (eq major-mode 'fundamental-mode)
                     (functionp mode))
            (funcall mode))))
      (cl-pushnew (current-buffer) zenit-scratch-buffers)
      (add-transient-hook! 'zenit-switch-buffer-hook (zenit--persist-scratch-buffers-h))
      (add-transient-hook! 'zenit-switch-window-hook (zenit--persist-scratch-buffers-h))
      (add-hook 'kill-buffer-hook #'zenit-persist-scratch-buffer-h nil 'local)
      (add-hook 'zenit-scratch-buffer-hook #'zenit-mark-buffer-as-real-h)
      (run-hooks 'zenit-scratch-buffer-hook)
      (current-buffer))))


;;
;;; Persistent scratch buffer

;;;###autoload
(defun zenit-persist-scratch-buffer-h ()
  "Save the current buffer to `zenit-scratch-dir'."
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
        (point (point))
        (mode major-mode))
    (with-temp-file
        (expand-file-name (concat (or zenit-scratch-current-project
                                      zenit-scratch-default-file)
                                  ".el")
                          zenit-scratch-dir)
      (prin1 (list content
                   point
                   mode)
             (current-buffer)))))

;;;###autoload
(defun zenit--persist-scratch-buffers-h ()
  "Save all scratch buffers to `zenit-scratch-dir'."
  (setq zenit-scratch-buffers
        (cl-delete-if-not #'buffer-live-p zenit-scratch-buffers))
  (dolist (buffer zenit-scratch-buffers)
    (with-current-buffer buffer
      (zenit-persist-scratch-buffer-h))))

;;;###autoload
(defun zenit-persist-scratch-buffers-after-switch-h ()
  "Kill scratch buffers when they are no longer visible, saving them to disk."
  (unless (cl-some #'get-buffer-window zenit-scratch-buffers)
    (mapc #'kill-buffer zenit-scratch-buffers)
    (remove-hook 'zenit-switch-buffer-hook #'zenit-persist-scratch-buffers-after-switch-h)))

;;;###autoload
(unless noninteractive
  (add-hook 'kill-emacs-hook #'zenit--persist-scratch-buffers-h))


;;
;;; Commands

(defvar projectile-enable-caching)
;;;###autoload
(defun zenit/open-scratch-buffer (&optional arg project-p same-window-p)
  "Pop up a persistent scratch buffer.
If passed the prefix ARG, do not restore the last scratch buffer.
If PROJECT-P is non-nil, open a persistent scratch buffer associated with the
  current project."
  (interactive "P")
  (let (projectile-enable-caching)
    (funcall
     (if same-window-p
         #'switch-to-buffer
       #'pop-to-buffer)
     (zenit-scratch-buffer
      arg
      (cond ((eq zenit-scratch-initial-major-mode t)
             (unless (or buffer-read-only
                         (derived-mode-p 'special-mode)
                         (string-match-p "^ ?\\*" (buffer-name)))
               major-mode))
            ((null zenit-scratch-initial-major-mode)
             nil)
            ((symbolp zenit-scratch-initial-major-mode)
             zenit-scratch-initial-major-mode))
      default-directory
      (when project-p
        (zenit-project-name))))))

;;;###autoload
(defun zenit/switch-to-scratch-buffer (&optional arg project-p)
  "Like `zenit/open-scratch-buffer', but switches to it in the current window.
If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (zenit/open-scratch-buffer arg project-p 'same-window))

;;;###autoload
(defun zenit/open-project-scratch-buffer (&optional arg same-window-p)
  "Opens the (persistent) project scratch buffer in a popup.
If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (zenit/open-scratch-buffer arg 'project same-window-p))

;;;###autoload
(defun zenit/switch-to-project-scratch-buffer (&optional arg)
  "Like `zenit/open-project-scratch-buffer', but switches to it in the current
window.
If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (zenit/open-project-scratch-buffer arg 'same-window))

;;;###autoload
(defun zenit/revert-scratch-buffer ()
  "Revert scratch buffer to last persistent state."
  (interactive)
  (unless (string-match-p "^\\*zenit:scratch" (buffer-name))
    (user-error "Not in a scratch buffer"))
  (when (zenit--load-persistent-scratch-buffer zenit-scratch-current-project)
    (message "Reloaded scratch buffer")))

;;;###autoload
(defun zenit/delete-persistent-scratch-file (&optional arg)
  "Deletes a scratch buffer file in `zenit-scratch-dir'.
If prefix ARG, delete all persistent scratches."
  (interactive)
  (if arg
      (progn
        (delete-directory zenit-scratch-dir t)
        (message "Cleared %S" (abbreviate-file-name zenit-scratch-dir)))
    (make-directory zenit-scratch-dir t)
    (let ((file (read-file-name "Delete scratch file > " zenit-scratch-dir "scratch")))
      (if (not (file-exists-p file))
          (message "%S does not exist" (abbreviate-file-name file))
        (delete-file file)
        (message "Successfully deleted %S" (abbreviate-file-name file))))))

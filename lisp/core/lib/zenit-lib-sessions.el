;; lisp/core/lib/zenit-lib-sessions.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

;; `bufferlo'
(declare-function bufferlo-bookmark-frame-load "ext:bufferlo")
(declare-function bufferlo-mode "ext:bufferlo")
(defvar bufferlo-mode)

;; `desktop'
(declare-function desktop-full-file-name "desktop")
(defvar desktop-base-file-name)
(defvar desktop-dirname)
(defvar desktop-file-modtime)
(defvar desktop-restore-eager)

;; `restart-emacs'
(declare-function restart-emacs--restore-frames-using-desktop "ext:restart-emacs")

;; `ui/workspaces'
(defvar +workspaces-autosave)
(defvar +workspaces-autosave-file)
(defvar +workspaces-bookmark-alist)
(defvar +workspaces-save-directory)

;; `zenit-modules'
(declare-function zenit-module-p "zenit-modules")


;;
;;; Helpers

;;;###autoload
(defun zenit-session-file (&optional name)
  "Return the session file path for the current session backend.

The function supports `bufferlo-mode' and `desktop' as session
backends. If NAME is provided, it is used as the filename for the
session file. Otherwise, the default file names for the
respective backends are used.

When `bufferlo-mode' is available, the file path is constructed
using `zenit-data-dir'. When `desktop' is available, the file
path is constructed using `desktop-full-file-name' and optionally
NAME.

If neither `bufferlo-mode' nor `desktop' is available, the
function signals an error."
  (cond ((and (zenit-module-p :ui 'workspaces)
              (require 'bufferlo nil t))
         (if name
             (expand-file-name name +workspaces-save-directory)
           (expand-file-name +workspaces-autosave-file +workspaces-save-directory)))
        ((require 'desktop nil t)
         (if name
             (expand-file-name name (file-name-directory (desktop-full-file-name)))
           (desktop-full-file-name)))
        ((error "No session backend available"))))

;;;###autoload
(defun zenit-save-session (&optional file)
  "Save the current session state to a file using the available
session backend.

The function supports `bufferlo-mode' and `desktop' as session
backends. If FILE is provided, it is used as the target file for
saving the session state. Otherwise, the default file names for
the respective backends are used.

When `bufferlo-mode' is available, the session state is saved
using `bufferlo-bookmark-tab-save' with the specified FILE. When
`desktop' is available, the session state is saved using
`desktop-save' with the specified FILE, and the `frameset' and
`restart-emacs' packages are used to enhance the session state.

If neither `bufferlo-mode' nor `desktop' is available, the
function signals an error."
  (setq file (expand-file-name (or file (zenit-session-file))))
  (cond ((and (zenit-module-p :ui 'workspaces)
              (require 'bufferlo nil t))
         (unless bufferlo-mode (bufferlo-mode +1))
         (setq +workspaces-autosave nil)
         (+workspace-save-session file))
        ((and (require 'frameset nil t)
              (require 'restart-emacs nil t))
         (let ((frameset-filter-alist (append '((client . restart-emacs--record-tty-file))
                                              frameset-filter-alist))
               (desktop-base-file-name (file-name-nondirectory file))
               (desktop-dirname (file-name-directory file))
               (desktop-restore-eager t)
               desktop-file-modtime)
           (make-directory desktop-dirname t)
           ;; Prevents confirmation prompts
           (let ((desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name)))))
             (desktop-save desktop-dirname t))))
        ((error "No session backend to save session with"))))

;;;###autoload
(defun zenit-load-session (&optional file)
  "Load a session state from a file using the available session
backend.

The function supports `bufferlo-mode' and `desktop' as session
backends. If FILE is provided, it is used as the source file for
loading the session state. Otherwise, the default file names for
the respective backends are used.

When `bufferlo-mode' is available, the session state is loaded
using `bufferlo-bookmark-tab-load' with the specified FILE. When
`desktop' is available, the session state is loaded using
`restart-emacs--restore-frames-using-desktop' with the specified
FILE.

If neither `bufferlo-mode' nor `desktop' is available, the function
signals an error."
  (setq file (expand-file-name (or file (zenit-session-file))))
  (message "Attempting to load %s" file)
  (cond ((and (zenit-module-p :ui 'workspaces)
              (require 'bufferlo nil t))
         (unless bufferlo-mode (bufferlo-mode +1))
         (let ((bookmark-alist nil))
           (bookmark-load file t)
           (setq +workspaces-bookmark-alist bookmark-alist)
           (if (length= bookmark-alist 0)
               (user-error "No session found to restore")
             (dolist (frame (nreverse (mapcar 'car bookmark-alist)))
               (let* ((bm-alist (alist-get frame +workspaces-bookmark-alist nil nil #'equal))
                      (allowed (delete "" (mapcar (lambda (tab) (alist-get 'tab-name tab))
                                                  (alist-get 'tabs bm-alist)))))
                 (unless (string= frame "frame-1")
                   (select-frame (make-frame)))
                 (bufferlo-bookmark-frame-load frame)
                 (cl-loop for name in (+workspace-list-names)
                          unless (member name allowed)
                          do (+workspace-kill name)))))))
        ((and (require 'frameset nil t)
              (require 'restart-emacs nil t))
         (restart-emacs--restore-frames-using-desktop file))
        ((error "No session backend to load session with"))))


;;
;;; Commands

;;;###autoload
(defun zenit/quickload-session (&optional force)
  "Load the last session saved.
If the FORCE \\[universal-argument] is provided
then no confirmation is asked."
  (interactive "P")
  (if (or force
          (yes-or-no-p "This will wipe your current session, do you want to continue? "))
      (progn (message "Restoring session...")
             (zenit-load-session)
             (message "Session restored. Welcome back."))
    (message "Session not restored.")))

;;;###autoload
(defun zenit/quicksave-session ()
  "Quickly save the current session state using the available
session backend.

This command calls `zenit-save-session' without a FILE argument,
so the default session file for the available session backend is
used."
  (interactive)
  (message "Saving session")
  (zenit-save-session)
  (message "Saving session...DONE"))

;;;###autoload
(defun zenit/load-session (file)
  "Interactively load a session state from a user-selected file.

The user is prompted to select a session file to load. This
command then calls `zenit-load-session' with the selected FILE."
  (interactive
   (let ((session-file (zenit-session-file)))
     (list (or (read-file-name "Session to restore: "
                               (file-name-directory session-file)
                               nil t
                               (file-name-nondirectory session-file))
               (user-error "No session selected. Aborting")))))
  (unless file
    (error "No session file selected"))
  (message "Loading '%s' session" file)
  (zenit-load-session file)
  (message "Session restored. Welcome back."))


;;;###autoload
(defun zenit/save-session (file)
  "Interactively save the current session state to a user-selected
file.

The user is prompted to select a session file to save to. This
command then calls `zenit-save-session' with the selected FILE."
  (interactive
   (let ((session-file (zenit-session-file)))
     (list (or (read-file-name "Save session to: "
                               (file-name-directory session-file)
                               nil nil
                               (file-name-nondirectory session-file))
               (user-error "No session selected. Aborting")))))
  (unless file
    (error "No session file selected"))
  (message "Saving '%s' session" file)
  (zenit-save-session file))

;; NOTE 2023-02-05: Emacs 29+ includes its own `restart-emacs' command which has
;;   less features. Emacs lacks namespaces so we need to require `restart-emacs'
;;   after `files' has loaded.
(eval-and-compile
  (with-eval-after-load 'files
    (require 'restart-emacs nil t)))
;;;###autoload
(defalias 'zenit/restart #'restart-emacs)

;;;###autoload
(defalias 'zenit/new-emacs #'restart-emacs-start-new-emacs)

;;;###autoload
(defun zenit/restart-and-restore (&optional debug)
  "Restart Emacs (and the daemon, if active).
If DEBUG (the prefix arg) is given, start the new instance with
the --debug switch."
  (interactive "P")
  (zenit/quicksave-session)
  (save-some-buffers nil t)
  (letf! ((#'save-buffers-kill-emacs #'kill-emacs)
          (confirm-kill-emacs)
          (tmpfile (make-temp-file "post-load")))
    ;; HACK 2023-02-05: `restart-emacs' does not properly escape arguments on
    ;;   Windows (in `restart-emacs--daemon-on-windows' and
    ;;   `restart-emacs--start-gui-on-windows'), so don't give it complex
    ;;   arguments at all. Should be fixed upstream, but restart-emacs seems to
    ;;   be unmaintained.
    (with-temp-file tmpfile
      (print `(progn (add-hook 'window-setup-hook #'zenit-load-session 100)
                     ;; (add-transient-hook! 'zenit-after-init-hook (dolist (buf (buffer-list))
                     ;;                                               (with-current-buffer buf
                     ;;                                                 (set-auto-mode t))))
                     (delete-file ,tmpfile))
             (current-buffer)))
    (restart-emacs
     (append (if debug (list "--debug-init"))
             (when (boundp 'chemacs-current-emacs-profile)
               (list "--with-profile" chemacs-current-emacs-profile))
             (list "-l" tmpfile)))))

(provide 'zenit-lib '(sessions))

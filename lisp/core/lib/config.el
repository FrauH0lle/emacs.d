;; lisp/core/lib/config.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar zenit-reload-hook nil
  "A list of hooks to run when `zenit/reload' is called.")

;;;###autoload
(defvar zenit-reloading-p nil
  "TODO")

;;;###autoload
(defun zenit/open-local-config ()
  "Browse your `zenit-local-conf-dir'."
  (interactive)
  (unless (file-directory-p zenit-local-conf-dir)
    (make-directory zenit-local-conf-dir t))
  (zenit-project-browse zenit-local-conf-dir))

;;;###autoload
(defun zenit/find-file-in-local-config ()
  "Search for a file in `zenit-local-conf-dir'."
  (interactive)
  (zenit-project-find-file zenit-local-conf-dir))

;;;###autoload
(defun zenit/goto-local-init-file ()
  "Open your local init.el file and jumps to the `modules!'
block."
  (interactive)
  (find-file (expand-file-name "init.el" zenit-local-conf-dir))
  (goto-char
   (or (save-excursion
         (goto-char (point-min))
         (search-forward "(modules!" nil t))
       (point))))

;;;###autoload
(defun zenit/goto-local-config-file ()
  "Open your private config.el file."
  (interactive)
  (find-file (expand-file-name "config.el" zenit-local-conf-dir)))

;;;###autoload
(defun zenit/goto-local-packages-file ()
  "Open your private packages.el file."
  (interactive)
  (find-file (expand-file-name "packages.el" zenit-local-conf-dir)))

;;;###autoload
(defun zenit/open-in-magit ()
  "Open magit in .emacs.d."
  (interactive)
  (if (fboundp 'magit-status)
      (magit-status zenit-emacs-dir)
    (user-error "Module :tools magit not installed")))

;;;###autoload
(defun zenit/search-in-emacsd (&optional arg)
  "Conduct a text search in the current project root.
If prefix ARG is set, include ignored/hidden files."
  (interactive "P")
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (current-prefix-arg (unless (eq arg 'other) arg))
         (default-directory zenit-emacs-dir))
    (call-interactively
     (cond ((modulep! :completion vertico) #'+vertico/project-search-from-cwd)
           (#'projectile-ripgrep)))))

;;;###autoload
(defun zenit/browse-emacsd ()
  "Browse files from `zenit-emacs-dir'."
  (interactive) (zenit-project-browse zenit-emacs-dir))

;;;###autoload
(defun zenit/find-in-emacsd ()
  "Find a file under `zenit-emacs-dir', recursively."
  (interactive) (zenit-project-find-file zenit-emacs-dir))

;;;###autoload
(defun zenit/insert-date (&optional arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d.%m.%Y")
            (format-time-string "%Y-%m-%d"))))

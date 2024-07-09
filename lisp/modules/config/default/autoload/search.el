;; config/default/autoload/search.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +default/search-cwd (&optional arg)
  "Conduct a text search in files under the current folder.
If prefix ARG is set, prompt for a directory to search from."
  (interactive "P")
  (let ((default-directory
          (if arg
              (read-directory-name "Search directory: ")
            default-directory)))
    (call-interactively
     (cond ((modulep! :completion vertico) #'+vertico/project-search-from-cwd)
           (#'rgrep)))))

;;;###autoload
(defun +default/search-other-cwd ()
  "Conduct a text search in another directory."
  (interactive)
  (+default/search-cwd 'other))

;;;###autoload
(defun +default/search-emacsd ()
  "Conduct a text search in files under `zenit-emacs-dir'."
  (interactive)
  (let ((default-directory zenit-emacs-dir))
    (call-interactively
     (cond ((modulep! :completion vertico) #'+vertico/project-search-from-cwd)
           (#'rgrep)))))

;;;###autoload
(defun +default/search-buffer ()
  "Conduct a text search on the current buffer.

If a selection is active and multi-line, perform a search
restricted to that region.

If a selection is active and not multi-line, use the selection as
the initial input and search the whole buffer for it."
  (interactive)
  (let (start end multiline-p)
    (save-restriction
      (when (region-active-p)
        (setq start (region-beginning)
              end   (region-end)
              multiline-p (/= (line-number-at-pos start)
                              (line-number-at-pos end)))
        (deactivate-mark)
        (when multiline-p
          (narrow-to-region start end)))
      (cond ((modulep! :completion vertico)
             (if (and start end (not multiline-p))
                 (consult-line
                  (replace-regexp-in-string
                   " " "\\\\ "
                   (zenit-pcre-quote
                    (buffer-substring-no-properties start end))))
               (call-interactively #'consult-line)))))))

;;;###autoload
(defun +default/search-project (&optional arg)
  "Conduct a text search in the current project root.
If prefix ARG is set, include ignored/hidden files."
  (interactive "P")
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (current-prefix-arg (unless (eq arg 'other) arg))
         (default-directory
           (if (eq arg 'other)
               (if-let (projects (projectile-relevant-known-projects))
                   (completing-read "Search project: " projects nil t)
                 (user-error "There are no known projects"))
             default-directory)))
    (call-interactively
     (cond ((modulep! :completion vertico) #'+vertico/project-search)
           (#'projectile-ripgrep)))))

;;;###autoload
(defun +default/search-other-project ()
  "Conduct a text search in a known project."
  (interactive)
  (+default/search-project 'other))

;;;###autoload
(defun +default/search-project-for-symbol-at-point (symbol dir)
  "Search current project for symbol at point.
If prefix ARG is set, prompt for a known project to search from."
  (interactive
   (list (zenit-pcre-quote (or (zenit-thing-at-point-or-region) ""))
         (let ((projectile-project-root nil))
           (if current-prefix-arg
               (if-let (projects (projectile-relevant-known-projects))
                   (completing-read "Search project: " projects nil t)
                 (user-error "There are no known projects"))
             (zenit-project-root default-directory)))))
  (cond ((modulep! :completion ivy)
         (+ivy/project-search nil symbol dir))
        ((modulep! :completion helm)
         (+helm/project-search nil symbol dir))
        ((modulep! :completion vertico)
         (+vertico/project-search nil symbol dir))
        ((rgrep (regexp-quote symbol)))))

;;;###autoload
(defun +default/search-notes-for-symbol-at-point (symbol)
  "Conduct a text search in the current project for symbol at point. If prefix
ARG is set, prompt for a known project to search from."
  (interactive
   (list (zenit-pcre-quote (or (zenit-thing-at-point-or-region) ""))))
  (require 'org)
  (+default/search-project-for-symbol-at-point
   symbol org-directory))

;;;###autoload
(defun +default/org-notes-search (query)
  "Perform a text search on `org-directory'."
  (interactive
   (list (if (zenit-region-active-p)
             (buffer-substring-no-properties
              (zenit-region-beginning)
              (zenit-region-end))
           "")))
  (require 'org)
  (+default/search-project-for-symbol-at-point
   query org-directory))

;;;###autoload
(defun +default/org-notes-headlines ()
  "Jump to an Org headline in `org-agenda-files'."
  (interactive)
  (zenit-completing-read-org-headings
   "Jump to org headline: " org-agenda-files
   :depth 3
   :include-files t))

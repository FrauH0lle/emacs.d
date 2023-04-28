;; lisp/core/lib/packages.el -*- lexical-binding: t; -*-

(mapc (zenit-rpartial #'load nil (not init-file-debug) 'nosuffix)
      (file-expand-wildcards (concat zenit-core-dir "cli/*.el")))

;;;###autoload
(defun zenit/search-versions ()
  (interactive)
  (let ((default-directory (concat zenit-emacs-dir "straight/versions")))
    (funcall-interactively #'+default/search-cwd)))

;;;###autoload
(defun zenit/find-duplicate-package-versions ()
  "Search for duplicate package versions and display them in a
seperate buffer."
  (interactive)
  (pp-display-expression
   (let ((versions nil)
         (duplicates nil)
         (last-version nil))
     (dolist (spec straight-profiles)
       (cl-destructuring-bind (_profile . lockfile) spec
         (let ((versions-alist (straight--lockfile-read lockfile)))
           (dolist (spec versions-alist)
             (cl-destructuring-bind (local-repo . commit) spec
               (when-let ((dup (alist-get local-repo versions nil nil #'equal)))
                 (let ((old (or (alist-get local-repo duplicates nil nil #'equal)
                                (alist-get local-repo last-version nil nil #'equal))))
                   (setq duplicates (straight--alist-set
                                     local-repo (straight--alist-set lockfile commit old) duplicates))))
               (let ((entry (cons lockfile commit)))
                 (setq last-version (straight--alist-set local-repo (list entry) last-version)))
               (setq versions (straight--alist-set
                               local-repo commit versions)))))))
     duplicates)
   "*zenit-duplicate-packages*"))

;;;###autoload
(defun zenit/update-package ()
  "Updates a single package."
  (interactive)
  (print! (start "Updating package (this may take a while)..."))
  (call-interactively #'straight-pull-package)
  (print! (start "Exit and run 'make referesh'!")))

;;;###autoload
(defun zenit/update-all-packages ()
  "Updates all packages and forces a rebuild of all packages."
  (interactive)
  (require 'init-cli)
  (let* ((evil-collection-mode-list nil)
         (default-directory zenit-emacs-dir)
         (buf (get-buffer-create " *emacs*"))
         (zenit-format-backend 'ansi)
         (ignore-window-parameters t)
         (noninteractive t)
         (noninteractive nil)
         (standard-output
          (lambda (char)
            (with-current-buffer buf
              (insert char)
              (when (memq char '(?\n ?\r))
                (require 'ansi-color)
                (ansi-color-apply-on-region (line-beginning-position -1) (line-end-position))
                (redisplay))))))
    (with-current-buffer (switch-to-buffer buf)
      (erase-buffer)

      (print! (start "Updating packages (this may take a while)..."))
      (straight-x-pull-all)
      (zenit-cli-packages-build t)))
  (print! (start "Exit and run 'make referesh'!")))

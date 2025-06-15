;; lisp/core/lib/zenit-lib-projects.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

;; `zenit-lib-files'
(declare-function zenit-path "zenit-lib-files" (&rest segments))


;;;###autoload (defvar projectile-project-root nil)
;;;###autoload (defvar projectile-enable-caching (not noninteractive))
;;;###autoload (defvar projectile-require-project-root 'prompt)

;;;###autoload
(cl-defun set-project-type! (name &key predicate compile run test configure dir)
  "Add a project type to `projectile-project-type'."
  (declare (indent 1))
  (after! projectile
    (add-to-list 'projectile-project-types
                 (list name
                       'marker-files predicate
                       'compilation-dir dir
                       'configure-command configure
                       'compile-command compile
                       'test-command test
                       'run-command run))))


;;
;;; Macros

;;;###autoload
(defmacro project-file-exists-p! (files &optional base-directory)
  "Checks if FILES exist at the current project's root.
The project's root is determined by `projectile', starting from
BASE-DIRECTORY (defaults to `default-directory'). FILES are paths
relative to the project root, unless they begin with a slash."
  `(file-exists-p! ,files (zenit-project-root ,base-directory)))

;;
;;; Commands

;;;###autoload
(defun zenit/find-file-in-other-project (project-root)
  "Preforms `projectile-find-file' in a known project of your
choosing."
  (interactive
   (list
    (completing-read "Find file in project: " (projectile-relevant-known-projects))))
  (unless (file-directory-p project-root)
    (error "Project directory '%s' doesn't exist" project-root))
  (zenit-project-find-file project-root))

;;;###autoload
(defun zenit/browse-in-other-project (project-root)
  "Preforms `find-file' in a known project of your choosing."
  (interactive
   (list
    (completing-read "Browse in project: " (projectile-relevant-known-projects))))
  (unless (file-directory-p project-root)
    (error "Project directory '%s' doesn't exist" project-root))
  (zenit-project-browse project-root))

;;;###autoload
(defun zenit/add-directory-as-project (dir)
  "Register an arbitrary directory as a project.

Unlike `projectile-add-known-project', if DIR isn't a valid
project, a .project file will be created within it so that it
will always be treated as one. This command will throw an error
if a parent of DIR is a valid project (which would mask DIR)."
  (interactive "D")
  (when-let* ((proj-dir (zenit-project-root dir)))
    (if (file-equal-p proj-dir dir)
        (user-error "ERROR: Directory is already a project: %s" proj-dir)
      (user-error "ERROR: Directory is already inside another project: %s" proj-dir)))
  (let ((short-dir (abbreviate-file-name dir)))
    (when (projectile-ignored-project-p dir)
      (user-error "ERROR: Directory is in projectile's ignore list: %s" short-dir))
    (dolist (proj projectile-known-projects)
      (when (file-in-directory-p proj dir)
        (user-error "ERROR: Directory contains a known project: %s" short-dir))
      (when (file-equal-p proj dir)
        (user-error "ERROR: Directory is already a known project: %s" short-dir)))
    (with-temp-file (zenit-path dir ".project"))
    (message "Added directory as a project: %s" short-dir)
    (projectile-add-known-project dir)))


;;
;;; Library

;;;###autoload
(defun zenit-project-p (&optional dir)
  "Return t if DIR (defaults to `default-directory') is a valid
project."
  (and (zenit-project-root dir)
       t))

;;;###autoload
(defun zenit-project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root
         (unless dir (bound-and-true-p projectile-project-root)))
        projectile-require-project-root)
    (projectile-project-root dir)))

;;;###autoload
(defun zenit-project-name (&optional dir)
  "Return the name of the current project."
  (if-let* ((project-root (or (zenit-project-root dir)
                              (if dir (expand-file-name dir)))))
      (funcall projectile-project-name-function project-root)
    "-"))

;;;###autoload
(defun zenit-project-expand (name &optional dir)
  "Expand NAME to project root."
  (expand-file-name name (zenit-project-root dir)))

;;;###autoload
(defun zenit-project-find-file (dir)
  "Jump to a file in DIR (searched recursively).
If DIR is not a project, it will be indexed (but not cached)."
  (unless (file-directory-p dir)
    (error "Directory %S does not exist" dir))
  (unless (file-readable-p dir)
    (error "Directory %S isn't readable" dir))
  (let* ((default-directory (file-truename dir))
         (projectile-project-root (zenit-project-root dir))
         (projectile-enable-caching projectile-enable-caching))
    (cond ((and projectile-project-root (file-equal-p projectile-project-root default-directory))
           (unless (zenit-project-p default-directory)
             ;; Disable caching if this is not a real project; caching
             ;; non-projects easily has the potential to inflate the projectile
             ;; cache beyond reason.
             (setq projectile-enable-caching nil))
           (call-interactively
            #'projectile-find-file))
          ((when-let* ((project-current-directory-override dir)
                       (pr (project-current t dir)))
             (condition-case _
                 (project-find-file-in nil (list dir) pr t)
               ;; project.el throws errors if DIR is an empty directory, which
               ;; is poor UX.
               (wrong-type-argument
                (call-interactively #'find-file)))))
          ((call-interactively #'find-file)))))

;;;###autoload
(defun zenit-project-browse (dir)
  "Traverse a file structure starting linearly from DIR."
  (let ((default-directory (file-truename (expand-file-name dir))))
    (call-interactively #'find-file)))

;;;###autoload
(defun zenit-project-ignored-p (project-root)
  "Return non-nil if remote or temporary file, or a straight
package."
  (unless (file-remote-p project-root)
    (or (file-in-directory-p project-root temporary-file-directory)
        (file-in-directory-p project-root (concat zenit-emacs-dir "straight"))
        (file-in-directory-p project-root zenit-local-dir))))

(provide 'zenit-lib '(projects))

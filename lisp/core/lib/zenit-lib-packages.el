;; lisp/core/lib/zenit-lib-packages.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

;; `ansi-color'
(declare-function ansi-color-apply-on-region "ansi-color")

;; `straight'
(declare-function straight--alist-set "ext:straight")
(declare-function straight--lockfile-read "ext:straight")
(defvar straight-profiles)


;; (mapc (zenit-rpartial #'load nil (not init-file-debug) 'nosuffix)
;;       (file-expand-wildcards (concat zenit-core-dir "cli/*.el")))

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

(provide 'zenit-lib '(packages))

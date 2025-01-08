;; lisp/core/lib/zenit-lib-packages.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

;; `cl-seq'
(declare-function cl-remove-if-not "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `jka-compr'
(declare-function jka-compr-insert-file-contents "jka-compr" (file &optional visit beg end replace))

;; `package'
(declare-function package-desc-archive "package" t t)
(defvar package-archive)
(defvar package-archives)
(defvar package-archive-contents)

;; `straight'
(declare-function straight--alist-set "ext:straight" (key val alist &optional symbol))
(declare-function straight--lockfile-read "ext:straight" (lockfile))
(defvar straight-profiles)
(defvar straight--recipe-cache)
(cl-eval-when (compile)
  (autoload #'straight--process-with-result "straight" nil nil 'macro)
  (autoload #'straight--with-plist "straight" nil nil 'macro))

;; `vertico'
(defvar vertico-sort-function)

;; `zenit-modules'
(declare-function zenit-module-list "zenit-modules" (&optional paths-or-all initorder?))

;; `zenit-packages'
(declare-function zenit-initialize-packages "zenit-packages" (&optional force-p))
(declare-function zenit-packages-get-lockfile "zenit-packages" (profile))
(declare-function zenit-package-list "zenit-packages" (&optional module-list))

;; `config/default'
(declare-function +default/search-cwd "../../modules/config/default/autoload/search.el" (&optional arg))


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
  (zenit-initialize-packages)
  (pp-display-expression
   (let ((versions nil)
         (duplicates nil)
         (last-version nil))
     (dolist (spec straight-profiles)
       (cl-destructuring-bind (_profile . lockfile) spec
         (let ((versions-alist (straight--lockfile-read lockfile)))
           (dolist (spec versions-alist)
             (cl-destructuring-bind (local-repo . commit) spec
               (when-let* ((dup (alist-get local-repo versions nil nil #'equal)))
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


;;
;;; Bump commands

;;;###autoload
(defun zenit/bump-package (package &optional commit)
  "Bump PACKAGE in all modules that install it.

If COMMIT is nil, it will prompt the user for the new commit in
the minibuffer. When used with the universal argument, the user
can specify the commit directly. Otherwise the last 50 commits
will used as candidates."
  (interactive
   (list (intern (completing-read "Bump package: "
                                  (mapcar #'car (zenit-package-list 'all))))))
  (zenit-initialize-packages)
  (let* ((packages (zenit-package-list 'all))
         (modules (plist-get (alist-get package packages) :modules))
         (lockfiles (delq nil (mapcar #'zenit-packages-get-lockfile (plist-get (alist-get package packages) :lockfile))))
         (recipe (gethash (symbol-name package) straight--recipe-cache))
         (local-repo (plist-get (gethash (symbol-name package) straight--recipe-cache) :local-repo)))
    (cl-block nil
      (unless local-repo
        (cl-return (message "The package %s isn't installed" package)))
      (unless modules
        (cl-return (message "The package %s isn't installed by any module" package)))
      (unless lockfiles
        (cl-return (message "The package %s does not have a lockfile and is probably built-in" package)))
      (unless commit
        (setq commit
              (if current-prefix-arg
                  (read-from-minibuffer (format "New commit for %s (leave empty to keep current): " package))
                (if-let* ((default-directory (straight--repos-dir local-repo))
                          (fetched (straight-vc-fetch-from-remote recipe)))
                    (let* ((candidates (straight--process-with-result
                                           (straight--process-run
                                            "git" "log" "FETCH_HEAD" "--oneline" "--format=\"%h (%as) %s | %H\"" "-n" "50")
                                         (if success
                                             (let* ((output (string-trim-right (or stdout "")))
                                                    (lines (split-string output "\n")))
                                               (mapcar (lambda (x) (split-string (string-replace "\"" "" x) "|" t "[ ]+")) lines))
                                           (format "ERROR: Couldn't collect commit list because: %s" stderr))))
                           vertico-sort-function
                           (choice (completing-read (format "New commit for %s (leave empty to keep current): " package) candidates)))
                      (or (car (alist-get choice candidates nil nil #'equal)) ""))
                  ""))))
      (dolist (lockfile lockfiles)
        (when-let* ((pin-list (straight--lockfile-read lockfile)))
          (let* ((old-commit (alist-get local-repo pin-list nil nil #'equal))
                 (new-commit (if (string-blank-p commit) old-commit commit)))
            (if (null new-commit)
                (user-error "No commit specified")
              (setf (alist-get local-repo pin-list nil nil #'equal) new-commit))
            (let ((kw (with-temp-buffer
                        (insert-file-contents lockfile)
                        (goto-char (point-max))
                        (let (match)
                          (when (re-search-backward "^\\(:.+\\)$" nil t)
                            (setq match (match-string 1)))
                          match))))
              (with-temp-file lockfile
                (insert
                 (format
                  "(%s)\n%s\n"
                  (mapconcat
                   (apply-partially #'format "%S")
                   pin-list
                   "\n ")
                  kw))))))))))

;;;###autoload
(defun zenit/bump-module (category &optional module)
  "Bump packages in CATEGORY MODULE.
If prefix arg is non-nil, prompt you to choose a specific commit
for each package."
  (interactive
   (let* ((module (completing-read
                   "Bump module: "
                   (let ((modules (zenit-module-list 'all)))
                     (mapcar (lambda (m)
                               (if (listp m)
                                   (format "%s %s" (car m) (cdr m))
                                 (format "%s" m)))
                             (append (delete-dups (mapcar #'car modules))
                                     modules)))
                   nil t nil nil))
          (module (split-string module " " t)))
     (list (intern (car module))
           (ignore-errors (intern (cadr module))))))
  (mapc (lambda! ((cat . mod))
          (if-let* ((packages (zenit-package-list (list (cons cat mod)))))
              (dolist (package packages)
                (let ((current-prefix-arg current-prefix-arg))
                  (zenit/bump-package (car package))))
            (message "Module %s has no packages" (cons cat mod))))
        (if module
            (list (cons category module))
          (cl-remove-if-not (lambda (m) (eq (car m) category))
                            (zenit-module-list 'all)))))


;;
;;; Package metadata

;;;###autoload
(defun zenit-package-homepage (package)
  "Return the url to PACKAGE's homepage (usually a repo)."
  (zenit-initialize-packages)
  (or (get package 'homepage)
      (put package 'homepage
           (cond ((when-let* ((location (locate-library (symbol-name package))))
                    (with-temp-buffer
                      (if (string-match-p "\\.gz$" location)
                          (jka-compr-insert-file-contents location)
                        (insert-file-contents (concat (file-name-sans-extension location) ".el")
                                              nil 0 4096))
                      (let ((case-fold-search t))
                        (when (re-search-forward " \\(?:URL\\|homepage\\|Website\\): \\(http[^\n]+\\)\n" nil t)
                          (match-string-no-properties 1))))))
                 ((when-let* ((recipe (straight-recipes-retrieve package)))
                    (straight--with-plist (straight--convert-recipe recipe)
                        (host repo)
                      (pcase host
                        (`github (format "https://github.com/%s" repo))
                        (`gitlab (format "https://gitlab.com/%s" repo))
                        (`bitbucket (format "https://bitbucket.com/%s" repo))
                        (`git repo)
                        (_ nil)))))
                 ((or package-archive-contents
                      (progn (package-refresh-contents)
                             package-archive-contents))
                  (pcase (ignore-errors (package-desc-archive (cadr (assq package package-archive-contents))))
                    (`nil nil)
                    ("org" "https://orgmode.org")
                    ((or "melpa" "melpa-mirror")
                     (format "https://melpa.org/#/%s" package))
                    ("gnu"
                     (format "https://elpa.gnu.org/packages/%s.html" package))
                    (archive
                     (if-let* ((src (cdr (assoc package package-archives))))
                         (format "%s" src)
                       (user-error "%S isn't installed through any known source (%s)"
                                   package archive)))))
                 ((user-error "Can't get homepage for %S package" package))))))

(provide 'zenit-lib '(packages))

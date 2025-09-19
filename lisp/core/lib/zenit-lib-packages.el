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
(declare-function straight-vc-get-commit "ext:straight" (type local-repo))
(defvar straight-profiles)
(defvar straight-recipe-repositories)
(defvar straight--recipe-cache)
(defvar straight--repo-cache)
(cl-eval-when (compile)
  (autoload #'straight--process-with-result "straight" nil nil 'macro)
  (autoload #'straight--with-plist "straight" nil nil 'macro))

;; `vertico'
(defvar vertico-sort-function)

;; `zenit-cli'
(declare-function zenit--cli-recipes-update "../cli/packages" ())
(cl-eval-when (compile)
  (autoload #'zenit--with-package-recipes (file-name-concat zenit-core-dir "cli" "packages") nil nil 'macro))

;; `zenit-packages'
(declare-function +straight--get-lockfile-version-id "zenit-packages" ())
(declare-function +straight--update-all-lockfile-version "zenit-packages" ())
(declare-function zenit-initialize-packages "zenit-packages" (&optional force-p))
(declare-function zenit-package-list "zenit-packages" (&optional module-list))
(defvar +straight--lockfile-prefer-local-conf-versions-p)
(defvar zenit-packages)

;; `config/default'
(declare-function +default/search-cwd "../../modules/config/default/autoload/search.el" (&optional arg))


;;;###autoload
(defun zenit/search-versions ()
  "Search through package versions in straight's versions directory."
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

(autoload #'zenit--cli-recipes-update (file-name-concat zenit-core-dir "cli" "packages"))
(defun zenit-packages--bump-recipe-repos ()
  "Update and bump recipe repositories"
  (unless (bound-and-true-p zenit--cli-updated-recipes)
    (zenit--cli-recipes-update)
    (zenit--with-package-recipes
        (delq
         nil (mapcar (zenit-rpartial #'gethash straight--repo-cache)
                     (mapcar #'symbol-name straight-recipe-repositories)))
        (type local-repo)
      (when-let* ((lockfile (alist-get 'core straight-profiles))
                  (lockfile (straight--versions-file lockfile))
                  (pin-list (straight--lockfile-read lockfile)))
        (let ((new-commit (straight-vc-get-commit type local-repo)))
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
                kw)))))))))

(autoload #'zenit-package-list "zenit-packages")
;;;###autoload
(defun zenit/bump-package (package &optional commit local-conf)
  "Bump PACKAGE in all modules that install it.

If COMMIT is nil, it will prompt the user for the new commit in the
minibuffer. When used with the universal argument, the user can specify
the commit directly. Otherwise the last 50 commits will used as
candidates.

If LOCAL-CONF is non-nil, write lockfiles into
`zenit-local-versions-dir'."
  (interactive
   (list (intern (completing-read "Bump package: "
                                  (mapcar #'car (zenit-package-list 'all))))))
  (zenit-initialize-packages)
  (zenit-packages--bump-recipe-repos)
  (cl-block nil
    (let* ((packages (zenit-package-list 'all))
           (modules (plist-get (alist-get package packages) :modules))
           (_built-in-p (when (plist-get (alist-get package packages) :built-in)
                          (cl-return (message "The package %s is built-in" package))))
           (lockfiles (delq nil (mapcar (zenit-rpartial #'alist-get straight-profiles) (plist-get (alist-get package packages) :lockfile))))
           (recipe (gethash (symbol-name package) straight--recipe-cache))
           (recipe (or recipe
                       (let ((rec (plist-get (alist-get package zenit-packages) :recipe)))
                         (if rec
                             (straight-register-package (cons package rec))
                           (straight-register-package package))
                         (gethash (symbol-name package) straight--recipe-cache)))))
      (straight-vc-git--destructure recipe
          (package local-repo branch remote host protocol repo)
        (unless modules
          (cl-return (message "The package %s isn't installed by any module" package)))
        (unless commit
          (setq commit
                (if current-prefix-arg
                    (read-from-minibuffer (format "New commit for %s (leave empty to keep current): " package))
                  (let* ((repo-dir (straight--repos-dir local-repo))
                         (temp-repo-dir (file-name-concat (temporary-file-directory) (make-temp-name local-repo)))
                         (use-temp-repo-p (or (not (file-exists-p repo-dir))
                                              (file-exists-p (file-name-concat (straight--repos-dir local-repo) ".straight-commit")))))
                    (if (not use-temp-repo-p)
                        (straight-vc-fetch-from-remote recipe)
                      ;; Do a shallow clone
                      (straight-vc-git--clone-internal
                       :depth '(1 single-branch)
                       :remote remote
                       :url (straight-vc-git--encode-url repo host protocol)
                       :repo-dir temp-repo-dir
                       :branch branch)
                      ;; Only fetch the commit metadata
                      (let ((straight--default-directory temp-repo-dir))
                        (straight--process-run "git" "fetch" "--unshallow" "--filter=tree:0" "--tags")))

                    (let* ((candidates (prog1
                                           (append
                                            ;; Collect the last 50 commits
                                            (straight--process-with-result
                                                (let ((straight--default-directory (or (and use-temp-repo-p temp-repo-dir)
                                                                                       repo-dir)))
                                                  (straight--process-run
                                                   "git" "log" "FETCH_HEAD" "--oneline" "--format=\"(%as) %h %s | %H\"" "-50"))
                                              (if success
                                                  (let* ((output (string-trim-right (or stdout "")))
                                                         (lines (split-string output "\n")))
                                                    (mapcar (lambda (x) (split-string (string-replace "\"" "" x) "|" t "[ ]+")) lines))
                                                (user-error "ERROR: Couldn't collect commit list because: %s" stderr)))
                                            ;; Collect the last 20 tags
                                            (straight--process-with-result
                                                (let ((straight--default-directory (or (and use-temp-repo-p temp-repo-dir)
                                                                                       repo-dir)))
                                                  (straight--process-run
                                                   "git" "log" "FETCH_HEAD" "--no-walk" "--tags" "--oneline" "--format=\"(%as) %h%d %s | %H\""))
                                              (if success
                                                  (let* ((output (string-trim-right (or stdout "")))
                                                         (lines (split-string output "\n"))
                                                         (lines (seq-take lines 20)))
                                                    (mapcar (lambda (x) (split-string (string-replace "\"" "" x) "|" t "[ ]+")) lines))
                                                (user-error "ERROR: Couldn't collect tag list because: %s" stderr))))
                                         (delete-directory temp-repo-dir 'recursive)))
                           vertico-sort-function
                           (current-commits (cl-loop for lockfile in lockfiles
                                                     for commit = (alist-get local-repo (straight--lockfile-read (straight--versions-file lockfile)) nil nil #'equal)
                                                     if commit
                                                     collect (cons lockfile commit)))
                           (choice (completing-read (format "New commit for %s%s(leave empty to keep current):"
                                                            package
                                                            (if (length> current-commits 0)
                                                                (concat
                                                                 "\nCurrent:\n"
                                                                 (mapconcat (lambda (pair)
                                                                              (format "\t- %s: %s" (car pair) (substring (cdr pair) 0 7)))
                                                                            current-commits
                                                                            "\n")
                                                                 "\n")
                                                              " "))
                                                    (zenit-packages--sort-candidates candidates))))
                      (or (car (alist-get choice candidates nil nil #'equal)) ""))))))

        (setq lockfiles (if local-conf
                            (if (member (alist-get 'local straight-profiles) lockfiles)
                                (cl-remove-if-not (lambda (x) (equal x (alist-get 'local straight-profiles))) lockfiles)
                              lockfiles)
                          (cl-remove-if (lambda (x) (equal x (alist-get 'local straight-profiles))) lockfiles)))
        (dolist (lockfile lockfiles)
          (+straight--update-all-lockfile-version)
          (let* ((+straight--lockfile-prefer-local-conf-versions-p local-conf)
                 (lockfile (straight--versions-file lockfile))
                 (pin-list (straight--lockfile-read lockfile))
                 (old-commit (alist-get local-repo pin-list nil nil #'equal))
                 (new-commit (if (string-blank-p commit) old-commit commit)))
            (if (null new-commit)
                (user-error "No commit specified")
              (setf (alist-get local-repo pin-list nil nil #'equal) new-commit))
            (let ((kw (+straight--get-lockfile-version-id)))
              (unless (file-exists-p lockfile)
                (make-directory (file-name-directory lockfile) 'parents))
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
(defun zenit/bump-local-conf-package (package &optional commit)
  "Bump local PACKAGE in all modules that install it.

Lockfiles will be written into `zenit-local-versions-dir'.

If COMMIT is nil, it will prompt the user for the new commit in the
minibuffer. When used with the universal argument, the user can specify
the commit directly. Otherwise the last 50 commits will used as
candidates."
  (interactive
   (list (intern (completing-read "Bump package: "
                                  (mapcar #'car (zenit-package-list 'all))))))
  (when current-prefix-arg
    (setq commit (read-from-minibuffer (format "New commit for %s (leave empty to keep current): " package))))
  (zenit/bump-package package commit 'local-conf))

(defun zenit-packages--sort-candidates (candidates)
  "Sort candidates by alpha.

Adapted from `vertico-sort-alpha'."
  (let* ((buckets (make-vector 32 nil)))
    (dolist (% candidates)
      (let ((idx (min 31 (if (equal (car %) "") 0 (/ (aref (car %) 0) 4)))))
        (aset buckets idx (cons % (aref buckets idx)))))
    (nconc
     (mapcan (lambda (bucket) (sort bucket (lambda (a b) (string> (car a) (car b)))))
             (nbutlast (append buckets nil)))
     (sort (aref buckets 31) (lambda (a b) (string> (car a) (car b)))))))

;;;###autoload
(defun zenit/bump-module (category &optional module)
  "Bump packages in CATEGORY MODULE.
If prefix arg is non-nil, prompt you to choose a specific commit for
each package."
  (interactive
   (let* ((module (completing-read
                   "Bump module: "
                   (let ((modules (zenit-module-list 'all)))
                     (mapcar (lambda (m)
                               (if (listp m)
                                   (if (cdr m)
                                       (format "%s %s" (car m) (cdr m))
                                     (format "%s" (car m)))
                                 (format "%s" m)))
                             (append (delete-dups (mapcar #'car modules))
                                     modules)))
                   nil t nil nil))
          (module (split-string module " " t)))
     (list (intern (car module))
           (ignore-errors (intern (cadr module))))))
  (zenit-initialize-packages)
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

(defun zenit/remove-undeclared-packages ()
  "Remove packages in lockfiles without a `package!' declaration."
  (interactive)
  (zenit-initialize-packages)
  (let ((declared-packages (mapcar (lambda (p)
                                     (unless (plist-get (cdr p) :built-in)
                                       (let ((name (car p)))
                                         (plist-get (straight--convert-recipe (or (straight--get-overridden-recipe name)
                                                                                  name))
                                                    :local-repo))))
                                   (zenit-package-list 'all)))
        removed)
    (dolist (spec straight-profiles removed)
      (cl-destructuring-bind (_profile . lockfile) spec
        (when-let* ((versions-alist (straight--lockfile-read lockfile))
                    (undeclared (cl-remove-if
                                 (lambda (el)
                                   (memq (intern el) straight-recipe-repositories))
                                 (cl-set-difference (mapcar #'car versions-alist) declared-packages :test #'string=))))
          (dolist (p undeclared)
            (setf (alist-get p versions-alist nil 'remove) nil)
            (push p removed))
          (let ((kw (+straight--get-lockfile-version-id))
                (lockfile (straight--versions-file lockfile)))
            (unless (file-exists-p lockfile)
              (make-directory (file-name-directory lockfile) 'parents))
            (with-temp-file lockfile
              (insert
               (format
                "(%s)\n%s\n"
                (mapconcat
                 (apply-partially #'format "%S")
                 versions-alist
                 "\n ")
                kw)))))))
    (when removed
      (message "Removed the following packages:\n - %s" (string-join removed "\n - ")))))



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

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

(defun zenit-packages--bump-prefix-action (&optional prefix)
  "Return the bump action requested by PREFIX."
  (cond ((null prefix) 'select)
        ((= (prefix-numeric-value prefix) 16) 'read)
        (t 'latest)))

(defun zenit-packages--bump-read-commit (package)
  "Prompt for a raw commit for PACKAGE."
  (read-from-minibuffer (format "New commit for %s (leave empty to keep current): " package)))

(defun zenit-packages--bump-recipe (package &optional plist)
  "Return a straight recipe for PACKAGE, using PLIST metadata when available."
  (or (gethash (symbol-name package) straight--recipe-cache)
      (let ((rec (or (plist-get plist :recipe)
                     (plist-get (alist-get package zenit-packages) :recipe))))
        (if rec
            (straight-register-package (cons package rec))
          (straight-register-package package))
        (gethash (symbol-name package) straight--recipe-cache))))

(defun zenit-packages--bump-lockfiles (plist &optional local-conf)
  "Return target lockfiles for package PLIST.

If LOCAL-CONF is non-nil, prefer the local profile lockfile when the
package declares it. Otherwise, omit the local profile lockfile. Packages
without explicit lockfile metadata use straight's default profile."
  (let* ((profiles (if (plist-member plist :lockfile)
                       (or (ensure-list (plist-get plist :lockfile)) '(nil))
                     '(nil)))
         (profiles (mapcar (lambda (profile)
                             (if (memq profile '(nil t)) nil profile))
                           profiles))
         (lockfiles (delq nil (mapcar (zenit-rpartial #'alist-get straight-profiles)
                                      profiles))))
    (if local-conf
        (if (member (alist-get 'local straight-profiles) lockfiles)
            (cl-remove-if-not (lambda (x) (equal x (alist-get 'local straight-profiles)))
                              lockfiles)
          lockfiles)
      (cl-remove-if (lambda (x) (equal x (alist-get 'local straight-profiles)))
                    lockfiles))))

(defun zenit-packages--bump-delete-temp-repo (dir)
  "Delete temporary package repo DIR without aborting the caller."
  (when (file-directory-p dir)
    (condition-case _err
        (delete-directory dir 'recursive)
      (error
       (sleep-for 0.1)
       (condition-case retry-err
           (when (file-directory-p dir)
             (delete-directory dir 'recursive))
         (error
          (message "Warning: couldn't delete temporary package repo %s: %s"
                   dir (error-message-string retry-err))))))))

(defun zenit-packages--bump-with-fetched-repo (recipe fn)
  "Fetch RECIPE metadata and call FN with the repo dir and local repo name.

If the package repo is not installed, or is pinned to a detached commit,
use a temporary shallow clone."
  (straight-vc-git--destructure recipe
      (_package local-repo branch remote host protocol repo)
    (let* ((repo-dir (straight--repos-dir local-repo))
           (temp-repo-dir (file-name-concat (temporary-file-directory) (make-temp-name local-repo)))
           (use-temp-repo-p (or (not (file-exists-p repo-dir))
                                (file-exists-p (file-name-concat repo-dir ".straight-commit")))))
      (unwind-protect
          (progn
            (if (not use-temp-repo-p)
                (straight-vc-fetch-from-remote recipe)
              ;; Do a shallow clone, then fetch only commit and tag metadata.
              (straight-vc-git--clone-internal
               :depth '(1 single-branch)
               :remote remote
               :url (straight-vc-git--encode-url repo host protocol)
               :repo-dir temp-repo-dir
               :branch branch)
              (let ((straight--default-directory temp-repo-dir))
                (straight--process-run "git" "fetch" "--unshallow" "--filter=tree:0" "--tags")))
            (funcall fn (if use-temp-repo-p temp-repo-dir repo-dir) local-repo))
        (when use-temp-repo-p
          (zenit-packages--bump-delete-temp-repo temp-repo-dir))))))

(defun zenit-packages--bump-git-log-candidates (repo-dir args error-message &optional limit)
  "Return commit candidates from running git with ARGS in REPO-DIR.

Signal ERROR-MESSAGE with stderr if git fails. If LIMIT is non-nil,
return at most LIMIT candidates."
  (straight--process-with-result
      (let ((straight--default-directory repo-dir))
        (apply #'straight--process-run "git" args))
    (if success
        (let* ((output (string-trim-right (or stdout "")))
               (lines (split-string output "\n" t))
               (lines (if limit (seq-take lines limit) lines)))
          (mapcar (lambda (x) (split-string (string-replace "\"" "" x) "|" t "[ ]+"))
                  lines))
      (user-error error-message stderr))))

(defun zenit-packages--bump-latest-commit (recipe)
  "Return the latest remote commit for RECIPE without fetching it locally."
  (straight-vc-git--destructure recipe
      (_package _local-repo branch _remote host protocol repo)
    (let ((url (straight-vc-git--encode-url repo host protocol))
          (ref (if branch (format "refs/heads/%s" branch) "HEAD")))
      (straight--process-with-result
          (straight--process-run "git" "ls-remote" url ref)
        (if success
            (if-let* ((line (car (split-string (string-trim-right (or stdout "")) "\n" t)))
                      (commit (car (split-string line "[[:space:]]+" t))))
                commit
              (user-error "ERROR: Couldn't resolve latest commit from %s %s" url ref))
          (user-error "ERROR: Couldn't resolve latest commit because: %s" stderr))))))

(defun zenit-packages--bump-select-commit (package local-repo lockfiles recipe)
  "Prompt for PACKAGE's target commit from recent commits and tags."
  (zenit-packages--bump-with-fetched-repo
   recipe
   (lambda (repo-dir _local-repo)
     (let* ((candidates (append
                         ;; Collect the last 50 commits.
                         (zenit-packages--bump-git-log-candidates
                          repo-dir
                          '("log" "FETCH_HEAD" "--oneline" "--format=\"(%as) %h %s | %H\"" "-50")
                          "ERROR: Couldn't collect commit list because: %s")
                         ;; Collect the last 20 tags.
                         (zenit-packages--bump-git-log-candidates
                          repo-dir
                          '("log" "FETCH_HEAD" "--no-walk" "--tags" "--oneline" "--format=\"(%as) %h%d %s | %H\"")
                          "ERROR: Couldn't collect tag list because: %s"
                          20)))
            vertico-sort-function
            (current-commits (cl-loop for lockfile in lockfiles
                                      for commit = (alist-get local-repo
                                                             (straight--lockfile-read (straight--versions-file lockfile))
                                                             nil nil #'equal)
                                      if commit
                                      collect (cons lockfile commit)))
            (choice (completing-read (format "New commit for %s%s(leave empty to keep current):"
                                             package
                                             (if (length> current-commits 0)
                                                 (concat
                                                  "\nCurrent:\n"
                                                  (mapconcat (lambda (pair)
                                                               (format "\t- %s: %s"
                                                                       (car pair)
                                                                       (substring (cdr pair) 0 7)))
                                                             current-commits
                                                             "\n")
                                                  "\n")
                                               " "))
                                     (zenit-packages--sort-candidates candidates))))
       (or (car (alist-get choice candidates nil nil #'equal)) "")))))

(defun zenit-packages--bump-resolve-commit (package local-repo lockfiles recipe action)
  "Resolve PACKAGE's target commit according to ACTION."
  (pcase action
    ('latest (zenit-packages--bump-latest-commit recipe))
    ('read (zenit-packages--bump-read-commit package))
    (_ (zenit-packages--bump-select-commit package local-repo lockfiles recipe))))

(defun zenit-packages--bump-write-lockfile (local-repo lockfile commit local-conf)
  "Write LOCAL-REPO's COMMIT into LOCKFILE.

If COMMIT is blank, keep the existing pin. If LOCAL-CONF is non-nil,
write through `zenit-local-versions-dir'."
  (+straight--update-all-lockfile-version)
  (let* ((+straight--lockfile-prefer-local-conf-versions-p local-conf)
         (lockfile (straight--versions-file lockfile))
         (pin-list (straight--lockfile-read lockfile))
         (old-commit (alist-get local-repo pin-list nil nil #'equal))
         (new-commit (if (or (null commit) (string-blank-p commit)) old-commit commit)))
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
          kw))))))

(defun zenit-packages--bump-package (package &optional commit local-conf action packages)
  "Bump PACKAGE to COMMIT using ACTION and package metadata from PACKAGES."
  (cl-block nil
    (let* ((packages (or packages (zenit-package-list 'all)))
           (plist (alist-get package packages))
           (modules (plist-get plist :modules)))
      (unless modules
        (cl-return (message "The package %s isn't installed by any module" package)))
      (when (plist-get plist :built-in)
        (cl-return (message "The package %s is built-in" package)))
      (let* ((lockfiles (zenit-packages--bump-lockfiles plist local-conf))
             (recipe (zenit-packages--bump-recipe package plist)))
        (straight-vc-git--destructure recipe
            (_package local-repo _branch _remote _host _protocol _repo)
          (unless commit
            (setq commit (zenit-packages--bump-resolve-commit
                          package local-repo lockfiles recipe (or action 'select))))
          (dolist (lockfile lockfiles)
            (zenit-packages--bump-write-lockfile local-repo lockfile commit local-conf)))))))

;;;###autoload
(defun zenit/bump-package (package &optional commit local-conf)
  "Bump PACKAGE in all modules that install it.

If COMMIT is nil, prompt for the new commit from recent commits and
tags. With a single universal prefix argument, bump to the latest remote
commit without prompting. With a double universal prefix argument, prompt
for a raw commit.

If LOCAL-CONF is non-nil, write lockfiles into
`zenit-local-versions-dir'."
  (interactive
   (list (intern (completing-read "Bump package: "
                                  (mapcar #'car (zenit-package-list 'all))))))
  (zenit-initialize-packages)
  (zenit-packages--bump-recipe-repos)
  (zenit-packages--bump-package
   package commit local-conf (zenit-packages--bump-prefix-action current-prefix-arg)))

;;;###autoload
(defun zenit/bump-local-conf-package (package &optional commit)
  "Bump local PACKAGE in all modules that install it.

Lockfiles will be written into `zenit-local-versions-dir'.

If COMMIT is nil, prompt for the new commit from recent commits and
tags. With a single universal prefix argument, bump to the latest remote
commit without prompting. With a double universal prefix argument, prompt
for a raw commit."
  (interactive
   (list (intern (completing-read "Bump package: "
                                  (mapcar #'car (zenit-package-list 'all))))))
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

With a single universal prefix argument, bump each package to its latest
remote commit without prompting. With a double universal prefix argument,
prompt for a raw commit for each package."
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

;;;###autoload
(defun zenit/bump-all-packages (&optional all)
  "Bump all package lockfiles to the latest remote commits.

By default, only bump packages declared by enabled modules. With any
prefix argument ALL, include packages declared by all modules, including
packages that are not currently installed or enabled."
  (interactive (list current-prefix-arg))
  (zenit-initialize-packages)
  (message "Updating package recipe repositories...")
  (zenit-packages--bump-recipe-repos)
  (let ((packages (zenit-package-list (and all 'all)))
        groups
        seen
        (bumped 0))
    (dolist (package packages)
      (let* ((name (car package))
             (plist (cdr package))
             (lockfiles (and (not (plist-get plist :built-in))
                             (zenit-packages--bump-lockfiles plist))))
        (when lockfiles
          (let ((recipe (zenit-packages--bump-recipe name plist)))
            (straight-vc-git--destructure recipe
                (_package local-repo _branch _remote _host _protocol _repo)
              (let ((group (assoc local-repo groups)))
                (unless group
                  (setq group (list local-repo recipe nil nil))
                  (push group groups))
                (push name (nth 3 group))
                (dolist (lockfile lockfiles)
                  (let ((key (cons local-repo lockfile)))
                    (unless (member key seen)
                      (push key seen)
                      (setf (nth 2 group) (cons lockfile (nth 2 group))))))))))))
    (let ((total (length groups))
          (current 0))
      (dolist (group (nreverse groups))
        (cl-destructuring-bind (_local-repo recipe lockfiles package-names) group
          (straight-vc-git--destructure recipe
              (_package local-repo _branch _remote _host _protocol _repo)
            (cl-incf current)
            (message "Bumping package %d/%d: %s (%s)"
                     current
                     total
                     (mapconcat #'symbol-name (nreverse package-names) ", ")
                     local-repo)
            (let ((commit (zenit-packages--bump-latest-commit recipe)))
              (dolist (lockfile (nreverse lockfiles))
                (zenit-packages--bump-write-lockfile local-repo lockfile commit nil)
                (cl-incf bumped)))))))
    (message "Bumped %d package lockfile entries" bumped)))

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

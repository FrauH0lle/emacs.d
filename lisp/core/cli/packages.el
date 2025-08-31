;; lisp/core/cli/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'comp-run nil t)


;;
;;; Helpers

(defun zenit--same-commit-p (abbrev-ref ref)
  (and (stringp abbrev-ref)
       (stringp ref)
       (string-match-p (concat "^" (regexp-quote abbrev-ref))
                       ref)))

(defun zenit--abbrev-commit (commit &optional full)
  (if full commit (substring commit 0 7)))

(defun zenit--commit-log-between (start-ref end-ref)
  (straight--process-with-result
      (straight--process-run
       "git" "log" "--oneline" "--no-merges"
       "-n" "26" end-ref start-ref)
    (if success
        (let* ((output (string-trim-right (or stdout "")))
               (lines (split-string output "\n")))
          (if (> (length lines) 25)
              (concat (string-join (butlast lines 1) "\n") "\n[...]")
            output))
      (format "ERROR: Couldn't collect commit list because: %s" stderr))))

(defmacro zenit--straight-with (form &rest body)
  (declare (indent 1))
  `(let-alist
       (let* ((buffer (straight--process-buffer))
              (start (with-current-buffer buffer (point-max)))
              (retval ,form)
              (output (with-current-buffer buffer (buffer-substring start (point-max)))))
         (save-match-data
           (list (cons 'it retval)
                 (cons 'stdout (substring-no-properties output))
                 (cons 'success (if (string-match "\n+\\[Return code: \\([0-9-]+\\)\\]\n+" output)
                                    (string-to-number (match-string 1 output))))
                 (cons 'output (string-trim output
                                            "^\\(\\$ [^\n]+\n\\)*\n+"
                                            "\n+\\[Return code: [0-9-]+\\]\n+")))))
     ,@body))

(defmacro zenit--with-package-recipes (recipes binds &rest body)
  "TODO"
  (declare (indent 2) (debug (form sexp body)))
  (let ((recipe-var  (make-symbol "recipe"))
        (recipes-var (make-symbol "recipes")))
    `(let* ((,recipes-var ,recipes)
            (built ())
            (straight-use-package-pre-build-functions
             (cons (lambda (pkg &rest _) (cl-pushnew pkg built :test #'equal))
                   straight-use-package-pre-build-functions)))
       (dolist (,recipe-var ,recipes-var (nreverse built))
         (cl-block nil
           (straight--with-plist (append (list :recipe ,recipe-var) ,recipe-var)
               ,(ensure-list binds)
             ,@body))))))

(defvar zenit--cli-updated-recipes nil)
(defun zenit--cli-recipes-update ()
  "Updates straight and recipe repos."
  (unless zenit--cli-updated-recipes
    (straight--make-build-cache-available)
    (print! (start "Updating recipe repos..."))
    (print-group!
      (zenit--with-package-recipes
          (delq
           nil (mapcar (zenit-rpartial #'gethash straight--repo-cache)
                       (mapcar #'symbol-name straight-recipe-repositories)))
          (recipe package type local-repo)
        (let ((esc (if init-file-debug "" "\033[1A"))
              (ref (straight-vc-get-commit type local-repo))
              newref output)
          (print! (start "\rUpdating recipes for %s...%s") package esc)
          (zenit--straight-with (straight-vc-fetch-from-remote recipe)
            (when .it
              (setq output .output)
              (straight-merge-package package)
              (unless (equal ref (setq newref (straight-vc-get-commit type local-repo)))
                (print! (success "\r%s updated (%s -> %s)")
                        package
                        (zenit--abbrev-commit ref)
                        (zenit--abbrev-commit newref))
                (unless (string-empty-p output)
                  (print-group! (print! (item "%s" output))))))))))
    (setq straight--recipe-lookup-cache (make-hash-table :test #'eq)
          zenit--cli-updated-recipes t)))

(defvar zenit--eln-output-expected nil)

(defvar zenit--eln-output-path (car (bound-and-true-p native-comp-eln-load-path)))

(defun zenit--eln-file-name (file)
  "Return the short .eln file name corresponding to `file'."
  (concat comp-native-version-dir "/"
          (file-name-nondirectory
           (comp-el-to-eln-filename file))))

(defun zenit--eln-output-file (eln-name)
  "Return the expected .eln file corresponding to `eln-name'."
  (concat zenit--eln-output-path eln-name))

(defun zenit--eln-error-file (eln-name)
  "Return the expected .error file corresponding to `eln-name'."
  (concat zenit--eln-output-path eln-name ".error"))

(defun zenit--find-eln-file (eln-name)
  "Find `eln-name' on the `native-comp-eln-load-path'."
  (cl-some (lambda (eln-path)
             (let ((file (concat eln-path eln-name)))
               (when (file-exists-p file)
                 file)))
           native-comp-eln-load-path))

(defun zenit--elc-file-outdated-p (file)
  "Check whether the corresponding .elc for `file' is outdated."
  (let ((elc-file (byte-compile-dest-file file)))
    ;; NOTE Ignore missing elc files, they could be missing due to
    ;; `no-byte-compile'. Rebuilding unnecessarily is expensive.
    (when (and (file-exists-p elc-file)
               (file-newer-than-file-p file elc-file))
      (zenit-log "%s is newer than %s" file elc-file)
      t)))

(defun zenit--eln-file-outdated-p (file)
  "Check whether the corresponding .eln for `file' is outdated."
  (let* ((eln-name (zenit--eln-file-name file))
         (eln-file (zenit--find-eln-file eln-name))
         (error-file (zenit--eln-error-file eln-name)))
    (cond (eln-file
           (when (file-newer-than-file-p file eln-file)
             (zenit-log "%s is newer than %s" file eln-file)
             t))
          ((file-exists-p error-file)
           (when (file-newer-than-file-p file error-file)
             (zenit-log "%s is newer than %s" file error-file)
             t)))))

(defun zenit--remove-eln-files (dir &optional all)
  "Check if corresponding .eln files from DIR exist and remove them.
If ALL is non-nil, simply remove all files in the eln cache."
  (if all
      (let ((dir (file-name-concat zenit-cache-dir "eln" comp-native-version-dir)))
        (cl-loop for file in (zenit-files-in dir :match "\\.eln$" :full t)
                 do (when (file-exists-p file) (delete-file file))))
    (cl-loop for file in (zenit-files-in dir :match "\\.el$" :full t)
             do (when (file-exists-p (byte-compile-dest-file file))
                  (when-let* ((eln-name (zenit--eln-file-name file))
                              (eln-file (zenit--find-eln-file eln-name)))
                    (when (file-exists-p eln-file)
                      (delete-file eln-file)))))))

(defun zenit-package-recipe-list ()
  "Return straight recipes for non-builtin packages with a local-repo."
  (let (recipes)
    (dolist (recipe (hash-table-values straight--recipe-cache))
      (cl-destructuring-bind (&key local-repo type &allow-other-keys)
          recipe
        (unless (or (null local-repo)
                    (eq type 'built-in))
          (push recipe recipes))))
    recipes))

(defun zenit--remove-wrong-eln-cache ()
  "Remove the ~/.emacs.d/eln-cache folder if it exists."
  (let ((dir (file-name-concat zenit-cache-dir "eln" comp-native-version-dir)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (let ((dir (expand-file-name "eln-cache" user-emacs-directory)))
    (when (file-directory-p dir)
      (cl-loop for file in (zenit-files-in dir)
               do (rename-file
                   file
                   (file-name-concat zenit-cache-dir "eln" comp-native-version-dir (file-name-nondirectory file)) t))
      (delete-directory dir t))))


;;
;;; Build

(defun zenit-cli-packages-install ()
  "Installs missing packages.
This function will install any primary package (i.e. a package
with a `package!' declaration) or dependency thereof that hasn't
already been."
  (require 'zenit-packages)
  (zenit-initialize-packages)
  (print! (start "Installing packages..."))
  ;; Collect package versions
  (let ((pinned (straight--lockfile-read-all)))
    (print-group!
      (if-let* ((built
                 (zenit--with-package-recipes (zenit-package-recipe-list)
                     (recipe package type local-repo)
                   (let ((repo-dir (straight--repos-dir local-repo))
                         (build-dir (straight--build-dir package))
                         (need-rebuild-p))
                     ;; Package is installed when both build and repo directories
                     ;; are present
                     (cond ((and (file-directory-p repo-dir)
                                 (file-directory-p build-dir))
                            (zenit-log "%s already installed" package))
                           ;; Only repo directory present, simply rebuild
                           ((and (file-directory-p repo-dir)
                                 (not (file-directory-p build-dir)))
                            (zenit-log "Rebuilding %s" package)
                            (setq need-rebuild-p t))
                           ;; Otherwise, try to install packge
                           (t (setq need-rebuild-p t)))
                     (when need-rebuild-p
                       ;; If local repo is not present, update recipes before
                       ;; cloning
                       (unless (file-directory-p (straight--repos-dir local-repo))
                         (zenit--cli-recipes-update))
                       (condition-case-unless-debug e
                           (let ((straight-use-package-pre-build-functions
                                  (cons (lambda (pkg &rest _)
                                          (when-let* ((commit (cdr (assoc pkg pinned))))
                                            (print! (item "Checked out %s: %s") pkg commit)))
                                        straight-use-package-pre-build-functions)))
                             (straight-use-package (intern package))
                             ;; HACK Line encoding issues can plague repos with
                             ;;   dirty worktree prompts when updating packages or
                             ;;   "Local variables entry is missing the suffix"
                             ;;   errors when installing them (see
                             ;;   hlissner/zenit-emacs#2637), so have git handle
                             ;;   conversion by force.
                             (when (and zenit--system-windows-p (stringp local-repo))
                               (let ((default-directory (straight--repos-dir local-repo)))
                                 (when (file-in-directory-p default-directory straight-base-dir)
                                   (straight--process-run "git" "config" "core.autocrlf" "true"))))

                             ;; Remove any existing .eln files. Compilation is done
                             ;; in async mode and thus, remaining .eln files can
                             ;; trigger rebuilds.
                             (when (featurep 'native-compile)
                               (zenit--remove-eln-files build-dir)))
                         (error
                          (signal 'zenit-package-error (list package e)))))))))
          (progn
            (print! (success "\rInstalled %d packages") (length built)))
        (print! (item "No packages need to be installed"))
        nil))))


;;
;;; Build

(defun zenit-cli-packages-build (&optional force-p)
  "(Re)build all packages."
  (require 'zenit-packages)
  (zenit-initialize-packages)
  (print! (start "(Re)building %spackages...") (if force-p "all " ""))
  (print-group!
    (let ((straight-check-for-modifications
           (when (file-directory-p (straight--modified-dir))
             '(find-when-checking)))
          (straight--allow-find
           (and straight-check-for-modifications
                (executable-find straight-find-executable)
                t))
          (straight--packages-not-to-rebuild
           (or straight--packages-not-to-rebuild (make-hash-table :test #'equal)))
          (straight--packages-to-rebuild
           (or (if force-p :all straight--packages-to-rebuild)
               (make-hash-table :test #'equal)))
          (recipes (zenit-package-recipe-list)))

      (unless force-p
        (straight--make-build-cache-available))
      (if-let* ((built
                 (zenit--with-package-recipes recipes (package local-repo recipe)
                   (unless force-p
                     ;; Ensure packages with outdated files/bytecode are rebuilt
                     (let* ((build-dir (straight--build-dir package))
                            (repo-dir  (straight--repos-dir local-repo))
                            (build (if (plist-member recipe :build)
                                       (plist-get recipe :build)
                                     t))
                            (want-byte-compile
                             (or (eq build t)
                                 (memq 'compile build)))
                            (want-native-compile
                             (or (eq build t)
                                 (memq 'native-compile build)))
                            rebuild-reason)
                       (and (eq (car-safe build) :not)
                            (setq want-byte-compile (not want-byte-compile)
                                  want-native-compile (not want-native-compile)))
                       (unless (featurep 'native-compile)
                         (setq want-native-compile nil))
                       (and (or want-byte-compile want-native-compile)
                            (or (when (file-newer-than-file-p repo-dir build-dir)
                                  (setq rebuild-reason "Repository newer than build directory") t)
                                (when (file-exists-p (straight--modified-dir (or local-repo package)))
                                  (setq rebuild-reason "Entry in 'straight/modified/' exists") t)
                                (when (cl-loop with outdated = nil
                                               for file in (zenit-files-in build-dir :match "\\.el$" :full t)
                                               if (or (when want-byte-compile
                                                        (zenit--elc-file-outdated-p file))
                                                      ;; Only check .eln files if
                                                      ;; .elc file actually exists.
                                                      ;; Otherwise you can get a
                                                      ;; bunch of unncessary
                                                      ;; rebuilds because a file in
                                                      ;; the eln cache is missing.
                                                      (when (and want-native-compile
                                                                 (file-exists-p (byte-compile-dest-file file)))
                                                        (zenit--eln-file-outdated-p file)))
                                               do (setq outdated t)
                                               (when want-native-compile
                                                 (push file zenit--eln-output-expected))
                                               finally return outdated)
                                  (setq rebuild-reason ".eln/.elc outdated") t))
                            (progn (zenit-log "%s is outdated, rebuilding" package)
                                   (zenit-log "Reason: %s" rebuild-reason)
                                   (puthash package t straight--packages-to-rebuild)
                                   (when (featurep 'native-compile)
                                     (zenit--remove-eln-files build-dir))))))
                   (if force-p
                       (progn
                         (when (featurep 'native-compile)
                           (zenit--remove-eln-files t 'all))
                         ;; NOTE 2024-05-02: This is the same as what
                         ;;   `straight-rebuild-all' does but we reverse the order
                         ;;   of the packages such that they are build in the order
                         ;;   they were declared.
                         (dolist (package (nreverse (hash-table-keys straight--recipe-cache)))
                           (straight-use-package (intern package))))
                     (when (gethash package straight--packages-to-rebuild)
                       (straight-register-repo-modification local-repo)
                       (straight-use-package (intern package)))))))

          (progn
            ;; HACK Every time you save a file in a package that straight tracks,
            ;;   it is recorded in ~/.emacs.d/straight/modified/. Typically,
            ;;   straight will clean these up after rebuilding, but our use-case
            ;;   circumnavigates that, leaving these files there and causing a
            ;;   rebuild of those packages each time `sync' or similar is run, so
            ;;   we clean it up ourselves:
            (delete-directory (straight--modified-dir) 'recursive)
            (print! (success "\rRebuilt %d package(s)") (length built)))
        (print! (item "No packages need rebuilding"))
        nil))))


;;
;;; Update

(defun zenit-cli-packages-update (&optional force-p)
  "Updates packages."
  (zenit-initialize-packages)
  (let* ((repo-dir (straight--repos-dir))
         (pinned (straight--lockfile-read-all))
         (recipes (zenit-package-recipe-list))
         (packages-to-rebuild (make-hash-table :test 'equal))
         (repos-to-rebuild (make-hash-table :test 'equal))
         (total (length recipes))
         (esc (if init-file-debug "" "\033[1A"))
         (i 0))
    (zenit--with-package-recipes recipes (recipe package type local-repo)
      (cl-incf i)
      (print-group!
        (unless (straight--repository-is-available-p recipe)
          (print! (error "(%d/%d) Couldn't find local repo for %s") i total package)
          (cl-return))
        (when (gethash local-repo repos-to-rebuild)
          (puthash package t packages-to-rebuild)
          (print! (success "(%d/%d) %s was updated indirectly (with %s)") i total package local-repo)
          (cl-return))
        (let ((default-directory (straight--repos-dir local-repo)))
          (unless (file-in-directory-p default-directory repo-dir)
            (print! (warn "(%d/%d) Skipping %s because it is out-of-tree...") i total package)
            (cl-return))
          (when (eq type 'git)
            (unless (file-exists-p ".git")
              (error "%S is not a valid repository" package)))
          (condition-case-unless-debug e
              (let ((ref (straight-vc-get-commit type local-repo))
                    (target-ref
                     (cdr (or (assoc local-repo pinned)
                              (assoc package pinned))))
                    commits
                    output)
                (or (cond
                     ;; When force-p is non-nil, re-clone every repo
                     (force-p
                      (print! (start "\r(%d/%d) Re-cloning %s...") i total local-repo esc)
                      (let ((repo (straight--repos-dir local-repo))
                            (straight-vc-git-default-clone-depth 'full))
                        (delete-directory repo 'recursive)
                        (print-group!
                          (straight-use-package (intern package) nil 'no-build))
                        (prog1 (file-directory-p repo)
                          (or (not (eq type 'git))
                              (setq output (zenit--commit-log-between ref target-ref)
                                    commits (length (split-string output "\n" t)))))))

                     ;; No target commit found, simply pull
                     ((not (stringp target-ref))
                      (print! (start "\r(%d/%d) Fetching %s...%s") i total package esc)
                      (zenit--straight-with (straight-vc-fetch-from-remote recipe)
                        (when .it
                          (straight-merge-package package)
                          (setq target-ref (straight-vc-get-commit type local-repo))
                          (setq output (zenit--commit-log-between ref target-ref)
                                commits (length (split-string output "\n" t)))
                          (or (not (zenit--same-commit-p target-ref ref))
                              (cl-return)))))

                     ;; Target commit and local commit are the same, do nothing
                     ((zenit--same-commit-p target-ref ref)
                      (print! (item "\r(%d/%d) %s is up-to-date...%s") i total package esc)
                      (cl-return))

                     ;; Target commit present but not checked out, check out
                     ;; target commit
                     ((if (straight-vc-commit-present-p recipe target-ref)
                          (print! (start "\r(%d/%d) Checking out %s (%s)...%s")
                                  i total package (zenit--abbrev-commit target-ref) esc)
                        (print! (start "\r(%d/%d) Fetching %s...%s") i total package esc)
                        (and (straight-vc-fetch-from-remote recipe)
                             (straight-vc-commit-present-p recipe target-ref)))
                      (straight-vc-check-out-commit recipe target-ref)
                      (or (not (eq type 'git))
                          (setq output (zenit--commit-log-between ref target-ref)
                                commits (length (split-string output "\n" t))))
                      (zenit--same-commit-p target-ref (straight-vc-get-commit type local-repo)))

                     ;; Otherwise delete local repo and clone it again
                     ((print! (start "\r(%d/%d) Re-cloning %s...") i total local-repo esc)
                      (let ((repo (straight--repos-dir local-repo))
                            (straight-vc-git-default-clone-depth 'full))
                        (delete-directory repo 'recursive)
                        (print-group!
                          (straight-use-package (intern package) nil 'no-build))
                        (prog1 (file-directory-p repo)
                          (or (not (eq type 'git))
                              (setq output (zenit--commit-log-between ref target-ref)
                                    commits (length (split-string output "\n" t))))))))
                    ;; Something went wrong
                    (progn
                      (print! (warn "\r(%d/%d) Failed to fetch %s")
                              i total local-repo)
                      (unless (string-empty-p output)
                        (print-group! (print! (item "%s" output))))
                      (cl-return)))
                (puthash local-repo t repos-to-rebuild)
                ;; HACK: Rebuild all packages that depend on PACKAGE after
                ;;   updating it. This ensures their bytecode don't contain stale
                ;;   references to symbols in silent dependencies.
                ;; TODO: Allow `package!' to control this.
                ;; TODO: Add cache+optimization step for this rebuild table.
                (letf! ((dependents (straight-dependents package))
                        (n 0)
                        (defun* add-to-rebuild (tree)
                                (cond ((null tree) nil)
                                      ((stringp tree)
                                       (unless (gethash tree packages-to-rebuild)
                                         (cl-incf n 1)
                                         (puthash tree t packages-to-rebuild)))
                                      ((listp tree)
                                       (add-to-rebuild (car tree))
                                       (add-to-rebuild (cdr tree))))))
                  (add-to-rebuild dependents)
                  (puthash package t packages-to-rebuild)
                  (print! (success "\r(%d/%d) %s: %s -> %s%s%s")
                          i total local-repo
                          (zenit--abbrev-commit ref)
                          (zenit--abbrev-commit target-ref)
                          (if (and (integerp commits) (> commits 0))
                              (format " [%d commit(s)]" commits)
                            "")
                          (if (> n 0)
                              (format " (w/ %d dependents)" n)
                            "")))
                (unless (string-empty-p output)
                  (let ((lines (split-string output "\n")))
                    (setq output
                          (if (> (length lines) 20)
                              (concat (string-join (cl-subseq (butlast lines 1) 0 20) "\n")
                                      "\n[...]")
                            output)))
                  (print-group! (print! "%s" (indent output 2)))))
            (user-error
             (signal 'user-error (error-message-string e)))
            (error
             (signal 'zenit-package-error (list package e)))))))
    (print-group!
      (if (hash-table-empty-p packages-to-rebuild)
          (ignore (print! (success "\rAll %d packages are up-to-date") total))
        (zenit--cli-recipes-update)
        (straight--transaction-finalize)
        (let ((default-directory (straight--build-dir)))
          (mapc (zenit-rpartial #'delete-directory 'recursive)
                (hash-table-keys packages-to-rebuild)))
        (print! (success "\rUpdated %d package(s)")
                (hash-table-count packages-to-rebuild))
        (zenit-cli-packages-build force-p)
        t))))


;;
;;; Purge

(defun zenit--packages-purge-build (build)
  (let ((build-dir (straight--build-dir build)))
    (delete-directory build-dir 'recursive)
    (if (file-directory-p build-dir)
        (ignore (print! (error "Failed to purg build/%s" build)))
      (print! (success "Purged build/%s" build))
      t)))

(defun zenit--packages-purge-builds (builds)
  (if (not builds)
      (prog1 0
        (print! (item "No builds to purge")))
    (print! (start "Purging straight builds..." (length builds)))
    (print-group!
      (length
       (delq nil (mapcar #'zenit--packages-purge-build builds))))))

(cl-defun zenit--packages-regraft-repo (repo)
  (let ((default-directory (straight--repos-dir repo)))
    (unless (file-directory-p ".git")
      (print! (warn "\rrepos/%s is not a git repo, skipping" repo))
      (cl-return))
    (unless (file-in-directory-p default-directory straight-base-dir)
      (print! (warn "\rSkipping repos/%s because it is local" repo))
      (cl-return))
    (let ((before-size (zenit-directory-size default-directory)))
      (zenit-call-process "git" "reset" "--hard")
      (zenit-call-process "git" "clean" "-ffd")
      (if (not (zerop (car (zenit-call-process "git" "replace" "--graft" "HEAD"))))
          (print! (item "\rrepos/%s is already compact\033[1A" repo))
        (zenit-call-process "git" "reflog" "expire" "--expire=all" "--all")
        (zenit-call-process "git" "gc" "--prune=now")
        (let ((after-size (zenit-directory-size default-directory)))
          (if (equal after-size before-size)
              (print! (success "\rrepos/%s cannot be compacted further" repo))
            (print! (success "\rRegrafted repos/%s (from %0.1fKB to %0.1fKB)")
                    repo before-size after-size)))))
    t))

(defun zenit--packages-regraft-repos (repos)
  (if (not repos)
      (prog1 0
        (print! (item "No repos to regraft")))
    (print! (start "Regrafting %d repos..." (length repos)))
    (let ((before-size (zenit-directory-size (straight--repos-dir))))
      (print-group!
        (prog1 (delq nil (mapcar #'zenit--packages-regraft-repo repos))
          ;; (princ "\033[K")
          (let ((after-size (zenit-directory-size (straight--repos-dir))))
            (print! (success "Finished regrafting. Size before: %0.1fKB and after: %0.1fKB (%0.1fKB)")
                    before-size after-size
                    (- after-size before-size))))))))

(defun zenit--packages-purge-repo (repo)
  (let ((repo-dir (straight--repos-dir repo)))
    (when (file-directory-p repo-dir)
      (delete-directory repo-dir 'recursive)
      (delete-file (straight--modified-file repo))
      (if (file-directory-p repo-dir)
          (ignore (print! (error "Failed to purge repos/%s" repo)))
        (print! (success "Purged repos/%s" repo))
        t))))

(defun zenit--packages-purge-repos (repos)
  (if (not repos)
      (prog1 0
        (print! (item "No repos to purge")))
    (print! (start "Purging straight repositories..."))
    (print-group!
      (length
       (delq nil (mapcar #'zenit--packages-purge-repo repos))))))

(defun zenit--packages-purge-elpa ()
  (require 'zenit-packages)
  (let ((dirs (zenit-files-in package-user-dir :type t :depth 0)))
    (if (not dirs)
        (prog1 0
          (print! (item "No ELPA packages to purge")))
      (print! (start "Purging ELPA packages..."))
      (dolist (path dirs (length dirs))
        (condition-case e
            (print-group!
              (if (file-directory-p path)
                  (delete-directory path 'recursive)
                (delete-file path))
              (print! (success "Deleted %s") (filename path)))
          (error
           (print! (error "Failed to delete %s because: %s")
                   (filename path)
                   e)))))))

(defun zenit--packages-purge-eln ()
  (dolist (dir (zenit-files-in zenit--eln-output-path :type 'dirs))
    (print! (item "Purging ELN cache %s" dir))
    (delete-directory dir t)))

(defun zenit-packages-purge (&optional elpa-p builds-p repos-p regraft-repos-p)
  "Auto-removes orphaned packages and repos.
An orphaned package is a package that isn't a primary
package (i.e. doesn't have a `package!' declaration) or isn't
depended on by another primary package.

If BUILDS-P, include straight package builds.

If REPOS-P, include straight repos.

If ELPA-P, include packages installed with package.el (M-x
package-install)."
  (zenit-initialize-packages)
  (print! (start "Purging orphaned packages (for the emperor)..."))
  (quiet! (straight-prune-build-cache))
  (cl-destructuring-bind (&optional builds-to-purge repos-to-purge repos-to-regraft)
      (let ((rdirs
             (and (or repos-p regraft-repos-p)
                  (straight--directory-files (straight--repos-dir) nil nil 'sort))))
        (list (when builds-p
                (let ((default-directory (straight--build-dir)))
                  (seq-filter #'file-directory-p
                              (seq-remove (zenit-rpartial #'gethash straight--profile-cache)
                                          (straight--directory-files default-directory nil nil 'sort)))))
              (when repos-p
                (seq-remove (zenit-rpartial #'straight--checkhash straight--repo-cache)
                            rdirs))
              (when regraft-repos-p
                (seq-filter (zenit-rpartial #'straight--checkhash straight--repo-cache)
                            rdirs))))
    (print-group!
      (delq
       nil (list
            (when (featurep 'native-compile)
              (zenit--packages-purge-eln))
            (if (not builds-p)
                (ignore (print! (item "Skipping builds")))
              (/= 0 (zenit--packages-purge-builds builds-to-purge)))
            (if (not elpa-p)
                (ignore (print! (item "Skipping elpa packages")))
              (/= 0 (zenit--packages-purge-elpa)))
            (if (not repos-p)
                (ignore (print! (item "Skipping repos")))
              (/= 0 (zenit--packages-purge-repos repos-to-purge)))
            (if (not regraft-repos-p)
                (ignore (print! (item "Skipping regrafting")))
              (zenit--packages-regraft-repos repos-to-regraft)))))))

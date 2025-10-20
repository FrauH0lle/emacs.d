;;; lisp/core/zenit-packages.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar zenit-init-packages-p nil
  "Non-nil if the package management system has been initialized.")

(defvar zenit-mandatory-packages '(straight async)
  "A list of packages that must be installed.

These packages will be auto-installed if missing and shouldn't be
deleted.")

(defvar zenit-packages ()
  "A list of enabled packages.

Each element is a sublist, whose CAR is the package's name as a symbol,
and whose CDR is the plist supplied to its `package!' declaration.")

(defvar zenit-disabled-packages ()
  "List of packages that should be ignored by `use-package!' and `after!'.")

(defvar zenit-packages-file "packages"
  "The basename of packages file for modules.")

(defvar zenit-local-versions-dir (file-name-concat zenit-local-conf-dir "versions/")
  "Where the local, machine specific package versions are placed.

Defaults to ~/.emacs.d/site-lisp/versions/. Must end in a slash.")


;;
;;; package.el

;; Ensure that, if we do need package.el, it is configured correctly. You really
;; shouldn't be using it, but it may be convenient for quick package testing.
(setq package-enable-at-startup nil)

(with-eval-after-load 'package
  (setq package-user-dir (concat zenit-local-dir "elpa/")
        package-gnupghome-dir (expand-file-name "gpg" package-user-dir)
        ;; I omit Marmalade because its packages are manually submitted rather
        ;; than pulled, so packages are often out of date with upstream.
        package-archives
        (let ((proto (if gnutls-verify-error "https" "http")))
          (list (cons "melpa" (concat proto "://melpa.org/packages/"))
                (cons "org"   (concat proto "://orgmode.org/elpa/")))))

  ;; Refresh package.el the first time you call `package-install'
  (add-transient-hook! 'package-install (package-refresh-contents)))


;;
;;; straight.el

(defvar straight-current-profile)

(setq straight-repository-branch "develop"
      ;; Since byte-code is rarely compatible across different versions of
      ;; Emacs, it's best we build them in separate directories, per emacs
      ;; version.
      ;; straight-build-dir (format "build-%s" emacs-version)
      ;; We already do this, and better.
      straight-cache-autoloads nil
      ;; Disabling this makes 'make refresh' instant (once everything set up),
      ;; which is much nicer UX than the several seconds modification checks.
      straight-check-for-modifications nil
      ;; We handle package.el ourselves
      straight-enable-package-integration nil
      ;; Use shallow clones
      ;; straight-vc-git-default-clone-depth 1
      ;; Tell straight.el about the profiles we are going to be using. Will be
      ;; dynamically modified by the `package!' macro.
      straight-profiles
      '(;; Packages registered by the packages.el files in `zenit-core-dir'
        (core . "core.el")
        ;; Packages registered interactively.
        (nil . "default.el")
        ;; Packages registered in the packages.el file in `zenit-local-conf-dir'
        (local . "local.el"))
      ;; Install archives from forges instead of cloning them. Much faster and
      ;; lighter.
      straight-vc-use-snapshot-installation (and (executable-find "tar") t))


(with-eval-after-load 'straight
  ;; HACK: We want to defer the compilation of the .elc files in order to save
  ;;   some minutes during config creation. To complete this, straight.el needs
  ;;   to be told not to do native-compilation, but it won't obey
  ;;   `straight-disable-native-compile', but `straight--native-comp-available',
  ;;   though. Trouble is: it's a constant; it resets itself when straight is
  ;;   loaded, so it must be changed afterwards.
  (setq straight--native-comp-available nil)
  ;; `let-alist' is built into Emacs 26 and onwards
  (add-to-list 'straight-built-in-pseudo-packages 'let-alist))


;;;; Straight hacks

(defadvice! +straight--fix-loaddefs-generate--parse-file-a (fn &rest args)
  "HACK Prevent `loaddefs-generate--parse-file' toggling `emacs-lisp-mode'.

This fixes an issue introduced in emacs-mirror/emacs@0d383b592c2f and is
present in >=29:

Straight.el uses `loaddefs-generate' if it is available, which activates
`emacs-lisp-mode' to read autoloads files, but does so without
suppressing its hooks. Some packages (like overseer) add hooks to
`emacs-lisp-mode-hook' in their autoloads, and once triggered,they will
try to load their dependencies (like dash or pkg-info), causing file
errors."
  :around #'loaddefs-generate--parse-file
  (let (emacs-lisp-mode-hook)
    (apply fn args)))

(defun +straight--fixup-profiles ()
  "Fixup packages profiles.

This involves

  1. Ensure that recipe repositories are only registered under
     the \\='core profile

  2. If a local repository provides multiple packages (like
     `magit') and one or more is registered as a dependency,
     ensure that each dependency is assigned to a proper profile.

  3. If a package is associated with multiple profiles and one of
     them is nil, remove nil. This ways we make sure that there
     are no interactively registered packages after init.

  4. Remove the \\='_pkg-dependency profile from packages which are also
     registered to proper profiles."
  ;; Build a map of repositories to their packages.
  (let ((repo-package-map (make-hash-table :test #'equal)))
    (maphash
     (lambda (package recipe)
       (straight--with-plist recipe
           (local-repo)
         (when local-repo
           (pushnew! (gethash local-repo repo-package-map) package))))
     straight--recipe-cache)

    ;; Normalize each package's profiles.
    (maphash (lambda (package profiles)
               (let ((new-profiles (copy-sequence profiles)))
                 ;; Set recipe repositories to 'core' only.
                 (when (memq (intern package) straight-recipe-repositories)
                   (setq new-profiles '(core)))

                 ;; Assign proper profiles based on the repository package map.
                 (when (memq '_pkg-dependency profiles)
                   (dolist (repo-package (hash-table-values repo-package-map))
                     (when (member package repo-package)
                       (dolist (pkg repo-package)
                         (unless (equal pkg package)
                           (setq new-profiles
                                 (cl-union new-profiles
                                           (gethash pkg straight--profile-cache)
                                           :test #'equal)))))))

                 ;; Remove '_pkg-dependency' profile if there are other proper
                 ;; profiles.
                 (when (and (memq '_pkg-dependency new-profiles) (> (length new-profiles) 1))
                   (setq new-profiles (remq '_pkg-dependency new-profiles)))

                 ;; Remove nil entries if there are other profiles.
                 (when (and (remq nil new-profiles) (> (length new-profiles) 1))
                   (setq new-profiles (remq nil new-profiles)))

                 ;; Update the profile cache.
                 (puthash package new-profiles straight--profile-cache)))
             straight--profile-cache)))

(defvar +straight--auto-options
  '(("has diverged from"
     . "^Reset [^ ]+ to ")
    ("but recipe specifies a URL of"
     . "Delete remote \"[^\"]+\", re-create it with correct URL")
    ("has a merge conflict:"
     . "^Abort merge$")
    ("has a dirty worktree:"
     . "^Discard changes$")
    ("^In repository \"[^\"]+\", [^ ]+ (on branch \"main\") is ahead of default branch \"master\""
     . "^Checkout branch \"master\"")
    ("^In repository \"[^\"]+\", [^ ]+ (on branch \"[^\"]+\") is ahead of default branch \"[^\"]+\""
     . "^Checkout branch \"")
    ("^In repository "
     . "^Reset branch \\|^Delete remote [^,]+, re-create it with correct URL"))
  "A list of regexps, mapped to regexps.
Their CAR is tested against the prompt, and CDR is tested against
the presented option, and is used by `straight-vc-git--popup-raw'
to select which option to recommend.")

;; HACK Remove dired & magit options from prompt, since they're inaccessible in
;;      noninteractive sessions.
(when noninteractive
  (advice-add #'straight-vc-git--popup-raw :override #'straight--popup-raw))

(defadvice! +straight--fallback-to-y-or-n-prompt-a (orig-fn &optional prompt)
  "HACK Replace GUI popup prompts (which hang indefinitely in tty
Emacs) with simple prompts."
  :around #'straight-are-you-sure
  (or (bound-and-true-p zenit-cli-auto-accept)
      (if noninteractive
          (y-or-n-p (format! "%s" (or prompt "")))
        (funcall orig-fn prompt))))

(defun +straight--recommended-option-p (prompt option)
  "Check if a given OPTION is recommended for a given PROMPT.

This function goes through `+straight--auto-options`, a list of
cons cells, where each cell is a pair of regular expressions: one
for the prompt and one for the option.

The function checks if the PROMPT matches any prompt regular
expressions in `+straight--auto-options`. If it finds a match, it
then checks if the OPTION matches the corresponding option
regular expression in the same cons cell.

The function returns t if both the PROMPT and OPTION match their
respective regular expressions; otherwise, it returns nil."
  (cl-loop for (prompt-re . opt-re) in +straight--auto-options
           if (string-match-p prompt-re prompt)
           return (string-match-p opt-re option)))

(defadvice! +straight--no-compute-prefixes-a (fn &rest args)
  "HACK Do not compute prefixes for autoloads."
  :around #'straight--build-autoloads
  (eval-when-compile
    (or (require 'loaddefs-gen nil 'noerror)
        (require 'autoload)))
  (let (autoload-compute-prefixes)
    (apply fn args)))

(defadvice! +straight--fallback-to-tty-prompt-a (orig-fn prompt actions)
  "Modifies straight to prompt on the terminal when in
noninteractive sessions."
  :around #'straight--popup-raw
  (if (not noninteractive)
      (funcall orig-fn prompt actions)
    (let ((+straight--auto-options +straight--auto-options))
      ;; We can't intercept C-g, so no point displaying any options for this key
      ;; when C-c is the proper way to abort batch Emacs.
      (delq! "C-g" actions 'assoc)
      ;; HACK These are associated with opening dired or magit, which isn't
      ;;      possible in tty Emacs, so...
      (delq! "e" actions 'assoc)
      (delq! "g" actions 'assoc)
      (if zenit-cli-auto-discard
          (cl-loop with zenit-cli-auto-accept = t
                   for (_key desc func) in actions
                   when desc
                   when (+straight--recommended-option-p prompt desc)
                   return (funcall func))
        (print! (start "%s") (red prompt))
        (print-group!
          (terpri)
          (let (recommended options)
            (print-group!
              (print! " 1) Abort")
              (cl-loop for (_key desc func) in actions
                       when desc
                       do (push func options)
                       and do
                       (print! "%2s) %s" (1+ (length options))
                               (if (+straight--recommended-option-p prompt desc)
                                   (progn
                                     (setq +straight--auto-options nil
                                           recommended (length options))
                                     (green (concat desc " (Choose this if unsure)")))
                                 desc))))
            (terpri)
            (let* ((options
                    (cons (lambda ()
                            (let ((zenit-format-indent 0))
                              (terpri)
                              (print! (warn "Aborted")))
                            (kill-emacs 1))
                          (nreverse options)))
                   (prompt
                    (format! "How to proceed? (%s%s) "
                             (mapconcat #'number-to-string
                                        (number-sequence 1 (length options))
                                        ", ")
                             (if (not recommended) ""
                               (format "; don't know? Pick %d" (1+ recommended)))))
                   answer fn)
              (while (null (nth (setq answer (1- (read-number prompt)))
                                options))
                (print! (warn "%s is not a valid answer, try again.")
                        answer))
              (funcall (nth answer options)))))))))

(setq straight-arrow " > ")
(defadvice! +straight--respect-print-indent-a (string &rest objects)
  "Same as `message' (which see for STRING and OBJECTS) normally.
However, in batch mode, print to stdout instead of stderr."
  :override #'straight--output
  (let ((msg (apply #'format string objects)))
    (save-match-data
      (when (string-match (format "^%s\\(.+\\)$" (regexp-quote straight-arrow)) msg)
        (setq msg (match-string 1 msg))))
    (and (string-match-p "^\\(Cloning\\|\\(Reb\\|B\\)uilding\\) " msg)
         (not (string-suffix-p "...done" msg))
         (zenit-print (concat "> " msg) :format t))))

(defadvice! +straight--ignore-gitconfig-a (fn &rest args)
  "Prevent user and system git configuration from interfering with git calls."
  :around #'straight--process-call
  (with-environment-variables
      (("GIT_CONFIG" nil)
       ("GIT_CONFIG_NOSYSTEM" "1")
       ("GIT_CONFIG_GLOBAL" "/dev/null"))
    (apply fn args)))

;; If the repo failed to clone correctly (usually due to a connection failure),
;; straight proceeds as normal until a later call produces a garbage result
;; (typically, when it fails to fetch the remote branch of the empty directory).
;; This causes Straight to throw an otherwise cryptic type error when it tries
;; to sanitize the result for its log buffer.
;;
;; This error is a common source of user confusion and false positive bug
;; reports, so this advice catches them to regurgitates a more cogent
;; explanation.
(defadvice! +straight--throw-error-on-no-branch-a (fn &rest args)
  :around #'straight--process-log
  (letf! ((defun shell-quote-argument (&rest args)
            (unless (car args)
              (error "Package was not properly cloned due to a connection failure, please try again later"))
            (apply shell-quote-argument args)))
    (apply fn args)))

(defadvice! +straight--regurgitate-empty-string-error-a (fn &rest args)
  :around #'straight-vc-git-local-repo-name
  (condition-case-unless-debug e
      (apply fn args)
    (wrong-type-argument
     (if (eq (cadr e) 'stringp)
         (error "Package was not properly cloned due to a connection failure, please try again later")
       (signal (car e) (cdr e))))))

;; HACK: Straight can sometimes fail to clone/update a repo, leaving behind an

;;   empty directory which, in future invocations, it will assume indicates a
;;   successful clone (causing load errors later).
(defvar +straight-retries 3
  "How many times to retry VC operations.")

(defvar +straight-retry-methods
  '(clone
    normalize
    fetch-from-remote
    fetch-from-upstream
    merge-from-remote
    merge-from-upstream)
  "Which `straight-vc' methods to retry, if they fail.")

(defadvice! +straight--retry-a (fn method type &rest args)
  :around #'straight-vc
  (if (or (not noninteractive)
          (memq type '(nil built-in))
          (not (memq method +straight-retry-methods)))
      (apply fn method type args)
    (let ((n +straight-retries)
          res)
      (while (> n 0)
        (condition-case err
            (setq res (apply fn method type args)
                  n 0)
          (error
           (cl-decf n)
           (when (= n 0)
             (signal (car err) (cdr err)))
           (print! (warn "Failed %S %S operation, retrying (attempt %d/%d)...")
                   type method (- (1+ +straight-retries) n)
                   +straight-retries)
           (sleep-for 1))))
      res)))

;; HACK: In some edge cases, either Straight or git silently fails to clone a
;;   package without triggering an catchable error (and thus evading the
;;   auto-retry logic in `+straight--retry-a') and leaves behind an empty
;;   directory. This detects this an forces straight to emit a catchable error.
(defadvice! +straight-vc-clone--emit-error-a (fn recipe)
  :around #'straight-vc-clone
  (prog1 (funcall fn recipe)
    (when noninteractive
      (straight--with-plist recipe (package type local-repo)
                            (let* ((local-repo (or local-repo package))
                                   (repo-dir (straight--repos-dir local-repo))
                                   (build-dir (straight--build-dir local-repo)))
                              (when (file-in-directory-p repo-dir straight-base-dir)
                                (unless (or (file-directory-p (zenit-path repo-dir ".git"))
                                            (file-exists-p (zenit-path repo-dir ".straight-commit")))
                                  (delete-directory repo-dir t)
                                  (delete-directory build-dir t)
                                  (error "Failed to clone %S ... " package))))))))

;; HACK: Line encoding issues can plague repos with dirty worktree prompts when
;;   updating packages or "Local variables entry is missing the suffix" errors
;;   when installing them (see #2637), so have git handle conversion by force.
(when zenit--system-windows-p
  (add-hook! 'straight-vc-git-post-clone-hook
    (lambda! (&key repo-dir)
      (let ((default-directory repo-dir))
        (straight--process-run "git" "config" "core.autocrlf" "true")))))

(defvar +straight--lockfile-prefer-local-conf-versions-p nil
  "If non-nil, `straight--versions-file' should prefer versions from `zenit-local-versions-dir'.")
(defadvice! +straight-versions-file--prefer-local-profile-a (fn &rest args)
  "Advice to handle local vs default version file precedence.

Reading precedence:
- `nil' and `local' profiles: ALWAYS use local conf dir
- Other profiles: prefer local conf dir if file exists there, else use default

Writing behavior (controlled by `+straight--lockfile-prefer-local-conf-versions-p'):
- When nil: write to default location (for `zenit/bump-package')
- When non-nil: write to local conf dir (for `zenit/bump-local-conf-package')"
  :around #'straight--versions-file
  (let* ((local-profile-files (cl-loop for (profile . file) in straight-profiles
                                       if (memq profile '(nil local))
                                       collect file))
         (local-path (apply #'zenit-path zenit-local-versions-dir args))
         (local-exists-p (file-exists-p local-path))
         ;; Determine if we should use local based on reading rules
         (should-read-local
          (or
           ;; Rule 1: nil and local profiles always use local
           (seq-some (lambda (x) (member x local-profile-files))
                     args)
           ;; Rule 2: Other profiles use local if it exists. If the current
           ;;   profile is 'local, use local even if the file does not exist,
           ;;   basically unpinning the package version.
           (or (memq (bound-and-true-p straight-current-profile) '(local))
               local-exists-p)))
         ;; For writing, respect the preference flag
         (should-use-local
          (if +straight--lockfile-prefer-local-conf-versions-p
              ;; When bumping local conf, always use local
              t
            ;; Otherwise, use reading rules
            should-read-local)))

    (if should-use-local
        local-path
      (apply fn args))))

(defadvice! +straight--lockfile-read-all--prefer-local-versions-a (versions-alist)
  "Filter merged VERSIONS-ALIST to prefer local versions for packages.

If a package has \\='local or nil as a profile, prefer the version from
the local lockfile. If the package isn't in the local lockfile, it
becomes unpinned (returns nil), allowing it to track the latest version."
  :filter-return #'straight--lockfile-read-all
  ;; Read local versions once, outside the loop
  (let ((local-versions
         (let ((lockfile-path (straight--versions-file (alist-get 'local straight-profiles))))
           (when (file-exists-p lockfile-path)
             (with-temp-buffer
               (insert-file-contents-literally lockfile-path)
               (read (current-buffer))))))
        (result nil))
    ;; Process each repo in the merged alist
    (dolist (spec versions-alist)
      (cl-destructuring-bind (local-repo . commit) spec
        ;; Look up package info
        (if-let* ((repo-info (gethash local-repo straight--repo-cache))
                  (package (plist-get repo-info :package))
                  (profiles (gethash package straight--profile-cache))
                  (local-p (seq-some (lambda (x) (memq x '(nil local))) profiles)))
            ;; Package has local profile - use local version (or nil if not
            ;; present)
            (when-let ((local-commit (alist-get local-repo local-versions nil nil #'equal)))
              (push (cons local-repo local-commit) result))
          ;; Package doesn't have local profile - keep original commit
          (push spec result))))
    (nreverse result)))

(defadvice! +straight-vc-clone--use-local-profile-a (fn recipe)
  "Ensure packages with \\='local profile are cloned with that profile."
  :around #'straight-vc-clone
  (straight--with-plist recipe (package)
    (let* ((profiles (gethash package straight--profile-cache))
           (has-local-p (seq-some (lambda (x) (memq x '(nil local))) profiles))
           ;; Set profile only if not already set
           (straight-current-profile
            (or (bound-and-true-p straight-current-profile)
                (if has-local-p 'local (car profiles)))))
      (funcall fn recipe))))

(defvar +straight--lockfile-version-id nil
  "Memoized version ID from `straight''s install.el file.")
(defun +straight--get-lockfile-version-id ()
  "Extract the version ID from `straight''s install.el file.
Returns the version string or nil if not found."
  (with-memoization +straight--lockfile-version-id
    (with-temp-buffer
      (let* ((straight-recipe (gethash "straight" straight--recipe-cache))
             (straight-repo (straight--repos-dir (plist-get straight-recipe :local-repo)))
             (install-file (file-name-concat straight-repo "install.el")))
        (insert-file-contents-literally install-file)
        (goto-char (point-min))
        (let (match)
          (when (re-search-forward "setq version \\(?1::[a-z]+\\)" nil t)
            (setq match (match-string-no-properties 1)))
          match)))))

(defvar +straight--lockfile-version-updated-p nil
  "Whether lockfile versions have been updated in this session.")
(defun +straight--update-all-lockfile-version ()
  "Update all lockfiles with the current `straight' version ID.
Only runs once per session to avoid unnecessary file operations."
  (unless +straight--lockfile-version-updated-p
    (let ((lockfiles (append
                      (zenit-files-in (file-name-concat zenit-emacs-dir "straight/versions") :match ".el$")
                      (zenit-files-in (file-name-concat zenit-local-conf-dir "versions") :match ".el$")))
          (new-version-id (+straight--get-lockfile-version-id)))
      (when new-version-id  ; Only proceed if we have a valid version ID
        (dolist (lockfile lockfiles)
          (with-temp-buffer
            (insert-file-contents-literally lockfile)
            (goto-char (point-max))
            (when (re-search-backward "^:.+$" nil t)
              (replace-match new-version-id t t))
            (write-region nil nil lockfile nil 'silent)))))
    (setq +straight--lockfile-version-updated-p t)))


;;
;;; native-comp

(when (featurep 'native-compile)
  (with-eval-after-load 'comp-run
    ;; HACK Disable native-compilation for some troublesome packages
    (mapc (zenit-partial #'add-to-list 'native-comp-jit-compilation-deny-list)
          (list "/emacs-jupyter.*\\.el\\'"
                "/evil-collection-vterm\\.el\\'"
                "/vterm\\.el\\'"
                "/with-editor\\.el\\'"))))


;;
;;; Bootstrap

(defun zenit-read-packages (&optional module-list)
  "Retrieve a list of explicitly declared packages from MODULE-LIST.

If MODULE-LIST is omitted, read enabled module list in
configdepth order (see `zenit-module-set'). Otherwise,
MODULE-LIST may be any symbol (or t) to mean read all modules in
`zenit-modules-dir', including :core and :local-conf. MODULE-LIST may
also be a list of module keys."
  (let ((module-list (cond ((null module-list) (zenit-module-list))
                           ((symbolp module-list) (zenit-module-list 'all))
                           (module-list)))
        (packages-file zenit-module-packages-file))
    (with-zenit-context 'packages
      (when (assq :local-conf module-list)
        ;; We load the local packages file twice to populate
        ;; `zenit-disabled-packages' ASAP ...
        (load (zenit-module-expand-path :local-conf nil packages-file) 'noerror 'nomessage 'nosuffix))
      (cl-loop for (cat . mod) in module-list
               if (zenit-module-locate-path cat mod packages-file)
               do (load it 'noerror 'nomessage 'nosuffix))
      (when (assq :local-conf module-list)
        ;; ... and a second time to ensure locally overridden packages are
        ;; properly overwritten.
        (load (zenit-module-expand-path :local-conf nil packages-file) 'noerror 'nomessage 'nosuffix)))))

(defun zenit-initialize-packages (&optional force-p)
  "Ensures that the package system and straight.el are initialized.
If FORCE-P is non-nil, do it anyway. Use this before any of
straight's API to ensure all the necessary package metadata is
initialized and available for them."
  (unless zenit-init-packages-p
    (setq force-p t))
  (when (or force-p (not (bound-and-true-p package--initialized)))
    (zenit-log "Initializing package.el")
    (require 'package)
    (package-initialize))
  (when (or force-p (not (fboundp 'straight--reset-caches)))
    (zenit-log "Initializing straight")
    (setq zenit-init-packages-p t
          zenit-disabled-packages nil
          zenit-packages nil)
    (let ((straight-current-profile 'core))
      (zenit-bootstrap-straight)
      (zenit-log "Installing mandatory packages")
      (mapc #'straight-use-package zenit-mandatory-packages))
    (zenit-log "Initializing packages")
    (zenit-read-packages)
    (setq straight-current-profile nil)
    (zenit-log "Normalizing profiles")
    (+straight--fixup-profiles)))

(defun zenit-bootstrap-straight ()
  "Bootstrap `straight'.

Ensure it is installed and was compiled with this version of Emacs."
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 7)
        ;; Get commit which is stored in the lockfile
        (commit (let ((lockfile (zenit-path user-emacs-directory
                                            "straight/versions/"
                                            (concat
                                             (symbol-name straight-current-profile)
                                             ".el"))))
                  (if (file-exists-p lockfile)
                      (cdr (assoc
                            "straight.el"
                            (with-temp-buffer
                              (insert-file-contents-literally
                               lockfile)
                              (read (current-buffer)))))
                    (car
                     (split-string
                      (cdr (zenit-call-process
                            "git" "ls-remote"
                            "https://github.com/radian-software/straight.el.git"
                            "HEAD"))))))))
    (or (require 'straight nil t)
        (file-readable-p bootstrap-file)
        ;; Download install.el from the specified commit
        (with-current-buffer
            (url-retrieve-synchronously
             (format
              (concat "https://raw.githubusercontent.com/"
                      "radian-software/straight.el/%s/install.el")
              commit)
             'silent 'inhibit-cookies)
          ;; If we don't change the url here, straight.el will download an older
          ;; version which has a different `straight--build-cache-version' and
          ;; this will lead to a whole rebuild of all packages, even if not
          ;; necessary.
          (goto-char (point-min))
          (while (re-search-forward "radian-software/straight.el/install/%s/straight.el" nil t)
            (replace-match (format "radian-software/straight.el/%s/straight.el"
                                   commit)))
          (goto-char (point-max))
          (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))


;;
;;; Package management API

(defun zenit-package-get (package &optional prop nil-value)
  "Return PACKAGE's `package!' recipe from `zenit-packages'.

If PROP is provided, returns that property's value from the recipe. If
the property doesn't exist, returns NIL-VALUE (defaults to nil)."
  (let ((plist (cdr (assq package zenit-packages))))
    (if prop
        (if (plist-member plist prop)
            (plist-get plist prop)
          nil-value)
      plist)))

(defun zenit-package-set (package prop value)
  "Set PROP in PACKAGE's recipe to VALUE."
  (setf (alist-get package zenit-packages)
        (plist-put (alist-get package zenit-packages)
                   prop value)))

(autoload 'straight--convert-recipe "straight")
(defun zenit-package-recipe-repo (package)
  "Resolve and return PACKAGE's (symbol) local-repo property."
  (if-let* ((recipe (straight--convert-recipe package))
            (repo (straight--with-plist recipe (local-repo) local-repo)))
      repo
    (symbol-name package)))

(defun zenit-package-build-recipe (package &optional prop nil-value)
  "Return the `straight' recipe PACKAGE was installed with.

If PROP is provided, returns that property's value from the recipe. If
the property doesn't exist, returns NIL-VALUE (defaults to nil)."
  (let ((plist (nth 2 (gethash (symbol-name package) straight--build-cache))))
    (if prop
        (if (plist-member plist prop)
            (plist-get plist prop)
          nil-value)
      plist)))


;;
;;; Predicate functions

(defun zenit-package-built-in-p (package)
  "Return non-nil if PACKAGE (a symbol) is built-in."
  (eq (zenit-package-build-recipe package :type)
      'built-in))

(defun zenit-package-backend (package)
  "Return PACKAGE's backend.

Backend is either \\='straight, \\='builtin, \\='elpa or \\='other,
depending on how PACKAGE is installed."
  (cond ((gethash (symbol-name package) straight--build-cache)
         'straight)
        ((or (zenit-package-built-in-p package)
             (assq package package--builtins))
         'builtin)
        ((assq package package-alist)
         'elpa)
        ((locate-library (symbol-name package))
         'other)))

;;
;;; Package getters

(defun zenit-packages--read (file)
  "Read package declarations from FILE and update `zenit-packages' accordingly.

Extracts `package!' forms, processes their properties, and merges with
existing package declarations. Handles errors gracefully with
context-specific signals."
  (condition-case-unless-debug e
      ;; Prevent buffer-local state from propagating
      (with-temp-buffer
        (when (file-exists-p file)
          (insert-file-contents file)
          (with-syntax-table emacs-lisp-mode-syntax-table
            ;; Scrape `package!' blocks from FILE for a comprehensive listing of
            ;; packages used by this module.
            (while (search-forward "(package!" nil t)
              (let ((ppss (save-excursion (syntax-ppss))))
                ;; Don't collect packages in comments or strings
                (unless (or (nth 3 ppss)
                            (nth 4 ppss))
                  (goto-char (match-beginning 0))
                  (cl-destructuring-bind (_ name . plist)
                      (read (current-buffer))
                    (when-let* ((lockfile (or (when (eq (car (zenit-module-from-path file)) :local-conf)
                                                'local)
                                              (plist-get plist :lockfile))))
                      (setq plist (plist-put plist :lockfile (ensure-list lockfile))))
                    ;; Record what module this declaration was found in
                    (setq plist (plist-put plist :modules (list (zenit-module-context-key))))
                    ;; When a package is already registered in `zenit-packages',
                    ;; merge the :lockfile and :modules properties
                    (if-let* ((old-plist (cdr (assq name zenit-packages))))
                        (dolist (key '(:lockfile :modules))
                          (setq plist (plist-put
                                       plist key
                                       (cl-remove-duplicates
                                        (append (plist-get old-plist key) (plist-get plist key))
                                        :test #'equal)))))
                    (setf (alist-get name zenit-packages) plist))))))))
    (user-error
     (user-error (error-message-string e)))
    (error
     (signal 'zenit-package-error
             (list (zenit-module-context-key)
                   file e)))))

(defun zenit-package-list (&optional module-list)
  "Retrieve a list of explicitly declared packages from MODULE-LIST.

If MODULE-LIST is omitted, read enabled module list in
configdepth order (see `zenit-module-set'). Otherwise,
MODULE-LIST may be any symbol (or t) to mean read all modules in
`zenit-modules-dir', including :core and :user. MODULE-LIST may
also be a list of module keys."
  (let ((module-list (cond ((null module-list) (zenit-module-list))
                           ((symbolp module-list) (zenit-module-list 'all))
                           (module-list)))
        (packages-file zenit-module-packages-file)
        zenit-disabled-packages
        zenit-packages)
    (letf! (defun read-packages (key)
             (with-zenit-module-context key
               (when-let* ((file (zenit-module-locate-path
                                  (car key) (cdr key) packages-file)))
                 (zenit-packages--read file))))
      (with-zenit-context 'packages
        (let ((user? (assq :local-conf module-list)))
          (when user?
            ;; We load the private packages file twice to populate
            ;; `zenit-disabled-packages' disabled packages are seen ASAP...
            (let (zenit-packages)
              (read-packages (cons :local-conf nil))))
          (mapc #'read-packages module-list)
          ;; ...Then again to ensure privately overriden packages are properly
          ;; overwritten.
          (if user? (read-packages (cons :local-conf nil)))
          (nreverse zenit-packages))))))


;;
;;; Package macros

(defun zenit--process-pkg-dependencies (package)
  "Collect and register all dependencies for PACKAGE."
  (let ((dependencies
         (cons package
               (cl-remove-duplicates
                (cl-loop for dep in (straight--get-dependencies package)
                         collect dep into deps
                         finally return (append deps (apply 'append (mapcar 'straight--get-dependencies deps))))
                :test #'equal))))

    (dolist (pkg-name dependencies)
      (unless (equal pkg-name package)
        (let ((straight-current-profile '_pkg-dependency))
          (straight-register-package (intern pkg-name))))
      (add-to-list 'load-path (directory-file-name (straight--build-dir pkg-name)))
      (straight--load-package-autoloads pkg-name))))

(cl-defmacro package! (name &rest args &key built-in recipe ignore disable lockfile _env)
  "A wrapper around `straight-register-package'.

It allows to specifiy a lockfile and profile. NAME is the package name,
ARGS same as MELPA-STYLE-RECIPE in `straight-register-package'.

 :BUILT-IN BOOL | \\='prefer
   Same as :ignore if the package is a built-in Emacs package. If
   set to \\='prefer, the package will not be installed if it is
   already provided by Emacs.

 :RECIPE RECIPE
   A straight.el recipe

 :IGNORE FORM
   Do not install this package.

 :DISABLE BOOL
   Do not install or update this package AND disable all of its
   `use-package!' and `after!' blocks.

 :LOCKFILE LOCKFILE | BOOL | \\='ignore
   Which profile and lockfile to use. Will create LOCKFILE entry
   in `straight-profiles' and let bind
   `straight-current-profile'to LOCKFILE. If nil or t, default
   profile will be used. If set to \\='ignore, no profile will be
   used and the package will not be tracked.

 :env ALIST
   Parameters and envvars to set while the package is building. If these
   values change, the package will be rebuilt on next \\='emacs-config
   refresh'."
  (declare (indent defun))
  (when (and recipe (keywordp (car-safe recipe)))
     (cl-callf plist-put args :recipe `(quote ,recipe)))
  ;; :built-in t is basically an alias for :ignore (locate-library NAME)
  (when built-in
    (when (and (not ignore)
               (equal built-in '(quote prefer)))
      (setq built-in (locate-library (symbol-name name) nil (get 'load-path 'initial-value))))
    (cl-callf map-delete args :built-in)
    (cl-callf plist-put args :ignore built-in)
    (setq ignore built-in))

  ;; Disable package if requested
  (when disable
    (add-to-list 'zenit-disabled-packages name)
    ;; Remove the package from the recipe cache, if it exists.
    (remhash (symbol-name name) straight--recipe-cache)
    (setq ignore t))

  ;; Check if package has been disabled
  (when (memq name zenit-disabled-packages)
    (setq ignore t))

  (unless ignore
    ;; Add profile to `straight-profiles' if lockfile should not be ignored
    (unless (memq lockfile '(ignore nil t))
      (pushnew! straight-profiles `(,(zenit-unquote lockfile) . ,(format "%s.el" (zenit-unquote lockfile)))))
    ;; Set `straight-current-profile' according to lockfile
    (let ((lockfile-profile (if (eq (car (zenit-module-from-path (protect-macros! (file!)))) :local-conf)
                                'local
                              (pcase lockfile
                                ((or `nil `t) nil)
                                (`ignore 'ignore)
                                (_ lockfile)))))
      (when lockfile
        (cl-callf plist-put args :lockfile `(quote ,lockfile-profile)))
      `(let* ((straight-current-profile ',(zenit-unquote lockfile-profile))
              (name ',name)
              (plist (cdr (assq name zenit-packages)))
              (dir (dir!))
              (module (zenit-module-from-path dir)))
         (unless (zenit-context-p 'packages)
           (signal 'zenit-module-error
                   (list module "package! can only be used in packages.el files")))
         ;; Record what module this declaration was found in
         (let ((module-list (plist-get plist :modules)))
           (unless (member module module-list)
             (cl-callf plist-put plist :modules
                       (append module-list
                               (list module)
                               (when (file-in-directory-p dir zenit-local-conf-dir)
                                 '((:local-conf . modules)))
                               nil))))
         ;; Merge given plist with pre-existing one
         (cl-loop for (key value) on (list ,@args) by 'cddr
                  when value
                  do (cl-callf plist-put plist key value))
         ;; Some basic key validation; throws an error on invalid properties
         (condition-case e
             (when-let (recipe (plist-get plist :recipe))
               (cl-destructuring-bind
                   (&key local-repo _files _flavor _build _pre-build _post-build
                         _includes _type _repo _host _branch _protocol _remote
                         _nonrecursive _fork _depth _source _inherit)
                   recipe
                 ;; Expand :local-repo from current directory
                 (when local-repo
                   (cl-callf plist-put plist :recipe
                             (plist-put recipe :local-repo
                                        (let ((local-path (expand-file-name local-repo dir)))
                                          (if (file-directory-p local-path)
                                              local-path
                                            local-repo)))))))
           (error
            (signal 'zenit-package-error
                    (cons ,(symbol-name name)
                          (error-message-string e)))))
         ;; Explicitly register dependencies so they don't get purged and add
         ;; them to the load-path. Use a virtual straight profile so later on, a
         ;; lockfile will not be written for it.
         (zenit--process-pkg-dependencies ,(symbol-name name))
         ,(if recipe
              `(progn
                 (when (gethash (symbol-name ',name) straight--recipe-cache)
                   (remhash (symbol-name ',name) straight--recipe-cache))
                 (straight-override-recipe '(,name ,@recipe))
                 (straight-register-package '(,name ,@recipe)))
            `(straight-register-package ',name))
         (cl-pushnew straight-current-profile
                     (gethash ,(symbol-name name) straight--profile-cache)
                     :test #'eq)
         ;; Return non-nil if we did register packages
         (setf (alist-get name zenit-packages) plist)
         (with-no-warnings
           (cons name plist))))))
(function-put 'package! 'lisp-indent-function 'defun)

(defmacro disable-packages! (&rest packages)
  "A convenience macro for disabling PACKAGES in bulk.
Only use this macro in a module's or `zenit-local-conf-dir''s packages.el
file."
  (macroexp-progn
   (mapcar (lambda (p) `(package! ,p :disable t))
           packages)))

(provide 'zenit-packages)

;;; zenit-packages.el ends here.

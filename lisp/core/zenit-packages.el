;; lisp/core/zenit-packages.el -*- lexical-binding: t; -*-

(defvar zenit-init-packages-p nil
  "Non-nil if the package management system has been initialized.")

(defvar zenit-mandatory-packages '(straight async)
  "A list of packages that must be installed.

These packages will be auto-installed if missing and shouldn't be
deleted.")

(defvar zenit-packages ()
  "A list of enabled packages.

 Each element is a sublist, whose CAR is the package's name as a
symbol, and whose CDR is the plist supplied to its `package!'
declaration.")

(defvar zenit-disabled-packages ()
  "List of packages that should be ignored by `use-package!' and `after!'.")

(defvar zenit-packages-file "packages"
  "The basename of packages file for modules.")


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
        ;; Packages which are pinned to a specific commit.
        (pinned . "pinned.el")
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

(defun zenit-packages-get-lockfile (profile)
  "Get the version lockfile for given PROFILE, a symbol.

Returns nil if the profile does not exist or does not have a
lockfile."
  (if-let* ((filename (alist-get profile straight-profiles)))
      (straight--versions-file filename)
    nil))

(defun +straight--normalize-profiles ()
  "Normalize packages profiles.

This involves

  1. Ensure that recipe repositories are only registered under
     the \\='core profile

  2. If a local repository provides multiple packages (like
     `magit') and one or more is registered as a dependency,
     ensure that each dependency is assigned to a proper profile.

  3. If a package is associated with multiple profiles and one of
     them is nil, remove nil. This ways we make sure that there
     are no interactively registered packages after init.

  4. Remove the \\='dep profile from packages which are also
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
                 (when (member (intern package) straight-recipe-repositories)
                   (setq new-profiles '(core)))

                 ;; Assign proper profiles based on the repository package map.
                 (when (member 'dep profiles)
                   (dolist (repo-package (hash-table-values repo-package-map))
                     (when (member package repo-package)
                       (dolist (pkg repo-package)
                         (unless (equal pkg package)
                           (setq new-profiles
                                 (cl-union new-profiles
                                           (gethash pkg straight--profile-cache)
                                           :test #'equal)))))))

                 ;; Remove 'dep' profile if there are other proper profiles.
                 (when (and (member 'dep new-profiles) (> (length new-profiles) 1))
                   (setq new-profiles (remove 'dep new-profiles)))

                 ;; Remove nil entries if there are other profiles.
                 (when (and (remove nil new-profiles) (> (length new-profiles) 1))
                   (setq new-profiles (remove nil new-profiles)))

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
  (or (bound-and-true-p zenit-auto-accept)
      (if (not noninteractive)
          (funcall orig-fn prompt)
        (y-or-n-p (format! "%s" (or prompt ""))))))

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
      (if zenit-auto-discard
          (cl-loop with zenit-auto-accept = t
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

(defadvice! +straight--respect-print-indent-a (args)
  "Indent straight progress messages to respect
`zenit-format-indent', so we don't have to pass whitespace to
`straight-use-package's fourth argument everywhere we use it (and
internally)."
  :filter-args #'straight-use-package
  (cl-destructuring-bind
      (melpa-style-recipe &optional no-clone no-build cause interactive)
      args
    (list melpa-style-recipe no-clone no-build
          (if (and (not cause)
                   (boundp 'zenit-format-indent)
                   (> zenit-format-indent 0))
              (make-string (1- (or zenit-format-indent 1)) 32)
            cause)
          interactive)))


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
        (packages-file zenit-module-packages-file)
        zenit-disabled-packages
        zenit-packages)
    (zenit-context-with 'packages
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
          zenit-packages (zenit-package-list))
    (setq straight-current-profile 'core)
    (zenit-bootstrap-straight)
    (zenit-log "Installing mandatory packages")
    (mapc #'straight-use-package zenit-mandatory-packages)
    (zenit-log "Initializing packages")
    (zenit-read-packages)
    (setq straight-current-profile nil)
    (zenit-log "Normalizing profiles")
    (+straight--normalize-profiles)))

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
                    (when-let* ((lockfile (plist-get plist :lockfile)))
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
             (zenit-module-context-with key
               (when-let* ((file (zenit-module-locate-path
                                  (car key) (cdr key) packages-file)))
                 (zenit-packages--read file))))
      (zenit-context-with 'packages
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
        (let ((straight-current-profile 'dep))
          (straight-register-package (intern pkg-name))))
      (add-to-list 'load-path (directory-file-name (straight--build-dir pkg-name)))
      (straight--load-package-autoloads pkg-name))))

(cl-defmacro package! (name &rest args &key built-in recipe ignore disable lockfile pin)
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

 :LOCKFILE LOCKFILE | BOOL | \\='ignore | \\='pinned
   Which profile and lockfile to use. Will create LOCKFILE entry
   in `straight-profiles' and let bind
   `straight-current-profile'to LOCKFILE. If nil or t, default
   profile will be used. If set to \\='ignore, no profile will be
   used and the package will not be tracked.
   If :pin is defined, lockfile will be set to \\='pinned.

 :PIN STR
   Pin this package to commit hash STR."
  (declare (indent defun))
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

  ;; Set lockfile to 'pinned if :pin is non-nil
  (when pin
    (setq lockfile 'pinned))
  (unless ignore
    ;; Add profile to `straight-profiles' if lockfile should not be ignored
    (unless (memq lockfile '(ignore nil t))
      (pushnew! straight-profiles `(,(zenit-unquote lockfile) . ,(format "%s.el" (zenit-unquote lockfile)))))
    ;; Set `straight-current-profile' according to lockfile
    (let ((lockfile-profile (pcase lockfile
                              ((or `nil `t) nil)
                              (`ignore 'ignore)
                              (`pinned 'pinned)
                              (_ lockfile))))
      `(let ((straight-current-profile ',(zenit-unquote lockfile-profile)))
         ;; Explicitly register dependencies so they don't get purged and add
         ;; them to the load-path. Use a virtual straight profile so later on, a
         ;; lockfile will not be written for it.
         (zenit--process-pkg-dependencies ,(symbol-name name))
         ,(if recipe
              `(straight-register-package '(,name ,@recipe))
            `(straight-register-package ',name))
         ,(when (and (eq lockfile 'pinned) pin)
            `(add-to-list 'straight-x-pinned-packages
              '(,(zenit-package-recipe-repo name) . ,pin)))
         ;; Return true if we did register packages
         t))))
(function-put 'package! 'lisp-indent-function 'defun)

(defmacro disable-packages! (&rest packages)
  "A convenience macro for disabling PACKAGES in bulk.
Only use this macro in a module's or `zenit-local-conf-dir''s packages.el
file."
  (macroexp-progn
   (mapcar (lambda (p) `(package! ,p :disable t))
           packages)))

(provide 'zenit-packages)

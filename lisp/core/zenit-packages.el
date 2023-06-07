;; lisp/init-packages.el -*- lexical-binding: t; -*-

(defvar zenit-init-packages-p nil
  "If non-nil, the package management system has been
initialized.")

(defvar zenit-mandatory-packages '(straight)
  "A list of packages that must be installed (and will be
auto-installed if missing) and shouldn't be deleted.")

(defvar zenit-disabled-packages ()
  "A list of packages that should be ignored by `use-package!'
 and `after!'.")

(defvar zenit-packages-file "packages"
  "The basename of packages file for modules.")

;;
;;; package.el

;; Ensure that, if we do need package.el, it is configured correctly. You really
;; shouldn't be using it, but it may be convenient for quick package testing.
(setq package-enable-at-startup nil
      package-user-dir (concat zenit-local-dir "elpa/")
      package-gnupghome-dir (expand-file-name "gpg" package-user-dir)
      ;; I omit Marmalade because its packages are manually submitted rather
      ;; than pulled, so packages are often out of date with upstream.
      package-archives
      (let ((proto (if gnutls-verify-error "https" "http")))
        (list (cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
              (cons "melpa" (concat proto "://melpa.org/packages/"))
              (cons "org"   (concat proto "://orgmode.org/elpa/")))))

(advice-add #'package--ensure-init-file :override #'ignore)

(defadvice! zenit--package-inhibit-custom-file-a (&optional value)
  "Don't save `package-selected-packages' to `custom-file'."
  :override #'package--save-selected-packages
  (if value (setq package-selected-packages value)))

;; Refresh package.el the first time you call `package-install'
(add-transient-hook! 'package-install (package-refresh-contents))


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
      ;; We handle package.el ourselves (and a little more comprehensively)
      straight-enable-package-integration nil
      ;; Use shallow clones
      ;; straight-vc-git-default-clone-depth 1
      ;; Tell straight.el about the profiles we are going to be using.
      straight-profiles
      '(;; Packages registered by the packages.el files in `zenit-core-dir'
        (core . "core.el")
        ;; Packages registered interactively.
        (nil . "default.el")
        ;; Packages which are pinned to a specific commit.
        (pinned . "pinned.el")
        ;; Packages registered in the packages.el file in `zenit-local-conf-dir'
        (local . "local.el")))

(with-eval-after-load 'straight
  ;; HACK: We want to defer the compilation of the .elc files in order to save
  ;; some minutes during config creation. To complete this, straight.el needs to
  ;; be told not to do native-compilation, but it won't obey
  ;; `straight-disable-native-compile', but `straight--native-comp-available',
  ;; though. Trouble is: it's a constant; it resets itself when straight is
  ;; loaded, so it must be changed afterwards.
  (setq straight--native-comp-available nil
        comp-enable-subr-trampolines nil)
  ;; `let-alist' is built into Emacs 26 and onwards
  (add-to-list 'straight-built-in-pseudo-packages 'let-alist))


;;;; Straight hacks

(defadvice! +straight--ignore-missing-lockfile-a (orig-fn profile)
  "If a profile is not defined in `straight-profiles', return a
random string which should never exist as a file name."
  :around #'straight--versions-lockfile
  (condition-case _
      (funcall orig-fn profile)
    (error
     (md5 (format "%s%s%s%s" (system-name) (emacs-pid) (current-time) (random))))))

(defun +straight--normalize-profiles ()
  "If a package is associated with multiple profiles and one of
them is nil, remove nil. This ways we make sure that there are no
interactively registered packages after init."
  (dolist (package (hash-table-keys straight--profile-cache))
    (let ((entry (gethash package straight--profile-cache)))
      (when (and (length> entry 1) (member nil entry))
        (puthash package (remq nil entry) straight--profile-cache)))))

(defvar +straight--auto-options
  '(("has diverged from"
     . "^Reset [^ ]+ to branch")
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
  (after! comp
    ;; HACK Disable native-compilation for some troublesome packages
    (mapc (zenit-partial #'add-to-list 'native-comp-jit-compilation-deny-list)
          (list "/emacs-jupyter.*\\.el\\'"
                "/evil-collection-vterm\\.el\\'"
                "/vterm\\.el\\'"
                "/with-editor\\.el\\'"))))


;;
;;; Bootstrap

(defun zenit-read-packages (&optional module-list)
  "Retrieve a list of explicitly declared packages from
 MODULE-LIST.

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
    (zenit-context-with 'packages
      (when (assq :user module-list)
        ;; We load the local packages file twice to populate
        ;; `zenit-disabled-packages' disabled packages are seen ASAP, and a
        ;; second time to ensure locally overridden packages are properly
        ;; overwritten.
        (load (zenit-module-expand-path :user nil packages-file) 'noerror 'nomessage 'nosuffix))
      (cl-loop for (cat . mod) in module-list
               if (zenit-module-locate-path cat mod packages-file)
               do (load it 'noerror 'nomessage 'nosuffix)))))

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
    (setq zenit-init-packages-p t)
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
  "Ensure `straight' is installed and was compiled with this
version of Emacs."
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5)
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

(autoload 'straight--convert-recipe "straight")
(defun zenit-package-recipe-repo (package)
  "Resolve and return PACKAGE's (symbol) local-repo property."
  (if-let* ((recipe (straight--convert-recipe package))
            (repo (straight--with-plist recipe (local-repo) local-repo)))
      repo
    (symbol-name package)))

(cl-defmacro package! (name &rest args &key built-in recipe ignore disable lockfile pin)
  "A wrapper around `straight-register-package' which allows to
specifiy a lockfile and profile. NAME is the package name.

 :built-in BOOL | \\='prefer
   Same as :ignore if the package is a built-in Emacs package. If
   set to 'prefer, the package will not be installed if it is
   already provided by Emacs.

 :recipe RECIPE
   A straight.el recipe

 :ignore FORM
   Do not install this package.

 :disable BOOL
   Do not install or update this package AND disable all of its
   `use-package!' and `after!' blocks.

 :lockfile LOCKFILE | BOOL | \\='ignore | \\='pinned
   Which profile and lockfile to use. Will create LOCKFILE entry
   in `straight-profiles' and let bind
   `straight-current-profile'to LOCKFILE. If nil or t, default
   profile will be used. If set to \\='ignore, no profile will be
   used and the package will not be tracked.
   If :pin is defined, lockfile will be set to \\='pinned.

 :pin STR
   Pin this package to commit hash STR."
  (declare (indent defun))
  (when built-in
    ;; :built-in t is basically an alias for :ignore (locate-library NAME)
    (when (and (not ignore)
               (equal built-in '(quote prefer)))
      (setq built-in (locate-library (symbol-name name) nil (get 'load-path 'initial-value))))
    (cl-callf map-delete args :built-in)
    (cl-callf plist-put args :ignore built-in)
    (setq ignore built-in))
  (when disable
    (add-to-list 'zenit-disabled-packages name)
    ;; Remove the package from the recipe cache, if it exists.
    (remhash (symbol-name name) straight--recipe-cache)
    (setq ignore t))
  (when pin
    ;; set lockfile if to 'pinned if :pin is non-nil
    (setq lockfile 'pinned))
  (unless ignore
    ;; Add profile to `straight-profiles' if lockfile should not be ignored
    (unless (memq lockfile '(ignore nil t))
      (pushnew! straight-profiles `(,(zenit-unquote lockfile) . ,(format "%s.el" (zenit-unquote lockfile)))))
    ;; Set `straight-current-profile' according to lockfile
    (let* ((lockfile-profile (cond ((not lockfile) nil)
                                   ((eq lockfile 'ignore) 'ignore)
                                   ((eq lockfile 'pinned) 'pinned)
                                   ((not (booleanp lockfile)) lockfile)
                                   (t nil)))
           ;; Collect all package dependencies (and their dependencies!)
           (all-deps (cons (symbol-name name)
                           (cl-remove-duplicates
                            (cl-loop for dep in (straight--get-dependencies (symbol-name name))
                                     collect dep into deps
                                     finally return (append deps (apply 'append (mapcar 'straight--get-dependencies deps))))
                            :test #'equal))))
      `(let ((straight-current-profile ',(zenit-unquote lockfile-profile)))
         ,(if recipe
              `(straight-register-package '(,name ,@recipe))
            `(straight-register-package ',name))
         ;; Explicitly register dependencies so they don't get purged and add
         ;; them to the load-path. Use a virtual straight profile so later on,
         ;; a lockfile will not be written for it.
         (dolist (pkg-name ',all-deps)
           (unless (equal pkg-name (symbol-name ',name))
             (let ((straight-current-profile 'dep))
               (straight-register-package (intern pkg-name))))
           (add-to-list 'load-path (directory-file-name (straight--build-dir pkg-name)))
           (straight--load-package-autoloads pkg-name))
         ,(when (and (eq lockfile 'pinned) pin)
            `(add-to-list 'straight-x-pinned-packages
              '(,(zenit-package-recipe-repo name) . ,pin)))))))

(provide 'zenit-packages)

;; lisp/core/zenit-projects.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

;; `cl-seq'
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `subr-x'
(declare-function hash-table-keys "subr-x" (hash-table))

;; `zenit-lib-files'
(cl-eval-when (compile)
  (autoload #'file-exists-p! "zenit-lib-files" nil nil 'macro))

;; `zenit-lib-process'
(declare-function zenit-call-process "zenit-lib-process" (command &rest args))

;; `zenit-lib-projects'
(declare-function zenit-project-name "zenit-lib-projects" (&optional dir))
(declare-function zenit-project-p "zenit-lib-projects" (&optional dir))
(declare-function zenit-project-ignored-p "zenit-lib-projects" (project-root))
(declare-function zenit-project-root "zenit-lib-projects" (&optional dir))


(defvar zenit-fd-executable (cl-find-if #'executable-find (list "fdfind" "fd"))
  "The filename of the `fd' executable.

On some distros it's \\='fdfind\\=' (ubuntu, debian, and
derivatives). On most it's \\='fd\\='.")

(defvar zenit-ripgrep-executable (executable-find "rg")
  "The filename of the Ripgrep executable.

Is nil if no executable is found in your PATH during startup.")


(defvar zenit-projectile-cache-dir (file-name-concat zenit-cache-dir "projectile/")
  "Directory with per-project projectile file index caches.
Must end with a slash.")


;;
;;; Packages

(use-package! project
  :defer t
  :init
  (setq project-list-file (file-name-concat zenit-cache-dir "projects"))
  :config
  (add-to-list 'project-vc-extra-root-markers ".jj"))


(use-package! projectile
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file
             projectile-relevant-known-projects)
  :init
  (setq projectile-enable-caching (if noninteractive t 'persistent)
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-ignored-projects '("~/")
        projectile-known-projects-file (concat zenit-projectile-cache-dir "projects.eld")
        projectile-ignored-project-function #'zenit-project-ignored-p
        projectile-fd-executable zenit-fd-executable)

  (global-set-key [remap evil-jump-to-tag] #'projectile-find-tag)
  (global-set-key [remap find-tag]         #'projectile-find-tag)

  :config
  (make-directory zenit-projectile-cache-dir t)

  ;; Projectile runs four functions to determine the root (in this order):
  ;;
  ;; + `projectile-root-local' -> checks the `projectile-project-root' variable
  ;;    for an explicit path.
  ;; + `projectile-root-bottom-up' -> searches from / to your current directory
  ;;   for the paths listed in `projectile-project-root-files-bottom-up'. This
  ;;   includes .git and .project
  ;; + `projectile-root-top-down' -> searches from the current directory down to
  ;;   / the paths listed in `projectile-root-files', like package.json,
  ;;   setup.py, or Cargo.toml
  ;; + `projectile-root-top-down-recurring' -> searches from the current
  ;;   directory down to / for a directory that has one of
  ;;   `projectile-project-root-files-top-down-recurring' but doesn't have a
  ;;   parent directory with the same file.
  ;;
  ;; These will be filled by other modules. We build this list manually so
  ;; projectile doesn't perform so many file checks every time it resolves a
  ;; project's root -- particularly when a file has no project.
  (setq projectile-project-root-files '()
        projectile-project-root-files-top-down-recurring '("Makefile"))

  ;; Adds a more editor/plugin-agnostic project marker.
  (add-to-list 'projectile-project-root-files-bottom-up ".project")

  (push (abbreviate-file-name zenit-local-dir) projectile-globally-ignored-directories)

  ;; Per-project compilation buffers
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name
        compilation-save-buffers-predicate #'projectile-current-project-buffer-p)

  ;; HACK 2025-04-02: Centralize Projectile's per-project cache files, so they
  ;;   don't litter projects with dotfiles.
  (defadvice! zenit--projectile-centralized-cache-files-a (fn &optional proot)
    :around #'projectile-project-cache-file
    (let* ((proot (or proot (zenit-project-root) default-directory))
           (projectile-cache-file
            (expand-file-name
             (format "%s-%s" (zenit-project-name proot) (sha1 proot))
             zenit-projectile-cache-dir)))
      (funcall fn proot)))

  ;; HACK 2025-04-02: `projectile-ensure-project' operates on the current value
  ;;   of `projectile-known-projects' when prompting the using for a project,
  ;;   which may not have been initialized yet, so do so the first time it is
  ;;   called.
  (defadvice! zenit--projectile-update-known-projects-a (dir)
    :before #'projectile-ensure-project
    (unless dir
      (when (and (eq projectile-require-project-root 'prompt)
                 (not projectile-known-projects))
        (projectile-known-projects))
      (advice-remove 'projectile-ensure-project #'zenit--projectile-update-known-projects-a)))

  ;; Support the more generic .project files as an alternative to .projectile
  (defadvice! zenit--projectile-dirconfig-file-a ()
    :override #'projectile-dirconfig-file
    (let ((proot (projectile-project-root)))
      (cond ((file-exists-p! (or projectile-dirconfig-file ".project") proot))
            ((expand-file-name ".project" proot)))))

  ;; Disable commands that we provide an alternative for.
  (put 'projectile-ag 'disabled "Use +default/search-project instead")
  (put 'projectile-ripgrep 'disabled "Use +default/search-project instead")
  (put 'projectile-grep 'disabled "Use +default/search-project instead")

  ;; HACK: Some MSYS utilities auto expanded the `/' path separator, so we need
  ;;   to prevent it.
  (eval-when! zenit--system-windows-p
    (setenv "MSYS_NO_PATHCONV" "1") ; Fix path in Git Bash
    (setenv "MSYS2_ARG_CONV_EXCL" "--path-separator")) ; Fix path in MSYS2

  ;; HACK Don't rely on VCS-specific commands to generate our file lists. That's
  ;;   7 commands to maintain, versus the more generic, reliable and performant
  ;;   `fd' or `ripgrep'.
  (defadvice! zenit--only-use-generic-command-a (fn vcs)
    "Only use `projectile-generic-command' for indexing project files.
And if it's a function, evaluate it."
    :around #'projectile-get-ext-command
    (if (and (functionp projectile-generic-command)
             (not (file-remote-p default-directory)))
        (funcall projectile-generic-command vcs)
      (let ((projectile-git-submodule-command
             (or projectile-git-submodule-command
                 (get 'projectile-git-submodule-command 'initial-value))))
        (funcall fn vcs))))

  ;; HACK: `projectile-generic-command' doesn't typically support a function,
  ;;   but the `zenit--only-use-generic-command-a' advice allows this. I do it
  ;;   this way so that projectile can adapt to remote systems (over TRAMP),
  ;;   rather then look for fd/ripgrep on the remote system simply because it
  ;;   exists on the host. It's faster too.
  (put 'projectile-git-submodule-command 'initial-value projectile-git-submodule-command)
  (setq projectile-git-submodule-command nil
        ;; Include and follow symlinks in file listings.
        projectile-git-fd-args (concat "-tl " projectile-git-fd-args)
        projectile-indexing-method 'hybrid
        projectile-generic-command
        (lambda (_)
          ;; If fd exists, use it for git and generic projects. fd is a rust
          ;; program that is significantly faster than git ls-files or find, and
          ;; it respects .gitignore. This is recommended in the projectile docs.
          (cond
           ((when-let*
                ((zenit-fd-executable)
                 (projectile-git-use-fd)
                 ;; REVIEW Temporary fix for #6618. Improve me later.
                 (version (with-memoization (get 'zenit-fd-executable 'version)
                            (cadr (split-string (cdr (zenit-call-process zenit-fd-executable "--version"))
                                                " " t))))
                 ((ignore-errors (version-to-list version))))
              (string-join
               (delq
                nil (list zenit-fd-executable "."
                          (if (version< version "8.3.0")
                              (replace-regexp-in-string "--strip-cwd-prefix" "" projectile-git-fd-args t t)
                            projectile-git-fd-args)
                          (when project-vc-backend-markers-alist
                            (mapconcat (fn! (format "-E %s" (shell-quote-argument (cdr %))))
                                       project-vc-backend-markers-alist
                                       " "))
                          (if zenit--system-windows-p " --path-separator=/" "")))
               " ")))
           ;; Otherwise, resort to ripgrep, which is also faster than find
           (zenit-ripgrep-executable
            (string-join
             (delq
              nil (list zenit-ripgrep-executable
                        "-0 --files --follow --color=never --hidden"
                        (when project-vc-backend-markers-alist
                          (mapconcat (fn! (format "-g!%s" (shell-quote-argument (cdr %))))
                                     project-vc-backend-markers-alist
                                     " "))
                        (if zenit--system-windows-p " --path-separator=/")))
             " "))
           ((not zenit--system-windows-p) "find . -type f | cut -c3- | tr '\\n' '\\0'")
           ("find . -type f -print0"))))

  (defadvice! zenit--projectile-default-generic-command-a (fn &rest args)
    "If projectile can't tell what kind of project you're in, it
issues an error when using many of projectile's command, e.g.
`projectile-compile-command', `projectile-run-project',
`projectile-test-project', and `projectile-configure-project',
for instance.

This suppresses the error so these commands will still run, but
prompt you for the command instead."
    :around #'projectile-default-generic-command
    (ignore-errors (apply fn args)))

  (projectile-mode +1))


;;
;;; Project-based minor modes

(defvar zenit-project-hook nil
  "Hook run when a project is enabled.
The name of the project's mode and its state are passed in.")

(cl-defmacro def-project-mode! (name &key
                                     modes
                                     files
                                     when
                                     match
                                     add-hooks
                                     on-load
                                     on-enter
                                     on-exit)
  "Define a project minor mode named NAME and where/how it is
activated.

Project modes allow you to configure \\='sub-modes\\=' for
major-modes that are specific to a folder, project structure,
framework or whatever arbitrary context you define. These project
modes can have their own settings, keymaps, hooks, snippets, etc.

This creates NAME-hook and NAME-map as well.

PLIST may contain any of these properties, which are all checked
to see if NAME should be activated. If they are *all* true, NAME
is activated.

  :modes MODES -- if buffers are derived from MODES (one or a
  list of symbols).

  :files FILES -- if project contains FILES; takes a string or a
    form comprised of nested (and ...) and/or (or ...) forms.
    Each path is relative to the project root, however, if
    prefixed with a \\='.\\=' or \\='..\\=', it is relative to
    the current buffer.

  :match REGEXP -- if file name matches REGEXP

  :when PREDICATE -- if PREDICATE returns true (can be a form or
    the symbol of a function)

  :add-hooks HOOKS -- HOOKS is a list of hooks to add this mode's
    hook.

  :on-load FORM -- FORM to run the first time this project mode
    is enabled.

  :on-enter FORM -- FORM is run each time the mode is activated.

  :on-exit FORM -- FORM is run each time the mode is disabled.

Relevant: `zenit-project-hook'."
  (declare (indent 1))
  (let ((init-var (intern (format "%s-init" name))))
    (macroexp-progn
     (append
      (when on-load
        `((defvar ,init-var nil)))
      `((define-minor-mode ,name
          "A project minor mode generated by `def-project-mode!'."
          :init-value nil
          :lighter ""
          :keymap (make-sparse-keymap)
          (if (not ,name)
              ,on-exit
            (run-hook-with-args 'zenit-project-hook ',name ,name)
            ,(when on-load
               `(unless ,init-var
                  ,on-load
                  (setq ,init-var t)))
            ,on-enter))
        (dolist (hook ,add-hooks)
          (add-hook ',(intern (format "%s-hook" name)) hook)))
      (cond ((or files modes when)
             (cl-check-type files (or null list string))
             (let ((fn
                    `(lambda ()
                       (and (not (bound-and-true-p ,name))
                            (and buffer-file-name (not (file-remote-p buffer-file-name nil t)))
                            ,(or (null match)
                                 `(if buffer-file-name (string-match-p ,match buffer-file-name)))
                            ,(or (null files)
                                 ;; Wrap this in `eval' to prevent eager expansion
                                 ;; of `project-file-exists-p!' from pulling in
                                 ;; autoloaded files prematurely.
                                 `(eval
                                   '(project-file-exists-p!
                                     ,(if (stringp (car files)) (cons 'and files) files))))
                            ,(or when t)
                            (,name 1)))))
               (if modes
                   `((dolist (mode ,modes)
                       (let ((hook-name
                              (intern (format "zenit--hook-enable-%s%s" ',name
                                              (if (eq mode t) "" (format "-in-%s" mode))))))
                         (fset hook-name #',fn)
                         (if (eq mode t)
                             (add-to-list 'auto-minor-mode-magic-alist (cons hook-name #',name))
                           (add-hook (intern (format "%s-hook" mode)) hook-name)))))
                 `((add-hook 'change-major-mode-after-body-hook #',fn)))))
            (match
             `((add-to-list 'auto-minor-mode-alist (cons ,match #',name)))))))))

(provide 'zenit-projects)

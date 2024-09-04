;; lisp/core/zenit-core.el -*- lexical-binding: t; -*-

;; This is the core of this emacs configuration. From here, all requirements get
;; loaded and started.
;;
;; Load order (>: load, X: execute)
;;
;;   > ~/.emacs.d/early-init.el
;;     > ~/.emacs.d/lisp/core/zenit-core.el
;;       > ~/.emacs.d/lisp/core/zenit-lib.el
;;     > ~/.emacs.d/lisp/zenit-start.el
;;       X hook: `zenit-before-init-hook'
;;       > ~/.emacs.d/site-lisp/init.el
;;   X hook: `before-init-hook'
;;   > ~/.emacs.d/init.el
;;     > ~/.emacs.d/zenit-{use-package,el-patch,keybinds,ui,projects,editor}.el
;;     X hook: `zenit-before-modules-init-hook'
;;     - {~/.emacs.d/site-lisp/, ~/.emacs.d/lisp}/modules/*/*/init.el
;;     X hook: `zenit-after-modules-init-hook'
;;     X hook: `zenit-before-modules-config-hook'
;;     - {~/.emacs.d/site-lisp/, ~/.emacs.d/lisp}/modules/*/*/config.el
;;     X hook: `zenit-after-modules-config-hook'
;;     - ~/.emacs.d/site-lisp/config.el
;;     - `custom-file' or ~/.emacs.d/site-lisp/custom.el
;;   X hook: `after-init-hook'
;;   X hook: `emacs-startup-hook'
;;   X hook: `window-setup-hook'
;;   X hook: `zenit-init-ui-hook'
;;   X hook: `zenit-after-init-hook'
;;   > After startup is complete:
;;     X On first input:              `zenit-first-input-hook'
;;     X On first switched-to buffer: `zenit-first-buffer-hook'
;;     X On first opened file:        `zenit-first-file-hook'

(eval-when-compile
  (require 'cl-lib))

;; `async'
(defvar async-byte-compile-log-file)

;; `desktop'
(defvar desktop-dirname)
(defvar desktop-base-file-name)
(defvar desktop-base-lock-name)

(defvar pcache-directory)
(defvar request-storage-directory)

;; `auth-source'
(defvar auth-sources)

;; `comp'
(defvar native-comp-async-report-warnings-errors)
(defvar native-comp-warning-on-missing-source)
(defvar comp-num-cpus)
(defvar native-comp-async-jobs-number)

;; `gnutls'
(defvar gnutls-verify-error)
(defvar gnutls-algorithm-priority)
(defvar gnutls-min-prime-bits)
(defvar gnutls-verify-error)

(defvar tls-checktrust)
(defvar tls-program)

;; `novice'
(declare-function en/disable-command "novice" (command disable))

;; `warnings'
(defvar warning-suppress-types)


;;
;;; Custom features & Global constants

(defconst zenit-operating-system
  (pcase system-type
    ('darwin                           '(macos unix))
    ((or 'cygwin 'windows-nt 'ms-dos)  '(windows))
    ((or 'gnu 'gnu/linux)              '(linux unix))
    ((or 'gnu/kfreebsd 'berkeley-unix) '(bsd unix)))
  "A list of symbols denoting the current operating system.")

;; Make the operating system available to `featurep'
(push :system features)
(put :system 'subfeatures zenit-operating-system)

;; Convenience aliases for internal use
(defconst zenit-system            (car zenit-operating-system))
(defconst zenit--system-windows-p (featurep :system 'windows))
(defconst zenit--system-macos-p   (featurep :system 'macos))
(defconst zenit--system-linux-p   (featurep :system 'linux))
(defconst zenit--system-unix-p    (featurep :system 'unix))
(defconst zenit--system-bsd-p     (featurep :system 'bsd))

;; Add build features to `features' so they can be checked via `featurep'
(if (bound-and-true-p module-file-suffix)
    (push 'dynamic-modules features))
(if (fboundp #'json-parse-string)
    (push 'jansson features))
(if (string-match-p "HARFBUZZ" system-configuration-features)
    (push 'harfbuzz features))

;; `native-compile' exists whether or not it is functional, so pretend it
;; doesn't exist if it isn't available.
(if (featurep 'native-compile)
    (if (not (native-comp-available-p))
        (delq 'native-compile features)))

;; $HOME isn't normally defined on Windows, but many unix tools expect it.
(when zenit--system-windows-p
  (when-let (realhome
             (and (null (getenv-internal "HOME"))
                  (getenv "USERPROFILE")))
    (setenv "HOME" realhome)
    (setq abbreviated-home-dir nil)))


;;
;;; Load library

;; Load the standard library early, so the macros and functions can be used for
;; the configuration.
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (file-name-concat (file-name-directory load-file-name) "lib"))
(require 'zenit-lib)


;;
;;; Core globals

(defgroup zenit nil
  "The top of my personal config bikeshedding."
  :group 'emacs)

(defvar zenit-init-time nil
  "The time it took, in seconds, for Emacs to initialize.")


;;
;;; Data directory variables

(defvar zenit-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the currently loaded .emacs.d directory. Must end
with a slash.")

(defconst zenit-core-dir (file-name-directory load-file-name)
  "The root directory of the config's core files. Must end with a
slash.")

(defvar zenit-modules-dir (expand-file-name "lisp/modules/" zenit-emacs-dir)
  "The root directory for the modules. Must end with a slash.")

(defvar zenit-local-conf-dir
  (let ((dir (getenv "ZENITLOCALCONFDIR")))
    (if dir
        (expand-file-name (file-name-as-directory dir))
      (file-name-concat zenit-emacs-dir "site-lisp/")))
  "Where the local, machine specific configuration is placed.

Defaults to ~/.emacs.d/site-lisp or the value of the ZENITLOCALCONFDIR
envvar; whichever is found first. Must end in a slash.")

(defvar zenit-local-dir (expand-file-name ".local/" zenit-emacs-dir)
  "Root directory for local storage.

Use this as a storage location for this system's installation of
Emacs.

These files should not be shared across systems. By default, it
is used by `zenit-data-dir' and `zenit-cache-dir'. Must end with
a slash.")

(defvar zenit-data-dir (file-name-concat zenit-local-dir "etc/")
  "Directory for non-volatile local storage.

Use this for files that don't change much, like server binaries,
external dependencies or long-term shared data. Must end with a
slash.")

(defvar zenit-cache-dir (file-name-concat zenit-local-dir "cache/")
  "Directory for volatile local storage.

Use this for files that change often, like cache files. Must end
with a slash.")

(defconst zenit-env-file (file-name-concat zenit-local-dir "env")
  "The location of your envvar file, generated by `make env`.

This file contains environment variables scraped from your shell
environment, which is loaded at startup (if it exists). This is
helpful if Emacs can't \(easily) be launched from the correct
shell session (particularly for MacOS users).")

(defconst zenit-config-init-file
  (file-name-concat user-emacs-directory "init.el")
  "Where `+emacs-autoloads-reload' stores its autoloads. This
file is responsible for informing Emacs where to find all
autoloaded functions.")

;;
;;; Startup optimizations

(unless (daemonp)
  ;; We can get a noteable boost to startup time by unsetting or simplifying
  ;; `file-name-handler-alist's value.
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (set-default-toplevel-value
     'file-name-handler-alist
     ;; HACK: If the bundled elisp for this Emacs install isn't
     ;;   byte-compiled (but is compressed), then leave the gzip file
     ;;   handler there so Emacs won't forget how to read read them.
     ;;
     ;;   calc-loaddefs.el is our heuristic for this because it is built-in
     ;;   to all supported versions of Emacs, and calc.el explicitly loads
     ;;   it uncompiled. This ensures that the only other, possible
     ;;   fallback would be calc-loaddefs.el.gz.
     (if (eval-when-compile
           (locate-file-internal "calc-loaddefs.el" load-path))
         nil
       (list (rassq 'jka-compr-handler old-value))))
    ;; Remember it ...
    (put 'file-name-handler-alist 'initial-value old-value)
    ;; Emacs will process any files passed to it via the command line, and will
    ;; do so *really* early in the startup process. These might contain special
    ;; file paths like TRAMP paths, so restore `file-name-handler-alist' just
    ;; for this portion of startup.
    (define-advice command-line-1 (:around (fn args-left) respect-file-handlers)
      (let ((file-name-handler-alist (if args-left old-value file-name-handler-alist)))
        (funcall fn args-left)))
    (eval-when-compile
      (declare-function command-line-1@respect-file-handlers nil))
    ;; ... so it can be reset where needed.
    (add-hook! 'emacs-startup-hook :depth 101
      (defun zenit--reset-file-handler-alist-h ()
        "Restore `file-name-handler-alist', because it is needed for
handling encrypted or compressed files, among other things."
        (set-default-toplevel-value 'file-name-handler-alist
                                    ;; Merge instead of overwrite because there may have been changes
                                    ;; to `file-name-handler-alist' since startup we want to preserve.
                                    (delete-dups (append file-name-handler-alist old-value))))))

  (unless noninteractive
    ;; Resizing the Emacs frame (to accommodate fonts that are smaller or larger
    ;; than the system font) appears to impact startup time dramatically.
    (setq frame-inhibit-implied-resize t)

    ;; Reduce *Message* noise at startup and shave seconds off startup time by
    ;; starting the scratch buffer in `fundamental-mode'.
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name
          initial-major-mode 'fundamental-mode
          initial-scratch-message nil)
    ;; Remove "For information about GNU Emacs..." message at startup.
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    ;; Suppress the vanilla startup screen completely.
    (advice-add #'display-startup-screen :override #'ignore)

    (unless initial-window-system
      ;; Inexplicably, `tty-run-terminal-initialization' can sometimes take 2-3s
      ;; when starting up Emacs in the terminal. Whatever slows it down at
      ;; startup doesn't appear to affect it if it's called a little later in
      ;; the startup process.
      (define-advice tty-run-terminal-initialization (:override (&rest _) defer)
        (advice-remove #'tty-run-terminal-initialization #'tty-run-terminal-initialization@defer)
        (add-hook 'window-setup-hook
                  (zenit-partial #'tty-run-terminal-initialization
                                 (selected-frame) nil t)))
      (eval-when-compile
        (declare-function tty-run-terminal-initialization@defer nil)))

    ;; `load-suffixes' and `load-file-rep-suffixes' are consulted on each
    ;; `require' and `load'. Removing .so gives a small boost. This is later
    ;; restored in zenit-start.el.
    (put 'load-suffixes 'initial-value (default-toplevel-value 'load-suffixes))
    (put 'load-file-rep-suffixes 'initial-value (default-toplevel-value 'load-file-rep-suffixes))
    (set-default-toplevel-value 'load-suffixes '(".elc" ".el"))
    (set-default-toplevel-value 'load-file-rep-suffixes '(""))

    (add-hook! 'zenit-before-init-hook
      (defun zenit--reset-load-suffixes-h ()
        "Undo any problematic startup optimizations."
        (setq load-suffixes (get 'load-suffixes 'initial-value)
              load-file-rep-suffixes (get 'load-file-rep-suffixes 'initial-value))))

    ;; Defer the initialization of `defcustom'.
    (setq custom-dont-initialize t)
    (add-hook! 'zenit-before-init-hook
      (defun zenit--reset-custom-dont-initialize-h ()
        (setq custom-dont-initialize nil)))

    ;; The mode-line procs a couple dozen times during startup. This is
    ;; normally quite fast, but disabling the default mode-line and reducing
    ;; the update delay timer seems to stave off ~30-50ms.
    (put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
    (setq-default mode-line-format nil)
    (dolist (buf (buffer-list))
      (with-current-buffer buf (setq mode-line-format nil)))

    ;; Premature redisplays can substantially affect startup times and produce
    ;; ugly flashes of unstyled Emacs.
    (setq-default inhibit-redisplay t
                  inhibit-message t)

    ;; If the above vars aren't reset, Emacs could appear frozen or garbled
    ;; after startup (or in case of an startup error).
    (defun zenit--reset-inhibited-vars-h ()
      (setq-default inhibit-redisplay nil
                    inhibit-message nil)
      (remove-hook 'post-command-hook #'zenit--reset-inhibited-vars-h))
    (eval-when-compile
      (declare-function zenit--reset-inhibited-vars-h nil))
    (add-hook 'post-command-hook #'zenit--reset-inhibited-vars-h -100)

    ;; Lazy load the toolbar until tool-bar-mode is actually used (see
    ;; `startup--load-user-init-file@undo-hacks').
    (advice-add #'tool-bar-setup :override #'ignore)

    ;; site-lisp files are often obnoxiously noisy (emitting output that isn't
    ;; useful to end-users, like load messages, deprecation notices, and
    ;; linter warnings. Displaying these in the minibuffer causes unnecessary
    ;; redraws at startup which can impact startup time drastically and cause
    ;; flashes of white. It also pollutes the logs. By suppressing it here, I
    ;; load it myself, later, in a more controlled way (see
    ;; `startup--load-user-init-file@undo-hacks').
    (put 'site-run-file 'initial-value site-run-file)
    (setq site-run-file nil)

    (define-advice startup--load-user-init-file (:around (fn &rest args) undo-hacks 95)
      "Undo startup optimizations to prep for the user's session."
      (unwind-protect
          (progn
            (when (setq site-run-file (get 'site-run-file 'initial-value))
              (let ((inhibit-startup-screen inhibit-startup-screen))
                (letf! ((defun load-file (file) (load file nil 'nomessage))
                        (defun load (file &optional noerror _nomessage &rest args)
                          (apply load file noerror t args)))
                  (load site-run-file t t))))
            (apply fn args))
        ;; Now it's safe to be verbose.
        (setq-default inhibit-message nil)
        ;; Once startup is sufficiently complete, undo our earlier optimizations
        ;; to reduce the scope of potential edge cases.
        (advice-remove #'tool-bar-setup #'ignore)
        (add-transient-hook! 'tool-bar-mode (tool-bar-setup))
        (unless (default-toplevel-value 'mode-line-format)
          (setq-default mode-line-format (get 'mode-line-format 'initial-value)))))
    (eval-when-compile
      (declare-function startup--load-user-init-file@undo-hacks nil))

    ;; Unset a non-trivial list of command line options that aren't relevant
    ;; to our current OS, but `command-line-1' still processes.
    (eval-unless! (featurep :system 'macos)
      (setq command-line-ns-option-alist nil))
    (eval-unless! (memq initial-window-system '(x pgtk))
      (setq command-line-x-option-alist nil))))


;;
;;; Zenit context

(defvar zenit-context '(t)
  "A list of symbols identifying all active execution contexts.

This should never be directly changed, only let-bound, and should
never be empty. Each context describes what phase Emacs is in,
and may respond to.

All valid contexts:

  cli      -- while executing CLI
  compile  -- while byte-compilation is in progress
  eval     -- during inline evaluation of elisp
  init     -- while Emacs is formally starting up for the
              first time,after its core libraries are loaded,
              but before user config is.
  modules  -- while loading modules and their files
  packages -- when packagedefs are being read
  reload   -- while reloading
  sandbox  -- while running in sandbox
  tests    -- while running unit tests")
(put 'zenit-context 'valid-values '(cli compile eval init modules packages reload sandbox tests))
(put 'zenit-context 'risky-local-variable t)

(defun zenit-context--check (context)
  "Check if the given CONTEXT is valid.

CONTEXT is a symbol representing the context to be checked.

If CONTEXT is not recognized as a valid context, a
`zenit-context-error' signal is raised."
  (let ((valid (get 'zenit-context 'valid-values)))
    (unless (memq context valid)
      (signal 'zenit-context-error
              (list context "Unrecognized context" valid)))))

(defun zenit-context-p (context)
  "Return t if CONTEXT is active (i.e. in `zenit-context')."
  (if (memq context zenit-context) t))

(defun zenit-context-push (context)
  "Add CONTEXT to `zenit-context', if it isn't already.

Return non-nil if successful. Throws an error if CONTEXT is
invalid."
  (unless (memq context zenit-context)
    (zenit-context--check context)
    (zenit-log ":context: +%s %s" context zenit-context)
    (push context zenit-context)))

(defun zenit-context-pop (context &optional strict?)
  "Remove CONTEXT from `zenit-context'.

Return non-nil if successful. If STRICT? is non-nil, throw an
error if CONTEXT wasn't active when this was called."
  (if (not (zenit-context-p context))
      (when strict?
        (signal 'zenit-context-error
                (list zenit-context "Attempt to pop missing context" context)))
    (zenit-log ":context: -%s %s" context zenit-context)
    (setq zenit-context (delq context zenit-context))))

(defmacro zenit-context-with (contexts &rest body)
  "Evaluate BODY with CONTEXT added to `zenit-context'."
  (declare (indent 1))
  `(let ((zenit-context zenit-context))
     (dolist (context (ensure-list ,contexts))
       (zenit-context-push context))
     ,@body))


;;
;;; Reasonable, global defaults

;; Don't litter
(setq async-byte-compile-log-file  (concat zenit-data-dir "async-bytecomp.log")
      custom-file                  (concat zenit-local-conf-dir "custom.el")
      desktop-dirname              (concat zenit-data-dir "desktop")
      desktop-base-file-name       "autosave"
      desktop-base-lock-name       "autosave-lock"
      pcache-directory             (concat zenit-cache-dir "pcache/")
      request-storage-directory    (concat zenit-cache-dir "request")
      shared-game-score-directory  (concat zenit-data-dir "shared-game-score/"))

(defadvice! zenit--write-to-etc-dir-a (orig-fn &rest args)
  "Resolve Emacs storage directory to `zenit-data-dir', to keep
local files from polluting `zenit-emacs-dir'."
  :around #'locate-user-emacs-file
  (let ((user-emacs-directory zenit-data-dir))
    (apply orig-fn args)))

(defadvice! zenit--save-enabled-commands-to-custom-file-a (orig-fn &rest args)
  "When enabling a disabled command, the `put' call is written to
~/.emacs.d/init.el, which causes issues, so write it to the
user's custom.el instead."
  :around #'en/disable-command
  :around #'locate-user-emacs-file
  (let ((user-emacs-directory zenit-data-dir)
        (user-init-file custom-file))
    (apply orig-fn args)))

;; By default, Emacs stores `authinfo' in $HOME and in plain-text.This file
;; stores usernames, passwords, and other treasures for the aspiring malicious
;; third party.
(setq auth-sources (list (file-name-concat zenit-data-dir "authinfo.gpg")
                         "~/.authinfo.gpg"))

;; Native compilation support (see http://akrl.sdf.org/gccemacs.html)
(when (boundp 'native-comp-eln-load-path)
  ;; (add-to-list 'native-comp-eln-load-path (expand-file-name "eln/" zenit-cache-dir))

  ;; Suppress compiler warnings and don't inundate users with their popups. They
  ;; are rarely more than warnings, so are safe to ignore.
  (setq native-comp-async-report-warnings-errors init-file-debug
        native-comp-warning-on-missing-source init-file-debug)

  ;; By default, native-comp uses 100% of half your cores. Lower this to 1/4.
  (define-advice comp-effective-async-max-jobs (:before (&rest _) set-default-cpus)
    "Default to 1/4 of cores in interactive sessions and all of
them otherwise."
    (and (null comp-num-cpus)
         (zerop native-comp-async-jobs-number)
         (setq comp-num-cpus
               (max 1 (/ (num-processors) (if noninteractive 1 4))))))
  (eval-when-compile
    (declare-function comp-effective-async-max-jobs@set-default-cpus nil)))

;; Suppress package.el
(setq package-enable-at-startup nil)

;; Reduce unnecessary/unactionable warnings/logs
(setq ad-redefinition-action 'accept)

;; Ignore warnings about "existing variables being aliased".
(setq warning-suppress-types '(defvaralias))

;; Reduce debug output unless we've asked for it.
(setq debug-on-error init-file-debug
      jka-compr-verbose init-file-debug)

;; Stricter security defaults
;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be a
;; *little* more discerning.
(setq gnutls-verify-error noninteractive
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not zenit--system-windows-p)
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      ;; Emacs is built with gnutls.el by default, so `tls-program' won't
      ;; typically be used, but in the odd case that it does, we ensure a more
      ;; secure default for it (falling back to `openssl' if absolutely
      ;; necessary). See https://redd.it/8sykl1 for details.
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"))


;;
;;; Custom hooks

(defcustom zenit-before-init-hook ()
  "A hook run after the core has initialized and before local
configuration.

Right before site-lisp/init.el is loaded, in the context of
early-init.el. Use this for configuration at the latest
opportunity before the session becomes unpredictably complicated
by packages, etc.

In contrast, `before-init-hook' is run just after
site-lisp/init.el is loaded, but long before modules and
site-lisp/config.el are loaded."
  :group 'zenit
  :type 'hook)

(defcustom zenit-after-init-hook ()
  "A hook run once core, modules and local config are loaded.

This triggers at the absolute latest point in the eager startup
process, and runs in both interactive and non-interactive
sessions, so guard hooks appropriately against `noninteractive'."
  :group 'zenit
  :type 'hook)


;;
;;; Last minute initialization

(add-hook! 'zenit-before-init-hook :depth -105
  (defun zenit--begin-init-h ()
    "Begin the startup process."
    (when (zenit-context-push 'init)
      ;; Remember these variables' initial values, so we can safely reset them at
      ;; a later time, or consult them without fear of contamination.
      (dolist (var '(exec-path load-path process-environment))
        (put var 'initial-value (default-toplevel-value var))))))

(add-hook! 'zenit-after-init-hook :depth 105
  (defun zenit--end-init-h ()
    "Set `zenit-init-time'."
    (when (zenit-context-pop 'init)
      (setq zenit-init-time (float-time (time-subtract (current-time) before-init-time))))))

(unless noninteractive
  ;; This is the absolute latest a hook can run in Emacs' startup process.
  (define-advice command-line-1 (:after (&rest _) run-after-init-hook)
    (zenit-run-hooks 'zenit-after-init-hook))
  (eval-when-compile
    (declare-function command-line-1@run-after-init-hook nil)))

(provide 'zenit-core)

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

(eval-when-compile (require 'subr-x))


;;
;;; Custom features

;; `system-configuration-features's documentation says it should not be used to
;; detect features.
(if (bound-and-true-p module-file-suffix)
    (push 'dynamic-modules features))
(if (fboundp #'json-parse-string)
    (push 'jansson features))

;; `native-compile' exists whether or not it is functional, so pretend it
;; doesn't exist if it isn't available.
(if (featurep 'native-compile)
    (if (not (native-comp-available-p))
        (delq 'native-compile features)))

;;
;;; Global constants

(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))
(defconst MODULES     (featurep 'dynamic-modules))
(defconst NATIVECOMP  (featurep 'native-compile))

;; $HOME isn't normally defined on Windows, but many unix tools expect it.
(when IS-WINDOWS
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
    (setq file-name-handler-alist
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
    ;; Make sure the new value survives any current let-binding.
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    ;; Remember it so it can be reset where needed.
    (put 'file-name-handler-alist 'initial-value old-value)
    (defhook! zenit--reset-file-handler-alist-h ()
      "Restore `file-name-handler-alist', because it is needed for
handling encrypted or compressed files, among other things."
      'emacs-startup-hook :depth 101
      (setq file-name-handler-alist
              ;; Merge instead of overwrite because there may have been changes
              ;; to `file-name-handler-alist' since startup we want to preserve.
              (delete-dups (append file-name-handler-alist old-value)))))

  (unless noninteractive
    ;; Resizing the Emacs frame (to accommodate fonts that are smaller or larger
    ;; than the system font) appears to impact startup time dramatically.
    (setq frame-inhibit-implied-resize t)

    ;; Reduce *Message* noise at startup.
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name)
    ;; Remove "For information about GNU Emacs..." message at startup.
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    ;; Suppress the vanilla startup screen completely.
    (advice-add #'display-startup-screen :override #'ignore)

    ;; Shave seconds off startup time by starting the scratch buffer in
    ;; `fundamental-mode'.
    (setq initial-major-mode 'fundamental-mode
          initial-scratch-message nil)

    (unless initial-window-system
      ;; Inexplicably, `tty-run-terminal-initialization' can sometimes take 2-3s
      ;; when starting up Emacs in the terminal. Whatever slows it down at
      ;; startup doesn't appear to affect it if it's called a little later in
      ;; the startup process.
      (define-advice tty-run-terminal-initialization (:override (&rest _) defer)
        (advice-remove #'tty-run-terminal-initialization #'tty-run-terminal-initialization@defer)
        (add-hook 'window-setup-hook
                  (zenit-partial #'tty-run-terminal-initialization
                                 (selected-frame) nil t))))

    (unless init-file-debug
      ;; Site files tend to use `load-file', which emits "Loading X..." messages
      ;; in the echo area. Writing to the echo-area triggers a redisplay, which
      ;; can be expensive during startup. This may also cause an flash of white
      ;; when creating the first frame.
      (define-advice load-file (:override (file) silence)
        (load file nil 'nomessage))
      ;; But undo our `load-file' advice later, as to limit the scope of any
      ;; edge cases it could induce.
      (define-advice startup--load-user-init-file (:before (&rest _) undo-silence)
        (advice-remove #'load-file #'load-file@silence))

      ;; `load-suffixes' and `load-file-rep-suffixes' are consulted on each
      ;; `require' and `load'. Removing .so gives a small boost. This is later
      ;; restored in FIXME.
      (put 'load-suffixes 'initial-value (default-toplevel-value 'load-suffixes))
      (put 'load-file-rep-suffixes 'initial-value (default-toplevel-value 'load-file-rep-suffixes))
      (set-default-toplevel-value 'load-suffixes '(".elc" ".el"))
      (set-default-toplevel-value 'load-file-rep-suffixes '(""))

      (defhook! zenit--reset-load-suffixes-h ()
          "Undo any problematic startup optimizations."
          'zenit-before-init-hook
          (setq load-suffixes (get 'load-suffixes 'initial-value)
                load-file-rep-suffixes (get 'load-file-rep-suffixes 'initial-value)))

      ;; Defer the initialization of `defcustom'.
      (setq custom-dont-initialize t)
      (defhook! zenit--reset-custom-dont-initialize-h ()
        'zenit-before-init-hook
        (setq custom-dont-initialize nil))

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

      ;; Then reset it with advice, because `startup--load-user-init-file' will
      ;; never be interrupted by errors. And if these settings are left set,
      ;; Emacs could appear frozen or garbled.
      (defun zenit--reset-inhibited-vars-h ()
        (setq-default inhibit-redisplay nil
                      ;; Inhibiting `message' only prevents redraws and
                      inhibit-message nil)
        (redraw-frame))
      (add-hook 'after-init-hook #'zenit--reset-inhibited-vars-h)
      (define-advice startup--load-user-init-file (:after (&rest _) undo-inhibit-vars)
        (when init-file-had-error
          (zenit--reset-inhibited-vars-h))
        (unless (default-toplevel-value 'mode-line-format)
          (setq-default mode-line-format (get 'mode-line-format 'initial-value))))

      ;; Lazy load the toolbar until tool-bar-mode is actually used.
      (advice-add #'tool-bar-setup :override #'ignore)
      (define-advice startup--load-user-init-file (:before (&rest _) defer-tool-bar-setup)
        (advice-remove #'tool-bar-setup #'ignore)
        (add-transient-hook! 'tool-bar-mode (tool-bar-setup)))

      ;; Unset a non-trivial list of command line options that aren't relevant
      ;; to our current OS, but `command-line-1' still processes.
      (unless IS-MAC
        (setq command-line-ns-option-alist nil))
      (unless (memq initial-window-system '(x pgtk))
        (setq command-line-x-option-alist nil)))))


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
               (max 1 (/ (num-processors) (if noninteractive 1 4)))))))

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
                (if (and (not IS-WINDOWS)
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

This triggers at the absolutel atest point in the eager startup
process, and runs in both interactive and non-interactive
sessions, so guard hooks appropriately against `noninteractive'."
  :group 'zenit
  :type 'hook)


;;
;;; Last minute initialization

(defhook! zenit--begin-init-h ()
  "Begin the startup process."
  'zenit-before-init-hook :depth -105
  (when (zenit-context-push 'init)
    ;; Remember these variables' initial values, so we can safely reset them at
    ;; a later time, or consult them without fear of contamination.
    (dolist (var '(exec-path load-path process-environment))
      (put var 'initial-value (default-toplevel-value var)))))

(defhook! zenit--end-init-h ()
  "Set `zenit-init-time'."
  'zenit-after-init-hook :depth 105
  (when (zenit-context-pop 'init)
    (setq zenit-init-time (float-time (time-subtract (current-time) before-init-time)))))

(unless noninteractive
  ;; This is the absolute latest a hook can run in Emacs' startup process.
  (define-advice command-line-1 (:after (&rest _) run-after-init-hook)
    (zenit-run-hooks 'zenit-after-init-hook)))

(provide 'zenit-core)

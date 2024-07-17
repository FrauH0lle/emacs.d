;; lisp/core/zenit-start.el -*- lexical-binding: t; -*-

;;
;;; Custom hooks

(defcustom zenit-first-input-hook ()
  "Transient hooks run before the first user input."
  :type 'hook
  :local 'permanant-local
  :group 'zenit)

(defcustom zenit-first-file-hook ()
  "Transient hooks run before the first interactively opened file."
  :type 'hook
  :local 'permanant-local
  :group 'zenit)

(defcustom zenit-first-buffer-hook ()
  "Transient hooks run before the first interactively opened
buffer."
  :type 'hook
  :local 'permanant-local
  :group 'zenit)


;;
;;; Reasonable defaults for interactive sessions

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; Disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling BPA makes redisplay faster, but might produce incorrect reordering
;; of bidirectional text with embedded parentheses.
(setq bidi-inhibit-bpa t)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly.
(setq idle-update-delay 1.0)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows.
(setq inhibit-compacting-font-caches t)

;; PGTK builds only: this timeout adds latency to frame operations, like
;; `make-frame-invisible', which are frequently called without a guard because
;; it's inexpensive in non-PGTK builds. Lowering the timeout from the default
;; 0.1 should make childframes and packages that manipulate them (like `lsp-ui',
;; `company-box', and `posframe') feel much snappier. See emacs-lsp/lsp-ui#613.
(eval-when! (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 64 1024))

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(eval-when! (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil    ; decrease file IO workload
        w32-pipe-read-delay 0               ; faster IPC
        w32-pipe-buffer-size (* 64 1024)))  ; read more at a time (was 4K)

;; The GC introduces annoying pauses and stuttering into our Emacs experience,
;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
;; when it's idle. However, if the idle delay is too long, we run the risk of
;; runaway memory usage in busy sessions. If it's too low, then we may as well
;; not be using gcmh at all.
(setq gcmh-idle-delay 'auto  ; default is 15s
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
(add-hook 'zenit-first-buffer-hook #'gcmh-mode)

;; Disable UI elements
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Encodings
(set-language-environment "UTF-8")
;; ...but `set-language-environment' also sets `default-input-method', which is
;; a step too opinionated.
(setq default-input-method nil)
;; ...And the clipboard on Windows could be in a wider encoding (UTF-16), so
;; leave Emacs to its own devices.
(eval-when! zenit--system-windows-p
  (setq selection-coding-system 'utf-8))

;; Add support for additional file extensions.
(dolist (entry '(("/LICENSE\\'" . text-mode)
                 ("\\.log\\'" . text-mode)
                 ("rc\\'" . conf-mode)
                 ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)))
  (push entry auto-mode-alist))


;;
;;; MODE-local-vars-hook

;; File+dir local variables are initialized after the major mode and its hooks
;; have run. If you want hook functions to be aware of these customizations, add
;; them to MODE-local-vars-hook instead.
(defvar zenit-inhibit-local-var-hooks nil
  "If `zenit-inhibit-local-var-hooks` is
non-nil,`zenit-run-local-var-hooks-h' will not run these hooks.
Default value is nil, allowing the hooks to run.")

(defun zenit-run-local-var-hooks-h ()
  "Run MODE-local-vars-hook after local variables are initialized."
  (unless (or zenit-inhibit-local-var-hooks
              delay-mode-hooks
              ;; Don't trigger local-vars hooks in temporary (internal) buffers
              (string-prefix-p
               " " (buffer-name (or (buffer-base-buffer)
                                    (current-buffer)))))
    (setq-local zenit-inhibit-local-var-hooks t)
    ;; The tree-sitter supported modes are usually derived from a common base
    ;; mode. Thus, instead of having to add to the non-ts-mode and normal-mode
    ;; hooks, we run the parent hooks as well.
    (let ((parent (get major-mode 'derived-mode-parent)))
      (when (string-suffix-p "base-mode" (symbol-name parent))
        (zenit-run-hooks (intern-soft (format "%s-local-vars-hook" parent))))
      (zenit-run-hooks (intern-soft (format "%s-local-vars-hook" major-mode))))))

;; If the user has disabled `enable-local-variables', then
;; `hack-local-variables-hook' is never triggered, so we trigger it at the end
;; of `after-change-major-mode-hook':
(defun zenit-run-local-var-hooks-maybe-h ()
  "Run `zenit-run-local-var-hooks-h' if
`enable-local-variables' is disabled."
  (unless enable-local-variables
    (zenit-run-local-var-hooks-h)))


;;
;;; Incremental lazy-loading

(defvar zenit-incremental-packages '(t)
  "A list of packages to load incrementally after startup. Any
large packages here may cause noticeable pauses, so it's
recommended you break them up into sub-packages. For example,
`org' is comprised of many packages, and can be broken up into:

  (zenit-load-packages-incrementally
 \\='(calendar find-func format-spec org-macs org-compat
     org-faces org-entities org-list org-pcomplete org-src
     org-footnote org-macro ob org org-clock org-agenda
     org-capture))

This is already done by the emacs/org module, however.

If you want to disable incremental loading altogether, either
remove `zenit-load-packages-incrementally-h' from
`emacs-startup-hook' or set `zenit-incremental-first-idle-timer'
to nil. Incremental loading does not occur in daemon
sessions (they are loaded immediately at startup).")

(defvar zenit-incremental-first-idle-timer (if (daemonp) 0 2.0)
  "How long (in idle seconds) until incremental loading starts.

Set this to nil to disable incremental loading. Set this to 0 to
load all incrementally deferred packages immediately at
`emacs-startup-hook'.")

(defvar zenit-incremental-idle-timer 0.75
  "How long (in idle seconds) in between incrementally loading packages.")

(defun zenit-load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.

If NOW is non-nil, load PACKAGES incrementally, in
`zenit-incremental-idle-timer' intervals."
  (let ((gc-cons-threshold most-positive-fixnum))
    (if (not now)
        (cl-callf append zenit-incremental-packages packages)
      (while packages
        (let ((req (pop packages))
              idle-time)
          (if (featurep req)
              (zenit-log "start:inc-loader: Already loaded %s (%d left)" req (length packages))
            (condition-case-unless-debug e
                (and
                 (or (null (setq idle-time (current-idle-time)))
                     (< (float-time idle-time) zenit-incremental-first-idle-timer)
                     (not
                      (while-no-input
                        (zenit-log "start:inc-loader: Loading %s (%d left)" req (length packages))
                        ;; If `default-directory' doesn't exist or is
                        ;; unreadable, Emacs throws file errors.
                        (let ((default-directory zenit-emacs-dir)
                              (inhibit-message t)
                              (file-name-handler-alist
                               (list (rassq 'jka-compr-handler file-name-handler-alist))))
                          (require req nil t)
                          t))))
                 (push req packages))
              (error
               (message "Error: failed to incrementally load %S because: %s" req e)
               (setq packages nil)))
            (if (null packages)
                (zenit-log "start:inc-loader: Finished!")
              (run-at-time (if idle-time
                               zenit-incremental-idle-timer
                             zenit-incremental-first-idle-timer)
                           nil #'zenit-load-packages-incrementally
                           packages t)
              (setq packages nil))))))))

(defun zenit-load-packages-incrementally-h ()
  "Begin incrementally loading packages in `zenit-incremental-packages'.

If this is a daemon session, load them all immediately instead."
  (when (numberp zenit-incremental-first-idle-timer)
    (if (zerop zenit-incremental-first-idle-timer)
        (mapc #'require (cdr zenit-incremental-packages))
      (run-with-idle-timer zenit-incremental-first-idle-timer
                           nil #'zenit-load-packages-incrementally
                           (cdr zenit-incremental-packages) t))))


;;
;;; Benchmark

(defun zenit-display-benchmark-h (&optional return-p)
  "Display a benchmark including number of packages and modules loaded.

If RETURN-P, return the message as a string instead of displaying
it."
  (funcall (if return-p #'format #'message)
           "Loaded %d packages across %d modules in %.03fs"
           (- (length load-path) (length (get 'load-path 'initial-value)))
           (hash-table-count zenit-modules)
           zenit-init-time))


;;
;;; Bootstrap

;; Load core modules and set up their autoloads
(require 'zenit-modules)
(autoload 'zenit-initialize-packages "zenit-packages")
(autoload 'package! "zenit-packages" nil nil t)
(function-put 'package! 'lisp-indent-function 'defun)
(autoload 'straight-x-fetch-all "straight-x" nil t)
(autoload 'straight-x-pull-all "straight-x" nil t)
(autoload 'straight-x-freeze-versions "straight-x" nil t)

;; In case we want to use package.el or straight via M-x
(with-eval-after-load 'package (require 'zenit-packages))
(with-eval-after-load 'straight (zenit-initialize-packages))
(with-eval-after-load 'straight-x (zenit-initialize-packages))

;; A last ditch opportunity to undo dodgy optimizations or do extra
;; configuration before the session is complicated.
(zenit-run-hooks 'zenit-before-init-hook)

;; Load envvar file
(when (and (or initial-window-system
               (daemonp))
           zenit-env-file)
  (zenit-load-envvars-file zenit-env-file 'noerror))

;; Last minute setup
(add-hook 'zenit-after-init-hook #'zenit-load-packages-incrementally-h 100)
(add-hook 'zenit-after-init-hook #'zenit-display-benchmark-h 110)
(zenit-run-hook-on 'zenit-first-buffer-hook '(find-file-hook zenit-switch-buffer-hook))
(zenit-run-hook-on 'zenit-first-file-hook   '(find-file-hook dired-initial-position-hook))
(zenit-run-hook-on 'zenit-first-input-hook  '(pre-command-hook))

;; Activate these later, otherwise they'll fire for every buffer created between
;; now and the end of startup.
(add-hook! 'after-init-hook
  (defun zenit-init-local-var-hooks-h ()
    "These fire `MAJOR-MODE-local-vars-hook' hooks. See the
`MODE-local-vars-hook' section above."
    (add-hook 'after-change-major-mode-hook #'zenit-run-local-var-hooks-maybe-h 100)
    (add-hook 'hack-local-variables-hook #'zenit-run-local-var-hooks-h)))

;; Load site-lisp/init.el early, but only when not in CLI mode.
(when (not (zenit-context-p 'cli))
  (load! (string-remove-suffix ".el" zenit-module-init-file) zenit-local-conf-dir t))

;; Load the rest of site-lisp/ + modules if noninteractive and not in CLI mode.
;; The idea is to be able to use this file in Emacs' batch mode and initialize
;; the local configuraton.
(when (and noninteractive (not (zenit-context-p 'cli)))
  (let ((init-file (file-name-concat zenit-emacs-dir "init.el")))
    (unless (file-exists-p init-file)
      (user-error "Init file hasn't been generated. Did you forgot to run 'make refresh'?"))
    (let (kill-emacs-query-functions
          kill-emacs-hook)
      ;; Loads modules, then site-lisp/config.el
      (zenit-load init-file 'noerror)
      (zenit-initialize-packages))))

;; Entry point
;; HACK: This advice hijacks Emacs' initfile loader to accomplish the following:
;;
;;   1. Load the initfile (generated on `make refresh`)
;;   2. Ignore initfiles we don't care about (like $EMACSDIR/init.el, ~/.emacs,
;;      and ~/_emacs) and spare us the IO of searching for them.
;;   3. Cut down on unnecessary logic in Emacs' bootstrapper.
;;   4. Offer a more user-friendly error state/screen.
(define-advice startup--load-user-init-file (:override (&rest _) init-zenit 100)
  (let ((debug-on-error-from-init-file nil)
        (debug-on-error-should-be-set nil)
        (debug-on-error-initial (if (eq init-file-debug t) 'startup init-file-debug))
        ;; The init file might contain byte-code with embedded NULs, which can
        ;; cause problems when read back, so disable nul byte detection. (Bug
        ;; #52554)
        (inhibit-null-byte-detection t))
    (let ((debug-on-error debug-on-error-initial))
      (condition-case-unless-debug error
          (when init-file-user
            (let ((init-file-name
                   ;; This dynamically generated init file stores a lot of
                   ;; precomputed information, such as module and package
                   ;; autoloads, and values for expensive variables like
                   ;; `zenit-modules', `zenit-disabled-packages', `load-path',
                   ;; `auto-mode-alist', and `Info-directory-list'. etc.
                   ;; Compiling them in one place is a big reduction in startup
                   ;; time, and by keeping a history of them, you get a snapshot
                   ;; of your config in time.
                   (file-name-concat zenit-emacs-dir "init.elc")))
              ;; If `user-init-file' is t, then `load' will store the name of
              ;; the next file it loads into `user-init-file'.
              (setq user-init-file t)
              (when init-file-name
                (load init-file-name 'noerror 'nomessage 'nosuffix))
              ;; (when init-file-name
              ;;   (load (concat (string-remove-suffix ".elc" init-file-name)
              ;;               ".el") 'noerror 'nomessage 'nosuffix))
              ;; If it's still `t', then it failed to load the profile initfile.
              (when (eq user-init-file t)
                (signal 'zenit-nosync-error (list init-file-name)))
              ;; If we loaded a compiled file, set `user-init-file' to the
              ;; source version if that exists.
              (setq user-init-file
                    (concat (string-remove-suffix ".elc" user-init-file)
                            ".el"))))
        (error
         (display-warning
          'initialization
          (format-message "\
An error occurred while loading `%s':\n\n%s%s%s\n\n\
To ensure normal operation, you should investigate and remove the
cause of the error in your initialization file.  Start Emacs with
the `--debug-init' option to view a complete error backtrace."
                          user-init-file
                          (get (car error) 'error-message)
                          (if (cdr error) ": " "")
                          (mapconcat (lambda (s) (prin1-to-string s t))
                                     (cdr error) ", "))
          :warning)
         (setq init-file-had-error t)))
      ;; If we can tell that the init file altered debug-on-error, arrange to
      ;; preserve the value that it set up.
      (or (eq debug-on-error debug-on-error-initial)
          (setq debug-on-error-should-be-set t
                debug-on-error-from-init-file debug-on-error)))
    (when debug-on-error-should-be-set
      (setq debug-on-error debug-on-error-from-init-file))))

(provide 'zenit-start)

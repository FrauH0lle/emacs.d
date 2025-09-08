;; lisp/core/lib/zenit-lib-debug.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

;; `backtrace'
(declare-function backtrace-frame-args "backtrace" t t)
(declare-function backtrace-frame-fun "backtrace" t t)
(declare-function backtrace-frame-locals "backtrace" t t)

;; `cl-seq'
(declare-function cl-remove-if-not "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `gcmh'
(declare-function gcmh-idle-garbage-collect "gcmh" ())

;;`profiler'
(declare-function profiler-report "profiler" ())
(declare-function profiler-stop "profiler" ())

;; `zenit-lib-print'
(defvar zenit-print-indent)

;; `zenit-lib-ui'
(declare-function zenit-shut-up-a "zenit-lib-ui" (orig-fn &rest args))


;;
;;; Profiling

(defvar zenit--profiler nil
  "Tracks whether the profiler is running.
Used by `zenit/toggle-profiler' to manage profiler state.")
;;;###autoload
(defun zenit/toggle-profiler ()
  "Toggle the Emacs profiler.
Run it again to see the profiling report."
  (interactive)
  (if (not zenit--profiler)
      (profiler-start 'cpu+mem)
    (profiler-report)
    (profiler-stop))
  (setq zenit--profiler (not zenit--profiler)))


;;
;;; Debug

;;;###autoload
(defvar zenit-debug--variables
  `(;; Custom variables
    (zenit-print-minimum-level . debug)
    (zenit-inhibit-log . nil)

    ;; Emacs variables
    (async-debug t 3)
    debug-on-error
    (gcmh-verbose t 3)
    init-file-debug
    (jka-compr-verbose t 3)
    (message-log-max 16384)
    (native-comp-async-report-warnings-errors silent 2)
    (native-comp-warning-on-missing-source t 2)
    url-debug
    use-package-verbose
    (warning-suppress-types nil))
  "A list of variable to toggle on `zenit-debug-mode'.
Each entry can be a variable symbol, a cons cell whose CAR is the
variable symbol and CDR is the value to set it to when
`zenit-debug-mode' is activated or a list of the form (VAR VAL LEVEL).")

;;;###autoload
(progn
  (cl-defun set-debug-variable! (var &optional (debug-val t) (level 1))
    "Set VAR to DEBUG-VAL (or `t') when `zenit-debug-mode' is active at >=LEVEL."
    (setf (alist-get var zenit-debug--variables) (cons debug-val level))))

(defvar zenit-debug--unbound-variables nil
  "Tracks unbound variables in `zenit-debug--variables'.
When `zenit-debug-mode' is enabled, this stores variables from
`zenit-debug--variables' that weren't bound at the time. They are
watched and set once they become available.")

(defun zenit-debug--watch-vars-h (&rest _)
  "Handle unbound variables from `zenit-debug--variables'.
When called, checks if any previously unbound variables are now
available and sets them according to their specification in
`zenit-debug--variables'. This function is used as a watcher for
both variable changes and after-load events."
  (when-let* ((vars (copy-sequence zenit-debug--unbound-variables)))
    (setq zenit-debug--unbound-variables nil)
    (mapc #'zenit-debug--set-var vars)))

(defvar zenit-debug-mode)
(defun zenit-debug--set-var (spec)
  "Set debug variables according to SPEC.
SPEC can be either a variable symbol or a cons cell (VAR . VAL).
For cons cells, the variable VAR will be set to VAL when debug
mode is active, and restored to its initial value when debug mode
is inactive. Unbound variables are tracked in
`zenit-debug--unbound-variables' to be set when they become available."
  (cond
   ;; Handle cons cells (VAR . VAL) and lists (VAR VAL LEVEL)
   ((consp spec)
    (let (var val level)
      (pcase spec
        ;; Three-element list: (VAR VAL LEVEL)
        (`(,v ,v2 ,l)
         (setq var v val v2 level l))
        ;; Cons cell: (VAR . VAL)
        (`(,v . ,v2)
         (setq var v val v2 level nil)))
      (if (boundp var)
          (set-default
           var (if (or (not zenit-debug-mode)
                       (> (or level 1) zenit-log-level))
                   (prog1 (get var 'initial-value)
                     (put var 'initial-value nil))
                 (zenit-log 3 "debug:vars: %s = %S" var (default-toplevel-value var))
                 (put var 'initial-value (default-toplevel-value var))
                 val))
        (add-to-list 'zenit-debug--unbound-variables (list var val level)))))
   ;; Handle bound symbols
   ((boundp spec)
    (zenit-log 3 "debug:vars: %s = %S" spec zenit-debug-mode)
    (set-default-toplevel-value spec zenit-debug-mode))
   ;; Track unbound symbols
   ((add-to-list 'zenit-debug--unbound-variables (cons spec t)))))


(defun zenit-debug--timestamped-message-a (format-string &rest _args)
  "Advice to run before `message' that prepends a timestamp to each message.

Activate this advice with:
(advice-add \\='message :before \\='zenit-debug--timestamped-message-a)"
  (when (and (stringp format-string)
             message-log-max  ; if nil, logging is disabled
             (not (equal format-string "%s%s"))
             (not (equal format-string "\n")))
    (with-current-buffer "*Messages*"
      (let ((timestamp (format-time-string "[%F %T] " (current-time)))
            (deactivate-mark nil))
        (with-silent-modifications
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
          (insert timestamp))))
    (let ((window (get-buffer-window "*Messages*")))
      (when (and window (not (equal (selected-window) window)))
        (with-current-buffer "*Messages*"
          (goto-char (point-max))
          (set-window-point window (point-max)))))))

;;;###autoload
(define-minor-mode zenit-debug-mode
  "Toggle `debug-on-error' and `init-file-debug' for logging."
  :global t
  :group 'zenit
  (when (or zenit-debug-mode
            (and (integerp current-prefix-arg)
                 (> current-prefix-arg 0)))
    (setq zenit-debug-mode t)
    (let ((level (max 1 zenit-log-level (min 3 (or current-prefix-arg 1)))))
      (put 'zenit-log-level 'initial-value zenit-log-level)
      (setq zenit-log-level level)))
  (zenit-log "debug: enabled! (log-level=%d)" zenit-log-level)
  (mapc #'zenit-debug--set-var zenit-debug--variables)
  ;; Watch for changes in `zenit-debug--variables', or when packages load (and
  ;; potentially define one of `zenit-debug--variables'), in case some of them
  ;; aren't defined when `zenit-debug-mode' is first loaded.
  (cond (zenit-debug-mode
         (unless noninteractive
           (message "Debug mode level %d enabled! (Run 'M-x view-echo-area-messages' to open the log buffer)"
                    zenit-log-level))
         ;; Produce more helpful (and visible) error messages from errors
         ;; emitted from hooks (particularly mode hooks), that usually go
         ;; unnoticed otherwise.
         (advice-add #'run-hooks :override #'zenit-run-hooks)
         ;; Add time stamps to lines in *Messages*
         (advice-add #'message :before #'zenit-debug--timestamped-message-a)
         ;; The constant debug output from GC is mostly unhelpful. I still want
         ;; it logged to *Messages*, just out of the echo area.
         (advice-add #'gcmh-idle-garbage-collect :around #'zenit-debug-shut-up-a)
         (add-variable-watcher 'zenit-debug--variables #'zenit-debug--watch-vars-h)
         (add-hook 'after-load-functions #'zenit-debug--watch-vars-h))
        (t
         (when-let* ((last-level (get 'zenit-log-level 'initial-value)))
           (put 'zenit-log-level 'initial-value nil)
           (setq zenit-log-level last-level))
         (advice-remove #'run-hooks #'zenit-run-hooks)
         (advice-remove #'message #'zenit-debug--timestamped-message-a)
         (advice-remove #'gcmh-idle-garbage-collect #'zenit-debug-shut-up-a)
         (remove-variable-watcher 'zenit-debug--variables #'zenit-debug--watch-vars-h)
         (remove-hook 'after-load-functions #'zenit-debug--watch-vars-h)
         (zenit-log "debug: disabled")
         (message "Debug mode disabled!"))))

(defun zenit-debug-shut-up-a (fn &rest args)
  "Suppress output from FN, even in debug mode."
  (let (init-file-debug)
    (apply #'zenit-shut-up-a fn args)))


;;
;;; Custom debugger

;; HACK: I advise `debug' instead of changing `debugger' to hide the debugger
;;   itself from the backtrace. Doing it manually would require reimplementing
;;   most of `debug', which is a lot of unnecessary work, when I only want to
;;   decorate the original one slightly.
(defadvice! zenit-debugger-a (fn &rest args)
  :around #'debug
  ;; Without `zenit-debug-mode', be as vanilla as possible.
  (if (not zenit-debug-mode)
      (apply fn args)
    ;; Work around Emacs's heuristic (in eval.c) for detecting errors in the
    ;; debugger, which would run this handler again on subsequent calls. Taken
    ;; from `ert--run-test-debugger'.
    (if (and noninteractive (fboundp 'zenit-cli-debugger))
        (apply #'zenit-cli-debugger args)
      (apply fn args))))

(autoload 'backtrace-get-frames "backtrace")
;;;###autoload
(defun zenit-backtrace ()
  "Return a stack trace as a list of `backtrace-frame' objects."
  (cdr (backtrace-get-frames debugger)))

(defun zenit-backtrace-write-to-file (backtrace file)
  "Write BACKTRACE to FILE with appropriate boilerplate."
  (make-directory (file-name-directory file) t)
  (let ((zenit-print-indent 0))
    (with-temp-file file
      (insert ";; -*- lisp-interaction -*-\n")
      (insert ";; vim: set ft=lisp:\n")
      (insert (format ";; command=%S\n" command-line-args))
      (insert (format ";; date=%S\n\n" (format-time-string "%Y-%m-%d %H-%M-%S" before-init-time)))
      (let ((standard-output (current-buffer))
            (print-quoted t)
            (print-escape-newlines t)
            (print-escape-control-characters t)
            (print-symbols-bare t)
            (print-level nil)
            (print-circle nil)
            (n -1))
        (mapc (lambda (frame)
                (princ (format ";;;; %d\n" (cl-incf n)))
                (pp (list (cons (backtrace-frame-fun frame)
                                (backtrace-frame-args frame))
                          (backtrace-frame-locals frame)))
                (terpri))
              backtrace))
      file)))


;;
;;; Hooks

;;;###autoload
(defun zenit-run-all-startup-hooks-h ()
  "Run all startup Emacs hooks.

Meant to be executed after starting Emacs with -q or -Q, for
example:

  emacs -Q -l init.el -f zenit-run-all-startup-hooks-h"
  (setq after-init-time (current-time))
  (let ((inhibit-startup-hooks nil))
    (zenit-run-hooks 'after-init-hook
                     'delayed-warnings-hook
                     'emacs-startup-hook
                     'tty-setup-hook
                     'window-setup-hook)))

(provide 'zenit-lib '(debug))

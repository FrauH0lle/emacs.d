;; lisp/core/lib/zenit-lib-debug.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

;; `cl-seq'
(declare-function cl-remove-if-not "cl-seq")

;;`profiler'
(declare-function profiler-report "profiler")
(declare-function profiler-stop "profiler")


;;
;;; Profiling

(defvar zenit--profiler nil)
;;;###autoload
(defun zenit/toggle-profiler ()
  "Toggle the Emacs profiler. Run it again to see the profiling report."
  (interactive)
  (if (not zenit--profiler)
      (profiler-start 'cpu+mem)
    (profiler-report)
    (profiler-stop))
  (setq zenit--profiler (not zenit--profiler)))


;;
;;; Debug

;;;###autoload
(defvar zenit-debug-variables
  '(async-debug
    debug-on-error
    init-file-debug
    garbage-collection-messages
    gcmh-verbose
    init-file-debug
    jka-compr-verbose
    url-debug
    use-package-verbose
    (message-log-max . 16384))
  "A list of variable to toggle on `zenit-debug-mode'.
Each entry can be a variable symbol or a cons cell whose CAR is the variable
symbol and CDR is the value to set it to when `zenit-debug-mode' is activated.")

(defvar zenit--debug-vars-undefined nil)

(defun zenit--watch-debug-vars-h (&rest _)
  (when-let (bound-vars (cl-remove-if-not #'boundp zenit--debug-vars-undefined))
    (zenit-log "New variables available: %s" bound-vars)
    (let ((message-log-max nil))
      (zenit-debug-mode -1)
      (zenit-debug-mode +1))))

;;;###autoload
(define-minor-mode zenit-debug-mode
  "Toggle `debug-on-error' and `init-file-debug' for verbose logging."
  :init-value nil
  :global t
  :group 'zenit
  (let ((enabled zenit-debug-mode))
    (setq zenit--debug-vars-undefined nil)
    (dolist (var zenit-debug-variables)
      (cond ((listp var)
             (cl-destructuring-bind (var . val) var
               (if (boundp var)
                   (set-default
                    var (if (not enabled)
                            (prog1 (get var 'initial-value)
                              (put 'x 'initial-value nil))
                          (put var 'initial-value (symbol-value var))
                          val))
                 (add-to-list 'zenit--debug-vars-undefined var))))
            ((if (boundp var)
                 (set-default var enabled)
               (add-to-list 'zenit--debug-vars-undefined var)))))
    (when (called-interactively-p 'any)
      (when (fboundp 'explain-pause-mode)
        (explain-pause-mode (if enabled +1 -1))))
    ;; Watch for changes in `zenit-debug-variables', or when packages load (and potentially define
    ;; one of `zenit-debug-variables'), in case some of them aren't defined when
    ;; `zenit-debug-mode' is first loaded.
    (cond (enabled
           (add-variable-watcher 'zenit-debug-variables #'zenit--watch-debug-vars-h)
           (add-hook 'after-load-functions #'zenit--watch-debug-vars-h))
          (t
           (remove-variable-watcher 'zenit-debug-variables #'zenit--watch-debug-vars-h)
           (remove-hook 'after-load-functions #'zenit--watch-debug-vars-h)))
    (message "Debug mode %s" (if enabled "on" "off"))))


;;
;;; Hooks

;;;###autoload
(defun zenit-run-all-startup-hooks-h ()
  "Run all startup Emacs hooks. Meant to be executed after starting Emacs with
-q or -Q, for example:
  emacs -Q -l init.el -f zenit-run-all-startup-hooks-h"
  (setq after-init-time (current-time))
  (let ((inhibit-startup-hooks nil))
    (mapc (lambda (hook)
            (run-hook-wrapped hook #'zenit-run-hook))
          '(after-init-hook
            delayed-warnings-hook
            emacs-startup-hook
            tty-setup-hook
            window-setup-hook))))

(provide 'zenit-lib '(debug))

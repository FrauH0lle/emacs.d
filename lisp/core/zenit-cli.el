;; lisp/core/zenit-cli.el -*- lexical-binding: t; -*-


(defvar zenit-auto-accept (getenv-internal "YES")
  "If non-nil, auto-accept any confirmation prompts.")

(defvar zenit-auto-discard (getenv-internal "FORCE")
  "If non-nil, discard all local changes while updating.")


;;
;;; Log settings

(defvar zenit-cli-log-file-format (expand-file-name "logs/cli.%s.%s.%s" zenit-data-dir)
  "Where to write any output/log file to.

Must have three arguments: the date/time, a context (e.g. stdout)
and the log type.")

(defvar zenit-cli-log-retain 10
  "Number of each log type to retain.")

(defvar zenit-cli-log-backtrace-depth 12
  "How many frames of the backtrace to display in stdout.")

(defvar zenit-cli-log-straight-error-lines 16
  "How many lines of straight.el errors to display in stdout.")

;; Setup logging
(defvar zenit-cli-log-buffers
  `((stdin   . ,(generate-new-buffer "* zenit-cli stdin*"))
    (stdout  . ,(generate-new-buffer "* zenit-cli stdout*"))
    (stderr  . ,(generate-new-buffer "* zenit-cli stderr*"))
    (complog . ,(generate-new-buffer "* zenit-cli compile*")))
  "An alist mapping log types and buffers.")

(defun zenit-cli-debugger (type data)
  "Print a more presentable backtrace to terminal and write it to file."
  ;; HACK Works around a heuristic in eval.c for detecting errors in the
  ;;   debugger, which executes this handler again on subsequent calls. Taken
  ;;   from `ert--run-test-debugger'.
  (cl-incf num-nonmacro-input-events)
  (let* ((inhibit-read-only nil)
         (inhibit-message nil)
         (inhibit-redisplay nil)
         (inhibit-trace t)
         (executing-kbd-macro nil)
         (load-read-function #'read)
         (backtrace (zenit-backtrace))
         (straight-error
          (and (bound-and-true-p straight-process-buffer)
               (or (member straight-process-buffer data)
                   (string-match-p (regexp-quote straight-process-buffer)
                                   (error-message-string data)))
               (with-current-buffer (straight--process-buffer)
                 (split-string (buffer-string) "\n" t)))))
    (cond
     (straight-error
      (let ((error-file (zenit-cli--output-file 'error 'straight)))
        (print! (error "The package manager threw an error"))
        (print! (error "Last %d lines of straight's error log:")
                zenit-cli-log-straight-error-lines)
        (print-group!
          (print!
           "%s" (string-join
                 (seq-subseq straight-error
                             (max 0 (- (length straight-error)
                                       zenit-cli-log-straight-error-lines))
                             (length straight-error))
                 "\n")))
        (print! (warn "Wrote extended straight log to %s")
                (path (let ((coding-system-for-write 'utf-8-auto))
                        (with-file-modes #o600
                          (with-temp-file error-file
                            (insert-buffer-substring (straight--process-buffer))))
                        error-file)))))
     ((eq type 'error)
      (let* ((generic? (eq (car data) 'error))
             (zenit-cli-log-backtrace-depth zenit-cli-log-backtrace-depth)
             (print-escape-newlines t)
             (error-file (zenit-cli--output-file 'error 'backtrace)))
        (print! (bold (error "An error has occured")))
        (print-group!
         (print! "%s %s" (bold "Message:")
                 (if generic?
                     (error-message-string data)
                   (get (car data) 'error-message)))
         (unless generic?
           (print! "%s %s" (bold "Details:")
                   (let* ((print-level 4)
                          (print-circle t)
                          (print-escape-newlines t))
                     (prin1-to-string (cdr data)))))
         (when backtrace
           (print! (bold "Backtrace:"))
           (print-group!
            (dolist (frame (seq-take backtrace zenit-cli-log-backtrace-depth))
              (print! "%s" (truncate (prin1-to-string
                                      (cons (backtrace-frame-fun  frame)
                                            (backtrace-frame-args frame)))
                                     (- 80
                                        zenit-print-indent
                                        1)
                                     "..."))))
           (when-let* ((backtrace-file (zenit-backtrace-write-to-file backtrace error-file)))
             (print! (warn "Wrote extended backtrace to %s")
                     (path backtrace-file))))))))))

(defmacro zenit-cli-redirect-output (&rest body)
  "Redirect output from BODY to the appropriate log buffers."
  (declare (indent 0))
  `(let* (;; Emit more user-friendly backtraces
          (debugger #'zenit-cli-debugger)
          (debug-on-error t))
     (with-output-to! `((>= notice ,(alist-get 'stdout zenit-cli-log-buffers))
                        (t . ,(alist-get 'stderr zenit-cli-log-buffers)))
       ,@body)))

(defun zenit-cli--output-file (type context)
  "Return a log file path for TYPE and CONTEXT.

See `zenit-cli-log-file-format' for details."
  (format zenit-cli-log-file-format
          (format-time-string "%Y%m%d%H%M%S")
          context
          type))

(defun zenit-cli--output-write-logs-h ()
  "Write log buffers to their appropriate files."
  (dolist (logbuf (cl-remove-if-not (lambda (x) (memq (car x) '(stderr complog))) zenit-cli-log-buffers))
    (cl-destructuring-bind
        (context buffer type)
        (list (car logbuf) (cdr logbuf) 'log)
      ;; Delete the last `zenit-cli-log-retain' logs
      (mapc #'delete-file
            (append (butlast (zenit-glob (format zenit-cli-log-file-format "*" "*" "log"))
                             zenit-cli-log-retain)
                    (butlast (zenit-glob (format zenit-cli-log-file-format "*" "*" "error"))
                             zenit-cli-log-retain)))

      ;; Then write the log file, if necessary
      (let* ((file (zenit-cli--output-file type context)))
        (when (> (buffer-size buffer) 0)
          (with-file-modes #o700
            (make-directory (file-name-directory file) t))
          (with-file-modes #o600
            (with-temp-file file
              (insert-buffer-substring buffer)
              (ansi-color-filter-region (point-min) (point-max)))))))))


;;
;;; Bootstrap

(when noninteractive
  (zenit-context-push 'cli)

  (setq gc-cons-threshold 134217728  ; 128mb
        gc-cons-percentage 1.0)

  ;; Create all our core directories to quell file errors.
  (mapc (zenit-rpartial #'make-directory 'parents)
        (list zenit-local-dir
              zenit-data-dir
              zenit-cache-dir))

  (delete-file async-byte-compile-log-file)

  (require 'cl-lib)

  (quiet!
   (require 'cl nil t)    ; "Package cl is deprecated"
   (unless site-run-file  ; unset in zenit-core.el
     (when-let* ((site-run-file (get 'site-run-file 'initial-value)))
       (load site-run-file t inhibit-message))))

  (setq-default
   ;; Don't generate superfluous files when writing temp buffers.
   make-backup-files nil
   ;; Stop user configuration from interfering with package management.
   enable-dir-local-variables nil
   ;; Reduce ambiguity, embrace specificity, enjoy predictability.
   case-fold-search nil
   ;; Don't clog the user's trash with our CLI refuse.
   delete-by-moving-to-trash nil)

  (require 'seq)
  (require 'map)

  ;; Suppress any possible coding system prompts during CLI sessions.
  (set-language-environment "UTF-8")

  ;; Eagerly load these libraries
  (mapc (zenit-rpartial #'load nil (not init-file-debug) 'nosuffix)
        (append (file-expand-wildcards (concat zenit-core-dir "lib/*.el"))
                (file-expand-wildcards (concat zenit-core-dir "cli/*.el"))))

  (if init-file-debug (zenit-debug-mode +1))

  ;; Ensure package management is ready
  (require 'zenit-packages)

  ;; Last minute initialization at the end of loading this file.
  (with-eval-after-load 'zenit-cli
    (zenit-run-hooks 'zenit-before-init-hook))

  (zenit-modules-initialize))


;;
;;; Errors
(define-error 'zenit-cli-error "There was an unexpected error" 'zenit-error)

(provide 'zenit-cli)

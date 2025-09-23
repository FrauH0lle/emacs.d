;;; lisp/core/zenit-lib.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains many helper functions and macros which are used throughout
;; the configuration.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))
(declare-function cl-subseq "cl-extra" (seq start &optional end))

;; `cl-lib'
(declare-function cl-evenp "cl-lib" (integer))

;; `cl-seq'
(declare-function cl-position "cl-seq" (cl-item cl-seq &rest cl-keys))

;; `subr-x'
(declare-function hash-table-keys "subr-x" (hash-table))
(declare-function string-remove-prefix "subr-x" (prefix string))
(autoload #'string-remove-prefix "subr-x")
(declare-function string-remove-suffix "subr-x" (suffix string))
(autoload #'string-remove-suffix "subr-x")

;; `zenit-core'
(defvar zenit-core-dir)
(defvar zenit-local-conf-dir)
(defvar zenit-module-control-file)
(defvar zenit-modules-dir)
(defvar zenit-modules-load-path)

;; `zenit-lib-compile'
(declare-function zenit-async-byte-compile-file "zenit-lib-compile" (file &rest kwargs))
(autoload #'zenit-async-byte-compile-file "zenit-lib-compile")
(declare-function zenit-compile-generate-args "zenit-lib-compile" (target))
(autoload #'zenit-compile-generate-args "zenit-lib-compile")

;; `zenit-lib-files'
(declare-function zenit-files-in "zenit-lib-files" (paths &rest rest))
(declare-function zenit-file-read "zenit-lib-files" (file &rest kwargs))

;; `zenit-lib-modules'
(defvar zenit-modules)


;;
;;; Custom error types

(define-error 'zenit-error "Error in Emacs")
(define-error 'zenit-nosync-error "Config could not be initialized; did you remember to run 'emacs-config refresh' in the shell?" 'zenit-error)
(define-error 'zenit-core-error "Unexpected error in core directory" 'zenit-error)
(define-error 'zenit-context-error "Incorrect context error" 'zenit-error)
(define-error 'zenit-hook-error "Error in a startup hook" 'zenit-error)
(define-error 'zenit-autoload-error "Error in an autoloads file" 'zenit-error)
(define-error 'zenit-module-error "Error in a module" 'zenit-error)
(define-error 'zenit-local-conf-error "Error in local config" 'zenit-error)
(define-error 'zenit-package-error "Error with packages" 'zenit-error)


;;
;;; Eval/compilation

(defmacro protect-macros! (&rest body)
  "Eval BODY, protecting macros from incorrect expansion.
This macro should be used in the following situation:

Some form is being evaluated, and this form contains as a
sub-form some code that will not be evaluated immediately, but
will be evaluated later. The code uses a macro that is not
defined at the time the top-level form is evaluated, but will be
defined by time the sub-form's code is evaluated. This macro
handles its arguments in some way other than evaluating them
directly. And finally, one of the arguments of this macro could
be interpreted itself as a macro invocation, and expanding the
invocation would break the evaluation of the outer macro.

You might think this situation is such an edge case that it would
never happen, but you'd be wrong, unfortunately. In such a
situation, you must wrap at least the outer macro in this form,
but can wrap at any higher level up to the top-level form."
  (declare (indent 0) (debug body))
  `(eval '(progn ,@body) lexical-binding))

(defmacro protect-macros-maybe! (feature &rest body)
  "Same as `protect-macros!', but only if FEATURE is unavailable.
Otherwise eval BODY normally (subject to eager macroexpansion).
In either case, eagerly load FEATURE during byte-compilation."
  (declare (indent 1) (debug (sexp body)))
  (let ((available (featurep feature)))
    (when byte-compile-current-file
      (setq available (require feature nil 'noerror)))
    (if available
        `(progn ,@body)
      `(protect-macros!
         (progn ,@body)))))


;;
;;; Logging

(defvar zenit-inhibit-log (not (or noninteractive init-file-debug))
  "If non-nil, suppress `zenit-log' output completely.")

(defvar zenit-log-level
  (if init-file-debug
      (if-let* ((level (getenv-internal "DEBUG"))
                (level (if (string-empty-p level) 1 (string-to-number level)))
                ((not (zerop level))))
          level
        2)
    0)
  "How verbosely to log from `zenit-log' calls.

0 -- No logging at all.
1 -- Only warnings.
2 -- Warnings and notices.
3 -- Debug info, warnings, and notices.")

(defun zenit--log (level text &rest args)
  "Log a message with the given TEXT and ARGS.

The message will be formatted with a timestamp, and optionally
`zenit-module-context'. The message will be logged only if
`init-file-debug' is non-nil.

If the TEXT starts with a colon, it is considered an absolute
context and the current `zenit-module-context' will not be used.
In this case, the colon will be removed from the logged message.

If LEVEL is below `zenit-log-level', do not log the output.

Any additional arguments to be used for formatting the message
text can be passed via ARGS."
  (let ((inhibit-message (if noninteractive
                             (not init-file-debug)
                           (> level zenit-log-level)))
        (absolute? (string-prefix-p ":" text)))
    (apply #'message
           (propertize (concat "* %.06f:%s" (if (not absolute?) ":") text)
                       'face 'font-lock-doc-face)
           (float-time (time-subtract (current-time) before-init-time))
           (mapconcat
            (lambda (x) (format "%s" x))
            (unless absolute?
              (append (if (bound-and-true-p zenit-module-context)
                          (let ((key (zenit-module-context-key)))
                            (delq nil (list (car key) (cdr key)))))))
            ":")
           args)))

(defmacro zenit-log (message &rest args)
  "Log a MESSAGE to stderr or *Messages*.
Does not emit the message in the echo area. This is a macro
instead of a function to prevent the potentially expensive
evaluation of its ARGS when debug mode is off. Return non-nil."
  (declare (debug t))
  (let ((level (if (integerp message)
                   (prog1 message
                     (setq message (pop args)))
                 2)))
    `(when (and (not zenit-inhibit-log)
                (or (not noninteractive)
                    (<= ,level zenit-log-level)))
       (zenit--log ,level ,message ,@args))))


;;
;;; Helpers

(defun zenit--resolve-hook-forms (hooks)
  "Convert a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list
is quoted, the list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (ensure-list (zenit-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun zenit--setq-hook-fns (hooks rest &optional singles)
  "Generate a list of hook setting functions for HOOKS and REST.

HOOKS can be a list of hooks or a single hook symbol. REST is a
list of variable-value pairs or a list of variables if SINGLES is
non-nil. SINGLES indicates whether REST contains only variable
names. If non-nil, the generated functions will set the variables
to nil when the hooks are run. If nil, the generated functions
will set the variables to the corresponding values provided in
REST.

This function creates a list of functions that set the specified
variables to their corresponding values when the hooks are run.
The generated functions have the format
`zenit--setq-VAR-for-MODE-h' where VAR is the variable name and
MODE is derived from the hook name."
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'cl-evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
                             vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook in (zenit--resolve-hook-forms hooks)
           for mode = (string-remove-suffix "-hook" (symbol-name hook))
           append
           (cl-loop for (var . val) in vars
                    collect
                    (list var val hook
                          (intern (format "zenit--setq-%s-for-%s-h"
                                          var mode))))))


;;
;;; Public library

(defun zenit-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun zenit-keyword-intern (str)
  "Convert STR (a string) into a keyword (`keywordp')."
  (declare (pure t) (side-effect-free t))
  (cl-check-type str string)
  (intern (concat ":" str)))

(defun zenit-keyword-name (keyword)
  "Return the string name of KEYWORD (`keywordp').

Removes the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))

(defalias 'zenit-partial #'apply-partially)

(defun zenit-rpartial (fn &rest args)
  "Return a partial application of FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result
is a new function which does the same as FN, except that the
last N arguments are fixed at the values with which this function
was called."
  (declare (side-effect-free t))
  (lambda (&rest pre-args)
    (apply fn (append pre-args args))))

(defun zenit-lookup-key (keys &rest keymaps)
  "Look up key sequence KEYS in KEYMAPS.

Like `lookup-key', but search active keymaps, if KEYMAPS is
omitted."
  (if keymaps
      (cl-some (zenit-rpartial #'lookup-key keys) keymaps)
    (cl-loop for keymap
             in (append (cl-loop for alist in emulation-mode-map-alists
                                 append (mapcar #'cdr
                                                (if (symbolp alist)
                                                    (if (boundp alist) (symbol-value alist))
                                                  alist)))
                        (list (current-local-map))
                        (mapcar #'cdr minor-mode-overriding-map-alist)
                        (mapcar #'cdr minor-mode-map-alist)
                        (list (current-global-map)))
             if (keymapp keymap)
             if (lookup-key keymap keys)
             return it)))

(defun zenit-load (path &optional noerror)
  "Load PATH and handle any errors that arise from it.

If NOERROR, don't throw an error if PATH doesn't exist."
  (zenit-log "load: %s %s" (abbreviate-file-name path) noerror)
  (condition-case-unless-debug e
      (load path noerror 'nomessage)
    (zenit-error
     (signal (car e) (cdr e)))
    (error
     (setq path (locate-file path load-path (get-load-suffixes)))
     (if (not (and path (featurep 'zenit-core)))
         (signal (car e) (cdr e))
       (cl-loop for (err . dir)
                in `((zenit-cli-error        . ,(expand-file-name "cli" zenit-core-dir))
                     (zenit-core-error       . ,zenit-core-dir)
                     (zenit-local-conf-error . ,zenit-local-conf-dir)
                     (zenit-module-error     . ,zenit-modules-dir))
                if (file-in-directory-p path dir)
                do (signal err (list (file-relative-name path (expand-file-name "../" dir))
                                     e)))))))

(defun zenit-require (feature &optional filename noerror)
  "Like `require', but handles subfeatures.

This will load FEATURE if not already present in `features'.
FILENAME works either as in `require' or can be a subfeature as a
symbol. For example

  (zenit-require \\='zenit-lib \\='files)

If NOERROR is non-nil, it will simply return nil instead of an
error."
  (let ((subfeature (if (symbolp filename) filename)))
    (or (featurep feature subfeature)
        (zenit-load
         (if subfeature
             (file-name-concat zenit-core-dir
                               (string-remove-prefix "zenit-" (symbol-name feature))
                               (concat "zenit-lib-" (symbol-name filename)))
           (symbol-name feature))
         noerror))))

(defun zenit-load-envvars-file (file &optional noerror)
  "Read and set envvars from FILE.
If NOERROR is non-nil, don't throw an error if the file doesn't
exist or is unreadable. Returns the names of envvars that were
changed."
  (if (null (file-exists-p file))
      (unless noerror
        (signal 'file-error (list "No envvar file exists" file)))
    (with-temp-buffer
      (insert-file-contents file)
      (when-let* ((env (read (current-buffer))))
        (let ((tz (getenv-internal "TZ")))
          (setq-default
           process-environment
           (append env (default-value 'process-environment))
           exec-path
           (append (split-string (getenv "PATH") path-separator t)
                   (list exec-directory))
           shell-file-name
           (or (getenv "SHELL")
               (default-value 'shell-file-name)))
          (when-let* ((newtz (getenv-internal "TZ")))
            (unless (equal tz newtz)
              (set-time-zone-rule newtz))))
        env))))

(defvar zenit--hook nil)
(defun zenit-run-hook (hook)
  "Run HOOK (a hook function) with better error handling.
Meant to be used with `run-hook-wrapped'."
  (zenit-log 2 "hook:%s: run %s" (or zenit--hook '*) hook)
  (condition-case-unless-debug e
      (funcall hook)
    (error
     (signal 'zenit-hook-error (list hook e))))
  ;; return nil so `run-hook-wrapped' won't short circuit
  nil)

(defun zenit-run-hooks (&rest hooks)
  "Run HOOKS with better error handling.
HOOKS is a list of hook variable symbols. It Is used as advice to
replace `run-hooks'."
  (dolist (hook hooks)
    (condition-case-unless-debug e
        (let ((zenit--hook hook))
          (run-hook-wrapped hook #'zenit-run-hook))
      (zenit-hook-error
       (unless debug-on-error
         (lwarn hook :error "Error running hook %S because: %s"
                (if (symbolp (cadr e))
                    (symbol-name (cadr e))
                  (cadr e))
                (caddr e)))
       (signal 'zenit-hook-error (cons hook (cdr e)))))))

(defun zenit-run-hook-on (hook-var trigger-hooks)
  "Configure HOOK-VAR to be invoked exactly once by TRIGGER-HOOKS.

HOOK-VAR is invoked exactly once when any of the TRIGGER-HOOKS
are invoked *after* Emacs has initialized (to reduce false
positives). Once HOOK-VAR is triggered, it is reset to nil.

HOOK-VAR is a quoted hook.

TRIGGER-HOOK is a list of quoted hooks and/or sharp-quoted
functions."
  (dolist (hook trigger-hooks)
    (let ((fn (make-symbol (format "chain-%s-to-%s-h" hook-var hook)))
          running?)
      (fset
       fn (lambda (&rest _)
            ;; Only trigger this after Emacs has initialized.
            (when (and (not running?)
                       (not (zenit-context-p 'startup))
                       (or (daemonp)
                           ;; In some cases, hooks may be lexically unset to
                           ;; inhibit them during expensive batch operations on
                           ;; buffers (such as when processing buffers
                           ;; internally). In these cases we should assume this
                           ;; hook wasn't invoked interactively.
                           (and (boundp hook)
                                (symbol-value hook))))
              (setq running? t)  ; prevent infinite recursion
              (zenit-run-hooks hook-var)
              (set hook-var nil))))
      (cond ((daemonp)
             ;; In a daemon session we don't need all these lazy loading
             ;; shenanigans. Just load everything immediately.
             (add-hook 'after-init-hook fn 'append))
            ((eq hook 'find-file-hook)
             ;; Advise `after-find-file' instead of using `find-file-hook'
             ;; because the latter is triggered too late (after the file has
             ;; opened and modes are all set up).
             (advice-add 'after-find-file :before fn '((depth . -101))))
            ((add-hook hook fn -101)))
      fn)))


;;
;;; Convenience

(defmacro file! ()
  "Return the filename of the file this macro was called."
  (or
   (bound-and-true-p zenit-include--current-file)
   (bound-and-true-p byte-compile-current-file)
   load-file-name
   (buffer-file-name (buffer-base-buffer))   ; for `eval'
   (macroexp-file-name)
   (error "`file!': cannot deduce the current file path")))

(defmacro dir! ()
  "Return the directory of the file this macro was called."
  (let (file-name-handler-alist)
    (file-name-directory (macroexpand '(file!)))))

(put 'defun* 'lisp-indent-function 'defun)
(defmacro letf! (bindings &rest body)
  "Temporarily rebind function, macros, and advice in BODY.

Intended as syntax sugar for `cl-letf', `cl-labels',
`cl-macrolet', and temporary advice (`define-advice').

BINDINGS is either:

  A list of (PLACE VALUE) bindings as `cl-letf*' would accept.
  A list of, or a single, `defun', `defun*', `defmacro', or
  `defadvice' forms.


The def* forms accepted are:

  (defun NAME (ARGS...) &rest BODY)
    Defines a temporary function with `cl-letf'

  (defun* NAME (ARGS...) &rest BODY)
    Defines a temporary function with `cl-labels' (allows
    recursive definitions).

  (defmacro NAME (ARGS...) &rest BODY)
    Uses `cl-macrolet'.

  (defadvice FUNCTION WHERE ADVICE)
    Uses `advice-add' (then `advice-remove' afterwards).

  (defadvice FUNCTION (HOW LAMBDA-LIST &optional NAME DEPTH) &rest BODY)
    Defines temporary advice with `define-advice'."
  (declare (indent defun)
           (debug ((&rest [&or
                           ;; ((PLACE VALUE))
                           ;; PLACE can be a symbol or a function (#'PLACE)
                           ([&or symbolp
                                 ("function" symbolp)]
                            form)
                           ;; (defun ...) or ((defun ...))
                           [&or [&define "defun" symbolp cl-lambda-list def-body]
                                (&define "defun" symbolp cl-lambda-list def-body)]
                           ;; (defun* ...) or ((defun* ...))
                           [&or [&define "defun*" symbolp cl-lambda-list def-body]
                                (&define "defun*" symbolp cl-lambda-list def-body)]
                           ;; (defmacro ...) or ((defmacro ...))
                           [&or [&define "defmacro" symbolp cl-macro-list def-body]
                                (&define "defmacro" symbolp cl-macro-list def-body)]
                           ;; (defadvice ...) or ((defadvice ...))
                           [&or ["defadvice" [&or [[&or symbolp ("function" symbolp)] keywordp form]
                                                  [sexp (keywordp sexp [&optional sexp] [&optional integerp]) body]]]
                                ("defadvice" . [&or ([&or symbolp ("function" symbolp)] keywordp form)
                                                    (sexp (keywordp sexp [&optional sexp] [&optional integerp]) body)])]])
                   body)))
  (setq body (macroexp-progn body))
  (when (memq (car bindings) '(defun defun* defmacro defadvice))
    (setq bindings (list bindings)))
  (dolist (binding (reverse bindings) body)
    (let ((type (car binding))
          (rest (cdr binding)))
      (setq
       body (pcase type
              (`defmacro `(cl-macrolet ((,@rest)) ,body))
              (`defadvice
                  (if (keywordp (cadr rest))
                      (cl-destructuring-bind (target where fn) rest
                        `(when-let* ((fn ,fn))
                           (advice-add ,target ,where fn)
                           (unwind-protect ,body (advice-remove ,target fn))))
                    (let* ((fn (pop rest))
                           (argspec (pop rest)))
                      (when (< (length argspec) 3)
                        (setq argspec
                              (list (nth 0 argspec)
                                    (nth 1 argspec)
                                    (or (nth 2 argspec) (gensym (format "%s-a" (symbol-name fn)))))))
                      ;; `define-advice' generates an advice function name of
                      ;; the form SYMBOL@NAME which we need to use in the
                      ;; following.
                      (let ((name (intern (format "%s@%s" (symbol-name fn) (symbol-name (nth 2 argspec))))))
                        `(progn
                           (define-advice ,fn ,argspec ,@rest)
                           (unwind-protect ,body
                             (advice-remove #',fn #',name)
                             ,(if name `(fmakunbound ',name))))))))
              (`defun
                  `(cl-letf ((,(car rest) (symbol-function #',(car rest))))
                     ;; Make the byte-compiler happy
                     (eval-when-compile
                       (declare-function ,(car rest) nil))
                     (ignore ,(car rest))
                     (cl-letf (((symbol-function #',(car rest))
                                (lambda! ,(cadr rest) ,@(cddr rest))))
                       ,body)))
              (`defun*
                `(cl-labels ((,@rest)) ,body))
              (_
               (when (eq (car-safe type) 'function)
                 (setq type (list 'symbol-function type)))
               (list 'cl-letf (list (cons type rest)) body)))))))

(defmacro quiet!! (&rest forms)
  "Run FORMS without generating any output (for real).

Unlike `quiet!', which will only suppress output in the echo area
in interactive sessions, this truly suppress all output from
FORMS."
  (declare (indent 0) (debug t))
  `(if init-file-debug
       (progn ,@forms)
     (letf! ((standard-output (lambda (&rest _)))
             (defun message (&rest _))
             (defun load (file &optional noerror _nomessage nosuffix must-suffix)
               (funcall load file noerror t nosuffix must-suffix))
             (defun write-region (start end filename &optional append visit lockname mustbenew)
               (unless visit (setq visit 'no-message))
               (funcall write-region start end filename append visit lockname mustbenew)))
       ,@forms)))

(defmacro quiet! (&rest forms)
  "Run FORMS without generating any output.

This silences calls to `message', `load', `write-region' and
anything that writes to `standard-output'. In interactive
sessions this inhibits output to the echo-area, but not to
*Messages*."
  (declare (indent 0) (debug t))
  `(if init-file-debug
       (progn ,@forms)
     ,(if noninteractive
          `(quiet!! ,@forms)
        `(let ((inhibit-message t)
               (save-silently t))
           (prog1 ,@forms (message ""))))))


;;
;;; Closure factories

(defmacro lambda! (arglist &rest body)
  "Return (cl-function (lambda ARGLIST BODY...)).

The closure is wrapped in `cl-function', meaning ARGLIST will
accept anything `cl-defun' will. Implicitly adds
`&allow-other-keys' if `&key' is present in ARGLIST."
  (declare (indent defun) (doc-string 1) (pure t) (side-effect-free t)
           (debug (&define cl-lambda-list def-body)))
  `(cl-function
    (lambda
      ,(letf! (defun* allow-other-keys (args)
                (mapcar
                 (lambda (arg)
                   (cond ((nlistp (cdr-safe arg)) arg)
                         ((listp arg) (allow-other-keys arg))
                         (arg)))
                 (if (and (memq '&key args)
                          (not (memq '&allow-other-keys args)))
                     (if (memq '&aux args)
                         (let (newargs arg)
                           (while args
                             (setq arg (pop args))
                             (when (eq arg '&aux)
                               (push '&allow-other-keys newargs))
                             (push arg newargs))
                           (nreverse newargs))
                       (append args (list '&allow-other-keys)))
                   args)))
         ;; Make the byte-compiler happy
         (eval-when-compile
           (declare-function allow-other-keys nil))
         (allow-other-keys arglist))
      ,@body)))

(setplist 'zenit--fn-crawl '(%2 2 %3 3 %4 4 %5 5 %6 6 %7 7 %8 8 %9 9))
;; NOTE 2024-08-05: The `eval-and-compile' is necessary so
;;   `zenit-compile-functions' defined below does not complain that
;;   `zenit--fn-crawl' is not defined dring compilation.
(eval-and-compile
  (defun zenit--fn-crawl (data args)
    "Recursively crawl DATA and populate ARGS array based on special
symbols found in DATA.

DATA is a nested list or vector structure containing symbols,
where special symbols such as '%*', '%%', '%%1' are of interest.

ARGS is an array used to store the special symbols found in DATA
at their respective positions:

- '%*' at position 0,
- '%%' or '%%1' at position 1.

If both '%*' and '%1' are found in DATA, an error is raised."
    (cond ((symbolp data)
           (when-let
               (pos (cond ((eq data '%*) 0)
                          ((memq data '(% %1)) 1)
                          ((get 'zenit--fn-crawl data))))
             (when (and (= pos 1)
                        (aref args 1)
                        (not (eq data (aref args 1))))
               (error "%% and %%1 are mutually exclusive"))
             (aset args pos data)))
          ((and (not (eq (car-safe data) 'fn!))
                (or (listp data)
                    (vectorp data)))
           (let ((len (length data))
                 (i 0))
             (while (< i len)
               (zenit--fn-crawl (elt data i) args)
               (cl-incf i)))))))

(defmacro fn! (&rest args)
  "Return an lambda with implicit, positional arguments.

The function's arguments are determined recursively from ARGS.
Each symbol from `%1' through `%9' that appears in ARGS is
treated as a positional argument. Missing arguments are named
`_%N', which keeps the byte-compiler quiet. `%' is a shorthand
for `%1'; only one of these can appear in ARGS. `%*' represents
extra `&rest' arguments.

Instead of:

  (lambda (a _ c &rest d)
    (if a c (cadr d)))

you can use this macro and write:

  (fn! (if %1 %3 (cadr %*)))

which expands to:

  (lambda (%1 _%2 %3 &rest %*)
    (if %1 %3 (cadr %*)))

This macro was adapted from llama.el (see URL
`https://git.sr.ht/~tarsius/llama'), minus font-locking and the
outer function call, plus some minor optimizations."
  (declare (debug t))
  `(lambda ,(let ((argv (make-vector 10 nil)))
              (zenit--fn-crawl args argv)
              `(,@(let ((i (1- (length argv)))
                        (n -1)
                        sym arglist)
                    (while (> i 0)
                      (setq sym (aref argv i))
                      (unless (and (= n -1) (null sym))
                        (cl-incf n)
                        (push (or sym (intern (format "_%%%d" i)))
                              arglist))
                      (cl-decf i))
                    arglist)
                ,@(and (aref argv 0) '(&rest %*))))
     ,@args))

(defmacro cmd! (&rest body)
  "Expands to (lambda () (interactive) ,@BODY).
A factory for quickly producing interaction commands,
particularly for keybinds or aliases."
  (declare (doc-string 1) (debug body))
  `(lambda (&rest _) (interactive) ,@body))

(defmacro cmd!! (command &optional new-prefix-arg &rest args)
  "Return an interactive closure.

The closure calls COMMAND with ARGS and NEW-PREFIX-ARG.

Like `cmd!', but allows you to change `current-prefix-arg' or
pass arguments to COMMAND. This macro is meant to be used as a
target for keybinds (e.g. with `define-key' or `map!')."
  (declare (doc-string 1) (debug t))
  `(lambda (arg &rest _) (interactive "P")
     (let ((current-prefix-arg (or ,new-prefix-arg arg)))
       (,(if args
             #'funcall-interactively
           #'call-interactively)
        ,command ,@args))))


;;
;;; Mutation

(defun zenit-splice-into (seq element &optional after before)
  "Insert ELEMENT (a symbol, string or list) into the list SEQ.

Insertion is done between the elements BEFORE and AFTER. If both AFTER
and BEFORE are given, ELEMENT is inserted directly after AFTER. If only
BEFORE is given, ELEMENT is inserted before BEFORE. If neither is given,
ELEMENT is prepended to SEQ.

Comparison is done via `equal'. Returns the modified list."
  (let ((after-position (and after (cl-position after seq :test #'equal)))
        (before-position (and before (cl-position before seq :test #'equal))))
    (append
     ;; If AFTER exists, include all elements up to and including AFTER
     (and after-position (cl-subseq seq 0 (1+ after-position)))
     ;; If AFTER doesn't exist but BEFORE does, include elements before BEFORE
     (and (not after-position) before-position (cl-subseq seq 0 before-position))
     ;; Insert the new ELEMENT (ensuring it's a list)
     (ensure-list element)
     ;; If both AFTER and BEFORE exist, include elements between them
     (and before-position after-position (cl-subseq seq (1+ after-position) before-position))
     ;; If BEFORE exists, include remaining elements from BEFORE onwards
     (and before-position (cl-subseq seq before-position))
     ;; If BEFORE doesn't exist but AFTER does, include remaining elements
     (and after-position (not before-position) (cl-subseq seq (1+ after-position)))
     ;; If neither AFTER nor BEFORE exist, include all elements
     (and (not after-position) (not before-position) seq))))

(defmacro spliceq! (seq element &optional after before)
  "Splice ELEMENT into SEQ in place.
See `zenit-splice-into' for details."
  (declare (debug t))
  `(setq ,seq (zenit-splice-into ,seq ,element ,after ,before)))

(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  (declare (debug t))
  `(setq ,sym (append ,sym ,@lists)))

(defmacro setq! (&rest settings)
  "A more sensible `setopt' for setting customizable variables.

SETTINGS are symbol value pairs as in `setq'. This can be used as
a drop-in replacement for `setq' and *should* be used instead of
`setopt'. Unlike `setq', this triggers custom setters on
variables. Unlike `setopt', this won't needlessly pull in
dependencies."
  (declare (debug t))
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set-default-toplevel-value)
                      ',var ,val))))

(defmacro setq-local! (&rest settings)
  "Like `setq!' but for local customizable variables.

SETTINGS are symbol value pairs as in `setq'. This behaves the
same way as `setq!' but uses `set' instead of
`set-default-toplevel-value' and will change the local value of a
buffer-local variable"
  (declare (debug t))
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set)
                      ',var ,val))))

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.
If FETCHER is a function, ELT is used as the key in LIST (an
alist)."
  (declare (debug t))
  `(setq ,list
         (delq ,(if fetcher
                    `(funcall ,fetcher ,elt ,list)
                  elt)
               ,list)))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (declare (debug t))
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  (declare (debug t))
  `(setq ,sym (append ,@lists ,sym)))


;;
;;; Loading

(defmacro add-load-path! (&rest dirs)
  "Add DIRS to `load-path', relative to the current file.
The current file is the file from which `add-to-load-path!' is
used."
  (declare (debug t))
  `(let ((default-directory ,(protect-macros! (dir!)))
         file-name-handler-alist)
     (dolist (dir (list ,@dirs))
       (cl-pushnew (expand-file-name dir) load-path :test #'equal))))

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol (or list of them) referring to Emacs
features (aka packages). PACKAGE may use :or/:any and :and/:all
operators. The precise format is:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted, nested list of compound package lists, using any
  combination of :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated
  once both magit and diff-hl have loaded)
    (after! (magit diff-hl) BODY...)
  If :or/:any/:and/:all are omitted, :and/:all are implied.

This emulates `eval-after-load' with a few key differences:

1. No-ops for package that are disabled by the user (via
   `package!') or not installed yet.

2. Supports compound package statements (see :or/:any and
   :and/:all above).

Since the contents of these blocks will never by byte-compiled,
avoid putting things you want byte-compiled in them! Like
function/macro definitions."
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (unless (memq package (bound-and-true-p zenit-disabled-packages))
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              `(with-eval-after-load ',package ,@body)))
    (let ((p (car package)))
      (cond ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (reverse (cdr package)) (car body))
               (setq body `((after! ,next ,@body)))))
            (`(after! (:and ,@package) ,@body))))))

(defmacro load! (filename &optional path noerror)
  "Load a file relative to the current file (`load-file-name').

FILENAME is either a file path string or a form that should
evaluate to such a string at run time. PATH is where to look for
the file (a string representing a directory path). If omitted,
the lookup is relative to either `load-file-name',
`byte-compile-current-file' or variable
`buffer-file-name' (checked in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't
exist."
  (declare (debug t))
  `(zenit-load (file-name-concat ,(or path `(dir!)) ,filename) ,noerror))

(defmacro zenit--with-local-load-history (file &rest body)
  "Evaluate BODY as part of FILE.

FILE is either a file path string or a form that should evaluate
to such a string.

This macro ensures that defined functions and variables show up
as being defined in FILE, instead of whatever file they are being
loaded from."
  (declare (indent 0) (debug t))
  (let ((file (eval file)))
    `(let ((current-load-list nil))
       ,@body
       (push (cons ',file current-load-list) load-history))))

(defvar zenit-include--current-file nil
  "Stores the filename of the file to be included.")
(defmacro zenit-include (file &optional noerror)
  "Include file contents from FILE when byte-compiling.

Otherwise it delegates to `zenit-load'. If NOERROR, don't throw
an error if FILE doesn't exist."
  (declare (debug t))
  (if (not (bound-and-true-p byte-compile-current-file))
      `(zenit-load ,file ,noerror)
    (zenit-log "include: %s %s" (abbreviate-file-name file) noerror)
    (let ((forms (zenit-file-read file :by 'read*)))
      `(zenit--with-local-load-history ,file ,@forms))))

(defvar zenit-include--previous-file nil
  "Stores the filename of the previously included file.")
(defmacro include! (filename &optional path noerror)
  "Include file contents relative to the current file.

Embeds file contents directly when byte-compiling, otherwise it
delegates to `zenit-load'.

FILENAME is either a file path string or a form that should
evaluate to such a string at run time. PATH is where to look for
the file (a string representing a directory path). See `load!'
for more information.

If NOERROR is non-nil, don't throw an error if the file doesn't
exist."
  (declare (indent defun) (debug t))
  (let* ((dir (or path (protect-macros! (dir!))))
         (file (expand-file-name
                (file-name-with-extension filename ".el")
                dir)))
    `(progn
       (eval-when-compile
         (setq zenit-include--previous-file ,zenit-include--current-file
               zenit-include--current-file ,file))
       ;; Include the file
       (zenit-include ,file ,noerror)
       (eval-when-compile
         (setq zenit-include--current-file zenit-include--previous-file)))))

(defmacro compile-along! (filename &optional path)
  "Compile FILENAME along the file this macro was called from.

FILENAME can be either a file or a directory. If it is a
directory, all files found in this directory will be compiled
along. The macro does not recurse into subdirectories.

PATH is where to look for FILENAME (a string representing a
directory path).

This macro will not do anything if the file it is used in is not
being compiled."
  (declare (indent defun) (debug t))
  (when (macroexp-compiling-p)
    (let* ((filename (expand-file-name filename (or path (protect-macros! (dir!)))))
           files)
      (if (not (file-directory-p filename))
          `(eval-when-compile
             (async-get (apply #'zenit-async-byte-compile-file
                               '(,(file-name-with-extension filename ".el")
                                 ,@(zenit-compile-generate-args
                                    (file-name-with-extension filename ".el"))))))
        (dolist (f (zenit-files-in filename :match ".el$" :depth 0) files)
          (push `(async-get (apply #'zenit-async-byte-compile-file
                                   '(,(file-name-with-extension f ".el")
                                     ,@(zenit-compile-generate-args
                                        (file-name-with-extension f ".el")))))
                files))
        `(eval-when-compile
           ,@files)))))

(defmacro autoload! (file &rest rest)
  "Generate autoloads from FILE.

FILE needs to be given without extension. REST can be mulitple
function symbols. Optionally the keywords :interactive and :type
can be specifed similar to the corresponding positional arguments
in `autoload'. Note, that the optional arguments apply to all
given FNS.

Can/should be used in conjunction with `compile-along!' so you
can profit from compilation.

\(fn FILE &rest FNS [:interactive :type])"
  (declare (indent defun) (debug t))
  (let ((file (expand-file-name file (protect-macros! (dir!))))
        (autoloads ())
        fns interactive type)
    (while rest
      (if (keywordp (car rest))
          (pcase (pop rest)
            (:interactive (setq interactive (pop rest)))
            (:type  (setq type (pop rest))))
        (push (pop rest) fns)))
    (while fns
      (let ((fn (pop fns)))
        (push `(autoload ,fn ,file nil ,interactive ,type) autoloads)))
    `(progn
       ,@autoloads)))

(defmacro defer-until! (condition &rest body)
  "Run BODY when CONDITION is true.

The function checks on `after-load-functions'. Meant to serve as
a predicated alternative to `after!'."
  (declare (indent defun) (debug t))
  `(if ,condition
       (progn ,@body)
     ,(let ((fn (intern (format "zenit--delay-form-%s-h" (sxhash (cons condition body))))))
        `(progn
           (fset ',fn (lambda (&rest args)
                        (when ,(or condition t)
                          (remove-hook 'after-load-functions #',fn)
                          (unintern ',fn nil)
                          (ignore args)
                          ,@body)))
           (put ',fn 'permanent-local-hook t)
           (add-hook 'after-load-functions #',fn)))))

(defmacro after-call! (feature &rest hooks-or-functions)
  "Load FEATURE after HOOKS-OR-FUNCTIONS are executed.

FEATURE is supposed to be a package.

See also `use-package!'."
  (declare (debug t))
  (let ((fn (make-symbol (format "zenit--after-call-%s-h" feature))))
    (macroexp-progn
     (append
      `((fset ',fn
         (lambda (&rest _)
           (zenit-log "Lazy loading %s from %s" ',feature ',fn)
           (condition-case e
               ;; If `default-directory' is a directory that doesn't exist or is
               ;; unreadable, Emacs throws up file-missing errors, so we set it
               ;; to a directory we know exists and is readable.
               (let ((default-directory zenit-emacs-dir))
                 (require ',feature))
             ((debug error)
              (message "Failed to load deferred package %s: %s" ',feature e)))
           (when-let* ((deferral-list (assq ',feature zenit--deferred-packages-alist)))
             (dolist (hook (cdr deferral-list))
               (advice-remove hook #',fn)
               (remove-hook hook #',fn))
             (delq! deferral-list zenit--deferred-packages-alist)
             (unintern ',fn nil))))
        ;; Make the byte-compiler happy
        (eval-when-compile (declare-function ,fn nil)))
      (let (forms)
        (dolist (hook hooks-or-functions forms)
          (push (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
                    `(add-hook ',hook #',fn)
                  `(advice-add #',hook :before #',fn))
                forms)))
      `((unless (assq ',feature zenit--deferred-packages-alist)
          (push '(,feature) zenit--deferred-packages-alist))
        (nconc (assq ',feature zenit--deferred-packages-alist)
               '(,@hooks-or-functions)))))))

(defmacro defer-feature! (feature &rest fns)
  "Pretend FEATURE hasn't been loaded until FEATURE-hook or FNS run.

Some packages (like `elisp-mode' and `lisp-mode') are loaded
immediately at startup, which will prematurely trigger
`after!' (and `with-eval-after-load') blocks. To get around this
we make Emacs believe FEATURE hasn't been loaded yet, then wait
until FEATURE-hook (or MODE-hook, if FN is provided) is triggered
to reverse this and trigger `after!' blocks at a more reasonable
time."
  (declare (debug t))
  (let ((advice-fn (intern (format "zenit--defer-feature-%s-a" feature))))
    `(progn
       (delq! ',feature features)
       (defadvice! ,advice-fn (&rest _)
         ,(format "This advice defers `%S'." feature)
         :before ',fns
         ;; Some plugins (like yasnippet) will invoke a fn early to parse code,
         ;; which would prematurely trigger this. In those cases, well behaved
         ;; plugins will use `delay-mode-hooks', which we can check for:
         (unless delay-mode-hooks
           ;; ...Otherwise, announce to the world this package has been loaded,
           ;; so `after!' handlers can react.
           (provide ',feature)
           (dolist (fn ',fns)
             (advice-remove fn #',advice-fn)))))))


;;
;;; Compilation

(defun zenit-compile-functions (&rest fns)
  "Queue FNS to be byte/natively-compiled after a brief delay."
  (with-memoization (get 'zenit-compile-function 'timer)
    (run-with-idle-timer
     1.5 t (fn! (when-let* ((fn (pop fns)))
                  (zenit-log 3 "compile-functions: %s" fn)
                  (or (if (featurep 'native-compile)
                          (or (native-comp-function-p (indirect-function fn))
                              (ignore-errors (native-compile fn))))
                      (byte-code-function-p fn)
                      (let (byte-compile-warnings)
                        (byte-compile fn))))
                (unless fns
                  (cancel-timer (get 'zenit-compile-function 'timer))
                  (put 'zenit-compile-function 'timer nil))))))


;;
;;; Hooks

(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first
invoked, then never again. The first element can be the optional
keyword `:after', in which case the self-removing function will
be appended.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted
function (which will be advised)."
  (declare (indent 1) (debug t))
  (let ((append? (if (eq (car forms) :after) (pop forms)))
        (fn (gensym "zenit-transient-hook")))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" (zenit-unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       ;; Make the byte-compiler happy
       (eval-when-compile (declare-function ,fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append? :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append?))))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted
     mode, an unquoted list of modes, a quoted hook variable or a
     quoted list of hook variables.
  2. Optional properties :local, :append, and/or :depth [N] (in
     REST), which will make the hook buffer-local or append to
     the list of hooks (respectively),
  3. The function(s) to be added: this can be a quoted function,
     a quoted list thereof, a list of `defun' or `cl-defun'
     forms, or arbitrary forms (will implicitly be wrapped in a
     lambda).

\(fn HOOKS [:append :local [:depth N]] FUNCTIONS-OR-FORMS...)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (zenit--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil))
                     next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     ;; Make the byte-compiler happy
                     (push `(eval-when-compile (declare-function ,(nth 1 next) nil)) defn-forms)
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook ',(nreverse hook-forms))
         (dolist (func (list ,@func-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))))))

(defmacro remove-hook! (hooks &rest rest)
  "A convenience macro for removing N functions from M hooks.

Takes the same arguments as `add-hook!'.

If N (REST) and M (HOOKS) = 1, there's no benefit to using this
macro over `remove-hook'.

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))

(defmacro setq-hook! (hooks &rest var-vals)
  "Set buffer-local variables on HOOKS.

VAR-VALS are SYM VAL pairs as in `setq-local'.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1) (debug t))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (zenit--setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest _)
                       ,(format "Set buffer-local value hook function.\n\n`%s' = %s" var
                                (let ((print-level nil)
                                      (print-length nil))
                                  (prin1-to-string val)))
                       (setq-local ,var ,val))
            ;; Make the byte-compiler happy
            collect `(eval-when-compile (declare-function ,fn nil))
            collect `(add-hook ',hook #',fn -90))))

(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1) (debug t))
  (macroexp-progn
   (cl-loop for (_var _val hook fn)
            in (zenit--setq-hook-fns hooks vars 'singles)
            collect `(remove-hook ',hook #',fn))))


;;
;;; Definers

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. DOCSTRING and BODY are as in
`defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...]
BODY\)"
  (declare (doc-string 3)
           (indent defun)
           (debug t))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       ;; Make the byte-compiler happy
       (eval-when-compile
         (declare-function ,symbol nil))
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro undefadvice! (symbol _arglist &optional docstring &rest body)
  "Undefine an advice called SYMBOL.

This has the same signature as `defadvice!' an exists as an easy
undefiner when testing advice (when combined with `rotate-text').

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...]
BODY\)"
  (declare (doc-string 3)
           (indent defun)
           (debug t))
  (let (where-alist)
    (unless (stringp docstring)
      (push docstring body))
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(dolist (targets (list ,@(nreverse where-alist)))
       (dolist (target (cdr targets))
         (advice-remove target #',symbol)))))


;;
;;; Zenit context

(defvar zenit-context '(t)
  "A list of symbols identifying all active execution contexts.

This should never be directly changed, only let-bound, and should
never be empty. Each context describes what phase Emacs is in,
and may respond to.

All valid contexts:

  cli     -- while executing CLI
  emacs   -- in an interactive Emacs session
  module  -- while loading modules and their files

  Universal sub-contexts:
    compile  -- while byte-compilation is in progress
    startup  -- while Emacs is formally starting up for the first time,
                after its core libraries are loaded, but before user
                config is.

  `emacs' sub-contexts:
    eval    -- during inline evaluation of elisp
    reload  -- while reloading

  `module' sub-contexts:
    config    -- loading a module's config.el
    init      -- loading a module's init.el
    packages  -- when packagedefs are being read
    tests     -- while running unit tests")
(put 'zenit-context 'valid-values
     '(cli emacs module compile startup eval reload config init packages tests))
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
    (zenit-log 3 ":context: +%s %s" context zenit-context)
    (push context zenit-context)))

(defun zenit-context-pop (context &optional strict?)
  "Remove CONTEXT from `zenit-context'.

Return non-nil if successful. If STRICT? is non-nil, throw an
error if CONTEXT wasn't active when this was called."
  (if (not (zenit-context-p context))
      (when strict?
        (signal 'zenit-context-error
                (list zenit-context "Attempt to pop missing context" context)))
    (zenit-log 3 ":context: -%s %s" context zenit-context)
    (setq zenit-context (delq context zenit-context))))

(defmacro with-zenit-context (contexts &rest body)
  "Evaluate BODY with CONTEXTS added to `zenit-context'."
  (declare (indent 1))
  `(let ((zenit-context zenit-context))
     (dolist (context (ensure-list ,contexts))
       (zenit-context-push context))
     ,@body))


;;
;;; Module context

(defvar zenit--empty-module-context [nil nil nil nil nil nil nil]
  "Empty module context.
This vector is used as a placeholder and represents a context
where no module specifics have been defined.")

(eval-and-compile
  (put 'zenit-module-context 'keys '(:index 0 :initdepth 1 :configdepth 2
                                     :group 3 :name 4 :flags 5 :features 6)))
(defvar zenit-module-context zenit--empty-module-context
  "A vector describing the module associated it with the active context.
Contains the following: [INDEX INITDEPTH CONFIGDEPTH :GROUP
MODULE FLAGS FEATURES]

Do not directly set this variable, only let-bind it.")

(defmacro zenit-module--context-field (field)
  "Retrieve the index of FIELD from the `zenit-module-context'.

Note that FIELD is evaluated at macro-expansion time, and it
should be a valid symbol that matches a key within the
`zenit-module-context' property list."
  (plist-get (get 'zenit-module-context 'keys) field))

(defun zenit-module-context-get (field &optional context)
  "Return the FIELD of CONTEXT.

FIELD should be one of `index', `initdepth', `configdepth',
`group', `name', `flags', or `features'. CONTEXT should be a
`zenit-module-context' vector. If omitted, defaults to
`zenit-module-context'."
  (aref (or context zenit-module-context)
        (plist-get (get 'zenit-module-context 'keys)
                   field)))

(defun zenit-module-context (group &optional name)
  "Create a `zenit-module-context' from a module by GROUP and NAME.
If NAME is omitted, GROUP is treated as a module key cons
cell: (GROUP . NAME)."
  (declare (side-effect-free t))
  (let ((key (if name (cons group name) group)))
    (or (get (or (car-safe key) key)
             (cdr-safe key))
        zenit--empty-module-context)))

(defun zenit-module-context-key (&optional context)
  "Return the module of the active CONTEXT as a module key.

CONTEXT is a `zenit-module-context'."
  (declare (side-effect-free t))
  (let ((context (or context zenit-module-context)))
    (cons (aref context (zenit-module--context-field :group))
          (aref context (zenit-module--context-field :name)))))

(defmacro with-zenit-module-context (module-key &rest body)
  "Evaluate BODY with `zenit-module-context' informed by MODULE-KEY."
  (declare (indent 1))
  `(let ((zenit-module-context
          (let ((module-key ,module-key))
            (zenit-module-context module-key))))
     (zenit-log 2 ":context:module: =%s" zenit-module-context)
     ,@body))


;;
;;; Module API

(defun zenit-module-p (category module &optional flag)
  "Return t if CATEGORY MODULE and optionally FLAG is enabled.
A module is enabled if it is present in `zenit-modules'."
  (declare (pure t) (side-effect-free t))
  (when-let* ((plist (gethash (cons category module) zenit-modules)))
    (or (null flag)
        (and (memq flag (plist-get plist :flags))
             t))))

(defun zenit-module-depth (category module &optional initdepth?)
  "Return the depth of CATEGORY MODULE.

If INITDEPTH? is non-nil, use the CAR if a module was given two depths (see
`zenit-module-set')."
  (if-let* ((depth (zenit-module-get category module :depth)))
      (or (if initdepth?
              (car-safe depth)
            (cdr-safe depth))
          depth)
    0))

(defun zenit-module--has-flag-p (flags wanted-flags)
  "Return t if the list of WANTED-FLAGS satisfies the list of FLAGS."
  (declare (pure t) (side-effect-free error-free))
  (cl-loop with flags = (ensure-list flags)
           for flag in (ensure-list wanted-flags)
           for flagstr = (symbol-name flag)
           if (if (eq ?- (aref flagstr 0))
                  (memq (intern (concat "+" (substring flagstr 1)))
                        flags)
                (not (memq flag flags)))
           return nil
           finally return t))

(defun zenit-module-get (category module &optional property)
  "Return the plist for CATEGORY MODULE.
Gets PROPERTY,specifically, if set."
  (declare (pure t) (side-effect-free t))
  (when-let* ((plist (gethash (cons category module) zenit-modules)))
    (if property
        (plist-get plist property)
      plist)))

(defun zenit-module-put (category module &rest plist)
  "Set a PROPERTY for CATEGORY MODULE to VALUE.
PLIST should be additional pairs of PROPERTY and VALUEs.

\(fn CATEGORY MODULE PROPERTY VALUE &rest [PROPERTY VALUE [...]])"
  (puthash (cons category module)
           (if-let* ((old-plist (zenit-module-get category module)))
               (if (null plist)
                   old-plist
                 (when (cl-oddp (length plist))
                   (signal 'wrong-number-of-arguments (list (length plist))))
                 (while plist
                   (plist-put old-plist (pop plist) (pop plist)))
                 old-plist)
             plist)
           zenit-modules))


(defun zenit-module-set (category module &rest plist)
  "Enables a module by adding it to `zenit-modules'.

CATEGORY is a keyword, MODULE is a symbol, PLIST is a plist that
accepts the following properties:

  :path STRING
    Path to the directory where this module lives.
  :depth INT|(INITDEPTH . CONFIGDEPTH)
    Determines module load order. If a cons cell, INITDEPTH
    determines the load order of the module's init.el, while
    CONFIGDEPTH determines the same for all other config
    files (config.el, packages.el, doctor.el, etc).
  :flags (SYMBOL...)
    A list of activated flags for this module.

If PLIST consists of a single nil, the module is purged from
memory instead."
  (if (car plist)
      (let* ((depth (ensure-list (or (plist-get plist :depth) 0)))
             (idepth (or (cdr depth) (car depth)))
             (cdepth (car depth))
             (idx (hash-table-count zenit-modules)))
        ;; We cache the module index, flags, and features in symbol plists for
        ;; fast lookups in `modulep!' and elsewhere. plists are lighter and
        ;; faster than hash tables for datasets this size, and this information
        ;; is looked up *very* often.
        (put category module
             (vector idx idepth cdepth
                     category module
                     (plist-get plist :flags)
                     (plist-get plist :features)))
        ;; The hash table will always been the formal storage for modules.
        (puthash (cons category module) plist zenit-modules))
    (remhash (cons category module) zenit-modules)
    (cl-remf (symbol-plist category) module)))


(defun zenit-module-list (&optional paths-or-all initorder?)
  "Return a list of (:group . name) module keys in order of their :depth.

PATHS-OR-ALL can either be a non-nil value or a list of directories. If
given a list of directories, return a list of module keys for all
modules present underneath it. If non-nil, return the same, but search
`zenit-modules-load-path' (includes :core and :local-conf). Modules that
are enabled are sorted first by their :depth, followed by disabled
modules in lexicographical order.

If INITORDER? is non-nil, sort modules by their initdepth, rather than
their configdepth. See `zenit-module-set' for details."
  (sort (if paths-or-all
            (delete-dups
             (append (seq-remove #'cdr (zenit-module-list nil initorder?))
                     (zenit-files-in (if (listp paths-or-all)
                                         paths-or-all
                                       zenit-modules-load-path)
                                     :map #'zenit-module-from-path
                                     :type 'dirs
                                     :mindepth 1
                                     :depth 1)))
          (hash-table-keys zenit-modules))
        (let ((idx (if initorder? 1 2)))
          (lambda! ((groupa . namea) (groupb . nameb))
            (let ((a (get groupa namea))
                  (b (get groupb nameb)))
              (or (null b)
                  (and
                   a (let ((adepth (aref a idx))
                           (bdepth (aref b idx)))
                       (if (= adepth bdepth)
                           (< (aref a 0) (aref b 0))
                         (< adepth bdepth))))))))))

(defun zenit-module-expand-path (category module &optional file)
  "Expands a path to FILE relative to CATEGORY and MODULE.

CATEGORY is a keyword. MODULE is a symbol. FILE is an optional
string path. If the category isn't enabled this returns nil. For
finding disabled modules use `zenit-module-locate-path'."
  (when-let* ((path (zenit-module-get category module :path)))
    (if file
        (file-name-concat path file)
      path)))

(defun zenit-module-locate-path (category &optional module file)
  "Search `zenit-modules-load-path' to find the path to a module.

CATEGORY is a keyword (e.g. :lang) and MODULE is a symbol (e.g.
\\='python). FILE is a string that will be appended to the
resulting path. If no path exists, this returns nil, otherwise an
absolute path."
  (let (file-name-handler-alist)
    (if-let* ((path (zenit-module-expand-path category module file)))
        (if (or (null file)
                (file-exists-p path))
            path)
      (let* ((category (zenit-keyword-name category))
             (module (if module (symbol-name module)))
             (path (file-name-concat category module file)))
        (if file
            ;; `locate-file-internal' is a little faster for finding files, but
            ;; its interface for finding directories is clumsy.
            (locate-file-internal path zenit-modules-load-path '("" ".elc" ".el"))
          (cl-loop for default-directory in zenit-modules-load-path
                   if (file-exists-p path)
                   return (expand-file-name path)))))))

(defun zenit-module-locate-paths (module-list file)
  "Return all existing paths to FILE under each module in MODULE-LIST.

MODULE-LIST is a list of cons cells (GROUP . NAME). See
`zenit-module-list' for an example."
  (cl-loop for (group . name) in (or module-list (zenit-module-list))
           if (zenit-module-locate-path group name file)
           collect it))

(defun zenit-module-from-path (path &optional enabled-only)
  "Return a cons cell (CATEGORY . MODULE) derived from PATH (a file path).
If ENABLED-ONLY, return nil if the containing module isn't
enabled."
  (let* ((file-name-handler-alist nil)
         (path (expand-file-name path)))
    (save-match-data
      (cond ((string-match "/modules/\\([^/]+\\)/\\([^/]+\\)\\(?:/.*\\)?$" path)
             (when-let* ((category (zenit-keyword-intern (match-string 1 path)))
                         (module   (intern (match-string 2 path))))
               (and (or (null enabled-only)
                        (zenit-module-p category module))
                    (cons category module))))
            ((file-in-directory-p path zenit-core-dir)
             (cons :core nil))
            ((file-in-directory-p path zenit-local-conf-dir)
             (cons :local-conf nil))))))

(defun zenit-module-load-path (&optional module-dirs)
  "Return a list of file paths to activated modules.

The list is in no particular order and its file paths are
absolute. If MODULE-DIRS is non-nil, include all modules (even
disabled ones) available in those directories."
  (declare (pure t) (side-effect-free t))
  (cl-loop for (cat . mod) in (zenit-module-list module-dirs)
           collect (zenit-module-locate-path cat mod)))

(defvar zenit--module-dependencies nil
  "Stores module dependency messages.")
(defvar zenit--module-conflicts nil
  "Stores module conflict messages.")

(defun zenit-module-resolve (module &optional conflicts)
  "Processes a MODULE's `zenit-module-control-file'.

If CONFLICTS is nil, the function will resolve dependencies and
add them to `zenit-modules' if not already set. Otherwise it look
for conflicts and add them to `zenit--module-conflicts'."
  (cl-destructuring-bind (category . module) module
    (let* ((path (zenit-module-locate-path category module))
           (control-file (file-name-concat path zenit-module-control-file)))
      (when (file-exists-p control-file)
        (let ((control
               (with-temp-buffer
                 (insert-file-contents-literally control-file)
                 (read (current-buffer))))
              (key (if conflicts :conflicts :depends)))
          (dolist (entry (plist-get control key))
            (cl-destructuring-bind (flag dep-category dep-module &rest dep-flags) entry
              ;; flag is t (module itself) or a module flag
              (when (and (if (not (eq flag t))
                             (zenit-module-p category module flag)
                           (setq flag nil)
                           (zenit-module-p category module))
                         ;; Check if module is already set as desired. Also take
                         ;; into account that there can be multiple dependency
                         ;; flags.
                         (if dep-flags
                             (let (result)
                               (dolist (flag dep-flags result)
                                 (if (zenit-module-p dep-category dep-module flag)
                                     (push t result)
                                   (push nil result)))
                               ;; For :depends we want to proceed only if a
                               ;; module is not set up and the opposite in case
                               ;; of :conflicts
                               (if (eq key :depends)
                                   (not (cl-every (lambda (x) (eq x t)) result))
                                 (cl-every (lambda (x) (eq x t)) result)))
                           (if (eq key :depends)
                               (not (zenit-module-p dep-category dep-module))
                             (zenit-module-p dep-category dep-module))))
                (cond ((eq key :depends)
                       (if (zenit-module-get dep-category dep-module)
                           (let ((old-flags (zenit-module-get
                                             dep-category dep-module :flags)))
                             (push (format "Adding %s %s %s as dependency of %s %s %s"
                                           dep-category dep-module dep-flags category module flag)
                                   zenit--module-dependencies)
                             ;; Append new flags to existing module
                             (zenit-module-set
                              dep-category dep-module
                              :path (zenit-module-locate-path dep-category dep-module)
                              :flags (append old-flags dep-flags)))
                         ;; If module is not set yet, add it
                         (push (format "Adding %s %s %s as dependency of %s %s"
                                       dep-category dep-module dep-flags category module)
                               zenit--module-dependencies)
                         (zenit-module-set
                          dep-category dep-module
                          :path (zenit-module-locate-path dep-category dep-module)
                          :flags dep-flags)))
                      ((eq key :conflicts)
                       (push (format"%s %s %s conflicts with %s %s %s"
                                    category module flag dep-category dep-module dep-flags)
                             zenit--module-conflicts)))))))))))

(put :if     'lisp-indent-function 2)
(put :when   'lisp-indent-function 'defun)
(put :unless 'lisp-indent-function 'defun)

(defmacro modules! (&rest modules)
  "Bootstraps Emacs and its modules.

If the first item in MODULES doesn't satisfy `keywordp', MODULES
is evaluated, otherwise, MODULES is a multiple-property list (a
plist where each key can have multiple, linear values).

The bootstrap process involves making sure the essential
directories exist, core packages are installed,
`zenit-config-init-file' is loaded, `zenit-packages-file' cache
exists (and is loaded) and, finally, loads your private
init.el (which should contain your `modules!' block).

Module load order is determined by your `modules!' block. See
`zenit-modules-load-path' for a list of all recognized module trees.
Order defines precedence (from most to least)."
  `(when noninteractive
     (zenit-module-mplist-map
      (lambda (category module &rest plist)
        (let ((path (zenit-module-locate-path category module)))
          (unless path
            (print! (warn "Failed to locate a '%s %s' module") category module))
          (apply #'zenit-module-set category module
                 :path path
                 plist)))
      ,@(if (keywordp (car modules))
            (list (list 'quote modules))
          modules))
     ;; Add dependencies
     (zenit-log "modules: Resolving module dependencies")
     (dolist (module (zenit-module-list))
       (zenit-module-resolve module))
     (dolist (msg zenit--module-dependencies)
       (zenit-log "modules: %s" msg))
     (zenit-log "modules: Dependencies resolved")
     ;; Check for conflicts
     (dolist (module (zenit-module-list))
       (zenit-module-resolve module 'conflicts))
     (when zenit--module-conflicts
       (print! (error "Module conflicts detected"))
       (print-group!
         (dolist (msg zenit--module-conflicts)
           (print! (item "%s" msg))))
       (signal 'zenit-module-error (list "Module conflicts detected")))
     zenit-modules))


(defmacro modulep! (category &optional module &rest flags)
  "Return t if :CATEGORY MODULE (and +FLAGS) are enabled.

If FLAGS is provided, returns t if CATEGORY MODULE has all of
FLAGS enabled.

  (modulep! :config default +flag)
  (modulep! :config default +flag1 +flag2 +flag3)

CATEGORY and MODULE may be omitted when this macro is used from a
module's source. Like so:

  (modulep! +flag3 +flag1 +flag2)
  (modulep! +flag)

FLAGS can be negated. E.g. This will return non-nil if ':tools
lsp' is enabled without `+eglot':

  (modulep! :tools lsp -eglot)

To interpolate dynamic values, use comma:

  (let ((flag '-eglot))
    (modulep! :tools lsp ,flag))

For more about modules and flags, see `modules!'."
  ;; This macro bypasses the module API to spare startup their runtime cost, as
  ;; `modulep!' gets called *a lot* during startup.
  (if (keywordp category)
      (let ((ctxtform `(get (backquote ,category) (backquote ,module))))
        (if flags
            `(when-let* ((ctxt ,ctxtform))
               (zenit-module--has-flag-p
                (aref (or ctxt zenit--empty-module-context)
                      (zenit-module--context-field :flags))
                (backquote ,flags)))
          `(and ,ctxtform t)))
    (let ((flags (delq nil (cons category (cons module flags)))))
      (if (aref zenit-module-context 0)
          `(zenit-module--has-flag-p
            ',(aref zenit-module-context
                    (zenit-module--context-field :flags))
            (backquote ,flags))
        `(let ((file (file!)))
           (if-let* ((module (zenit-module-from-path file)))
               (zenit-module--has-flag-p
                (aref (or (get (car module) (cdr module))
                          zenit--empty-module-context)
                      (zenit-module--context-field :flags))
                (backquote ,flags))
             (error "(modulep! %s) couldn't resolve current module from %s"
                    (backquote ,flags) (abbreviate-file-name file))))))))

(provide 'zenit-lib)

;;; zenit-lib.el ends here.

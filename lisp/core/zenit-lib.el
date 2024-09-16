;; lisp/core/zenit-lib.el -*- lexical-binding: t; -*-

;; This file contains many helper functions and macros which are used throughout
;; the configuration.

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
(declare-function string-remove-prefix "subr-x" (prefix string))
(autoload #'string-remove-prefix "subr-x")
(declare-function string-remove-suffix "subr-x" (suffix string))
(autoload #'string-remove-suffix "subr-x")

;; `zenit-core'
(defvar zenit-core-dir)
(defvar zenit-local-conf-dir)
(defvar zenit-modules-dir)

;; `zenit-lib-compile'
(declare-function zenit-async-byte-compile-file "zenit-lib-compile" (file &rest kwargs))
(autoload #'zenit-async-byte-compile-file "zenit-lib-compile")
(declare-function zenit-compile-generate-args "zenit-lib-compile" (target))
(autoload #'zenit-compile-generate-args "zenit-lib-compile")

;; `zenit-lib-files'
(declare-function zenit-files-in "zenit-lib-files" (paths &rest rest))
(declare-function zenit-file-read "zenit-lib-files" (file &rest kwargs))

;; `zenit-modules'
(declare-function zenit-module-context-key "zenit-modules" (&optional context))


;;
;;; Custom error types

(define-error 'zenit-error "Error in Emacs")
(define-error 'zenit-nosync-error "Config could not be initialized; did you remember to run 'make refresh' in the shell?" 'zenit-error)
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
  (declare (indent 0))
  `(eval '(progn ,@body) lexical-binding))

(defmacro protect-macros-maybe! (feature &rest body)
  "Same as `protect-macros!', but only if FEATURE is unavailable.
Otherwise eval BODY normally (subject to eager macroexpansion).
In either case, eagerly load FEATURE during byte-compilation."
  (declare (indent 1))
  (let ((available (featurep feature)))
    (when byte-compile-current-file
      (setq available (require feature nil 'noerror)))
    (if available
        `(progn ,@body)
      `(protect-macros!
         (progn ,@body)))))

(defmacro eval-if! (cond then &rest else)
  "Like `if', but COND is evaluated at compile/expansion time.

The macro expands directly to either THEN or ELSE, and the other
branch is not compiled. This can be helpful to deal with code
that that can be omitted entirely if a certain feature is not
available."
  (declare (indent 2) (debug (sexp sexp &rest sexp)))
  (if (eval cond)
      then
    (macroexp-progn else)))

(defmacro eval-when! (cond &rest body)
  "Like `when', but COND is checked at compile/expansion time.

BODY is only compiled if COND evaluates to non-nil. See
`eval-if!'."
  (declare (indent 1) (debug (sexp &rest sexp)))
  (when (eval cond)
    (macroexp-progn body)))

(defmacro eval-unless! (cond &rest body)
  "Like `unless', but COND is checked at compile/expansion time.

BODY is only compiled if COND evaluates to non-nil. See
`eval-if!'."
  (declare (indent 1) (debug (sexp &rest sexp)))
  (unless (eval cond)
    (macroexp-progn body)))


;;
;;; Logging

(defvar zenit-inhibit-log (not (or noninteractive init-file-debug))
  "If non-nil, suppress `zenit-log' output completely.")

(defvar zenit-log-level
  (if init-file-debug
      (if-let ((level (getenv-internal "DEBUG"))
               (level (string-to-number level))
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
  "Log a MESSAGE in *Messages*.
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
      (when-let (env (read (current-buffer)))
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
          (when-let (newtz (getenv-internal "TZ"))
            (unless (equal tz newtz)
              (set-time-zone-rule newtz))))
        env))))

(defvar zenit--hook nil)
(defun zenit-run-hook (hook)
  "Run HOOK (a hook function) with better error handling.
Meant to be used with `run-hook-wrapped'."
  (zenit-log "hook:%s: run %s" (or zenit--hook '*) hook)
  (condition-case-unless-debug e
      (funcall hook)
    (error
     (signal 'zenit-hook-error (list hook e))))
  ;; return nil so `run-hook-wrapped' won't short circuit
  nil)

(defun zenit-run-hooks (&rest hooks)
  "Run HOOKS (a list of hook variable symbols) with better error
handling. Is used as advice to replace `run-hooks'."
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
  "Configure HOOK-VAR to be invoked exactly once when any of the
TRIGGER-HOOKS are invoked *after* Emacs has initialized (to
reduce false positives). Once HOOK-VAR is triggered, it is reset
to nil.

HOOK-VAR is a quoted hook.

TRIGGER-HOOK is a list of quoted hooks and/or sharp-quoted
functions."
  (dolist (hook trigger-hooks)
    (let ((fn (make-symbol (format "chain-%s-to-%s-h" hook-var hook)))
          running?)
      (fset
       fn (lambda (&rest _)
            ;; Only trigger this after Emacs has initialized.
            (when (and after-init-time
                       (not running?)
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
   (error "file!: cannot deduce the current file path")))

(defmacro dir! ()
  "Return the directory of the file this macro was called."
  (let (file-name-handler-alist)
    (file-name-directory (macroexpand '(file!)))))

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
  (declare (indent defun))
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
                        `(when-let (fn ,fn)
                           (advice-add ,target ,where fn)
                           (unwind-protect ,body (advice-remove ,target fn))))
                    (let* ((fn (pop rest))
                           (argspec (pop rest)))
                      (when (< (length argspec) 3)
                        (setq argspec
                              (list (nth 0 argspec)
                                    (nth 1 argspec)
                                    (or (nth 2 argspec) (gensym (format "%s-a" (symbol-name fn)))))))
                      (let ((name (nth 2 argspec)))
                        `(progn
                           (define-advice ,fn ,argspec ,@rest)
                           (unwind-protect ,body
                             (advice-remove #',fn #',name)
                             ,(if name `(fmakunbound ',name))))))))
              (`defun
                  `(cl-letf ((,(car rest) (symbol-function #',(car rest))))
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

(defmacro quiet! (&rest forms)
  "Run FORMS without generating any output.

This silences calls to `message', `load', `write-region' and
anything that writes to `standard-output'. In interactive
sessions this inhibits output to the echo-area, but not to
*Messages*."
  `(if init-file-debug
       (progn ,@forms)
     ,(if noninteractive
          `(letf! ((standard-output (lambda (&rest _)))
                   (defun message (&rest _))
                   (defun load (file &optional noerror _nomessage nosuffix must-suffix)
                     (funcall load file noerror t nosuffix must-suffix))
                   (defun write-region (start end filename &optional append visit lockname mustbenew)
                     (unless visit (setq visit 'no-message))
                     (funcall write-region start end filename append visit lockname mustbenew)))
             ,@forms)
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
  (declare (indent defun) (doc-string 1) (pure t) (side-effect-free t))
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
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (&rest _) (interactive) ,@body))

(defmacro cmd!! (command &optional new-prefix-arg &rest args)
  "Return a closure that interactively calls COMMAND with ARGS and
NEW-PREFIX-ARG. Like `cmd!', but allows you to change
`current-prefix-arg' or pass arguments to COMMAND. This macro is
meant to be used as a target for keybinds (e.g. with `define-key'
or `map!')."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (arg &rest _) (interactive "P")
     (let ((current-prefix-arg (or ,new-prefix-arg arg)))
       (,(if args
             #'funcall-interactively
           #'call-interactively)
        ,command ,@args))))


;;
;;; Mutation

(defun zenit-splice-into (seq element after &optional before)
  "Insert ELEMENT (a symbol, string or list) into the list SEQ,
between elements BEFORE and AFTER.

Comparison is done via `equal'. Returns the modified list.

ELEMENT will be inserted directly after AFTER or before BEFORE.
If both AFTER and BEFORE are given, it will be inserted directly
after AFTER."
  (let (;; Find the position of AFTER in SEQ, if it exists
        (after-position (and after (cl-position after seq :test #'equal)))
        ;; Find the position of BEFORE in SEQ, if it exists
        (before-position (and before (cl-position before seq :test #'equal))))
    ;; Combine the following parts to create the new sequence:
    (append
     ;; 1. If AFTER exists, include all elements up to and including AFTER
     (and after-position (cl-subseq seq 0 (1+ after-position)))
     ;; 2. If AFTER doesn't exist but BEFORE does, include all elements before
     ;; BEFORE
     (and (not after-position) (cl-subseq seq 0 before-position))
     ;; 3. Insert the new ELEMENT (ensuring it's a list)
     (ensure-list element)
     ;; 4. If both AFTER and BEFORE exist, include elements between them
     (and before-position after-position (cl-subseq seq (1+ after-position) before-position))
     ;; 5. If BEFORE exists, include all elements from BEFORE onwards
     (and before-position (cl-subseq seq before-position))
     ;; 6. If BEFORE doesn't exist but AFTER does, include all elements after
     ;; AFTER
     (and (not before-position) (cl-subseq seq (1+ after-position))))))

(defmacro spliceq! (seq element after &optional before)
  "Splice ELEMENT into SEQ in place.
See `zenit-splice-into' for details."
  `(setq ,seq (zenit-splice-into ,seq ,element ,after ,before)))

(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

(defmacro setq! (&rest settings)
  "A more sensible `setopt' for setting customizable variables.

This can be used as a drop-in replacement for `setq' and *should*
be used instead of `setopt'. Unlike `setq', this triggers custom
setters on variables. Unlike `setopt', this won't needlessly pull
in dependencies."
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set-default-toplevel-value)
                      ',var ,val))))

(defmacro setq-local! (&rest settings)
  "A more sensible `setopt' for setting the local value of
acustomizable variables.

This behaves the same way as `setq!' but uses `set' instead of
`set-default-toplevel-value' and will change the local value of a
buffer-local variable"
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set)
                      ',var ,val))))

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.
If FETCHER is a function, ELT is used as the key in LIST (an
alist)."
  `(setq ,list
         (delq ,(if fetcher
                    `(funcall ,fetcher ,elt ,list)
                  elt)
               ,list)))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))


;;
;;; Loading

(defmacro add-load-path! (&rest dirs)
  "Add DIRS to `load-path', relative to the current file.
The current file is the file from which `add-to-load-path!' is
used."
  `(let ((default-directory ,(protect-macros! (dir!)))
         file-name-handler-alist)
     (dolist (dir (list ,@dirs))
       (cl-pushnew (expand-file-name dir) load-path))))

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
  `(zenit-load (file-name-concat ,(or path `(dir!)) ,filename) ,noerror))

(defmacro zenit--with-local-load-history (file &rest body)
  "Evaluate BODY as part of FILE.

FILE is either a file path string or a form that should evaluate
to such a string.

This macro ensures that defined functions and variables show up
as being defined in FILE, instead of whatever file they are being
loaded from."
  (declare (indent 0))
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
  (declare (indent defun))
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
getting compiled."
  (declare (indent defun))
  (when (macroexp-compiling-p)
    (let* ((filename (expand-file-name filename (or path (protect-macros! (dir!)))))
           (files))
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
  (declare (indent defun))
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
  "Run BODY when CONDITION is true (checks on
`after-load-functions'). Meant to serve as a predicated
alternative to `after!'."
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
  "Load FEATURE (e.g. a package) after any of the hooks or functions
in HOOKS-OR-FUNCTIONS are executed.

See also `use-package!'."
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
           (when-let (deferral-list (assq ',feature zenit--deferred-packages-alist))
             (dolist (hook (cdr deferral-list))
               (advice-remove hook #',fn)
               (remove-hook hook #',fn))
             (delq! deferral-list zenit--deferred-packages-alist)
             (unintern ',fn nil))))
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
  "Pretend FEATURE hasn't been loaded yet, until FEATURE-hook or FN
runs.

Some packages (like `elisp-mode' and `lisp-mode') are loaded
immediately at startup, which will prematurely trigger
`after!' (and `with-eval-after-load') blocks. To get around this
we make Emacs believe FEATURE hasn't been loaded yet, then wait
until FEATURE-hook (or MODE-hook, if FN is provided) is triggered
to reverse this and trigger `after!' blocks at a more reasonable
time."
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
     1.5 t (fn! (when-let (fn (pop fns))
                  (zenit-log "compile-functions: %s" fn)
                  (or (if (featurep 'native-compile)
                          (or (subr-native-elisp-p (indirect-function fn))
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
invoked, then never again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted
function (which will be advised)."
  (declare (indent 1))
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
  2. Optional properties :local, :append, and/or :depth [N],
     which will make the hook buffer-local or append to the list
     of hooks (respectively),
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

If N and M = 1, there's no benefit to using this macro over
`remove-hook'.

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))

(defmacro setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (zenit--setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest _)
                       ,(format "%s = %s" var (pp-to-string val))
                       (setq-local ,var ,val))
            collect `(eval-when-compile (declare-function ,fn nil))
            collect `(add-hook ',hook #',fn -90))))

(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
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
           (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
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
           (indent defun))
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
;;; Backports

;; NOTE 2024-05-11: Currently no backports

(provide 'zenit-lib)

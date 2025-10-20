;;; lisp/core/zenit-cli.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; This module provides the CLI infrastructure for the `bin/emacs-config'
;; script. It includes a declarative option parsing system for CLI subcommands
;; with automatic help generation and flexible value syntax.
;;
;; The main entry point for subcommand implementations is
;; `zenit-cli-with-options', which provides:
;;
;;   - Declarative option specifications with type validation
;;   - Automatic variable binding based on parsed arguments
;;   - Support for boolean, string, file, number, list, and choice types
;;   - Multiple value syntax forms (space-separated, equals, comma-separated)
;;   - Automatic error handling for unknown options and invalid values
;;   - Help text generation via `zenit-cli-show-subcommand-help'
;;
;; For detailed usage and examples, see the documentation for
;; `zenit-cli-with-options'.

;;; Code:

(defvar zenit-cli-auto-accept (getenv-internal "YES")
  "If non-nil, auto-accept any confirmation prompts.")

(defvar zenit-cli-auto-discard (getenv-internal "FORCE")
  "If non-nil, discard all local changes while updating.")

(defvar zenit-cli-known-subcommands
  '("help" "clean" "deploy" "env" "eval" "sync" "refresh" "freeze" "test")
  "Available CLI subcommands.

In this variable, they are sorted by their execution order if multiple
subcommands are used.")


;;
;;; Subcommand parsing

(defvar zenit-cli--subcommand-args nil
  "Arguments that may belong to subcommands.

This is populated during the main argument parsing loop and consumed by
individual subcommands during the dispatch phase.")

;;;; Internal helper functions

(defun zenit-cli--option-match-p (arg option-names)
  "Return non-nil if ARG matches any of OPTION-NAMES.
Handles both --opt=value and --opt formats."
  (or (member arg option-names)
      (cl-some (lambda (opt)
                 (and (string-prefix-p (concat opt "=") arg)
                      opt))
               option-names)))

(defun zenit-cli--extract-inline-value (arg)
  "Extract value from ARG in --option=value format.
Returns (option . value) or nil if not in that format."
  (when (string-match "\\`\\(--?[^=]+\\)=\\(.*\\)\\'" arg)
    (cons (match-string 1 arg)
          (match-string 2 arg))))

(defun zenit-cli--parse-inline-value (type value-str &optional explicit-value)
  "Parse VALUE-STR according to TYPE for inline --opt=value syntax.
EXPLICIT-VALUE, if provided, is used for boolean types instead of t.
Returns the parsed value."
  (pcase type
    ('boolean (or explicit-value t))
    ('list
     ;; Support comma-separated values: --allow=PATH,HOME,USER
     (split-string value-str "," t))
    ('number
     (let ((num (string-to-number value-str)))
       (when (and (zerop num) (not (string-match-p "\\`0+\\'" value-str)))
         (error "Invalid number: %s" value-str))
       num))
    ('file
     (expand-file-name value-str))
    ((or 'string 'choice)
     value-str)))

(defun zenit-cli--parse-option-value (type args known-options known-subcommands choices &optional explicit-value)
  "Parse option value(s) from ARGS based on TYPE.

- KNOWN-OPTIONS is an alist of (option-names . spec).
- KNOWN-SUBCOMMANDS is a list of subcommand names.
- CHOICES is required for \\='choice type.
- EXPLICIT-VALUE, if provided, is used for boolean types instead of t.

Returns (values . remaining-args)."
  (pcase type
    ('boolean
     ;; Boolean flags don't consume arguments
     (cons (or explicit-value t) args))

    ('list
     ;; Greedy: consume until next option or subcommand
     (let (values)
       (while (and args
                   (let ((arg (car args)))
                     (and (not (string-prefix-p "-" arg))
                          (not (cl-some (lambda (opt-spec)
                                          (member arg (car opt-spec)))
                                        known-options))
                          (not (member arg known-subcommands)))))
         (push (pop args) values))
       (when (null values)
         (error "Option of type 'list requires at least one value"))
       (cons (nreverse values) args)))

    ('number
     (unless args
       (error "Option of type 'number requires a value"))
     (let* ((value-str (pop args))
            (num (string-to-number value-str)))
       (when (and (zerop num) (not (string-match-p "\\`0+\\'" value-str)))
         (error "Invalid number: %s" value-str))
       (cons num args)))

    ('file
     (unless args
       (error "Option of type 'file requires a value"))
     (cons (expand-file-name (pop args)) args))

    ('string
     (unless args
       (error "Option of type 'string requires a value"))
     (cons (pop args) args))

    ('choice
     (unless args
       (error "Option of type 'choice requires a value"))
     (let ((value (pop args)))
       (unless (member value choices)
         (error "Invalid choice '%s'. Valid choices are: %s"
                value (string-join choices ", ")))
       (cons value args)))))

;;
;;; Public API

(defun zenit-cli-parse-options (spec args &optional known-subcommands)
  "Parse ARGS according to SPEC.

- SPEC is a list of option specifications.
- ARGS is a list of string arguments to parse.
- KNOWN-SUBCOMMANDS is an optional list of subcommand names to stop
  parsing at.

Returns a plist with:
  :bindings - alist of (var . value) pairs
  :remaining - unparsed args (for next subcommand or errors)
  :unknown - list of unknown options encountered"
  (let ((known-options spec)
        (known-subcommands (or known-subcommands zenit-cli-known-subcommands))
        ;; Initialize bindings with default values from spec
        (bindings (mapcan (lambda (opt-spec)
                            (let* ((opt-plist (cdr opt-spec))
                                   (opt-var (plist-get opt-plist :var))
                                   (default-val (plist-get opt-plist :default)))
                              ;; Only add to bindings if :default is explicitly provided
                              (when (plist-member opt-plist :default)
                                (list (cons opt-var default-val)))))
                          spec))
        remaining
        unknown)

    (while args
      (let ((arg (pop args)))
        (cond
         ;; Check for --opt=value syntax
         ((and (string-prefix-p "-" arg)
               (string-match-p "=" arg))
          (let* ((parts (zenit-cli--extract-inline-value arg))
                 (opt-name (car parts))
                 (value-str (cdr parts))
                 (opt-spec (cl-find-if (lambda (spec)
                                         (member opt-name (car spec)))
                                       known-options)))
            (if opt-spec
                (let* ((opt-plist (cdr opt-spec))
                       (opt-type (plist-get opt-plist :type))
                       (opt-var (plist-get opt-plist :var))
                       (choices (plist-get opt-plist :choices))
                       (explicit-value (plist-get opt-plist :value))
                       (value (zenit-cli--parse-inline-value opt-type value-str explicit-value)))
                  ;; Validate choice
                  (when (and (eq opt-type 'choice) (not (member value choices)))
                    (error "Invalid choice '%s' for %s. Valid choices are: %s"
                           value opt-name (string-join choices ", ")))
                  ;; Handle list type accumulation
                  (if (eq opt-type 'list)
                      (let ((existing (alist-get opt-var bindings)))
                        (setf (alist-get opt-var bindings)
                              (append existing (ensure-list value))))
                    (setf (alist-get opt-var bindings) value)))
              ;; Unknown option
              (push arg unknown))))

         ;; Check for --opt value syntax
         ((string-prefix-p "-" arg)
          (let ((opt-spec (cl-find-if (lambda (spec)
                                        (member arg (car spec)))
                                      known-options)))
            (if opt-spec
                (let* ((opt-plist (cdr opt-spec))
                       (opt-type (plist-get opt-plist :type))
                       (opt-var (plist-get opt-plist :var))
                       (choices (plist-get opt-plist :choices))
                       (explicit-value (plist-get opt-plist :value))
                       (parse-result (zenit-cli--parse-option-value
                                      opt-type args known-options
                                      known-subcommands choices explicit-value))
                       (value (car parse-result)))
                  ;; Update remaining args
                  (setq args (cdr parse-result))
                  ;; Handle list type accumulation
                  (if (eq opt-type 'list)
                      (let ((existing (alist-get opt-var bindings)))
                        (setf (alist-get opt-var bindings)
                              (append existing (ensure-list value))))
                    (setf (alist-get opt-var bindings) value)))
              ;; Unknown option
              (push arg unknown))))

         ;; Hit a known subcommand - stop parsing
         ((member arg known-subcommands)
          (push arg remaining)
          (setq remaining (nreverse (append (nreverse args) remaining)))
          (setq args nil))

         ;; Non-option argument
         (t
          (push arg remaining)))))

    (list :bindings bindings
          :remaining (nreverse remaining)
          :unknown (nreverse unknown))))

(defmacro zenit-cli-with-options (spec args-var &rest body)
  "Parse CLI options according to SPEC from ARGS-VAR.
Variables are bound accordingly during the execution of BODY.

This is the main entry point for declarative option parsing in CLI
subcommands. It provides automatic argument parsing, type validation,
and variable binding.

SPEC is either:
  - A list of option specifications
  - A plist starting with :subcommand NAME followed by option specifications

Option specifications have the form:

  ((OPTION-NAMES...) :type TYPE :var VAR-NAME :help HELP-TEXT
   [:choices CHOICES] [:value VALUE] [:default DEFAULT])

Where:
  - OPTION-NAMES is a list of option strings (e.g., (\"-a\"
    \"--allow-all\"))
  - TYPE is one of: boolean, string, file, number, list, choice
  - VAR-NAME is the symbol to bind the parsed value to
  - HELP-TEXT is the documentation string for help generation
  - CHOICES (for type choice only) is a list of valid values
  - VALUE (optional) is the explicit value to set when flag is provided
  - DEFAULT (optional) is the value to use when flag is not provided

ARGS-VAR is a symbol containing the argument list to parse. It will be
updated in-place to contain any remaining unparsed arguments (typically
for the next subcommand).

BODY is executed with variables bound according to the :var properties
in SPEC.

AUTOMATIC HELP HANDLING:

The macro automatically adds a help option if not already present:
  ((\"-h\" \"--help\") :type boolean :var zenit-cli--show-help
   :help \"Show this help message\")

When help is requested (via -h or --help), the macro automatically:
  1. Displays help using `zenit-cli-show-subcommand-help'
  2. Exits without executing BODY

To provide a subcommand name for help display, use the :subcommand property:
  (zenit-cli-with-options (:subcommand \"env\" . ,zenit-cli-env-options)
      args
    ...body...)

Alternatively, the subcommand name can be inferred from the spec variable name
if it follows the pattern zenit-cli-NAME-options.

OPTION TYPES:

  :type boolean
    Flag presence (true/false). Doesn't consume additional arguments.
    Example: --verbose
    Result: t if present, nil if absent

    Optional :value property:
      Specify an explicit value to set instead of t.
      Useful for negation flags or multiple flags affecting the same variable.
      Example: ((\"-f\" \"--force\") :type boolean :var force-flag :value t)
               ((\"--no-force\") :type boolean :var force-flag :value nil)
      Last flag wins: --force --no-force results in force-flag = nil

  :type string
    Arbitrary string value. Consumes one argument.
    Example: --name \"foo\"
    Result: \"foo\"

  :type file
    File path with automatic expansion. Consumes one argument.
    Example: --output ~/file.txt
    Result: \"/home/user/file.txt\" (expanded)

  :type number
    Numeric value with validation. Consumes one argument.
    Example: --count 42
    Result: 42

  :type list
    Accumulates multiple values. Can be specified multiple ways:
    - Multiple flags: --allow PATH --allow HOME
    - Greedy consumption: --allow PATH HOME USER
    - Comma-separated: --allow=PATH,HOME,USER
    Result: (\"PATH\" \"HOME\" \"USER\")

  :type choice
    One value from a predefined set (requires :choices property).
    Example: --format json (with :choices (\"elisp\" \"shell\" \"json\"))
    Result: \"json\"

OPTIONAL PROPERTIES:

  :value VALUE
    Specify an explicit value to set when the option IS provided.
    Useful for boolean negation flags or setting specific values.

    Without :value (default boolean behavior):
      (\"--verbose\") :type boolean :var verbose)
      --verbose → verbose = t
      (no flag) → verbose = nil

    With :value:
      ((\"--color\") :type boolean :var color-mode :value 'always)
      --color   → color-mode = 'always
      (no flag) → color-mode = nil

    Common pattern - negation flags sharing a variable:
      ((\"-f\" \"--force\") :type boolean :var force :value t)
      ((\"--no-force\") :type boolean :var force :value nil)
      --force --no-force → force = nil (last wins)

  :default DEFAULT
    Specify a default value to use when the option is NOT provided.
    Works with all option types.

    Without :default:
      ((\"--output\") :type file :var output-file)
      --output foo → output-file = \"foo\"
      (no flag)    → output-file = nil

    With :default:
      ((\"--output\") :type file :var output-file :default \"~/.emacs.d/.local/env\")
      --output foo → output-file = \"foo\"
      (no flag)    → output-file = \"~/.emacs.d/.local/env\"

    Combining :value and :default:
      ((\"--color\") :type boolean :var color-mode :value 'always :default 'auto)
      --color   → color-mode = 'always
      (no flag) → color-mode = 'auto

VALUE SYNTAX:

The parser supports multiple value syntax forms:
  1. Space-separated:     --option value
  2. Equals syntax:       --option=value
  3. Comma-separated:     --option=value1,value2  (for lists)
  4. Multiple flags:      --option value1 --option value2  (for lists)
  5. Greedy consumption:  --option value1 value2 value3  (for lists)

For values starting with '-', use equals syntax:
  --flag=\"-DSOME_VALUE\"

PARSING BEHAVIOR:

The parser consumes arguments greedily until it encounters:
  - An unknown option (triggers an error)
  - Another known subcommand (stops parsing, adds to remaining args)
  - End of arguments

ERROR HANDLING:

The macro automatically validates:
  - Unknown options (error)
  - Invalid choice values (error)
  - Missing required values (error)
  - Type conversion failures (error)

EXAMPLES:

Example 1: Define an option spec
  (defvar zenit-cli-env-options
    \\='(((\"-a\" \"--allow-all\") :type boolean :var allow-all
        :help \"Allow all environment variables\")
       ((\"--allow\") :type list :var allow-list
        :help \"Allow specific environment variables\")
       ((\"-o\" \"--output\") :type file :var output-file
        :help \"Output file path\")
       ((\"--format\") :type choice :choices (\"elisp\" \"shell\" \"json\")
        :var output-format :help \"Output format\")))

Example 2: Use in a subcommand handler (with automatic help)
  ((string= option \"env\")
   (zenit-cli-with-options (:subcommand \"env\" . zenit-cli-env-options)
       zenit-cli--subcommand-args
     ;; Help is automatically handled - if --help was passed, the help
     ;; message is displayed and this body is not executed.
     ;; Variables like allow-list, output-file, force-flag are bound.
     (let ((force (or force-flag t))
           (env-file (or output-file default-env-file)))
       (zenit-cli-reload-env-file force env-file allow-list deny-list))))

Example 3: Boolean and string options
  Command: emacs-config env --allow-all --output /tmp/env.el
  Result:  allow-all = t, output-file = \"/tmp/env.el\"

Example 4: List options (various forms)
  Command: emacs-config env --allow PATH HOME USER
  Command: emacs-config env --allow PATH --allow HOME --allow USER
  Command: emacs-config env --allow=PATH,HOME,USER
  All result in: allow-list = (\"PATH\" \"HOME\" \"USER\")

Example 5: Choice options
  Command: emacs-config env --format json
  Result:  output-format = \"json\"

Example 6: Multiple subcommands with options
  Command: emacs-config env --allow PATH sync --force
  Result:  env gets allow-list = (\"PATH\")
           After env completes, zenit-cli--subcommand-args = (\"sync\"
           \"--force\")
           The next subcommand (sync) can then parse its own options

Example 7: Boolean options with :value property (negation flags)
  Spec: ((\"-f\" \"--force\") :type boolean :var force-flag :value t
          :help \"Force regeneration\")
        ((\"--no-force\") :type boolean :var force-flag :value nil
          :help \"Disable force regeneration\")

  Command: emacs-config env --force
  Result:  force-flag = t

  Command: emacs-config env --no-force
  Result:  force-flag = nil

  Command: emacs-config env --force --no-force
  Result:  force-flag = nil (last flag wins)

Example 8: Default values with :default property
  Spec: ((\"--output\") :type file :var output-file
          :default \"~/.emacs.d/.local/env\"
          :help \"Output file path\")
        ((\"--format\") :type choice :choices (\"elisp\" \"shell\" \"json\")
          :var format :default \"elisp\"
          :help \"Output format\")

  Command: emacs-config env --allow PATH
  Result:  output-file = \"~/.emacs.d/.local/env\" (default)
           format = \"elisp\" (default)

  Command: emacs-config env --output /tmp/foo --format json
  Result:  output-file = \"/tmp/foo\" (provided)
           format = \"json\" (provided)

Example 9: Combining :value and :default
  Spec: ((\"--color\") :type boolean :var color-mode
          :value 'always :default 'auto
          :help \"Enable color output\")

  Command: emacs-config sync --color
  Result:  color-mode = 'always

  Command: emacs-config sync
  Result:  color-mode = 'auto (default, not nil)

See also:
  `zenit-cli-parse-options' - Lower-level parsing function
  `zenit-cli-show-subcommand-help' - Generate help text from spec"
  (declare (indent 2))
  (let* ((spec-form (if (and (consp spec) (eq (car spec) :subcommand))
                        ;; Extract subcommand name and spec from plist form
                        (cons (cadr spec) (cddr spec))
                      ;; Try to infer from variable name or use spec as-is
                      (cons nil spec)))
         (subcommand-name (car spec-form))
         (spec-value (cdr spec-form))
         (evaluated-spec (eval spec-value))
         ;; Check if help option already exists
         (has-help-p (cl-some (lambda (opt-spec)
                                (or (member "-h" (car opt-spec))
                                    (member "--help" (car opt-spec))))
                              evaluated-spec))
         ;; Add help option if not present
         (augmented-spec (if has-help-p
                             evaluated-spec
                           (append evaluated-spec
                                   '((("-h" "--help") :type boolean
                                      :var zenit-cli--show-help
                                      :help "Show this help message")))))
         (parse-result-sym (gensym "parse-result-"))
         (bindings-sym (gensym "bindings-"))
         (unknown-sym (gensym "unknown-"))
         (spec-sym (gensym "spec-"))
         (subcommand-sym (gensym "subcommand-"))
         (var-names (mapcar (lambda (opt-spec)
                              (plist-get (cdr opt-spec) :var))
                            augmented-spec)))
    `(let* ((,spec-sym ',augmented-spec)
            (,subcommand-sym ,subcommand-name)
            (,parse-result-sym (zenit-cli-parse-options ,spec-sym ,args-var))
            (,bindings-sym (plist-get ,parse-result-sym :bindings))
            (,unknown-sym (plist-get ,parse-result-sym :unknown))
            ,@(mapcar (lambda (var)
                        `(,var (alist-get ',var ,bindings-sym nil nil #'eq)))
                      var-names))
       ;; Error on unknown options
       (when ,unknown-sym
         (print! (error "Unknown options for %s: %s")
                 ,subcommand-sym
                 (string-join ,unknown-sym ", "))
         (kill-emacs 1))
       ;; Update args-var with remaining arguments
       (setq ,args-var (plist-get ,parse-result-sym :remaining))
       ;; Handle help request
       (if ,(if has-help-p
                ;; Use existing help var name
                (plist-get (cdr (cl-find-if (lambda (opt-spec)
                                              (or (member "-h" (car opt-spec))
                                                  (member "--help" (car opt-spec))))
                                            evaluated-spec))
                           :var)
              ;; Use our added help var name
              'zenit-cli--show-help)
           (when ,subcommand-sym
             (zenit-cli-show-subcommand-help ,subcommand-sym ,spec-sym))
         ;; Execute body if not showing help
         (progn ,@body)))))

(defun zenit-cli-show-subcommand-help (subcommand spec)
  "Show help for SUBCOMMAND using option SPEC.
SUBCOMMAND is the name of the subcommand (string).
SPEC is the option specification list."
  (print! (bold "Usage: emacs-config %s [OPTIONS]\n") subcommand)
  (when spec
    (print! (bold "OPTIONS"))
    (dolist (opt-spec spec)
      (let* ((flags (car opt-spec))
             (plist (cdr opt-spec))
             (type (plist-get plist :type))
             (help (plist-get plist :help))
             (choices (plist-get plist :choices))
             (flag-str (string-join flags ", "))
             (type-hint (pcase type
                          ('boolean "")
                          ('list " VALUES...")
                          ('choice (format " {%s}" (string-join choices "|")))
                          (_ " VALUE"))))
        (print! "  %-25s \t%s" (bold (concat flag-str type-hint)) help)))))


;;
;;; Log settings

(defvar zenit-cli-log-file-format (expand-file-name "logs/cli.%s.%s.%s" zenit-state-dir)
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
  "Print a more presentable backtrace to terminal and write it to file.

TYPE is the error type. DATA is the error data."
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

(defun zenit-cli--rotate-log-file (logfile &optional num-backups)
  "Rotate LOGFILE, keeping NUM-BACKUPS (default 20).

This renames `file.log' to `file-1.log', `file-1.log' to `file-2.log',
and so on, deleting the oldest log file."
  (let ((backups (or num-backups 20)))
    ;; We only need to do something if the log file to be rotated exists.
    (when (file-exists-p logfile)
      (let* ((dir       (file-name-directory logfile))
             (base      (file-name-sans-extension (file-name-nondirectory logfile)))
             (ext       (concat "." (file-name-extension logfile)))
             (oldest    (expand-file-name (format "%s-%d%s" base backups ext) dir)))

        ;; 1. Delete the oldest backup file (e.g., "async-bytecomp-20.log").
        (when (file-exists-p oldest)
          (delete-file oldest))

        ;; 2. Rename all other backups, counting down from the oldest.
        ;;    (e.g., rename "-19.log" to "-20.log", then "-18.log" to "-19.log",
        ;;    etc.)
        (let ((i (1- backups)))
          (while (>= i 1)
            (let ((source (expand-file-name (format "%s-%d%s" base i ext) dir))
                  (target (expand-file-name (format "%s-%d%s" base (1+ i) ext) dir)))
              (when (file-exists-p source)
                (rename-file source target t)))
            (setq i (1- i))))

        ;; 3. Rename the current log file to the first backup slot.
        ;;    (e.g., rename "async-bytecomp.log" to "async-bytecomp-1.log")
        (rename-file logfile
                     (expand-file-name (format "%s-1%s" base ext) dir)
                     t)))))



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
              zenit-cache-dir
              zenit-state-dir))

  (zenit-cli--rotate-log-file async-byte-compile-log-file)

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

  (zenit-require 'zenit-lib 'debug)
  (if init-file-debug (zenit-debug-mode +1))

  ;; Suppress any possible coding system prompts during CLI sessions.
  (set-language-environment "UTF-8")

  ;; Eagerly load these libraries
  (require 'seq)
  (require 'map)
  (mapc (zenit-rpartial #'load nil (not init-file-debug) 'nosuffix)
        (append (file-expand-wildcards (concat zenit-core-dir "lib/*.el"))
                (file-expand-wildcards (concat zenit-core-dir "cli/*.el"))))

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

;;; zenit-cli.el ends here.

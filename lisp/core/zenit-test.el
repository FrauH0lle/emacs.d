;;; lisp/core/zenit-test.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Adapted from https://github.com/radian-software/straight.el

(defmacro zenit-test--template (template &optional vars &rest bindings)
  "Generate multiple test templates.

TEMPLATE is an implicitly backquoted form that serves as the base
structure for the generated tests. It can contain placeholders
that will be filled with values from the bindings.

VARS is an optional list of symbols that define the destructuring
pattern for BINDINGS. Each symbol in VARS will be bound to
corresponding values from BINDINGS.

BINDINGS is a list of values that will be used to fill the
template. The number of bindings must be evenly divisible by the
number of VARS. BINDINGS can include :doc keywords followed by
documentation strings for individual test cases.

The macro returns a list of filled templates, with each template
having its placeholders replaced by the corresponding values from
BINDINGS. Each template is paired with its documentation string.

Implementation Details:
- If no VARS or BINDINGS are provided, returns just the TEMPLATE
- Normalizes BINDINGS to ensure each test case has a docstring
- Validates that BINDINGS match VARS structure
- Processes BINDINGS into environment variables
- Generates templates with proper variable bindings

Example:
  (zenit-test--template
    (should (equal ,input ,expected))
  (input expected)
  1 1
  :doc \"First docstring\"
  2 4
  :doc \"Second docstring\"
  3 9)

This would generate:
  ((\"\"
  (should
   (equal 1 1)))
 (\"First docstring\"
  (should
   (equal 2 4)))
 (\"Second docstring\"
  (should
   (equal 3 9))))"
  (declare (indent 1) (debug t))
  ;; If no vars or bindings provided, return just the template
  (if (or (null vars) (null bindings))
      (list `("" ,template))
    ;; Ensure that each binding is preceeded by :doc "DOCSTRING" (or empty) and
    ;; assing them to docstrings and bindings
    (let ((normalized-bindings (zenit-test--normalize-cases vars bindings))
          docstrings bindings)
      (while normalized-bindings
        (let ((item (pop normalized-bindings)))
          (if (and (keywordp item) (eq item :doc))
              (push (pop normalized-bindings) docstrings)
            (push item bindings))))
      ;; Restore order
      (setq bindings (nreverse bindings)
            docstrings (nreverse docstrings))
      ;; Check if bindings are evenly divisible by number of vars
      (let ((unbound (mod (length bindings) (length vars))))
        ;; Error if bindings don't match vars
        (unless (zerop unbound)
          (error "Uneven binding list: %S" (last bindings unbound)))
        ;; Process the bindings
        (let ((body nil)
              (bindings
               (eval
                `(cl-loop for ,vars on ',bindings
                          by (lambda (l) (nthcdr ,(length vars) l))
                          collect
                          (apply #'append
                                 (cl-mapcar #'list ',vars (list ,@vars)))))))
          ;; Iterate through bindings and generate templates
          (let ((cases (dolist (env bindings (mapcar (lambda (it) (eval it t))
                                                     (nreverse body)))
                         ;; Check if environment has even number of elements
                         (let ((even (mod (length env) 2)))
                           (unless even (error "Uneven binding list: %S" env)))
                         ;; Build the let bindings
                         (let (e)
                           (cl-loop for (var val) on env by #'cddr
                                    do (push (list var `(quote ,val)) e))
                           ;; Generate the template with bindings
                           (push `(let* ,(nreverse e) (backquote ,template)) body)))))
            ;; Combine dostrings and test cases
            (cl-loop for case in cases
                     for doc in docstrings
                     collect `(,doc . ,(list case)))))))))

(defun zenit-test--normalize-cases (vars bindings)
  "Ensure each test case group has a docstring.
VARS is the list of variable names. BINDINGS is the list of test
inputs/outputs and optional docstrings."
  (let ((i 0)
        item
        result)
    (while bindings
      (setq item (pop bindings))
      ;; Test if the current item is at the start of a test case (number of vars +
      ;; :doc + docstring)
      (if (zerop (mod i (+ (length vars) 2)))
          ;; Test if the current item is the keyword :doc
          (if (and (keywordp item) (eq item :doc))
              ;; Add current and next item to result
              (let ((next (pop bindings)))
                (push item result)
                (push next result)
                ;; increment index +1
                (cl-incf i))
            ;; Else add :doc keyword
            (push :doc result)
            ;; Add empty docstring
            (push "" result)
            ;; Add current item
            (push item result)
            ;; Increment index by 2
            (cl-incf i 2))
        (push item result))
      (cl-incf i))
    (nreverse result)))

(cl-defmacro zenit-deftest (object
                            (&key before-each after-each expected-result
                                  doc tags vars vars* &allow-other-keys)
                            &rest template)
  "Define one or more ERT tests for OBJECT with TEMPLATE.

OBJECT is the symbol being tested. It can be a function, macro,
or other symbol.

Keyword arguments:
:before-each - Form(s) to run before each test case
:after-each  - Form(s) to run after each test case
:expected-result - Expected result type (:passed, :failed, etc)
:doc         - Documentation string for the test
:tags        - List of tags to apply to the test
:vars        - Variables to bind using `let`
:vars*       - Variables to bind using `let*`

The `let'/`let*' binding introduced via :vars and :vars* will
encompass the whole test body, including the code from
:before-each and :after-each.

TEMPLATE is a list of forms that will be expanded into test cases
using `zenit-test--template'. Each template form becomes a
separate test case.

The macro automatically adds tags based on the OBJECT:
- The object's name itself as a tag
- \\='private if the name contains \\='--', otherwise \\='public
- \\='macro if the object is a macro

Examples:
  (zenit-deftest my-function
  (:before-each (setup)
   :after-each (cleanup)
   :tags (integration)
   :doc \"Test my-function's behavior\")
  (should (equal (my-function ,input1) t))
  (input1)
  \\='foo)

  (zenit-deftest my-function
  (:before-each (setup)
   :after-each (cleanup)
   :tags (integration)
   :doc \"Test my-function's behavior\")
  (,assert (my-function ,input))
  (assert input)
  should \\='foo
  should-not \\='bar)"
  (declare (indent defun) (debug t))
  ;; Initialize test counter and automatic tags
  (let ((counter 0)
        (autotags
         (delq nil
               (list
                object
                (if (string-match-p "--" (symbol-name object))
                    'private 'public)
                (if (macrop object) 'macro))))
        ;; Generate tests from template
        (tests (when template
                 (macroexpand `(zenit-test--template ,@template)))))
    ;; Combine automatic and manual tags
    (setq tags (append autotags tags))
    ;; Generate the test forms
    `(progn
       ,@(mapcar
          (lambda (test)
            (let ((test-body
                   `(,@(when before-each
                         (if (cl-every #'listp before-each)
                             before-each
                           (list before-each)))
                     ,@(cdr test)
                     ,@(when after-each
                         (if (cl-every #'listp after-each)
                             after-each
                           (list after-each))))))
              `(ert-deftest
                   ,(intern (concat
                             (format "%s/test" object)
                             (when (> (length tests) 1)
                               (format "@%d" (cl-incf counter)))))
                   ()
                 ,(or (and (stringp (car test))
                           (not (string-empty-p (car test)))
                           (car test))
                      doc
                      (when (fboundp object) (documentation object)))
                 ,@(when tags `(:tags ',tags))
                 ,@(when expected-result `(:expected-result ,expected-result))
                 ,@(cond
                   (vars*  `((let* ,vars* ,@test-body)))
                   (vars   `((let ,vars ,@test-body)))
                   (t      test-body)))))
          tests))))

(defun zenit-test-enable-fontlocking ()
  "Enable fontlocking for `zenit-deftest'."
  (font-lock-add-keywords
   nil
   '(("(\\(\\<zenit-deftest\\)\\>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))
;; Activate the font-locking
(zenit-test-enable-fontlocking)

;;; Utilities

(defun zenit-test-make-temp-file (&optional dir-flag suffix text)
  "Create a temporary file.

Passes DIR-FLAG, SUFFIX and TEXT on to `make-temp-file', which
see."
  (make-temp-file "zenit-emacs-test-" dir-flag suffix text))

(defun zenit-test-same-items-p (expected actual &rest cl-keys)
  "Verify that EXPECTED and ACTUAL have the same items.
The order of items does not matter. Returns t if lists match, nil
otherwise.
CL-KEYS as in `cl-set-difference'.
\nKeywords supported:  :test :test-not :key
\n(fn EXPECTED ACTUAL [KEYWORD VALUE]...)"
  (and (null (apply #'cl-set-difference expected actual cl-keys))
       (null (apply #'cl-set-difference actual expected cl-keys))))

(defun zenit-test-contains-items-p (expected actual &rest cl-keys)
  "Verify that ACTUAL contains EXPECTED items.
The order of items does not matter. Returns t if lists match, nil
otherwise.
CL-KEYS as in `cl-set-difference'. \nKeywords
supported: :test :test-not :key \n(fn EXPECTED ACTUAL [KEYWORD
VALUE]...)"
  (null (apply #'cl-set-difference expected actual cl-keys)))

(provide 'zenit-test)

;;; zenit-test.el ends here.

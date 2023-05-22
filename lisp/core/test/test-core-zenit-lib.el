;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-core-zenit-lib.el

(describe "core/zenit-lib"

  (load! "zenit-lib" zenit-core-dir)


  (describe "zenit--log"
    (it "logs a message"
      (let* ((zenit-inhibit-log nil)
             (init-file-debug nil)
             (output (zenit--log "Test message: %s" "Hello")))
        (expect output
                :to-match
                "\\*.*::Test message: Hello"))))


  (describe "zenit-log"
    (it "logs a message"
      (spy-on 'zenit--log :and-call-through)
      (let ((zenit-inhibit-log nil)
            (init-file-debug nil))
        (zenit-log "Test message: %s" "Hello")
        (expect 'zenit--log
                :to-have-been-called-with
                "Test message: %s" "Hello")))

    (it "does not log a message if zenit-inhibit-log is t"
      (spy-on 'zenit--log :and-call-through)
      (let ((zenit-inhibit-log t)
            (init-file-debug nil))
        (zenit-log "Test message: %s" "Hello")
        (expect 'zenit--log :not :to-have-been-called))))


  (describe "zenit--resolve-hook-forms"
    (it "converts a list of modes into a list of hook symbols"
      (expect (zenit--resolve-hook-forms '(text-mode prog-mode))
              :to-equal '(text-mode-hook prog-mode-hook))
      (expect (zenit--resolve-hook-forms '(quote (text-mode-hook prog-mode-hook)))
              :to-equal '(text-mode-hook prog-mode-hook)))

    (it "leaves quoted mode as is"
      (expect (zenit--resolve-hook-forms '(text-mode 'prog-mode-hook))
              :to-equal '(text-mode-hook prog-mode-hook)))

    (it "returns an empty list when HOOKS is empty"
      (expect (zenit--resolve-hook-forms '())
              :to-equal '())))


  (describe "zenit--setq-hook-fns"
    (it "generates a list of hook setting functions with variable-value pairs"
      (let ((result (zenit--setq-hook-fns 'text-mode-hook
                                          '(var1 1 var2 2))))
        (expect (length result) :to-equal 2)
        (expect (car (nth 0 result)) :to-equal 'var1)
        (expect (cadr (nth 0 result)) :to-equal 1)
        (expect (car (nth 1 result)) :to-equal 'var2)
        (expect (cadr (nth 1 result)) :to-equal 2)))

    (it "generates a list of hook setting functions with single variables"
      (let ((result (zenit--setq-hook-fns '(text-mode-hook)
                                          '(var1 var2) t)))
        (expect (length result) :to-equal 2)
        (expect (car (nth 0 result)) :to-equal 'var1)
        (expect (cadr (nth 0 result)) :to-equal nil)
        (expect (car (nth 1 result)) :to-equal 'var2)
        (expect (cadr (nth 1 result)) :to-equal nil)))

    (it "throws an error if the length of REST is not even and SINGLES is nil"
      (expect (zenit--setq-hook-fns '(text-mode-hook) '(var1 1 var2))
              :to-throw 'wrong-number-of-arguments))

    (it "returns an empty list when HOOKS is empty"
      (expect (zenit--setq-hook-fns '() '((var1 . 1) (var2 . 2)))
              :to-equal '())))


  (describe "zenit-unquote"
    (it "unquotes an expression"
      (expect (zenit-unquote ''''foo) :to-be 'foo)))


  (describe "zenit-keyword-intern"
    (it "converts a string into a keyword"
      (expect (keywordp (zenit-keyword-intern "foo")) :to-be t)))


  (describe "zenit-keyword-name"
    (it "converts a keyword into a string"
      (expect (zenit-keyword-name :foo) :to-equal "foo")))


  (describe "zenit-rpartial"
    (it "returns a partially applied function with the right-hand arguments fixed"
      (let ((subtract-fn (zenit-rpartial #'- 5)))
        (expect (funcall subtract-fn 10) :to-equal 5)))

    (it "works with multiple arguments"
      (let ((subtract-fn (zenit-rpartial #'- 5 2)))
        (expect (funcall subtract-fn 10) :to-equal 3)))

    (it "returns a function that can be called with no arguments when all arguments are fixed"
      (let ((subtract-fn (zenit-rpartial #'- 10 5)))
        (expect (funcall subtract-fn) :to-equal 5))))


  (describe "zenit-lookup-key"
    (it "returns the command bound to KEYS in the specified keymaps"
      (let ((keymap (make-sparse-keymap)))
        (define-key keymap (kbd "C-x") 'test-command)
        (expect (zenit-lookup-key (kbd "C-x") keymap) :to-equal 'test-command)))

    (it "returns the command bound to KEYS in the active keymaps if no keymap is specified"
      (let ((keymap (make-sparse-keymap)))
        (define-key keymap (kbd "C-x") 'test-command)
        (define-key (current-global-map) (kbd "<F12>") 'test-command)
        (expect (zenit-lookup-key (kbd "<F12>")) :to-equal 'test-command)
        (define-key (current-global-map) (kbd "<F12>") nil)))

    (it "returns nil if the key sequence is not bound in the specified keymaps"
      (let ((keymap (make-sparse-keymap)))
        (expect (zenit-lookup-key (kbd "C-x") keymap) :to-be nil)))

    (it "returns nil if the key sequence is not bound in the active keymaps and no keymap is specified"
      (let ((keymap (make-sparse-keymap)))
        (expect (zenit-lookup-key (kbd "<F12>")) :to-be nil))))


  (describe "zenit-load"
    (it "loads an existing file without errors"
      (spy-on 'load)
      (with-temp-buffer
        (zenit-load (make-temp-file "test")))
      (expect 'load :to-have-been-called))

    ;; NOTE 2023-04-30: `debug-on-error' needs to be nil for
    ;;      `condition-case-unless-debug' to work
    (it "throws an error if the file does not exist and NOERROR is nil"
      (let ((debug-on-error nil))
        (expect (zenit-load "nonexistent-file" nil)
                :to-throw)))

    (it "does not throw an error if the file does not exist and NOERROR is non-nil"
      (let ((debug-on-error nil))
        (expect (zenit-load "nonexistent-file" t)
                :not :to-throw)))

    (it "handles zenit-error and other errors correctly"
      (spy-on 'load :and-throw-error 'error)
      (let ((debug-on-error nil))
        (expect (zenit-load "path" nil)
                :to-throw 'error))))


  (describe "zenit-embed"
    :var (temp-file-1 temp-file-2)

    (before-each
      (setq temp-file-1 (make-temp-file "test-1")
            temp-file-2 (make-temp-file "test-2"))

      (with-temp-file temp-file-1
        (insert (concat "(zenit-embed \"" temp-file-2 "\")")))
      (with-temp-file temp-file-2
        (insert (format "%s" '(eval-if! byte-compile-current-file
                                  (setq foo 42)
                                (setq foo 43))))))

    (it "embeds file contents if byte-compiling"
      (let ((comp-file (byte-compile-dest-file temp-file-1))
            (byte-compile-warnings '(not free-vars)))
        (byte-compile-file temp-file-1)
        (load comp-file nil t)
        (expect foo :to-be 42)))

    (it "load the file contents if not byte-compiling"
      (load temp-file-1 nil t)
      (expect foo :to-be 43)))


  (describe "zenit-load-envvars-file"
    (defun zenit-test-create-temp-env-file (content)
      (let ((temp-file (make-temp-file "zenit-test-env-")))
        (with-temp-file temp-file
          (insert content))
        temp-file))

    (it "loads environment variables from the file and returns changed variables"
      (let* ((temp-file (zenit-test-create-temp-env-file "(\"FOO=bar\"\n\"BAZ=qux\"\n)"))
             (result (zenit-load-envvars-file temp-file)))
        (expect (getenv "FOO") :to-equal "bar")
        (expect (getenv "BAZ") :to-equal "qux")
        (expect result :to-equal '("FOO=bar" "BAZ=qux"))))

    (it "handles non-existing file and doesn't throw an error if NOERROR is non-nil"
      (let ((non-existent-file "non_existent_env_file.el"))
        (expect (zenit-load-envvars-file non-existent-file t) :not :to-throw)))

    (it "throws an error when the file doesn't exist and NOERROR is nil"
      (let ((non-existent-file "non_existent_env_file.el"))
        (expect (zenit-load-envvars-file non-existent-file nil) :to-throw 'file-error))))


  (describe "zenit-run-hook"
    (it "runs the hook function without error"
      (let ((hook-fn (lambda () (setq test-var "success"))))
        (setq test-var nil)
        (zenit-run-hook hook-fn)
        (expect test-var :to-equal "success")))

    (it "handles errors in the hook function and signals 'zenit-hook-error'"
      (let ((error-hook-fn (lambda () (error "Test error")))
            (debug-on-error nil))
        (expect (zenit-run-hook error-hook-fn) :to-throw 'zenit-hook-error)))

    (it "returns nil even if the hook function runs successfully"
      (let ((hook-fn (lambda () (setq test-var "success"))))
        (setq test-var nil)
        (expect (zenit-run-hook hook-fn) :to-be nil))))


  (describe "zenit-run-hooks"
    (before-each
      (setq test-hook nil))

    (it "runs multiple hooks without error"
      (let ((hook-fn-1 (lambda () (push "hook-1" test-hook)))
            (hook-fn-2 (lambda () (push "hook-2" test-hook))))
        (add-hook 'test-hook-1 hook-fn-1)
        (add-hook 'test-hook-2 hook-fn-2)
        (zenit-run-hooks 'test-hook-1 'test-hook-2)
        (expect test-hook :to-have-same-items-as '("hook-1" "hook-2"))))

    (it "logs a warning when a hook function encounters an error"
      (let ((error-hook-fn (lambda () (error "Test error")))
            (debug-on-error nil))
        (add-hook 'test-error-hook error-hook-fn)
        (spy-on 'lwarn)
        (ignore-errors (zenit-run-hooks 'test-error-hook))
        (expect 'lwarn :to-have-been-called))))


  (describe "zenit-run-hook-on"
    (before-each
      (setq test-lazy-hook nil)
      (setq test-trigger-hook nil))

    (it "triggers the lazy hook after the trigger hook is invoked"
      (let ((hook-fn (lambda () (push "lazy-hook" test-lazy-hook))))
        (add-hook 'test-trigger-hook hook-fn)
        (zenit-run-hook-on 'test-lazy-hook '(test-trigger-hook))
        (run-hooks 'test-trigger-hook)
        (expect test-lazy-hook :to-equal '("lazy-hook"))))

    (it "does not trigger the lazy hook if the trigger hook is not invoked"
      (let ((hook-fn (lambda () (push "lazy-hook" test-lazy-hook))))
        (add-hook 'test-trigger-hook hook-fn)
        (zenit-run-hook-on 'test-lazy-hook '(test-trigger-hook))
        (expect test-lazy-hook :to-be nil)))

    (it "resets the lazy hook after it has been triggered"
      (let ((hook-fn (lambda () (push "lazy-hook" test-lazy-hook)))
            (trigger-hook-fn (lambda () (push "trigger-hook" test-trigger-hook))))
        (add-hook 'test-lazy-hook hook-fn)
        (add-hook 'test-trigger-hook trigger-hook-fn)
        (zenit-run-hook-on 'test-lazy-hook '(test-trigger-hook))
        (run-hooks 'test-trigger-hook)
        (expect (default-value 'test-lazy-hook) :to-be nil))))


  (describe "file! and dir! macros"
    (it "returns the correct file path and directory path"
      (let* ((temp-file (make-temp-file "temp-file" nil ".el"))
             (temp-dir (file-name-directory temp-file)))
        (with-temp-file temp-file
          (insert "(setq test-file-path (file!))\n")
          (insert "(setq test-dir-path (dir!))\n"))
        (load temp-file nil t)
        (expect test-file-path :to-equal temp-file)
        (expect test-dir-path :to-equal temp-dir)
        (delete-file temp-file)))

    (it "returns the correct file path and directory path in an eval buffer"
      (let* ((temp-file (make-temp-file "temp-file" nil ".el"))
             (temp-dir (file-name-directory temp-file)))
        (with-temp-buffer
          (insert-file-contents temp-file)
          (setq buffer-file-name temp-file)
          (setq test-file-path (eval '(file!)))
          (setq test-dir-path (eval '(dir!))))
        (expect test-file-path :to-equal temp-file)
        (expect test-dir-path :to-equal temp-dir)
        (delete-file temp-file))))


  (describe "letf! macro"
    (it "temporarily rebinds defun"
      (defun test-letf-func () (eval "original"))
      (expect (test-letf-func) :to-equal "original")
      (letf! (defun test-letf-func () (eval "rebound"))
        (expect (test-letf-func) :to-equal "rebound"))
      (expect (test-letf-func) :to-equal "original"))

    (it "temporarily rebinds defmacro"
      (defmacro test-letf-macro () "original")
      (expect (test-letf-macro) :to-equal "original")
      (letf! (defmacro test-letf-macro () "rebound")
        (expect (test-letf-macro) :to-equal "rebound"))
      (expect (test-letf-macro) :to-equal "original"))

    (it "temporarily rebinds defadvice"
      (defun test-letf-advice-func () (eval "first"))
      (defadvice! test-letf-advice-func-a (&rest _)
        :around #'test-letf-advice-func
        (eval "original"))
      (expect (test-letf-advice-func) :to-equal "original")
      (letf! (defadvice test-letf-advice-func-b (&rest _)
               :around #'test-letf-advice-func
               (eval "advised"))
        (expect (test-letf-advice-func) :to-equal "advised"))
      (expect (test-letf-advice-func) :to-equal "original")))


  (describe "quiet! macro"
    (it "silences message output"
      (spy-on 'message :and-call-through)
      (quiet! (message "This message should be silenced"))
      (expect 'message :not :to-have-been-called-with "This message should be silenced"))

    (it "silences load output"
      (spy-on 'load :and-call-through)
      (let ((test-file (make-temp-file "test-load")))
        (quiet! (load test-file 'noerror))
        (expect 'load :to-have-been-called-with test-file 'noerror t nil nil)))

    (it "silences write-region output"
      (spy-on 'write-region :and-call-through)
      (let ((test-file (make-temp-file "test-write-region")))
        (quiet! (write-region "This text should be written" nil test-file))
        (expect 'write-region :to-have-been-called-with
                "This text should be written" nil test-file nil 'no-message nil nil)))

    (it "does not silence output when init-file-debug is non-nil"
      (let ((init-file-debug t))
        (spy-on 'message :and-call-through)
        (quiet! (message "This message should not be silenced"))
        (expect 'message :to-have-been-called-with "This message should not be silenced"))))


  (describe "lambda! macro"
    (it "creates a lambda function with a simple argument list"
      (let ((simple-fn (lambda! (a b) (+ a b))))
        (expect (funcall simple-fn 1 2) :to-equal 3)))

    (it "creates a lambda function with &optional arguments"
      (let ((optional-fn (lambda! (a &optional b) (list a b))))
        (expect (funcall optional-fn 1) :to-equal '(1 nil))
        (expect (funcall optional-fn 1 2) :to-equal '(1 2))))

    (it "creates a lambda function with &rest arguments"
      (let ((rest-fn (lambda! (a &rest b) (append (list a) b))))
        (expect (funcall rest-fn 1 2 3 4) :to-equal '(1 2 3 4))))

    (it "creates a lambda function with &key arguments and adds &allow-other-keys implicitly"
      (let ((key-fn (lambda! (a &key b) (list a b))))
        (expect (funcall key-fn 1 :b 2) :to-equal '(1 2))
        (expect (funcall key-fn 1 :b 2 :c 3) :to-equal '(1 2)))))


  (describe "zenit--fn-crawl"
    (it "populates ARGS array with special symbols found in DATA"
      (let ((data '(a b %* c (% d) e (f %)))
            (args (make-vector 2 nil)))
        (zenit--fn-crawl data args)
        (expect (aref args 0) :to-be '%*)
        (expect (aref args 1) :to-be '%)))

    (it "handles nested structures"
      (let ((data '((a b) %* (c (% d)) (e (f %))))
            (args (make-vector 2 nil)))
        (zenit--fn-crawl data args)
        (expect (aref args 0) :to-be '%*)
        (expect (aref args 1) :to-be '%)))

    (it "handles vector structures"
      (let ((data ['a 'b '%* '(c '%) 'e '(f '%)])
            (args (make-vector 2 nil)))
        (zenit--fn-crawl data args)
        (expect (aref args 0) :to-be '%*)
        (expect (aref args 1) :to-be '%)))

    (it "raises an error when both %% and %%1 are found in DATA"
      (let ((data '(a b %* c (% d) e %1 (f %) g %%))
            (args (make-vector 2 nil)))
        (expect (zenit--fn-crawl data args) :to-throw))))


  (describe "fn!"
    (it "creates a lambda with implicit, positional arguments"
      (let ((f (fn! (+ %1 %2))))
        (expect (funcall f 3 4) :to-equal 7)))

    (it "handles missing arguments with placeholder symbols"
      (let ((f (fn! (if %1 %3))))
        (expect (funcall f 1 nil 2) :to-equal 2)))

    (it "supports shorthand '%' for the first positional argument"
      (let ((f (fn! (+ % %2))))
        (expect (funcall f 5 6) :to-equal 11)))

    (it "supports '&rest' arguments with '%*'"
      (let ((f (fn! (car (cdr %*)))))
        (expect (funcall f 1 2 3 4 5) :to-equal 2)))

    (it "handles nested structures in macro arguments"
      (let ((f (fn! (if %1 (list %2 %3) (list %3 %2)))))
        (expect (funcall f t 2 3) :to-equal '(2 3))
        (expect (funcall f nil 2 3) :to-equal '(3 2))))

    (it "works with higher-order functions"
      (let ((f (fn! (* %1 %2)))
            (g (fn! (+ %1 (funcall %2 3 4)))))
        (expect (funcall g 1 f) :to-equal 13))))


  (describe "cmd!"
    (it "creates a command with no arguments"
      (let ((command (cmd! (message "Hello, world!"))))
        (expect (commandp command) :to-be t)))

    (it "executes the command body when called interactively"
      (spy-on 'message)
      (let ((command (cmd! (message "Hello, world!"))))
        (call-interactively command))
      (expect 'message :to-have-been-called-with "Hello, world!"))

    (it "ignores extra arguments when called with apply"
      (spy-on 'message)
      (let ((command (cmd! (message "Hello, world!"))))
        (apply command '(1 2 3)))
      (expect 'message :to-have-been-called-with "Hello, world!")))


  (describe "cmd!!"
    (it "creates a command that calls another command interactively"
      (spy-on 'message)
      (let ((command (cmd!! #'message nil "Hello, world!")))
        (expect (commandp command) :to-be t)
        (call-interactively command))
      (expect 'message :to-have-been-called-with "Hello, world!"))

    (it "creates a command that calls another command with arguments"
      (spy-on 'message)
      (let ((command (cmd!! #'message nil "Hello, %s!" "world")))
        (call-interactively command))
      (expect 'message :to-have-been-called-with "Hello, %s!" "world"))

    (it "creates a command that overrides current-prefix-arg"
      (spy-on 'message)
      (let ((command (cmd!! #'message 4 "Hello, world!")))
        (with-temp-buffer
          (funcall-interactively command t)))
      (expect 'message :to-have-been-called-with "Hello, world!")))


  (describe "appendq!"
    (it "appends lists to a symbol"
      (let ((my-list '(1 2 3)))
        (appendq! my-list '(4 5) '(6 7 8))
        (expect my-list :to-equal '(1 2 3 4 5 6 7 8))))

    (it "appends an empty list without changes"
      (let ((my-list '(1 2 3)))
        (appendq! my-list)
        (expect my-list :to-equal '(1 2 3))))

    (it "appends lists with different types of elements"
      (let ((my-list '(1 "two" :three)))
        (appendq! my-list '(:four "five" 6) '(t nil))
        (expect my-list :to-equal '(1 "two" :three :four "five" 6 t nil))))

    (it "works with an empty initial list"
      (let ((my-list '()))
        (appendq! my-list '(1 2 3) '(4 5 6))
        (expect my-list :to-equal '(1 2 3 4 5 6)))))


  (describe "setq!"
    (it "sets a variable's value"
      (setq! my-test-var 42)
      (expect my-test-var :to-equal 42))

    (it "sets multiple variables at once"
      (setq! my-test-var-1 "hello" my-test-var-2 "world")
      (expect my-test-var-1 :to-equal "hello")
      (expect my-test-var-2 :to-equal "world"))

    (it "calls custom setter when available"
      (let ((custom-setter-called nil))
        (defcustom my-custom-var nil
          "Test custom variable"
          :type 'integer
          :set (lambda (sym val)
                 (setq custom-setter-called t)
                 (set-default sym val)))
        (setq! my-custom-var 5)
        (expect custom-setter-called :to-be-truthy))))


  (describe "delq!"
    (it "removes an element from a list in-place"
      (let ((my-list '(1 2 3 4)))
        (delq! 3 my-list)
        (expect my-list :to-equal '(1 2 4))))

    (it "removes an element from a list with duplicates"
      (let ((my-list '(1 2 3 2 4)))
        (delq! 2 my-list)
        (expect my-list :to-equal '(1 3 4))))

    (it "removes an element from an alist in-place using a custom fetcher"
      (let ((my-alist '((a . 1) (b . 2) (c . 3) (d . 4))))
        (delq! 'b my-alist #'assq)
        (expect my-alist :to-equal '((a . 1) (c . 3) (d . 4)))))

    (it "doesn't modify the list when the element is not found"
      (let ((my-list '(1 2 3 4)))
        (delq! 5 my-list)
        (expect my-list :to-equal '(1 2 3 4)))))


  (describe "pushnew!"
    (it "pushes unique values to a list in-place"
      (let ((my-list '(1 2 3)))
        (pushnew! my-list 4 5)
        (expect my-list :to-equal '(5 4 1 2 3))))

    (it "does not push duplicate values to a list"
      (let ((my-list '(1 2 3)))
        (pushnew! my-list 2 3)
        (expect my-list :to-equal '(1 2 3))))

    (it "handles a mix of unique and duplicate values"
      (let ((my-list '(1 2 3)))
        (pushnew! my-list 2 4 3 5)
        (expect my-list :to-equal '(5 4 1 2 3))))

    (it "works with an empty list"
      (let ((my-list '()))
        (pushnew! my-list 1 2 3)
        (expect my-list :to-equal '(3 2 1))))

    (it "works with an empty values list"
      (let ((my-list '(1 2 3)))
        (pushnew! my-list)
        (expect my-list :to-equal '(1 2 3)))))


  (describe "prependq!"
    (it "prepends values to a list in-place"
      (let ((my-list '(3 4 5)))
        (prependq! my-list '(1 2))
        (expect my-list :to-equal '(1 2 3 4 5))))

    (it "prepends multiple lists to a list in-place"
      (let ((my-list '(4 5 6)))
        (prependq! my-list '(1 2) '(2 3))
        (expect my-list :to-equal '(1 2 2 3 4 5 6))))

    (it "prepends values to an empty list"
      (let ((my-list '()))
        (prependq! my-list '(1 2 3))
        (expect my-list :to-equal '(1 2 3))))

    (it "prepends empty lists to a list"
      (let ((my-list '(1 2 3)))
        (prependq! my-list '() '())
        (expect my-list :to-equal '(1 2 3))))

    (it "prepends empty lists to an empty list"
      (let ((my-list '()))
        (prependq! my-list '() '())
        (expect my-list :to-equal '()))))


  (describe "add-load-path!"
    (it "adds a single directory to the load-path"
      (let ((temp-dir (make-temp-file "add-load-path-test" t)))
        (add-load-path! temp-dir)
        (expect (member temp-dir load-path) :not :to-be nil)))

    (it "adds multiple directories to the load-path"
      (let ((temp-dir-1 (make-temp-file "add-load-path-test-1" t))
            (temp-dir-2 (make-temp-file "add-load-path-test-2" t)))
        (add-load-path! temp-dir-1 temp-dir-2)
        (expect (member temp-dir-1 load-path) :not :to-be nil)
        (expect (member temp-dir-2 load-path) :not :to-be nil)))

    (it "does not add the same directory twice"
      (let ((temp-dir (make-temp-file "add-load-path-test" t)))
        (add-load-path! temp-dir)
        (add-load-path! temp-dir)
        (expect (length (cl-remove-if-not (lambda (path) (equal path temp-dir)) load-path))
                :to-be 1))))


  (describe "after!"
    (it "expands to an `eval-after-load' form for a single package"
      (expect '(after! test-package (test-fn))
              :to-expand-into
              '(progn (with-eval-after-load 'test-package (test-fn)))))

    (it "expands to multiple `eval-after-load' forms for an :or/:any package list"
      (expect '(after! (:or test-package-1 test-package-2) (test-fn))
              :to-expand-all-into
              '(progn
                 (progn (eval-after-load 'test-package-1 #'(lambda nil (test-fn))))
                 (progn (eval-after-load 'test-package-2 #'(lambda nil (test-fn)))))))

    (it "expands to a nested `eval-after-load' form for an :and/:all package list"
      (expect '(after! (:and test-package-1 test-package-2) (test-fn))
              :to-expand-all-into
              '(progn
                 (eval-after-load 'test-package-1
                   #'(lambda nil (progn (eval-after-load 'test-package-2 #'(lambda nil (test-fn)))))))))

    (it "expands to a nested `eval-after-load' form for a complex package list"
      (expect '(after! (:and test-package-1 (:or test-package-2 test-package-3)) (test-fn))
              :to-expand-all-into
              '(progn
                 (eval-after-load 'test-package-1
                   #'(lambda nil (progn
                                   (progn (eval-after-load 'test-package-2 #'(lambda nil (test-fn))))
                                   (progn (eval-after-load 'test-package-3 #'(lambda nil (test-fn))))))))))

    (it "expands to nil if package is in zenit-disabled-packages"
      (defvar zenit-disabled-packages ())
      (cl-pushnew 'test-package zenit-disabled-packages)
      (expect '(after! test-package (test-fn)) :to-expand-into nil)))


  (describe "defer-until!"
    (it "executes body immediately when condition is true"
      (let ((x 0))
        (defer-until! t (setq x 1))
        (expect x :to-equal 1)))

    (it "does not execute body immediately when condition is false"
      (let ((x 0))
        (defer-until! nil (setq x 1))
        (expect x :to-equal 0)))

    (it "executes body after condition becomes true"
      (let ((x 0))
        (defer-until! (eq x 1) (setq x 2))
        (setq x 1)
        (run-hooks 'after-load-functions)
        (expect x :to-equal 2))))


  (describe "after-call!"
    :var (zenit--deferred-packages-alist)

    (it "loads feature after function call"
      (spy-on 'require :and-return-value t)
      (spy-on 'my-function :and-return-value t)
      (after-call! my-feature my-function)
      (expect 'require :not :to-have-been-called)
      (my-function)
      (expect 'require :to-have-been-called-with 'my-feature))

    (it "loads feature after hook execution"
      (spy-on 'require :and-return-value t)
      (after-call! my-feature my-hook)
      (expect 'require :not :to-have-been-called)
      (run-hooks 'my-hook)
      (expect 'require :to-have-been-called-with 'my-feature))

    (it "removes itself from hooks and advices after execution"
      (spy-on 'require :and-return-value t)
      (spy-on 'my-function :and-return-value t)

      (after-call! my-feature my-function my-hook)
      (my-function)
      (run-hooks 'my-hook)
      (expect 'require :to-have-been-called-times 1)))


  (describe "defer-feature!"
    (it "removes feature from features list initially"
      (add-to-list 'features 'my-feature)
      (defer-feature! my-feature my-function)
      (expect (memq 'my-feature features) :not :to-be-truthy))

    (it "adds feature back to features list after function call"
      (spy-on 'provide :and-return-value t)
      (spy-on 'my-function :and-return-value t)
      (defer-feature! my-feature my-function)
      (my-function)
      (expect 'provide :to-have-been-called-with 'my-feature))

    (it "does not add feature back to features list if delay-mode-hooks is non-nil"
      (spy-on 'provide :and-return-value t)
      (spy-on 'my-function :and-return-value t)
      (defer-feature! my-feature my-function)
      (let ((delay-mode-hooks t))
        (my-function))
      (expect 'provide :not :to-have-been-called)))


  (describe "add-transient-hook!"
    (it "executes forms once when hook is run"
      (let ((x 0))
        (add-transient-hook! 'my-hook (setq x (+ x 1)))
        (run-hooks 'my-hook)
        (run-hooks 'my-hook)
        (expect x :to-equal 1)))

    (it "executes forms once when function is called"
      (spy-on 'my-function :and-return-value t)
      (let ((x 0))
        (add-transient-hook! #'my-function (setq x (+ x 1)))
        (my-function)
        (my-function)
        (expect x :to-equal 1)))

    (it "removes itself from hook after execution"
      (let ((x 0))

        (add-transient-hook! 'my-hook (setq x (+ x 1)))
        (run-hooks 'my-hook)
        (expect (memq 'my-hook (symbol-value 'my-hook)) :not :to-be-truthy)))

    (it "removes itself from function advice after execution"
      (spy-on 'my-function :and-return-value t)
      (let ((x 0))

        (add-transient-hook! #'my-function (setq x (+ x 1)))
        (my-function)
        (expect (advice-member-p 'my-function #'my-function) :not :to-be-truthy))))


  (describe "add-hook!"
    (it "adds function to hook"
      (spy-on 'add-hook)
      (add-hook! 'my-hook #'my-function)
      (expect 'add-hook :to-have-been-called-with 'my-hook #'my-function nil nil))

    (it "adds multiple functions to hook"
      (spy-on 'add-hook)
      (add-hook! 'my-hook #'my-function1 #'my-function2)
      (expect 'add-hook :to-have-been-called-with 'my-hook #'my-function1 nil nil)
      (expect 'add-hook :to-have-been-called-with 'my-hook #'my-function2 nil nil))

    (it "adds function to multiple hooks"
      (spy-on 'add-hook)
      (add-hook! '(my-hook1 my-hook2) #'my-function)
      (expect 'add-hook :to-have-been-called-with 'my-hook1 #'my-function nil nil)
      (expect 'add-hook :to-have-been-called-with 'my-hook2 #'my-function nil nil))

    (it "adds function to hook with :append"
      (spy-on 'add-hook)
      (add-hook! 'my-hook :append #'my-function)
      (expect 'add-hook :to-have-been-called-with 'my-hook #'my-function t nil))

    (it "adds function to hook with :local"
      (spy-on 'add-hook)
      (add-hook! 'my-hook :local #'my-function)
      (expect 'add-hook :to-have-been-called-with 'my-hook #'my-function nil t))

    (it "removes function from hook with :remove"
      (spy-on 'remove-hook)
      (add-hook! 'my-hook :remove #'my-function)
      (expect 'remove-hook :to-have-been-called-with 'my-hook #'my-function nil)))


  (describe "remove-hook!"
    (it "removes function from hook"
      (spy-on 'remove-hook)
      (remove-hook! 'my-hook #'my-function)
      (expect 'remove-hook :to-have-been-called-with 'my-hook #'my-function nil))

    (it "removes multiple functions from hook"
      (spy-on 'remove-hook)
      (remove-hook! 'my-hook #'my-function1 #'my-function2)
      (expect 'remove-hook :to-have-been-called-with 'my-hook #'my-function1 nil)
      (expect 'remove-hook :to-have-been-called-with 'my-hook #'my-function2 nil))

    (it "removes function from multiple hooks"
      (spy-on 'remove-hook)
      (remove-hook! '(my-hook1 my-hook2) #'my-function)
      (expect 'remove-hook :to-have-been-called-with 'my-hook1 #'my-function nil)
      (expect 'remove-hook :to-have-been-called-with 'my-hook2 #'my-function nil))

    (it "removes function from hook with :local"
      (spy-on 'remove-hook)
      (remove-hook! 'my-hook :local #'my-function)
      (expect 'remove-hook :to-have-been-called-with 'my-hook #'my-function t)))


  (describe "setq-hook!"
    (it "sets buffer-local variable on hook"
      (setq-hook! 'my-hook my-variable 1)
      (with-temp-buffer
        (run-hooks 'my-hook)
        (expect (buffer-local-value 'my-variable (current-buffer)) :to-equal 1)))

    (it "sets multiple buffer-local variables on hook"
      (setq-hook! 'my-hook my-variable1 1 my-variable2 2)
      (with-temp-buffer
        (run-hooks 'my-hook)
        (expect (buffer-local-value 'my-variable1 (current-buffer)) :to-equal 1)
        (expect (buffer-local-value 'my-variable2 (current-buffer)) :to-equal 2)))

    (it "sets buffer-local variable on multiple hooks"
      (setq-hook! '(my-hook1 my-hook2) my-variable 1)
      (with-temp-buffer
        (run-hooks 'my-hook1)
        (expect (buffer-local-value 'my-variable (current-buffer)) :to-equal 1)
        (kill-local-variable 'my-variable)
        (run-hooks 'my-hook2)
        (expect (buffer-local-value 'my-variable (current-buffer)) :to-equal 1))))


  (describe "unsetq-hook!"
    (it "removes setq hook for variable"
      (setq-hook! 'my-hook my-variable 1)
      (unsetq-hook! 'my-hook my-variable)
      (with-temp-buffer
        (run-hooks 'my-hook)
        (expect (local-variable-p 'my-variable) :not :to-be-truthy)))

    (it "removes setq hooks for multiple variables"
      (setq-hook! 'my-hook my-variable1 1 my-variable2 2)
      (unsetq-hook! 'my-hook my-variable1 my-variable2)
      (with-temp-buffer
        (run-hooks 'my-hook)
        (expect (local-variable-p 'my-variable1) :not :to-be-truthy)
        (expect (local-variable-p 'my-variable2) :not :to-be-truthy)))

    (it "removes setq hook for variable from multiple hooks"
      (setq-hook! '(my-hook1 my-hook2) my-variable 1)
      (unsetq-hook! '(my-hook1 my-hook2) my-variable)
      (with-temp-buffer
        (run-hooks 'my-hook1)
        (expect (local-variable-p 'my-variable) :not :to-be-truthy)
        (run-hooks 'my-hook2)
        (expect (local-variable-p 'my-variable) :not :to-be-truthy))))


  (describe "defadvice!"
    (it "expands into an `advice-add' form"
      (expect '(defadvice! test-advice ()
                 "Teststring"
                 :before #'test-function
                 (message "Hello World!"))
              :to-expand-into
              '(progn
                 (defun test-advice nil "Teststring"
                        (message "Hello World!"))
                 (eval-when-compile
                   (declare-function test-advice nil))
                 (dolist
                     (targets
                      (list (cons :before (ensure-list (function test-function)))))
                   (dolist (target (cdr targets))
                     (advice-add target (car targets)
                                 (function test-advice))))))))


  (describe "undefadvice!"
    (it "expands into an `advice-remove' form"
      (expect '(undefadvice! test-advice ()
                 "Teststring"
                 :before #'test-function
                 (message "Hello World!"))
              :to-expand-into
              '(dolist (targets (list (cons :before (ensure-list (function test-function)))))
                 (dolist (target (cdr targets))
                   (advice-remove target (function test-advice)))))))


  (describe "defhook!"
    (it "expands into an `add-hook!' form"
      (expect '(defhook! test-hook ()
                 "Teststring"
                 'test-hook
                 (message "Hello World!"))
              :to-expand-into
              '(progn
                 (defun test-hook nil "Teststring"
                        (message "Hello World!"))
                 (add-hook! 'test-hook 'test-hook)))))


  (describe "protect-macros!"
    (it "expands correctly"
      (expect '(protect-macros!
                 (message "Hello World!"))
              :to-expand-into
              '(eval
                '(progn
                   (message "Hello World!"))
                lexical-binding))))


  (describe "protect-macros-maybe!"
    (it "expands correctly if feature is not available"
      (expect '(protect-macros-maybe! my-feature
                 (message "Hello World!"))
              :to-expand-into
              '(protect-macros!
                 (progn
                   (message "Hello World!")))))

    (it "expands correctly if feature is available"
      (spy-on 'featurep :and-return-value t)
      (expect '(protect-macros-maybe! my-feature
                 (message "Hello World!"))
              :to-expand-into
              '(progn
                 (message "Hello World!")))))


  (describe "eval-if!"
    (it "evaluates then branch at compile time if condition is true"
      (let ((x (eval-if! t 1 2)))
        (expect x :to-equal 1)))

    (it "evaluates else branch at compile time if condition is false"
      (let ((x (eval-if! nil 1 2)))
        (expect x :to-equal 2)))

    (it "does not compile unused branch"
      (spy-on 'my-unused-function :and-throw-error 'error)
      (let ((x (eval-if! t 1 (my-unused-function))))
        (expect 'my-unused-function :not :to-have-been-called)
        (expect x :to-equal 1))))


  (describe "eval-when!"
    (it "compiles body at compile time if condition is true"
      (let ((x 0))
        (eval-when! t (setq x 1))
        (expect x :to-equal 1)))

    (it "does not compile body at compile time if condition is false"
      (let ((x 0))
        (eval-when! nil (setq x 1))
        (expect x :to-equal 0)))

    (it "does not compile unused body"
      (spy-on 'my-unused-function :and-throw-error 'error)
      (eval-when! nil (my-unused-function))
      (expect 'my-unused-function :not :to-have-been-called)))


  (describe "eval-unless!"
    (it "compiles body at compile time if condition is false"
      (let ((x 0))
        (eval-unless! nil (setq x 1))
        (expect x :to-equal 1)))

    (it "does not compile body at compile time if condition is true"
      (let ((x 0))
        (eval-unless! t (setq x 1))
        (expect x :to-equal 0)))

    (it "does not compile unused body"
      (spy-on 'my-unused-function :and-throw-error 'error)
      (eval-unless! t (my-unused-function))
      (expect 'my-unused-function :not :to-have-been-called)))


  (describe "zenit-compile-functions"
    (xit "is difficult to test because it relies on idle timers")))

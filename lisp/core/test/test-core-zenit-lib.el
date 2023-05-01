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
      (expect (test-letf-advice-func) :to-equal "original"))))

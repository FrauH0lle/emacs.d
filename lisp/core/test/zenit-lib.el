;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/zenit-lib.el

(require 'zenit-test)
(require 'zenit-lib)
(zenit-require 'zenit-lib 'files)
(zenit-require 'zenit-lib 'modules)

(zenit-deftest zenit-error
  (:doc "`zenit-error' is defined")
  (should (get 'zenit-error 'error-conditions)))

(zenit-deftest zenit-nosync-error
  (:doc "`zenit-nosync-error' is defined")
  (should (get 'zenit-nosync-error 'error-conditions)))

(zenit-deftest zenit-core-error
  (:doc "`zenit-core-error' is defined")
  (should (get 'zenit-core-error 'error-conditions)))

(zenit-deftest zenit-context-error
  (:doc "`zenit-context-error' is defined")
  (should (get 'zenit-context-error 'error-conditions)))

(zenit-deftest zenit-hook-error
  (:doc "`zenit-hook-error' is defined")
  (should (get 'zenit-hook-error 'error-conditions)))

(zenit-deftest zenit-autoload-error
  (:doc "`zenit-autoload-error' is defined")
  (should (get 'zenit-autoload-error 'error-conditions)))

(zenit-deftest zenit-module-error
  (:doc "`zenit-module-error' is defined")
  (should (get 'zenit-module-error 'error-conditions)))

(zenit-deftest zenit-local-conf-error
  (:doc "`zenit-local-conf-error' is defined")
  (should (get 'zenit-local-conf-error 'error-conditions)))

(zenit-deftest zenit-package-error
  (:doc "`zenit-package-error' is defined")
  (should (get 'zenit-package-error 'error-conditions)))

(zenit-deftest protect-macros!
  (:doc "`protect-macros!' expands correctly")
  (should (equal '(eval
                   '(progn
                      (message "Hello World!"))
                   lexical-binding)
                 (macroexpand '(protect-macros!
                                 (message "Hello World!"))))))

(zenit-deftest protect-macros-maybe! ()
  ,test
  (test)
  :doc "`protect-macros-maybe!' expands correctly if feature is not available"
  (should (equal '(protect-macros!
                    (progn
                      (message "Hello World!")))
                 (macroexpand-1 '(protect-macros-maybe! my-feature
                                   (message "Hello World!")))))
  :doc "`protect-macros-maybe!' expands correctly if feature is available"
  (should (equal '(progn
                    (message "Hello World!"))
                 (macroexpand-1 '(protect-macros-maybe! emacs
                                   (message "Hello World!"))))))

(zenit-deftest zenit-inhibit-log
  (:doc "`zenit-inhibit-log ' is defined")
  (should (boundp 'zenit-inhibit-log )))

(zenit-deftest zenit-log-level
  (:doc "`zenit-log-level' is defined")
  (should (boundp 'zenit-log-level)))

(zenit-deftest zenit--log
  (:before-each (setq messages nil)
   :vars ((zenit-log-level 3)
          (zenit-inhibit-log nil)
          (noninteractive nil)
          (before-init-time (current-time))
          messages))

  (letf! ((defun message (format &rest args)
            (unless inhibit-message
              (push (apply #'format format args) messages))))
    ,call
    ,test)
  (call test)

  :doc "`zenit--log' logs messages with proper formatting"
  (zenit--log 2 "Test message")
  (should (string-match-p "\\* [0-9]+\\.[0-9]+::Test message" (car messages)))

  :doc "`zenit--log' logs messages with arguments"
  (zenit--log 2 "Formatted %s %d" "test" 42)
  (should (string-match-p "Formatted test 42" (car messages)))

  :doc "`zenit--log' logs messages with level filtering"
  (let ((zenit-log-level 1))  ; Only show warnings
    (zenit--log 2 "Should not appear"))
  (should (null messages)))

(zenit-deftest zenit-log
  (:before-each (setq messages nil)
   :vars ((zenit-log-level 3)
          (zenit-inhibit-log nil)
          (noninteractive nil)
          (before-init-time (current-time))
          messages))

  (letf! ((defun message (format &rest args)
            (unless inhibit-message
              (push (apply #'format format args) messages))))
    ,call
    ,test)
  (call test)

  :doc "`zenit-log' respects `zenit-inhibit-log'"
  nil
  (let ((zenit-inhibit-log t))
    (zenit-log "Test message")
    (should (null messages)))

  :doc "`zenit-log' respects log levels"
  nil
  (let ((zenit-log-level 1))
    (zenit-log 2 "Should not appear")
    (should (null messages))
    (zenit-log 1 "Should appear")
    (should (string-match-p "Should appear" (car messages))))

  :doc "`zenit-log' formats messages with arguments"
  (zenit-log "Test %s %d" "message" 42)
  (should (string-match-p "Test message 42" (car messages)))

  :doc "`zenit-log' includes timestamps and context"
  (zenit-log "Test message") 
  (should (string-match-p "\\* [0-9]+\\.[0-9]+::Test message" (car messages))))

(zenit-deftest zenit--resolve-hook-forms ()
  (should (equal ',exp (zenit--resolve-hook-forms ',in)))
  (in exp)
  :doc "`zenit--resolve-hook-forms' converts a list of modes into a list of hook symbols"
  (text-mode prog-mode) (text-mode-hook prog-mode-hook)
  :doc "`zenit--resolve-hook-forms' leaves quoted mode as is"
  (text-mode 'prog-mode-hook) (text-mode-hook prog-mode-hook)
  :doc "`zenit--resolve-hook-forms' returns an empty list when HOOKS is empty"
  () ())

(zenit-deftest zenit--setq-hook-fns ()
  (should (equal ',exp (zenit--setq-hook-fns ',hooks ',rest ',singles)))
  (hooks rest singles exp)

  :doc "`zenit--setq-hook-fns' generates hook function for single variable"
  text-mode (foo 42) nil
  ((foo 42 text-mode-hook zenit--setq-foo-for-text-mode-h))

  :doc "`zenit--setq-hook-fns' generates hook functions for multiple variables"
  text-mode (foo 42 bar "test") nil
  ((foo 42 text-mode-hook zenit--setq-foo-for-text-mode-h)
   (bar "test" text-mode-hook zenit--setq-bar-for-text-mode-h))

  :doc "`zenit--setq-hook-fns' handles multiple hooks"
  (text-mode prog-mode) (foo 42) nil
  ((foo 42 text-mode-hook zenit--setq-foo-for-text-mode-h)
   (foo 42 prog-mode-hook zenit--setq-foo-for-prog-mode-h))

  :doc "`zenit--setq-hook-fns' handles singles mode (setting variables to nil)"
  text-mode (foo bar) t
  ((foo nil text-mode-hook zenit--setq-foo-for-text-mode-h)
   (bar nil text-mode-hook zenit--setq-bar-for-text-mode-h)))

(zenit-deftest zenit-unquote (:doc "`zenit-unquote' unquotes an expression")
  (should (equal 'test (zenit-unquote ''''''test))))

(zenit-deftest zenit-keyword-intern (:doc "`zenit-keyword-intern' converts string to keyword")
  (progn
    (should (equal :test (zenit-keyword-intern "test")))
    (should-error (zenit-keyword-intern nil))))

(zenit-deftest zenit-keyword-name (:doc "`zenit-keyword-name' converts keyword to string")
  (progn
    (should (equal "test" (zenit-keyword-name :test)))
    (should-error (zenit-keyword-name nil))))

(zenit-deftest zenit-rpartial ()
  ,test
  (test)
  :doc "`zenit-rpartial' creates function with fixed right arguments"
  (let* ((subtract (lambda (a b) (- a b)))
         (subtract-5 (zenit-rpartial subtract 5)))
    (should (= 5 (funcall subtract-5 10))))

  :doc "`zenit-rpartial' works with multiple fixed arguments"
  (let* ((list-builder (lambda (a b c) (list a b c)))
         (with-fixed (zenit-rpartial list-builder 2 3)))
    (should (equal '(1 2 3) (funcall with-fixed 1)))))

(zenit-deftest zenit-lookup-key
  (:vars ((map (make-sparse-keymap))
          (minor-mode-map-alist nil)
          (emulation-mode-map-alists nil))
   :before-each
   (define-key map (kbd "C-c a") #'forward-char))
  ,test
  (test)
  :doc "`zenit-lookup-key' finds key in single keymap"
  (should (eq #'forward-char (zenit-lookup-key (kbd "C-c a") map)))

  :doc "`zenit-lookup-key' returns nil for unbound key"
  (should (eq nil (zenit-lookup-key (kbd "C-c b") map)))

  :doc "`zenit-lookup-key' searches active keymaps when none provided"
  (let ((minor-mode-map-alist `((t . ,map))))
    (should (eq #'forward-char (zenit-lookup-key (kbd "C-c a"))))))

(zenit-deftest zenit-load
  (:vars ((load-path load-path)
          (zenit-core-dir default-directory)
          (test-file (zenit-test-make-temp-file nil ".el" "(defvar test-var 42)\n")))
   :after-each
   (progn
     (delete-file test-file)
     (makunbound 'test-var)))
  ,test
  (test)
  :doc "`zenit-load' loads files successfully"
  (progn
    (should (zenit-load test-file))
    (should (boundp 'test-var))
    (should (= test-var 42)))

  :doc "`zenit-load' respects noerror argument"
  (should-not (zenit-load "nonexistent.el" t)))

(zenit-deftest zenit-require
  (:vars ((load-path (append `(,temporary-file-directory) load-path))
          (zenit-core-dir temporary-file-directory)
          (test-lib (file-name-concat temporary-file-directory "test-lib.el"))
          (test-lib-sub (file-name-concat temporary-file-directory "test-lib" "zenit-lib-sub.el")))
   :before-each
   (progn
     (with-temp-file test-lib
       (insert "(provide 'test-lib)\n"))
     (make-directory (file-name-concat temporary-file-directory "test-lib") t)
     (with-temp-file test-lib-sub
       (insert "(provide 'test-lib '(sub))\n")))
   :after-each
   (progn
     (delete-file test-lib)
     (delete-file test-lib-sub)
     (setq features (delq 'test-lib features))
     (setq features (delq 'test-lib-sub features))))
  ,test
  (test)
  :doc "`zenit-require' loads features successfully"
  (progn
    (should (zenit-require 'test-lib))
    (should (memq 'test-lib features)))

  :doc "`zenit-require' handles subfeatures"
  (progn
    (should (zenit-require 'test-lib 'sub))
    (should (featurep 'test-lib 'sub)))

  :doc "`zenit-require' respects noerror argument"
  (should-not (zenit-require 'nonexistent nil t)))

(zenit-deftest zenit-load-envvars-file
  (:vars ((process-environment (copy-sequence process-environment))
          (exec-path (copy-sequence exec-path))
          (shell-file-name (copy-sequence shell-file-name))
          (envfile (zenit-test-make-temp-file nil nil "(\"PATH=/test/bin\" \"SHELL=/bin/zsh\" \"TZ=UTC\")\n")))
   :after-each
   (delete-file envfile))
  ,test
  (test)
  :doc "`zenit-load-envvars-file' loads environment variables from file"
  (progn
    (should (zenit-load-envvars-file envfile))
    (should (equal "/test/bin" (getenv "PATH")))
    (should (equal "/bin/zsh" (getenv "SHELL")))
    (should (equal "UTC" (getenv "TZ"))))

  :doc "`zenit-load-envvars-file' updates exec-path from PATH"
  (progn
    (zenit-load-envvars-file envfile)
    (should (equal '("/test/bin") 
                   (cl-remove-if-not (lambda (p) (string-prefix-p "/test" p))
                                     exec-path))))

  :doc "`zenit-load-envvars-file' updates shell-file-name from SHELL"
  (progn
    (zenit-load-envvars-file envfile)
    (should (equal "/bin/zsh" shell-file-name)))

  :doc "`zenit-load-envvars-file' respects noerror argument"
  (progn
    (should-not (zenit-load-envvars-file "nonexistent.env" t))
    (should-error (zenit-load-envvars-file "nonexistent.env") :type 'file-error)))

(zenit-deftest zenit--hook
  (:doc "`zenit--hook' is defined")
  (should (boundp 'zenit--hook)))

(zenit-deftest zenit-run-hook
  (:vars ((test-value nil)))
  ,test
  (test)
  :doc "`zenit-run-hook' runs hooks successfully"
  (progn
    (zenit-run-hook (lambda () (setq test-value t)))
    (should test-value))

  :doc "`zenit-run-hook' handles errors gracefully"
  (let ((debug-on-error nil))
    (should-error (zenit-run-hook (lambda () (error "Test error"))) :type 'zenit-hook-error)))

(zenit-deftest zenit-run-hooks
  (:vars ((test-value nil)
          (test-hook-1 nil)
          (test-hook-2 nil))
   :before-each
   (progn
     (setq test-value nil)
     (add-hook 'test-hook-1 (lambda () (push 1 test-value)))
     (add-hook 'test-hook-2 (lambda () (push 2 test-value)))))
  ,test
  (test)
  :doc "`zenit-run-hooks' runs multiple hooks in order"
  (progn
    (zenit-run-hooks 'test-hook-1 'test-hook-2)
    (should (equal '(2 1) test-value)))

  :doc "`zenit-run-hooks' handles errors in hooks"
  (let ((test-hook-3 nil)
        (debug-on-error nil))
    (add-hook 'test-hook-3 (lambda () (error "Test error")))
    (quiet!!
      (should-error (zenit-run-hooks 'test-hook-3) :type 'zenit-hook-error))))

(zenit-deftest zenit-run-hook-on
  (:doc "`zenit-run-hook-on' is defined")
  (should (fboundp 'zenit-run-hook-on)))

(zenit-deftest file!
  (:vars ((zenit-include--current-file nil)
          (byte-compile-current-file nil)
          (load-file-name nil)
          (buffer-file-name nil)))
  ,test
  (test)
  :doc "`file!' returns `zenit-include--current-file' when set"
  (let ((zenit-include--current-file "/path/to/test.el"))
    (should (equal "/path/to/test.el" (macroexpand '(file!)))))

  :doc "`file!' returns `byte-compile-current-file' when set"
  (let ((byte-compile-current-file "/path/to/compiled.el"))
    (should (equal "/path/to/compiled.el" (macroexpand '(file!)))))

  :doc "`file!' returns `load-file-name' when set"
  (let ((load-file-name "/path/to/loaded.el"))
    (should (equal "/path/to/loaded.el" (macroexpand '(file!)))))

  :doc "`file!' returns `buffer-file-name' when set"
  (let ((buffer-file-name "/path/to/buffer.el"))
    (should (equal "/path/to/buffer.el" (macroexpand '(file!)))))

  :doc "`file!' signals error when no file path can be determined"
  (should-error (macroexpand '(file!)) :type 'error))

(zenit-deftest dir!
  (:vars ((zenit-include--current-file nil)
          (byte-compile-current-file nil)
          (load-file-name nil)
          (buffer-file-name nil)))
  ,test
  (test)
  :doc "`dir!' returns directory of `zenit-include--current-file' when set"
  (let ((zenit-include--current-file "/path/to/test.el"))
    (should (equal "/path/to/" (macroexpand '(dir!)))))

  :doc "`dir!' returns directory of `byte-compile-current-file' when set"
  (let ((byte-compile-current-file "/path/to/compiled.el"))
    (should (equal "/path/to/" (macroexpand '(dir!)))))

  :doc "`dir!' returns directory of `load-file-name' when set"
  (let ((load-file-name "/path/to/loaded.el"))
    (should (equal "/path/to/" (macroexpand '(dir!)))))

  :doc "`dir!' returns directory of `buffer-file-name' when set"
  (let ((buffer-file-name "/path/to/buffer.el"))
    (should (equal "/path/to/" (macroexpand '(dir!)))))

  :doc "`dir!' signals error when no file path can be determined"
  (should-error (macroexpand '(dir!)) :type 'error))

(zenit-deftest letf!
  (:vars ((test-fn (lambda (x) (+ x 1)))
          (test-var 0)))
  ,test
  (test)
  :doc "`letf!' temporarily rebinds functions"
  (progn
    (should (= 2 (funcall test-fn 1)))
    (letf! ((defun test-fn (x) (* x 2)))
      (should (= 4 (test-fn 2))))
    (should (= 2 (funcall test-fn 1))))

  :doc "`letf!' handles cl-letf style bindings"
  (progn
    (letf! ((test-var 42))
      (should (= test-var 42)))
    (should (= test-var 0)))

  :doc "`letf!' supports defmacro"
  (progn
    (letf! ((defmacro test-macro (x) `(* ,x 2)))
      (should (= 4 (test-macro 2))))
    (should-error (test-macro 2)))

  :doc "`letf!' supports recursive functions with defun*"
  (letf! ((defun* factorial (n)
            (if (<= n 1) 1
              (* n (factorial (1- n))))))
    (should (= 120 (factorial 5))))

  :doc "`letf!' supports advice with defadvice"
  (progn
    (defun orig-test-fn (x) x)
    (letf! ((defadvice orig-test-fn (:around (fn x) wrap)
              (* (funcall fn x) 2)))
      (should (= 4 (orig-test-fn 2))))
    (should (= 2 (orig-test-fn 2))))

  :doc "`letf!' supports keyword-style advice"
  (let ((fn-adv (lambda (orig-fn &rest args)
                  (* (apply orig-fn args) 2))))
    (letf! ((defadvice #'+ :around fn-adv))
      (should (= 8 (+ 2 2))))
    (should (= 4 (+ 2 2)))))

(zenit-deftest quiet!!
  (:vars* ((messages nil)
           (standard-output-string "")
           (standard-output (lambda (&rest args)
                              (setq standard-output-string
                                    (concat standard-output-string
                                            (concat args))))))
   :before-each
   (setq messages nil
         standard-output-string ""))
  (letf! ((defun message (format &rest args)
            (unless inhibit-message
              (push (apply #'format format args) messages))))
    ,test)
  (test)
  :doc "`quiet!!' suppresses all output"
  (progn
    (quiet!!
      (message "Test message")
      (princ "Test output"))
    (should (string-empty-p standard-output-string))
    (should (null messages)))

  :doc "`quiet!!' allows output when init-file-debug is t"
  (let ((init-file-debug t))
    (quiet!!
      (message "Test message")
      (princ "Test output"))
    (should (equal "Test output" standard-output-string))
    (should (equal (car messages) "Test message"))))

(zenit-deftest quiet!
  (:vars* ((messages nil)
           (standard-output-string "")
           (standard-output (lambda (&rest args)
                              (setq standard-output-string
                                    (concat standard-output-string
                                            (concat args))))))
   :before-each
   (setq messages nil
         standard-output-string ""))
  (letf! ((defun message (format &rest args)
            (unless inhibit-message
              (push (apply #'format format args) messages))))
    ,test)
  (test)
  :doc "`quiet!' suppresses output in interactive sessions"
  (let ((noninteractive nil))
    (quiet!
      (message "Test message")
      (princ "Test output"))
    (should (string-empty-p standard-output-string))
    (should (equal (car messages) nil)))

  :doc "`quiet!' suppresses all output in non-interactive sessions"
  (let ((noninteractive t))
    (quiet!
      (message "Test message")
      (princ "Test output"))
    (should (string-empty-p standard-output-string))
    (should (null messages)))

  :doc "`quiet!' allows output when init-file-debug is t"
  (let ((init-file-debug t))
    (quiet!
      (message "Test message")
      (princ "Test output"))
    (should (equal "Test output" standard-output-string))
    (should (equal (car messages) "Test message"))))

(zenit-deftest lambda!
  (:vars ((test-fn (lambda! (x &key y z) (+ x (or y 0) (or z 0))))))
  ,test
  (test)
  :doc "`lambda!' creates functions with cl-lambda-list support"
  (should (= 3 (funcall test-fn 3)))
  
  :doc "`lambda!' handles keyword arguments"
  (should (= 5 (funcall test-fn 3 :y 2)))
  
  :doc "`lambda!' handles multiple keyword arguments"
  (should (= 6 (funcall test-fn 3 :y 2 :z 1)))
  
  :doc "`lambda!' implicitly adds &allow-other-keys"
  (should (= 3 (funcall test-fn 3 :foo 'bar))))

(zenit-deftest zenit--fn-crawl ()
  ,test
  (test)
  :doc "`zenit--fn-crawl' populates ARGS array with special symbols found in DATA"
  (let ((data '(a b %* c (% d) e (f %)))
        (args (make-vector 2 nil)))
    (zenit--fn-crawl data args)
    (should (equal [%* %] args)))

  :doc "`zenit--fn-crawl' handles nested structures"
  (let ((data '((a b) %* (c (% d)) (e (f %))))
        (args (make-vector 2 nil)))
    (zenit--fn-crawl data args)
    (should (equal [%* %] args)))

  :doc "`zenit--fn-crawl' handles vector structures"
  (let ((data ['a 'b '%* '(c '%) 'e '(f '%)])
        (args (make-vector 2 nil)))
    (zenit--fn-crawl data args)
    (should (equal [%* %] args)))

  :doc "`zenit--fn-crawl' raises an error when both %% and %%1 are found in DATA"
  (let ((data '(a b %* c (% d) e %1 (f %) g %%))
        (args (make-vector 2 nil)))
    (should-error (zenit--fn-crawl data args))))

(zenit-deftest fn! ()
  ,test
  (test)
  :doc "`fn!' creates a lambda with implicit, positional arguments"
  (let ((f (fn! (+ %1 %2))))
    (should (equal 7 (funcall f 3 4))))

  :doc "`fn!' handles missing arguments with placeholder symbols"
  (let ((f (fn! (if %1 %3))))
    (should (equal 2 (funcall f 1 nil 2))))

  :doc "`fn!' supports shorthand '%' for the first positional argument"
  (let ((f (fn! (+ % %2))))
    (should (equal 11 (funcall f 5 6))))

  :doc "`fn!' supports '&rest' arguments with '%*'"
  (let ((f (fn! (if %1 (list %2 %3) (list %3 %2)))))
    (should (equal '(2 3) (funcall f t 2 3)))
    (should (equal '(3 2) (funcall f nil 2 3))))

  :doc "`fn!' works with higher-order functions"
  (let ((f (fn! (* %1 %2)))
        (g (fn! (+ %1 (funcall %2 3 4)))))
    (should (equal 13 (funcall g 1 f)))))

(zenit-deftest cmd!
  (:doc "`cmd!' expands into interactive lambda")
  (should (equal '#'(lambda
                      (&rest _)
                      (interactive)
                      (message "Hello, world!"))
                 (macroexpand '(cmd! (message "Hello, world!"))))))

(zenit-deftest cmd!!
  (:doc "`cmd!!' expands into interactive lambda")
  (should (equal '#'(lambda
                      (arg &rest _)
                      (interactive "P")
                      (let
                          ((current-prefix-arg
                            (or nil arg)))
                        (call-interactively
                         (message "Hello, world!"))))
                 (macroexpand '(cmd!! (message "Hello, world!"))))))
(let ((l '(1 2)))
  (add-to-list 'l 1)
  (add-to-list 'l 2)
  (add-to-list 'l 3 t)
  l)
(zenit-deftest zenit-splice-into
  (:vars ((test-list '("a" "b" "c" "d" "e"))))
  ,test
  (test)
  :doc "`zenit-splice-into' prepends element if after and before arguments are not used"
  (progn
    (should (equal '("foo" "a" "b" "c" "d" "e") (zenit-splice-into test-list "foo")))
    (should (equal '("foo" "bar" "a" "b" "c" "d" "e") (zenit-splice-into test-list '("foo" "bar")))))
  :doc "`zenit-splice-into' inserts element after argument"
  (progn
    (should (equal '("a" "b" "foo" "c" "d" "e") (zenit-splice-into test-list "foo" "b")))
    (should (equal '("a" "b" "foo" "bar" "c" "d" "e") (zenit-splice-into test-list '("foo" "bar") "b"))))
  :doc "`zenit-splice-into' inserts element before argument"
  (progn
    (should (equal '("a" "b" "c" "foo" "d" "e") (zenit-splice-into test-list "foo" nil "d")))
    (should (equal '("a" "b" "c" "foo" "bar" "d" "e") (zenit-splice-into test-list '("foo" "bar") nil "d"))))
  :doc "`zenit-splice-into' inserts element between after and before arguments"
  (progn
    (should (equal '("a" "b" "foo" "c" "d" "e") (zenit-splice-into test-list "foo" "b" "e")))
    (should (equal '("a" "b" "foo" "bar" "c" "d" "e") (zenit-splice-into test-list '("foo" "bar") "b" "e")))))

(zenit-deftest spliceq!
  (:vars ((test-list '("a" "b" "c" "d" "e"))))
  ,test
  (test)
    :doc "`spliceq!' inserts element after argument in place"
  (progn
    (spliceq! test-list "foo")
    (should (equal '("foo" "a" "b" "c" "d" "e") test-list))
    (spliceq! test-list '("foo" "bar"))
    (should (equal '("foo" "bar" "foo" "a" "b" "c" "d" "e") test-list)))
  :doc "`spliceq!' inserts element after argument in place"
  (progn
    (spliceq! test-list "foo" "b")
    (should (equal '("a" "b" "foo" "c" "d" "e") test-list))
    (spliceq! test-list '("foo" "bar") "b")
    (should (equal '("a" "b" "foo" "bar" "foo" "c" "d" "e") test-list)))
  :doc "`spliceq!' inserts element before argument in place"
  (progn
    (spliceq! test-list "foo" nil "d")
    (should (equal '("a" "b" "c" "foo" "d" "e") test-list))
    (spliceq! test-list '("foo" "bar") nil "d")
    (should (equal '("a" "b" "c" "foo" "foo" "bar" "d" "e") test-list)))
  :doc "`spliceq!' inserts element between after and before arguments in place"
  (progn
    (spliceq! test-list "foo" "b" "e")
    (should (equal '("a" "b" "foo" "c" "d" "e") test-list))
    (spliceq! test-list '("foo" "bar") "b" "e")
    (should (equal '("a" "b" "foo" "bar" "foo" "c" "d" "e") test-list))))

(zenit-deftest appendq!
  (:vars ((test-list '("a" "b" "c"))))
  ,test
  (test)
  :doc "`appendq!' appends single list to symbol in place"
  (progn
    (appendq! test-list '("d" "e"))
    (should (equal '("a" "b" "c" "d" "e") test-list)))
  :doc "`appendq!' appends multiple lists to symbol in place"
  (progn
    (appendq! test-list '("d" "e") '("f" "g"))
    (should (equal '("a" "b" "c" "d" "e" "f" "g") test-list)))
  :doc "`appendq!' handles empty lists"
  (progn
    (appendq! test-list '() '("d" "e"))
    (should (equal '("a" "b" "c" "d" "e") test-list)))
  :doc "`appendq!' handles nil values"
  (progn
    (appendq! test-list nil '("d" "e"))
    (should (equal '("a" "b" "c" "d" "e") test-list))))

(zenit-deftest setq!
  (:before-each
   (progn
     (defvar test-var1 nil)
     (defvar test-var2 nil))
   :after-each
   (progn
     (makunbound 'test-var1)
     (makunbound 'test-var2)))
  ,test
  (test)
  :doc "`setq!' sets variables"
  (progn
    (setq! test-var1 42)
    (should (= 42 test-var1)))

  :doc "`setq!' handles multiple variables"
  (progn
    (setq! test-var1 1 test-var2 2)
    (should (= 1 test-var1))
    (should (= 2 test-var2)))

  :doc "`setq!' respects custom setters"
  (let ((custom-set-count 0))
    (put 'test-var1 'custom-set
         (lambda (sym val)
           (set-default-toplevel-value sym val)
           (setq custom-set-count (1+ custom-set-count))))
    (setq! test-var1 99)
    (should (= 99 test-var1))
    (should (= 1 custom-set-count))))

(zenit-deftest setq-local!
  (:before-each
   (progn
     (defvar-local test-var1 nil)
     (defvar-local test-var2 nil))
   :after-each
   (progn
     (makunbound 'test-var1)
     (makunbound 'test-var2)))
  ,test
  (test)
  :doc "`setq-local!' sets buffer-local variables"
  (progn
    (setq test-var1 1)
    (with-temp-buffer
      (setq-local! test-var1 42)
      (should (= 42 test-var1)))
    (should (= 1 test-var1)))

  :doc "`setq-local!' handles multiple variables"
  (progn
    (setq test-var1 1)
    (setq test-var2 2)
    (with-temp-buffer
      (setq-local! test-var1 41 test-var2 42)
      (should (= 41 test-var1))
      (should (= 42 test-var2)))
    (should (= 1 test-var1))
    (should (= 2 test-var2)))

  :doc "`setq-local!' respects custom setters"
  (let ((custom-set-count 0))
    (put 'test-var1 'custom-set
         (lambda (sym val)
           (set sym val)
           (setq custom-set-count (1+ custom-set-count))))
    (put 'test-var2 'custom-set
         (lambda (sym val)
           (set sym val)
           (setq custom-set-count (1+ custom-set-count))))
    (setq test-var1 1)
    (setq test-var2 2)
    (with-temp-buffer
      (setq-local! test-var1 41 test-var2 42)
      (should (= 41 test-var1))
      (should (= 42 test-var2)))
    (should (= 1 test-var1))
    (should (= 2 test-var2))
    (should (= custom-set-count 2))))

(let
    ((test-list
      '(a b c d e)))
  (progn
    (delq! 'z test-list)
    test-list
    ;; (should
    ;;  (equal
    ;;   '(a b c d e)
    ;;   test-list))
    ))

(zenit-deftest delq! ()
  ,test
  (test)
  :doc "`delq!' removes element from list in place"
  (let ((test-list '(a b c d e)))
    (delq! 'c test-list)
    (should (equal '(a b d e) test-list)))
  
  :doc "`delq!' handles non-existent elements"
  (let ((test-list '(a b c d e)))
    (delq! 'z test-list)
    (should (equal '(a b c d e) test-list)))
  
  :doc "`delq!' works with fetcher function"
  (let ((test-list '((a . 1) (b . 2) (c . 3) (d . 4))))
    (delq! 'b test-list #'assq)
    (should (equal '((a . 1) (c . 3) (d . 4)) test-list))))

(zenit-deftest pushnew!
  (:vars ((test-list '(a b c))))
  ,test
  (test)
  :doc "`pushnew!' adds new elements to list"
  (progn
    (pushnew! test-list 'd 'e)
    (should (equal '(e d a b c) test-list)))
  
  :doc "`pushnew!' doesn't add duplicate elements"
  (progn
    (pushnew! test-list 'a 'b)
    (should (equal '(a b c) test-list)))
  
  :doc "`pushnew!' handles multiple new elements"
  (progn
    (pushnew! test-list 'x 'y 'z)
    (should (equal '(z y x a b c) test-list))))

(zenit-deftest prependq!
  (:vars ((test-list '(a b c))))
  ,test
  (test)
  :doc "`prependq!' prepends single list to symbol in place"
  (progn
    (prependq! test-list '("d" "e"))
    (should (equal '("d" "e" a b c) test-list)))
  
  :doc "`prependq!' prepends multiple lists to symbol in place"
  (progn
    (prependq! test-list '("d" "e") '("f" "g"))
    (should (equal '("d" "e" "f" "g" a b c) test-list)))
  
  :doc "`prependq!' handles empty lists"
  (progn
    (prependq! test-list '() '("d" "e"))
    (should (equal '("d" "e" a b c) test-list)))
  
  :doc "`prependq!' handles nil values"
  (progn
    (prependq! test-list nil '("d" "e"))
    (should (equal '("d" "e" a b c) test-list))))

(zenit-deftest add-load-path! ()
  ,test
  (test)
  :doc "`add-load-path!' adds a single directory to the load-path"
  (let ((load-path (copy-sequence load-path)))
    (add-load-path! "temp-dir")
    (should (member (file-name-concat (dir!) "temp-dir") load-path)))

  :doc "`add-load-path!' adds a directory only once to the load-path"
  (let ((load-path (copy-sequence load-path)))
    (add-load-path! "temp-dir")
    (add-load-path! "temp-dir")
    (should (length= (cl-remove-if-not (lambda (p) (equal p (file-name-concat (dir!) "temp-dir"))) load-path) 1)))

  :doc "`add-load-path!' adds multiple directories to the load-path"
  (let ((load-path (copy-sequence load-path)))
    (add-load-path! "temp-dir-1" "temp-dir-2")
    (should (member (file-name-concat (dir!) "temp-dir-1") load-path))
    (should (member (file-name-concat (dir!) "temp-dir-2") load-path))))

(zenit-deftest after! ()
  ,test
  (test)
  :doc "`after!' expands to an `eval-after-load' form for a single package"
  (should (equal
           '(progn (with-eval-after-load 'test-package (test-fn)))
           (macroexpand '(after! test-package (test-fn)))))

  :doc "`after!' expands to multiple `eval-after-load' forms for an :or/:any package list"
  (should (equal
           '(progn
              (progn (eval-after-load 'test-package-1 #'(lambda nil (test-fn))))
              (progn (eval-after-load 'test-package-2 #'(lambda nil (test-fn)))))
           (macroexpand-all '(after! (:or test-package-1 test-package-2) (test-fn)))))

  :doc "`after!' expands to a nested `eval-after-load' form for an :and/:all package list"
  (should (equal
           '(progn
              (eval-after-load 'test-package-1
                #'(lambda nil (progn (eval-after-load 'test-package-2 #'(lambda nil (test-fn)))))))
           (macroexpand-all '(after! (:and test-package-1 test-package-2) (test-fn)))))

  :doc  "`after!' expands to a nested `eval-after-load' form for a complex package list"
  (should (equal
           '(progn
              (eval-after-load 'test-package-1
                #'(lambda nil (progn
                                (progn (eval-after-load 'test-package-2 #'(lambda nil (test-fn))))
                                (progn (eval-after-load 'test-package-3 #'(lambda nil (test-fn))))))))
           (macroexpand-all '(after! (:and test-package-1 (:or test-package-2 test-package-3)) (test-fn)))))

  :doc "`after!' expands to nil if package is in zenit-disabled-packages"
  (progn
    (defvar zenit-disabled-packages nil)
    (cl-pushnew 'test-package zenit-disabled-packages)
    (should-not (macroexpand-all '(after! test-package (test-fn))))
    (makunbound 'zenit-disabled-packages)))

(zenit-deftest load!
  (:doc "`load!' expands into a `zenit-load' call")
  (progn
    (should (equal '(zenit-load (file-name-concat (dir!) "test-file.el") nil)
                   (macroexpand '(load! "test-file.el"))))
    (should (equal '(zenit-load (file-name-concat "my-path" "test-file.el") nil)
                   (macroexpand '(load! "test-file.el" "my-path"))))
    (should (equal '(zenit-load (file-name-concat "my-path" "test-file.el") t)
                   (macroexpand '(load! "test-file.el" "my-path" t))))))

(zenit-deftest zenit--with-local-load-history
  (:doc "`zenit--with-local-load-history' wraps `current-load-list' around BODY")
  (should (equal '(let ((current-load-list nil))
                    (message "Hello, world!")
                    (push
                     (cons '"test-file.el" current-load-list)
                     load-history))
                 (macroexpand '(zenit--with-local-load-history "test-file.el" (message "Hello, world!"))))))

(zenit-deftest zenit-include--current-file
  (:doc "`zenit-include--current-file' is defined")
  (should (boundp 'zenit-include--current-file)))

(zenit-deftest zenit-include ()
  ,test
  (test)
  :doc "`zenit-include' expands correctly when not compiling"
  (should (equal '(zenit-load "test-file.el" t) (macroexpand '(zenit-include "test-file.el" t))))

  :doc "`zenit-include' expands correctly when compiling"
  (let ((byte-compile-current-file (file!))
        (test-file (zenit-test-make-temp-file nil ".el" "Hello\n")))
    (should (equal `(let ((current-load-list nil))
                      Hello
                      (push (cons ',test-file current-load-list) load-history))
                   (macroexpand `(zenit-include ,test-file t))))))

(zenit-deftest zenit-include--previous-file
  (:doc "`zenit-include--previous-file' is defined")
  (should (boundp 'zenit-include--previous-file)))

(zenit-deftest include! ()
  ,test
  (test)
  :doc "`include!' expands correctly"
  (should (equal
           `(progn
              (eval-when-compile
                (setq zenit-include--previous-file nil zenit-include--current-file ,(file-name-concat (dir!) "test-file.el")))
              (zenit-include ,(file-name-concat (dir!) "test-file.el") nil)
              (eval-when-compile
                (setq zenit-include--current-file zenit-include--previous-file)))
           (macroexpand `(include! "test-file.el" ,(dir!))))))

(zenit-deftest compile-along! ()
  ,test
  (test)
  :doc "`compile-along!' expands correctly with a single file"
  (letf! ((defun macroexp-compiling-p () t))
    (should (equal
             `(eval-when-compile
                (async-get
                 (apply
                  #'zenit-async-byte-compile-file
                  '(,(file-name-concat (dir!) "test-file.el")
                    :req-core-lib t :req-core t :req-core-libs all :req-extra
                    (cl-lib zenit-use-package zenit-el-patch zenit-keybinds zenit-projects zenit-editor)
                    :modulep t :autoloads t))))
             (macroexpand-1 `(compile-along! "test-file.el" ,(dir!))))))

  :doc "`compile-along!' expands correctly with a directory"
  (letf! ((defun macroexp-compiling-p () t)
          (dir (zenit-test-make-temp-file t)))
    (zenit-file-write (file-name-concat dir "test-1.el") "Hello 1")
    (zenit-file-write (file-name-concat dir "test-2.el") "Hello 2")
    (should (equal
             `(eval-when-compile
                (async-get
                 (apply
                  #'zenit-async-byte-compile-file
                  '(,(file-name-concat dir "test-1.el"))))
                (async-get
                 (apply
                  #'zenit-async-byte-compile-file
                  '(,(file-name-concat dir "test-2.el")))))
             (macroexpand-1 `(compile-along! ,dir ,(file-name-parent-directory dir)))))
    (delete-directory dir t)))

(zenit-deftest autoload!
  (:vars ((buffer-file-name (file-name-concat (dir!) "test-file.el"))))
  ,test
  (test)
  :doc "`autoload!' expands correctly with a single function"
  (should (equal
           `(progn (autoload #'fun-1 ,(file-name-concat (dir!) "test-file.el") nil nil nil))
           (macroexpand '(autoload! "test-file.el" #'fun-1))))

  :doc "`autoload!' expands correctly with multiple functions"
  (should (equal
           `(progn
              (autoload #'fun-1 ,(file-name-concat (dir!) "test-file.el") nil nil nil)
              (autoload #'fun-2 ,(file-name-concat (dir!) "test-file.el") nil nil nil)
              (autoload #'fun-3 ,(file-name-concat (dir!) "test-file.el") nil nil nil))
           (macroexpand '(autoload! "test-file.el" #'fun-1 #'fun-2 #'fun-3))))

  :doc "`autoload!' expands correctly using keyword args"
  (should (equal
           `(progn
              (autoload #'fun-1 ,(file-name-concat (dir!) "test-file.el") nil t 'macro)
              (autoload #'fun-2 ,(file-name-concat (dir!) "test-file.el") nil t 'macro)
              (autoload #'fun-3 ,(file-name-concat (dir!) "test-file.el") nil t 'macro))
           (macroexpand '(autoload! "test-file.el" #'fun-1 #'fun-2 #'fun-3 :interactive t :type 'macro)))))

(zenit-deftest defer-until! ()
  ,test
  (test)
  :doc "`defer-until!' evaluates body when condition is true"
  (let ((x 0))
    (defer-until! t
      (setq x 1))
    (should (= 1 x)))

  :doc "`defer-until!' defers body when condition is false"
  (let ((x 0)
        (condition nil))
    (defer-until! condition
      (setq x 1))
    (should (= 0 x))
    ;; Simulate loading something that makes condition true
    (setq condition t)
    (run-hooks 'after-load-functions)
    (should (= 1 x)))

  :doc "`defer-until!' removes hook after condition becomes true"
  (let ((x 0)
        (condition nil)
        (orig-after-load-functions after-load-functions))
    (defer-until! condition
      (setq x 1))
    (should (= (length after-load-functions) (1+ (length orig-after-load-functions))))
    (setq condition t)
    (run-hooks 'after-load-functions)
    ;; Hook should be removed after running
    (should (= (length after-load-functions) (length orig-after-load-functions)))))

(zenit-deftest after-call!
  (:before-each
   (defvar zenit--deferred-packages-alist nil)
   :after-each
   (progn
     (makunbound 'zenit--deferred-packages-alist)
     (advice-mapc (lambda (advice _props) (advice-remove 'test-fn advice))
                  'test-fn)
     (advice-mapc (lambda (advice _props) (advice-remove 'test-fn-1 advice))
                  'test-fn-1)
     (advice-mapc (lambda (advice _props) (advice-remove 'test-fn-2 advice))
                  'test-fn-2)))
  ,test
  (test)
  :doc "`after-call!' defers loading until function is called"
  (progn
    (after-call! test-package test-fn)
    (should (assq 'test-package zenit--deferred-packages-alist))
    (should (member #'test-fn (cdr (assq 'test-package zenit--deferred-packages-alist)))))

  :doc "`after-call!' works with multiple trigger functions"
  (progn
    (after-call! test-package test-fn-1 test-fn-2)
    (should (assq 'test-package zenit--deferred-packages-alist))
    (should (member #'test-fn-1 (cdr (assq 'test-package zenit--deferred-packages-alist))))
    (should (member #'test-fn-2 (cdr (assq 'test-package zenit--deferred-packages-alist))))))

(zenit-deftest defer-feature!
  (:before-each
   (defun test-fun () nil)
   :after-each
   (fmakunbound 'test-fun))
  ,test
  (test)
  :doc "`defer-feature!' removes feature from features list"
  (progn
    (provide 'test-feature)
    (should (memq 'test-feature features))
    (defer-feature! test-feature test-fun)
    (should-not (memq 'test-feature features)))

  :doc "`defer-feature!' adds advice to trigger functions"
  (let ((advice-fn (intern (format "zenit--defer-feature-%s-a" 'test-feature))))
    (defer-feature! test-feature test-funn)
    (should (advice-member-p advice-fn #'test-funn)))

  :doc "`defer-feature!' restores feature when function is called"
  (progn
    (provide 'test-feature)
    (should (memq 'test-feature features))
    (defer-feature! test-feature test-fun)
    (should-not (memq 'test-feature features))
    (funcall 'test-fun)
    (should (memq 'test-feature features)))

  :doc "`defer-feature!' removes advice after feature is restored"
  (let ((advice-fn (intern (format "zenit--defer-feature-%s-a" 'test-feature))))
    (defer-feature! test-feature test-fun)
    (should (advice-member-p advice-fn #'test-fun))
    (funcall 'test-fun)
    (should-not (advice-member-p advice-fn #'test-fun)))

  :doc "`defer-feature!' works with multiple trigger functions"
  (progn
    (defun test-fn1 () nil)
    (defun test-fn2 () nil)
    (let ((advice-fn (intern (format "zenit--defer-feature-%s-a" 'test-feature))))
      (defer-feature! test-feature test-fn1 test-fn2)
      (should (advice-member-p advice-fn #'test-fn1))
      (should (advice-member-p advice-fn #'test-fn2))
      (funcall 'test-fn1)
      (should-not (advice-member-p advice-fn #'test-fn1))
      (should-not (advice-member-p advice-fn #'test-fn2)))
    (fmakunbound 'test-fn1)
    (fmakunbound 'test-fn2)))

(zenit-deftest zenit-compile-functions
  (:doc "`zenit-compile-functions' is defined")
  (should (fboundp 'zenit-compile-functions)))

(zenit-deftest add-transient-hook!
  (:vars ((test-value nil)
          (test-hook nil))
   :before-each
   (setq test-value nil))
  ,test
  (test)
  :doc "`add-transient-hook!' runs hook once and removes it"
  (progn
    (add-transient-hook! 'test-hook
      (setq test-value (cons 1 test-value)))
    (run-hooks 'test-hook)
    (should (equal '(1) test-value))
    (run-hooks 'test-hook)
    (should (equal '(1) test-value)))

  :doc "`add-transient-hook!' works with function advice"
  (progn
    (defun test-fn () nil)
    (add-transient-hook! #'test-fn
      (setq test-value (cons 2 test-value)))
    (test-fn)
    (should (equal '(2) test-value))
    (test-fn)
    (should (equal '(2) test-value))
    (fmakunbound 'test-fn))

  :doc "`add-transient-hook!' supports :after keyword"
  (progn
    (add-transient-hook! 'test-hook
      (setq test-value (cons 3 test-value)))
    (add-transient-hook! 'test-hook
      :after
      (setq test-value (cons 4 test-value)))
    (run-hooks 'test-hook)
    (should (equal '(4 3) test-value))
    (run-hooks 'test-hook)
    (should (equal '(4 3) test-value))))

(zenit-deftest add-hook!
  (:vars ((test-hook nil)
          (test-hook-2 nil)
          (test-value nil))
   :before-each
   (setq test-hook nil
         test-hook-2 nil
         test-value nil))
  ,test
  (test)
  :doc "`add-hook!' adds a single function to a hook"
  (progn
    (add-hook! 'test-hook (setq test-value 42))
    (run-hooks 'test-hook)
    (should (= 42 test-value)))

  :doc "`add-hook!' adds multiple functions to a hook"
  (progn
    (add-hook! 'test-hook
      (setq test-value (cons 1 test-value))
      (setq test-value (cons 2 test-value)))
    (run-hooks 'test-hook)
    (should (equal '(2 1) test-value)))

  :doc "`add-hook!' adds functions to multiple hooks"
  (progn
    (add-hook! '(test-hook test-hook-2)
      (setq test-value (cons 1 test-value)))
    (run-hooks 'test-hook)
    (run-hooks 'test-hook-2)
    (should (equal '(1 1) test-value)))

  :doc "`add-hook!' respects :append property"
  (progn
    (add-hook! 'test-hook (setq test-value (cons 1 test-value)))
    (add-hook! 'test-hook :append (setq test-value (cons 2 test-value)))
    (run-hooks 'test-hook)
    (should (equal '(2 1) test-value)))

  :doc "`add-hook!' respects :depth property"
  (progn
    (add-hook! 'test-hook :depth -90 (setq test-value (cons 1 test-value)))
    (add-hook! 'test-hook :depth 90 (setq test-value (cons 2 test-value)))
    (run-hooks 'test-hook)
    (should (equal '(2 1) test-value)))

  :doc "`add-hook!' supports quoted hooks"
  (progn
    (add-hook! 'test-hook (setq test-value 42))
    (run-hooks 'test-hook)
    (should (= 42 test-value)))

  :doc "`add-hook!' supports defun forms"
  (progn
    (add-hook! 'test-hook
      (defun test-hook-fn ()
        (setq test-value 43)))
    (run-hooks 'test-hook)
    (should (= 43 test-value)))

  :doc "`add-hook!' supports local hooks"
  (progn
    (with-temp-buffer
      (add-hook! 'test-hook :local (setq test-value 42))
      (should (local-variable-p 'test-hook))
      (run-hooks 'test-hook)
      (should (= 42 test-value)))))

(zenit-deftest remove-hook!
  (:doc "`remove-hook!' expands correctly")
  (should (equal
           '(progn
              (dolist (hook '(test-hook))
                (dolist (func (list #'test-fn))
                  (remove-hook hook func nil))))
           (macroexpand '(remove-hook! 'test-hook #'test-fn)))))

(zenit-deftest setq-hook!
  (:doc "`setq-hook!' expands correctly")
  (should (equal
           '(progn
              (defun zenit--setq-test-val-1-for-test-h (&rest _)
                "Set buffer-local value hook function.

`test-val-1' = 1"
                (setq-local test-val-1 1))
              (eval-when-compile
                (declare-function zenit--setq-test-val-1-for-test-h
                                  nil))
              (add-hook 'test-hook
                        (function zenit--setq-test-val-1-for-test-h)
                        -90)
              (defun zenit--setq-test-val-2-for-test-h (&rest _)
                "Set buffer-local value hook function.

`test-val-2' = 2"
                (setq-local test-val-2 2))
              (eval-when-compile
                (declare-function zenit--setq-test-val-2-for-test-h
                                  nil))
              (add-hook 'test-hook
                        (function zenit--setq-test-val-2-for-test-h)
                        -90))
           (macroexpand '(setq-hook! 'test-hook test-val-1 1 test-val-2 2)))))

(zenit-deftest unsetq-hook!
  (:doc "`unsetq-hook!' expands correctly")
  (should (equal
           '(progn
              (remove-hook 'test-hook #'zenit--setq-test-val-1-for-test-h)
              (remove-hook 'test-hook #'zenit--setq-1-for-test-h)
              (remove-hook 'test-hook #'zenit--setq-test-val-2-for-test-h)
              (remove-hook 'test-hook #'zenit--setq-2-for-test-h))
           (macroexpand '(unsetq-hook! 'test-hook test-val-1 1 test-val-2 2)))))

(zenit-deftest defadvice!
  (:vars ((test-value nil))
   :before-each
   (progn
     (setq test-value nil)
     (defun test-fn (x) x))
   :after-each
   (progn
     (advice-mapc (lambda (advice _props) (advice-remove 'test-fn advice))
                  'test-fn)
     (fmakunbound 'test-fn)))
  ,test
  (test)
  :doc "`defadvice!' adds basic advice"
  (progn
    (defadvice! test-advice (orig-fn &rest args)
      :around #'test-fn
      (* (apply orig-fn args) 2))
    (should (= 4 (test-fn 2))))

  :doc "`defadvice!' adds advice to multiple functions"
  (progn
    (defun test-fn-2 (x) x)
    (defadvice! test-advice-2 (orig-fn &rest args)
      :around #'test-fn
      :around #'test-fn-2
      (* (apply orig-fn args) 2))
    (should (= 4 (test-fn 2)))
    (should (= 4 (test-fn-2 2)))
    (fmakunbound 'test-fn-2))

  :doc "`defadvice!' supports different advice types"
  (progn
    (defadvice! test-advice-3 (x)
      :before #'test-fn
      (setq test-value x))
    (test-fn 3)
    (should (= 3 test-value)))

  :doc "`defadvice!' properly documents advice"
  (progn
    (defadvice! test-advice-5 (orig-fn &rest args)
      "Test documentation."
      :around #'test-fn
      (* (apply orig-fn args) 2))
    (should (string= "Test documentation."
                     (documentation 'test-advice-5)))))

(zenit-deftest undefadvice!
  (:vars ((test-value nil))
   :before-each
   (progn
     (setq test-value nil)
     (defun test-fn (x) x))
   :after-each
   (progn
     (advice-mapc (lambda (advice _props) (advice-remove 'test-fn advice))
                  'test-fn)
     (fmakunbound 'test-fn)))
  ,test
  (test)
  :doc "`undefadvice!' removes advice from a single function"
  (progn
    (defadvice! test-advice (orig-fn &rest args)
      :around #'test-fn
      (* (apply orig-fn args) 2))
    (should (= 4 (test-fn 2)))
    (undefadvice! test-advice (&rest _)
      :around #'test-fn)
    (should (= 2 (test-fn 2))))

  :doc "`undefadvice!' removes advice from multiple functions"
  (progn
    (defun test-fn-2 (x) x)
    (defadvice! test-advice-2 (orig-fn &rest args)
      :around #'test-fn
      :around #'test-fn-2
      (* (apply orig-fn args) 2))
    (should (= 4 (test-fn 2)))
    (should (= 4 (test-fn-2 2)))
    (undefadvice! test-advice-2 (&rest _)
      :around #'test-fn
      :around #'test-fn-2)
    (should (= 2 (test-fn 2)))
    (should (= 2 (test-fn-2 2)))
    (fmakunbound 'test-fn-2))

  :doc "`undefadvice!' handles different advice types"
  (progn
    (defadvice! test-advice-3 (x)
      :before #'test-fn
      (setq test-value x))
    (test-fn 3)
    (should (= 3 test-value))
    (undefadvice! test-advice-3 (x)
      :before #'test-fn)
    (setq test-value nil)
    (test-fn 3)
    (should (null test-value))))

(zenit-deftest zenit-context
  (:doc "`zenit-context' is defined")
  (should (boundp 'zenit-context)))

(zenit-deftest zenit-context--check
  (:doc "`zenit-context--check' does not return an error if the context is valid")
  (progn
    (should-not (zenit-context--check 'init))
    (should-error (zenit-context--check 'does-not-exist))))

(zenit-deftest zenit-context-p
  (:doc "`zenit-context-p' returns non-nil if context is active"
   :vars ((zenit-context '(init eval t))))
  (progn
    (should (zenit-context-p 'init))
    (should (zenit-context-p 'eval))
    (should-not (zenit-context-p 'does-not-exist))))

(zenit-deftest zenit-context-push
  (:doc "`zenit-context-push' returns non-nil if context is added"
   :vars ((zenit-context '(t))))
  (progn
    (should (zenit-context-push 'init))
    (should (zenit-context-push 'eval))
    (should-error (zenit-context-push 'does-not-exist))))

(zenit-deftest zenit-context-pop
  (:doc "`zenit-context-pop' returns non-nil if context is removed"
   :vars ((zenit-context '(init eval t))))
  (progn
    (should (zenit-context-pop 'init))
    (should (zenit-context-pop 'eval))
    (should-not (zenit-context-pop 'does-not-exist))
    (should-error (zenit-context-pop 'does-not-exist t))))

(zenit-deftest with-zenit-context
  (:doc "`with-zenit-context' returns non-nil if context is added"
   :vars ((zenit-context '(t))))
  (with-zenit-context '(init eval t)
    (should (zenit-context-p 'init))
    (should (zenit-context-p 'eval))
    (should-not (zenit-context-p 'does-not-exist))))

(zenit-deftest zenit--empty-module-context
  (:doc "`zenit--empty-module-context' is defined")
  (should (boundp 'zenit--empty-module-context)))

(zenit-deftest zenit-module-context-var
  (:doc "`zenit-module-context' is defined")
  (should (boundp 'zenit-module-context)))

(zenit-deftest zenit-module--context-field
  (:doc "`zenit-module--context-field' retrieves the index of the given field")
  (progn
    (should (= 0 (zenit-module--context-field :index)))
    (should (= 1 (zenit-module--context-field :initdepth)))
    (should (= 2 (zenit-module--context-field :configdepth)))
    (should (= 3 (zenit-module--context-field :group)))
    (should (= 4 (zenit-module--context-field :name)))
    (should (= 5 (zenit-module--context-field :flags)))
    (should (= 6 (zenit-module--context-field :features)))))

(zenit-deftest zenit-module-context-get
  (:doc "`zenit-module-context-get' retrieves the value of the given field")
  (progn
    (should (= 22 (zenit-module-context-get :index [22 nil nil nil nil nil nil])))
    (should (eq nil (zenit-module-context-get :initdepth)))))

(zenit-deftest zenit-module-context-fn
  (:doc "`zenit-module-context' creates a context for given group and name")
  (progn
    (should (equal [nil nil nil nil nil nil nil] (zenit-module-context :nosuchcat 'nosuchname)))
    (let ((zenit--empty-module-context [40 0 0 :testcat testname nil nil]))
      (should (equal [40 0 0 :testcat testname nil nil] (zenit-module-context :testcat 'testname)))
      (should (equal [40 0 0 :testcat testname nil nil] (zenit-module-context '(:testcat . testname)))))))

(zenit-deftest zenit-module-context-key
  (:doc "`zenit-module-context-key' returns the module of the active context")
  (progn
    (should (equal '(:testcat . testname) (zenit-module-context-key [40 0 0 :testcat testname nil nil])))))

(zenit-deftest with-zenit-module-context
  (:doc "`with-zenit-module-context' evaluates body in specified context")
  (let ((zenit--empty-module-context [40 0 0 :testcat testname nil nil]))
    (should (eq 'testname
                (with-zenit-module-context '(:testcat . testname)
                  (zenit-module-context-get :name))))))

(zenit-deftest zenit-module-p
  (:doc "`zenit-module-p' returns non-nil if module is enabled")
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (puthash (cons :cat1 'module1) '(:flags (flag1)) zenit-modules)
    (puthash (cons :cat2 'module2) '(:flags nil) zenit-modules)
    (should (zenit-module-p :cat1 'module1))
    (should (zenit-module-p :cat1 'module1 'flag1))
    (should (zenit-module-p :cat2 'module2))
    (should-not (zenit-module-p :cat2 'module2 'flag2))))

(zenit-deftest zenit-module-depth ()
  ,test
  (test)
  :doc "`zenit-module-depth' returns the depth if the category-module pair is present, and no initdepth is specified"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (puthash (cons :cat1 'module1) '(:depth (2 . 3)) zenit-modules)
    (should (equal 3 (zenit-module-depth :cat1 'module1))))
  :doc "`zenit-module-depth' returns the car of the depth if initdepth is specified"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (puthash (cons :cat1 'module1) '(:depth (2 . 3)) zenit-modules)
    (should (equal 2 (zenit-module-depth :cat1 'module1 t))))
  :doc "`zenit-module-depth' returns 0 if the category-module pair is not present"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (puthash (cons :cat1 'module1) '(:depth (2 . 3)) zenit-modules)
    (should (equal 0 (zenit-module-depth :cat2 'module2)))))

(zenit-deftest zenit-module--has-flag-p ()
  ,test
  (test)
  :doc "`zenit-module--has-flag-p' returns t if flags satisfy wanted-flags"
  (let ((flags '(+present +multiple -negated)))
    (should (zenit-module--has-flag-p flags '(+present)))
    (should (zenit-module--has-flag-p flags '(+present +multiple)))
    (should (zenit-module--has-flag-p flags '(-negated)))
    (should (zenit-module--has-flag-p flags '(+present -nonexistent))))

  :doc "`zenit-module--has-flag-p' returns nil if flags don't satisfy wanted-flags"
  (let ((flags '(+present +multiple -negated)))
    (should-not (zenit-module--has-flag-p flags '(+missing)))
    (should-not (zenit-module--has-flag-p flags '(-present)))
    (should-not (zenit-module--has-flag-p flags '(+present -multiple)))
    (should-not (zenit-module--has-flag-p flags '(+present +multiple -negated +extra)))))

(zenit-deftest zenit-module-get ()
  ,test
  (test)
  :doc "`zenit-module-get' retrieves module properties"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (puthash (cons :test 'module) '(:flags (+a +b) :depth 5 :path "/test") zenit-modules)
    (should (equal '(+a +b) (zenit-module-get :test 'module :flags)))
    (should (equal 5 (zenit-module-get :test 'module :depth))))

  :doc "`zenit-module-get' returns nil for missing properties"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (should-not (zenit-module-get :test 'module :nonexistent))))

(zenit-deftest zenit-module-put ()
  ,test
  (test)
  :doc "`zenit-module-put' updates module properties"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (zenit-module-put :test 'module :flags '(a b) :depth 2)
    (should (equal '(:flags (a b) :depth 2) (zenit-module-get :test 'module))))

  :doc "`zenit-module-put' updates existing properties"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (zenit-module-put :test 'module :flags '(a))
    (zenit-module-put :test 'module :flags '(a b))
    (should (equal '(a b) (zenit-module-get :test 'module :flags)))))

(zenit-deftest zenit-module-set ()
  ,test
  (test)
  :doc "`zenit-module-set' adds new modules"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (zenit-module-set :test 'module :flags '(a) :depth 3)
    (should (equal '(:flags (a) :depth 3) (zenit-module-get :test 'module))))

  :doc "`zenit-module-set' updates existing modules"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (zenit-module-set :test 'module :flags '(a))
    (zenit-module-set :test 'module :flags '(b))
    (should (equal '(b) (zenit-module-get :test 'module :flags))))

  :doc "`zenit-module-set' removes modules with nil plist"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (zenit-module-set :test 'module :flags '(a))
    (zenit-module-set :test 'module nil)
    (should-not (gethash (cons :test 'module) zenit-modules))))

(zenit-deftest zenit-module-list
  (:vars ((zenit-modules-load-path (list (file-name-concat temporary-file-directory "modules")))
          (zenit-modules (make-hash-table :test #'equal)))
   :before-each
   (progn
     (make-directory (file-name-concat temporary-file-directory "modules" "cat1" "mod1") t)
     (make-directory (file-name-concat temporary-file-directory "modules" "cat2" "mod2") t)
     (zenit-module-set :cat1 'mod1 :path (file-name-concat temporary-file-directory "cat1" "mod1") :depth '(2 . 1))
     (zenit-module-set :cat2 'mod2 :path (file-name-concat temporary-file-directory "cat2" "mod2") :depth '(1 . 2)))
   :after-each
   (progn
     (delete-directory (file-name-concat temporary-file-directory "modules") t)))
  ,test
  (test)
  :doc "`zenit-module-list' returns modules sorted by depth"
  (should (equal '((:cat2 . mod2) (:cat1 . mod1)) (zenit-module-list nil)))

  :doc "`zenit-module-list' returns modules sorted by their initdepth when initorder? is non-nil"
  (should (equal '((:cat1 . mod1) (:cat2 . mod2)) (zenit-module-list nil t)))

  :doc "`zenit-module-list' finds modules in directories"
  (should (member '(:cat1 . mod1) (zenit-module-list t))))

(zenit-deftest zenit-module-expand-path
  (:vars ((zenit-modules (make-hash-table :test #'equal)))
   :before-each
   (zenit-module-set :test 'mod :path "/test/path"))
  ,test
  (test)
  :doc "`zenit-module-expand-path' constructs path with file"
  (should (equal "/test/path/file.el"
                 (zenit-module-expand-path :test 'mod "file.el")))

  :doc "`zenit-module-expand-path' returns base path without file"
  (should (equal "/test/path"
                 (zenit-module-expand-path :test 'mod)))

  :doc "`zenit-module-expand-path' returns nil for disabled modules"
  (should-not (zenit-module-expand-path :disabled 'mod "file.el")))

(zenit-deftest zenit-module-locate-path
  (:vars ((zenit-modules-load-path (list temporary-file-directory))
          (zenit-modules (make-hash-table :test #'equal)))
   :before-each
   (progn
     (make-directory (file-name-concat temporary-file-directory "test" "mod") t)
     (with-temp-file (file-name-concat temporary-file-directory "test" "mod" "test-file.el")
       (insert "test content")))
   :after-each
   (delete-directory (file-name-concat temporary-file-directory "test") t))
  ,test
  (test)
  :doc "`zenit-module-locate-path' finds existing files"
  (should (zenit-module-locate-path :test 'mod "test-file.el"))

  :doc "`zenit-module-locate-path' returns nil for missing files"
  (should-not (zenit-module-locate-path :test 'mod "nonexistent.el"))

  :doc "`zenit-module-locate-path' handles directory paths"
  (should (zenit-module-locate-path :test 'mod)))

(zenit-deftest zenit-module-locate-paths
  (:vars ((zenit-modules-load-path (list temporary-file-directory))
          (zenit-modules (make-hash-table :test #'equal)))
   :before-each
   (progn
     (make-directory (file-name-concat temporary-file-directory "cat1" "mod1") t)
     (make-directory (file-name-concat temporary-file-directory "cat2" "mod2") t)
     (with-temp-file (file-name-concat temporary-file-directory "cat1" "mod1" "common.el")
       (insert "test content"))
     (with-temp-file (file-name-concat temporary-file-directory "cat2" "mod2" "common.el")
       (insert "test content"))
     (zenit-module-set :cat1 'mod1 :path (file-name-concat temporary-file-directory "cat1" "mod1"))
     (zenit-module-set :cat2 'mod2 :path (file-name-concat temporary-file-directory "cat2" "mod2")))
   :after-each
   (progn
     (delete-directory (file-name-concat temporary-file-directory "cat1") t)
     (delete-directory (file-name-concat temporary-file-directory "cat2") t)))
  ,test
  (test)
  :doc "`zenit-module-locate-paths' finds all instances of file"
  (let ((paths (zenit-module-locate-paths '((:cat1 . mod1) (:cat2 . mod2)) "common.el")))
    (should (= 2 (length paths)))))

(zenit-deftest zenit-module-load-path
  (:vars ((zenit-modules (make-hash-table :test #'equal)))
   :before-each
   (progn
     (zenit-module-set :test 'enabled :path "/path/to/enabled")
     (zenit-module-set :test 'disabled nil)))
  ,test
  (test)
  :doc "`zenit-module-load-path' includes enabled modules"
  (should (member "/path/to/enabled" (zenit-module-load-path)))

  :doc "`zenit-module-load-path' excludes disabled modules"
  (should-not (member nil (zenit-module-load-path))))

(zenit-deftest zenit-module-from-path
  (:vars ((zenit-modules (make-hash-table :test #'equal))
          (tmp-zenit-core-dir (zenit-test-make-temp-file t))
          (tmp-zenit-local-conf-dir (zenit-test-make-temp-file t)))
   :before-each
   (progn
     (zenit-module-set :lang 'python :path "/modules/lang/python")
     (zenit-module-set :tools 'lsp :path "/modules/tools/lsp")
     (zenit-file-write (file-name-concat tmp-zenit-core-dir "init.el") "Hello World")
     (zenit-file-write (file-name-concat tmp-zenit-local-conf-dir "init.el") "Hello World"))
   :after-each
   (progn
     (delete-directory tmp-zenit-core-dir t)
     (delete-directory tmp-zenit-local-conf-dir t)))
  ,test
  (test)
  :doc "`zenit-module-from-path' extracts module info from path"
  (should (equal '(:lang . python)
                 (zenit-module-from-path "/modules/lang/python/config.el")))

  :doc "`zenit-module-from-path' handles core directory"
  (let ((zenit-core-dir tmp-zenit-core-dir))
    (should (equal '(:core) (zenit-module-from-path (file-name-concat tmp-zenit-core-dir "file.el")))))

  :doc "`zenit-module-from-path' handles local config directory"
  (let ((zenit-local-conf-dir tmp-zenit-local-conf-dir))
    (should (equal '(:local-conf) (zenit-module-from-path (file-name-concat tmp-zenit-local-conf-dir "init.el")))))

  :doc "`zenit-module-from-path' respects enabled-only flag"
  (should-not (zenit-module-from-path "/modules/lang/disabled/config.el" t)))

(zenit-deftest zenit--module-dependencies
  (:doc "`zenit--module-dependencies' is defined")
  (should (boundp 'zenit--module-dependencies)))

(zenit-deftest zenit--module-conflicts
  (:doc "`zenit--module-conflicts' is defined")
  (should (boundp 'zenit--module-conflicts)))

(zenit-deftest zenit-module-resolve
  (:vars* ((zenit--module-dependencies nil)
           (zenit--module-conflicts nil)
           (tmp-dir (zenit-test-make-temp-file t))
           (zenit-modules-load-path (list tmp-dir))
           (zenit-modules (make-hash-table :test #'equal)))
   :before-each
   (progn
     (make-directory (file-name-concat tmp-dir "test" "mod") t)
     (zenit-module-set :test 'mod :path (file-name-concat tmp-dir "test" "mod") :flags '(+flag)))
   :after-each
   (progn
     (delete-directory tmp-dir t)))
  ,test
  (test)
  :doc "`zenit-module-resolve' adds dependencies"
  (progn
    (with-temp-file (file-name-concat tmp-dir "test" "mod" zenit-module-control-file)
      (insert "(:depends ((t :depcat1 depmod1 +flag) (+flag :depcat2 depmod2 +flag)))"))
    (make-directory (file-name-concat tmp-dir "depcat1" "depmod1") t)
    (make-directory (file-name-concat tmp-dir "depcat2" "depmod2") t)

    (zenit-module-resolve '(:test . mod))
    (should (string-match-p "Adding :depcat2 depmod2 (\\+flag) as dependency"
                            (car zenit--module-dependencies)))
    (should (length= zenit--module-dependencies 2))
    (should (zenit-module-p :depcat1 'depmod1 '+flag))
    (should (zenit-module-p :depcat2 'depmod2 '+flag)))

  :doc "`zenit-module-resolve' handles existing dependencies"
  (progn
    (zenit-module-set :depcat 'depmod :path (file-name-concat tmp-dir "test" "mod") :flags '(+oldflag))
    (with-temp-file (file-name-concat tmp-dir "test" "mod" zenit-module-control-file)
      (insert "(:depends ((t :depcat depmod +newflag)))"))

    (zenit-module-resolve '(:test . mod))
    (should (equal '(+oldflag +newflag)
                   (zenit-module-get :depcat 'depmod :flags))))

  :doc "`zenit-module-resolve' detects conflicts"
  (progn
    (zenit-module-set :conflict 'mod :path (file-name-concat tmp-dir "test" "mod") :flags '(+flag))
    (with-temp-file (file-name-concat tmp-dir "test" "mod" zenit-module-control-file)
      (insert "(:conflicts ((t :conflict mod) (+flag :conflict mod)))"))

    (zenit-module-resolve '(:test . mod) 'conflicts)
    (should (string-match-p "conflicts with :conflict mod"
                            (car zenit--module-conflicts)))
    (should (length= zenit--module-conflicts 2))))

(zenit-deftest modules!
  (:vars ((zenit-modules (make-hash-table :test #'equal))))
  ,test
  (test)
  :doc "`modules!' creates modules and sets path for each one"
  (letf! ((#'print! #'ignore)
          (defun zenit-module-locate-path (category &optional module file)
            "/test/path"))
    (modules! :test mod1 mod2 mod3)
    (dolist (key (hash-table-keys zenit-modules))
      (should (equal '(:path "/test/path" :flags nil) (gethash key zenit-modules)))))

  :doc "`modules!' creates modules and sets path for each one"
  (letf! ((#'print! #'ignore)
          (defun zenit-module-locate-path (category &optional module file)
            t))
    (should-error (modules! :test mod1 mod2 mod3))))

(zenit-deftest modulep!
  (:doc "`modulep!' returns t if :CATEGORY MODULE (and +FLAGS) are enabled"
   :vars ((zenit-modules (make-hash-table :test #'equal)))
   :before-each
   (zenit-module-set :testcat 'testmod :flags '(+flag1 +flag2))
   :afer-each
   (zenit-module-set :testcat 'testmod nil))
  (should (eq ,exp (modulep! ,@in)))
  (in exp)
  (:testcat testmod) t
  (:testcat nosuchmod) nil
  (:nosuchcat testmod) nil
  (:nosuchcat nosuchmod) nil
  (:testcat testmod +flag1) t
  (:testcat testmod +flag2) t
  (:testcat testmod +flag1 +flag2) t
  (:testcat testmod +flag3) nil
  (:testcat testmod -flag3) t
  (:testcat testmod -flag3 -flag4) t
  (:nosuchcat nosuchmod -flag3) nil)

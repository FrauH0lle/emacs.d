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
      (expect (zenit-keyword-name :foo) :to-equal "foo"))))

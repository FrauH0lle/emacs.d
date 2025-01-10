;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-lib-process.el

(describe "core/lib/process"

  (load! "lib/process" zenit-core-dir)
  (load! "lib/format" zenit-core-dir)

  (describe "zenit-call-process"
    (it "returns the correct error code and output for a successful command"
      (let ((result (zenit-call-process "echo" "hello world")))
        (expect (car result) :to-be 0)
        (expect (cdr result) :to-equal "hello world")))

    (it "returns the correct error code for an unsuccessful command"
      (let ((result (zenit-call-process "false")))
        (expect (car result) :to-be 1)))

    (it "trims the output of the command"
      (let ((result (zenit-call-process "echo" " hello world ")))
        (expect (cdr result) :to-equal "hello world"))))


  (describe "zenit-exec-process"
    (it "executes the given command and arguments synchronously"
      (let ((result (quiet! (zenit-exec-process "echo" "Hello, World!"))))
        (expect (car result) :to-be 0)
        (expect (cdr result) :to-equal "Hello, World!")))

    (it "throws an error if the command failed to execute"
      (expect (zenit-exec-process "nonexistent-command") :to-throw))

    (it "executes the command and correctly handles its output"
      (let ((result (quiet! (zenit-exec-process "ls" "-al"))))
        (expect (car result) :to-be 0)
        (expect (cdr result) :not :to-equal "")))

    (it "correctly handles arguments with spaces"
      (let ((result (quiet! (zenit-exec-process "echo" "Hello," " World!"))))
        (expect (car result) :to-be 0)
        (expect (cdr result) :to-equal "Hello,  World!")))))

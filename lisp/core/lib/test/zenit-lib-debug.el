;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-debug.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'debug)

(zenit-deftest zenit--profiler
  (:doc "`zenit--profiler' is defined")
  (should (boundp 'zenit--profiler)))

(zenit-deftest zenit/toggle-profiler
  (:doc "`zenit/toggle-profiler' is defined")
  (should (fboundp 'zenit/toggle-profiler)))

(zenit-deftest zenit-debug--variables
  (:doc "`zenit-debug--variables' is defined")
  (should (boundp 'zenit-debug--variables)))

(zenit-deftest set-debug-variable!
  (:doc "`set-debug-variable!' is defined")
  (should (fboundp 'set-debug-variable!)))

(zenit-deftest zenit-debug--unbound-variables
  (:doc "`zenit-debug--unbound-variables' is defined")
  (should (boundp 'zenit-debug--unbound-variables)))

(zenit-deftest zenit-debug--watch-vars-h
  (:doc "`zenit-debug--watch-vars-h' is defined")
  (should (fboundp 'zenit-debug--watch-vars-h)))

(zenit-deftest zenit-debug--set-var
  (:doc "`zenit-debug--set-var' is defined")
  (should (fboundp 'zenit-debug--set-var)))

(zenit-deftest zenit-debug--timestamped-message-a
  (:doc "`zenit-debug--timestamped-message-a' is defined")
  (should (fboundp 'zenit-debug--timestamped-message-a)))

(zenit-deftest zenit-debug-mode
  (:doc "`zenit-debug-mode' is defined")
  (progn
    (should (fboundp 'zenit-debug-mode))
    (should (boundp 'zenit-debug-mode))))

(zenit-deftest zenit-debug-shut-up-a
  (:doc "`zenit-debug-shut-up-a' is defined")
  (should (fboundp 'zenit-debug-shut-up-a)))

(zenit-deftest zenit-debugger-a
  (:doc "`zenit-debugger-a' advises `debug'")
  (progn
    (should (fboundp 'zenit-debugger-a))
    (should (advice-member-p 'zenit-debugger-a #'debug))))

(zenit-deftest zenit-backtrace
  (:doc "`zenit-backtrace' is defined")
  (should (fboundp 'zenit-backtrace)))

(zenit-deftest zenit-backtrace-write-to-file
  (:doc "`zenit-backtrace-write-to-file' is defined")
  (should (fboundp 'zenit-backtrace-write-to-file)))

(zenit-deftest zenit-run-all-startup-hooks-h
  (:doc "`zenit-run-all-startup-hooks-h' is defined")
  (should (fboundp 'zenit-run-all-startup-hooks-h)))

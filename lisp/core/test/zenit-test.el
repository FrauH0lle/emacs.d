;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/zenit-test.el

(require 'zenit-test)

(ert-deftest zenit-test--template/test ()
  "Test expansion of `zenit-test--template'."
  (should (equal '(("" (should (equal 1 1)))
                   ("First docstring" (should (equal 2 4)))
                   ("Second docstring" (should (equal 3 9))))
                 (macroexpand '(zenit-test--template
                                   (should (equal ,input ,expected))
                                 (input expected)
                                 1 1
                                 :doc "First docstring"
                                 2 4
                                 :doc "Second docstring"
                                 3 9)))))

(ert-deftest zenit-deftest/test ()
  "Test expansion of `zenit-deftest'."
  (should (equal '(progn
                    (ert-deftest my-function/test nil "Test my-function's behavior" :tags
                                 '(my-function public integration)
                                 (setup)
                                 (should
                                  (equal
                                   (my-function 'foo)
                                   t))
                                 (cleanup)))
                 (macroexpand '(zenit-deftest my-function
                                 (:before-each (setup)
                                  :after-each (cleanup)
                                  :tags (integration)
                                  :doc "Test my-function's behavior")
                                 (should (equal (my-function ,input1) t))
                                 (input1)
                                 'foo))))
  (should (equal '(progn
                    (ert-deftest my-function/test nil "Test my-function's behavior" :tags
                                 '(my-function public integration)
                                 (let ((foo 1)
                                       bar)
                                   (setup)
                                   (should
                                    (equal
                                     (my-function 'foo)
                                     t))
                                   (cleanup))))
                 (macroexpand '(zenit-deftest my-function
                                 (:before-each (setup)
                                  :after-each (cleanup)
                                  :vars ((foo 1) bar)
                                  :tags (integration)
                                  :doc "Test my-function's behavior")
                                 (should (equal (my-function ,input1) t))
                                 (input1)
                                 'foo))))
  (should (equal '(progn
                    (ert-deftest my-function/test nil "Test my-function's behavior" :tags
                                 '(my-function public integration)
                                 (let* ((foo 1)
                                        bar)
                                   (setup)
                                   (should
                                    (equal
                                     (my-function 'foo)
                                     t))
                                   (cleanup))))
                 (macroexpand '(zenit-deftest my-function
                                 (:before-each (setup)
                                  :after-each (cleanup)
                                  :vars* ((foo 1) bar)
                                  :tags (integration)
                                  :doc "Test my-function's behavior")
                                 (should (equal (my-function ,input1) t))
                                 (input1)
                                 'foo))))
  (should (equal '(progn
                    (ert-deftest my-function/test@1 nil "Test my-function's behavior, var 1" :tags
                                 '(my-function public integration)
                                 (setup)
                                 (should
                                  (my-function 'foo))
                                 (cleanup))
                    (ert-deftest my-function/test@2 nil "Test my-function's behavior, var 2" :tags
                                 '(my-function public integration)
                                 (setup)
                                 (should-not
                                  (my-function 'bar))
                                 (cleanup)))
                 (macroexpand '(zenit-deftest my-function
                                 (:before-each (setup)
                                  :after-each (cleanup)
                                  :tags (integration))
                                 (,assert (my-function ,input))
                                 (assert input)
                                 :doc "Test my-function's behavior, var 1"
                                 should 'foo
                                 :doc "Test my-function's behavior, var 2"
                                 should-not 'bar)))))

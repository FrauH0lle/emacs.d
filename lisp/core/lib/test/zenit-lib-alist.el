;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-alist.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'alist)

(zenit-deftest zenit-alist-set ()
  (should (equal ',out (zenit-alist-set ,@in)))
  (in out)
  :doc "Changes KEY to VAL in ALIST"
  ("a" 3 '(("a" . 1) ("b" . 2))) (("a" . 3) ("b" . 2))
  :doc "Adds KEY with VAL to ALIST if missing"
  ("c" 3 '(("a" . 1) ("b" . 2))) (("c" . 3) ("a" . 1) ("b" . 2))
  :doc "Uses `eq' for key comparison when SYMBOL is non-nil"
  ('a 3 '((a . 1) (b . 2)) 'symbol) ((a . 3) (b . 2))
  :doc "Adds KEY with VAL to ALIST if missing and uses `eq' for key
comparison when SYMBOL is non-nil"
  ('c 3 '((a . 1) (b . 2)) 'symbol) ((c . 3) (a . 1) (b . 2))
  :doc "Does not create duplicate associations"
  ("a" 3 '(("a" . 1) ("b" . 2))) (("a" . 3) ("b" . 2)))

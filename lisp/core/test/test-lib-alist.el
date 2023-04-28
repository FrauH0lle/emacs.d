;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/tests/test-lib-alist.el

(describe "core/lib/alist"

  (load! "lib/alist" zenit-core-dir)

  (describe "zenit-alist-set"
    (it "sets the property KEY to VAL in ALIST"
      (let ((alist '(("a" . 1) ("b" . 2))))
        (expect (zenit-alist-set "a" 3 alist) :to-equal '(("a" . 3) ("b" . 2)))
        (expect (zenit-alist-set "c" 3 alist) :to-equal '(("c" . 3) ("a" . 3) ("b" . 2)))))

    (it "uses eq for key comparison when SYMBOL is non-nil"
      (let ((alist '((a . 1) (b . 2))))
        (expect (zenit-alist-set 'a 3 alist t) :to-equal '((a . 3) (b . 2)))
        (expect (zenit-alist-set 'c 3 alist t) :to-equal '((c . 3) (a . 3) (b . 2)))))

    (it "returns new alist"
      (let ((alist '(("a" . 1) ("b" . 2))))
        (expect (zenit-alist-set "a" 3 alist) :not :to-be '(("a" . 1) ("b" . 2)))))

    (it "does not create duplicate associations"
      (let ((alist '(("a" . 1) ("b" . 2))))
        (expect (zenit-alist-set "a" 3 alist) :to-equal '(("a" . 3) ("b" . 2)))
        (expect (zenit-alist-set "a" 4 alist) :to-equal '(("a" . 4) ("b" . 2)))))))

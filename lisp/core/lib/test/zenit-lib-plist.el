;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-plist.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'plist)

(zenit-deftest zenit-plist-map
  (:doc "`zenit-plist-map' maps fn to plist")
  (let ((plist '(:a 1 :b 2 :c 3)))
    (zenit-plist-map (lambda (key val) (1+ val)) plist)
    (should (equal '(:a 2 :b 3 :c 4) plist))))

(zenit-deftest zenit-plist-map*
  (:doc "`zenit-plist-map*' maps fn to vplist")
  (let ((plist '(:a 1 2 :c 3)))
    (should (equal
             '((2 3) (4))
             (zenit-plist-map*
              (lambda (key vals) (cl-loop for val in vals collect (1+ val)))
              plist)))))

(zenit-deftest zenit-plist-get*
  (:doc "`zenit-plist-map*' gets values from vplist")
  (let ((plist '(:a 1 2 3 :b 2 4 :c hi)))
    (progn
      (should (equal '(1 2 3) (zenit-plist-get* plist :a)))
      (should (equal '(2 4) (zenit-plist-get* plist :b)))
      (should (equal '(hi) (zenit-plist-get* plist :c))))))

(zenit-deftest zenit-plist-merge
  (:doc "`zenit-plist-merge' merges two plists")
  (let ((plist1 '(:a 1 :b 2))
        (plist2 '(:b 4 :c 3)))
    (should (equal '(:b 2 :c 3 :a 1) (zenit-plist-merge plist1 plist2)))))

(zenit-deftest zenit-plist-delete-nil
  (:doc "`zenit-plist-delete-nil' deletes nil properties from plist")
  (let ((plist '(:a 1 nil nil :c 2 nil)))
    (should (equal '(:a 1 :c 2) (zenit-plist-delete-nil plist)))))

(zenit-deftest zenit-plist-keys
  (:doc "`zenit-plist-keys' returns keys of plist")
  (let ((plist '(:a 1 :b 2)))
    (should (equal '(:b :a) (zenit-plist-keys plist)))))

(zenit-deftest zenit-plist-values
  (:doc "`zenit-plist-values' returns values of plist")
  (let ((plist '(:a 1 :b 2)))
    (should (equal '(2 1) (zenit-plist-values plist)))))

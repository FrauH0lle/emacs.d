;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-fonts.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'fonts)

(zenit-deftest zenit-font-increment
  (:doc "`zenit-font-increment' is defined")
  (should (boundp 'zenit-font-increment)))

(zenit-deftest zenit-big-font
  (:doc "`zenit-big-font' is defined")
  (should (boundp 'zenit-big-font)))

(zenit-deftest zenit-big-font-increment
  (:doc "`zenit-big-font-increment' is defined")
  (should (boundp 'zenit-big-font-increment)))

(zenit-deftest zenit-normalize-font
  (:doc "`zenit-normalize-font' is defined")
  (should (fboundp 'zenit-normalize-font)))

(zenit-deftest zenit-adjust-font-size
  (:doc "`zenit-adjust-font-size' is defined")
  (should (fboundp 'zenit-adjust-font-size)))

(zenit-deftest zenit/reload-font
  (:doc "`zenit/reload-font' is defined")
  (should (fboundp 'zenit/reload-font)))

(zenit-deftest zenit/increase-font-size
  (:doc "`zenit/increase-font-size' is defined")
  (should (fboundp 'zenit/increase-font-size)))

(zenit-deftest zenit/decrease-font-size
  (:doc "`zenit/decrease-font-size' is defined")
  (should (fboundp 'zenit/decrease-font-size)))

(zenit-deftest zenit/reset-font-size
  (:doc "`zenit/reset-font-size' is defined")
  (should (fboundp 'zenit/reset-font-size)))

(zenit-deftest zenit-big-font-mode
  (:doc "`zenit-big-font-mode' is defined")
  (progn
    (should (fboundp 'zenit-big-font-mode))
    (should (boundp 'zenit-big-font-mode))))

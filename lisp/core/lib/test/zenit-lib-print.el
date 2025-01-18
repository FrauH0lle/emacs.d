;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-print.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'print)

(zenit-deftest zenit-print-ansi-alist
  (:doc "`zenit-print-ansi-alist' is defined")
  (should (boundp 'zenit-print-ansi-alist)))

(zenit-deftest zenit-print-class-alist
  (:doc "`zenit-print-class-alist' is defined")
  (should (boundp 'zenit-print-class-alist)))

(zenit-deftest zenit-print-indent
  (:doc "`zenit-print-indent' is defined")
  (should (boundp 'zenit-print-indent)))

(zenit-deftest zenit-print-indent-increment
  (:doc "`zenit-print-indent-increment' is defined")
  (should (boundp 'zenit-print-indent-increment)))

(zenit-deftest zenit-print-backend
  (:doc "`zenit-print-backend' is defined")
  (should (boundp 'zenit-print-backend)))

(zenit-deftest zenit-print-stream
  (:doc "`zenit-print-stream' is defined")
  (should (boundp 'zenit-print-stream)))

(zenit-deftest zenit-print-level
  (:doc "`zenit-print-level' is defined")
  (should (boundp 'zenit-print-level)))

(zenit-deftest zenit-print-minimum-level
  (:doc "`zenit-print-minimum-level' is defined")
  (should (boundp 'zenit-print-minimum-level)))

(zenit-deftest zenit-print
  (:doc "`zenit-print' is defined")
  (should (fboundp 'zenit-print)))

(zenit-deftest format!
  (:doc "`format!' is defined")
  (should (fboundp 'format!)))

(zenit-deftest print-group!
  (:doc "`print-group!' is defined")
  (should (fboundp 'print-group!)))

(zenit-deftest print!
  (:doc "`print!' is defined")
  (should (fboundp 'print!)))

(zenit-deftest insert!
  (:doc "`insert!' is defined")
  (should (fboundp 'insert!)))

(zenit-deftest zenit-print--output-depth
  (:doc "`zenit-print--output-depth' is defined")
  (should (boundp 'zenit-print--output-depth)))

(zenit-deftest with-output-to!
  (:doc "`with-output-to!' is defined")
  (should (fboundp 'with-output-to!)))

(zenit-deftest zenit-print--redirect-streams
  (:doc "`zenit-print--redirect-streams' is defined")
  (should (fboundp 'zenit-print--redirect-streams)))

(zenit-deftest zenit-print--redirect-standard-output
  (:doc "`zenit-print--redirect-standard-output' is defined")
  (should (fboundp 'zenit-print--redirect-standard-output)))

(zenit-deftest zenit-print--redirect-message
  (:doc "`zenit-print--redirect-message' is defined")
  (should (fboundp 'zenit-print--redirect-message)))

(zenit-deftest zenit-print--format
  (:doc "`zenit-print--format' is defined")
  (should (fboundp 'zenit-print--format)))

(zenit-deftest zenit-print--indent
  (:doc "`zenit-print--indent' is defined")
  (should (fboundp 'zenit-print--indent)))

(zenit-deftest zenit-print--fill
  (:doc "`zenit-print--fill' is defined")
  (should (fboundp 'zenit-print--fill)))

(zenit-deftest zenit-print--paragraph
  (:doc "`zenit-print--paragraph' is defined")
  (should (fboundp 'zenit-print--paragraph)))

(zenit-deftest zenit-print--join
  (:doc "`zenit-print--join' is defined")
  (should (fboundp 'zenit-print--join)))

(zenit-deftest zenit-print--truncate
  (:doc "`zenit-print--truncate' is defined")
  (should (fboundp 'zenit-print--truncate)))

(zenit-deftest zenit-print--buffer
  (:doc "`zenit-print--buffer' is defined")
  (should (fboundp 'zenit-print--buffer)))

(zenit-deftest zenit-print--cli-markup
  (:doc "`zenit-print--cli-markup' is defined")
  (should (fboundp 'zenit-print--cli-markup)))

(zenit-deftest zenit-print--org
  (:doc "`zenit-print--org' is defined")
  (should (fboundp 'zenit-print--org)))

(zenit-deftest zenit-print--style
  (:doc "`zenit-print--style' is defined")
  (should (fboundp 'zenit-print--style)))

(zenit-deftest zenit-print--class
  (:doc "`zenit-print--class' is defined")
  (should (fboundp 'zenit-print--class)))

(zenit-deftest zenit-print--apply
  (:doc "`zenit-print--apply' is defined")
  (should (fboundp 'zenit-print--apply)))

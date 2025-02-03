;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-autoloads.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'autoloads)

(zenit-deftest zenit-autoloads-excluded-packages
  (:doc "`zenit-autoloads-excluded-packages' is defined")
  (should (boundp 'zenit-autoloads-excluded-packages)))

(zenit-deftest zenit-autoloads-excluded-files
  (:doc "`zenit-autoloads-excluded-files' is defined")
  (should (boundp 'zenit-autoloads-excluded-files)))

(zenit-deftest zenit-autoloads-cached-vars
  (:doc "`zenit-autoloads-cached-vars' is defined")
  (should (boundp 'zenit-autoloads-cached-vars)))

(zenit-deftest zenit-autoloads-files
  (:doc "`zenit-autoloads-files' is defined")
  (should (boundp 'zenit-autoloads-files)))

(zenit-deftest zenit-autoloads--compile-file
  (:doc "`zenit-autoloads--compile-file' is defined")
  (should (fboundp 'zenit-autoloads--compile-file)))

(zenit-deftest zenit-autoloads--cleanup-form
  (:doc "`zenit-autoloads--cleanup-form' is defined")
  (should (fboundp 'zenit-autoloads--cleanup-form)))

(zenit-deftest zenit-autoloads--scan-autodefs
  (:doc "`zenit-autoloads--scan-autodefs' is defined")
  (should (fboundp 'zenit-autoloads--scan-autodefs)))

(zenit-deftest zenit-loaddefs-temp-file
  (:doc "`zenit-loaddefs-temp-file' is defined")
  (should (boundp 'zenit-loaddefs-temp-file)))

(zenit-deftest zenit-autoloads--scan-file
  (:doc "`zenit-autoloads--scan-file' is defined")
  (should (fboundp 'zenit-autoloads--scan-file)))

(zenit-deftest zenit-autoloads--scan
  (:doc "`zenit-autoloads--scan' is defined")
  (should (fboundp 'zenit-autoloads--scan)))

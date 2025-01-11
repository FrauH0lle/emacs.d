;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-regexp.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'regexp)

(zenit-deftest zenit-pcre-quote
  (:doc "`zenit-pcre-quote' escapes special character")
  (should (equal "1\\.\\^\\$\\*\\+\\?\\{}\\\\\\[\\|\\(2"
                 (zenit-pcre-quote "1.^$*+?{}\\[|(2"))))

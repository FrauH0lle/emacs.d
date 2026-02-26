;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-sessions.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'sessions)

(zenit-deftest zenit-session-file
  (:doc "`zenit-session-file' is defined")
  (should (fboundp 'zenit-session-file)))

(zenit-deftest zenit-save-session
  (:doc "`zenit-save-session' is defined")
  (should (fboundp 'zenit-save-session)))

(zenit-deftest zenit-load-session
  (:doc "`zenit-load-session' is defined")
  (should (fboundp 'zenit-load-session)))

(zenit-deftest zenit/quickload-session
  (:doc "`zenit/quickload-session' is defined")
  (should (fboundp 'zenit/quickload-session)))

(zenit-deftest zenit/quicksave-session
  (:doc "`zenit/quicksave-session' is defined")
  (should (fboundp 'zenit/quicksave-session)))

(zenit-deftest zenit/load-session
  (:doc "`zenit/load-session' is defined")
  (should (fboundp 'zenit/load-session)))

(zenit-deftest zenit/save-session
  (:doc "`zenit/save-session' is defined")
  (should (fboundp 'zenit/save-session)))

(zenit-deftest zenit/restart
  (:doc "`zenit/restart' is defined")
  (should (fboundp 'zenit/restart)))

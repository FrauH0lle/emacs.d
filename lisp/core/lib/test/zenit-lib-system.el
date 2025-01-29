;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-system.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'system)
(zenit-require 'zenit-lib 'process)

(zenit-deftest zenit-system-cpus
  (:doc "`zenit-system-cpus' returns 1 or more")
  (should (<= 1 (zenit-system-cpus))))

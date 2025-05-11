;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/zenit-projects.el

(require 'zenit-test)
(require 'zenit-use-package)
(require 'zenit-projects)

(zenit-deftest zenit-fd-executable
  (:doc "`zenit-fd-executable' is defined")
  (should (boundp 'zenit-fd-executable)))

(zenit-deftest zenit-ripgrep-executable
  (:doc "`zenit-ripgrep-executable' is defined")
  (should (boundp 'zenit-ripgrep-executable)))

(zenit-deftest zenit-projectile-cache-dir
  (:doc "`zenit-projectile-cache-dir' is defined")
  (should (boundp 'zenit-projectile-cache-dir)))

(zenit-deftest zenit-project-hook
  (:doc "`zenit-project-hook' is defined")
  (should (boundp 'zenit-project-hook)))

(zenit-deftest def-project-mode!
  (:doc "`def-project-mode!' is defined")
  (should (fboundp 'def-project-mode!)))

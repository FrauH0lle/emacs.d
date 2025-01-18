;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-packages.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'packages)

(zenit-deftest zenit/search-versions
  (:doc "`zenit/search-versions' is defined")
  (should (fboundp 'zenit/search-versions)))

(zenit-deftest zenit/find-duplicate-package-versions
  (:doc "`zenit/find-duplicate-package-versions' is defined")
  (should (fboundp 'zenit/find-duplicate-package-versions)))

(zenit-deftest zenit/bump-package
  (:doc "`zenit/bump-package' is defined")
  (should (fboundp 'zenit/bump-package)))

(zenit-deftest zenit/bump-module
  (:doc "`zenit/bump-module' is defined")
  (should (fboundp 'zenit/bump-module)))

(zenit-deftest zenit-package-homepage
  (:doc "`zenit-package-homepage' is defined")
  (should (fboundp 'zenit-package-homepage)))

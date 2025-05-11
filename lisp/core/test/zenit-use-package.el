;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/zenit-use-package.el

(require 'zenit-test)
(require 'zenit-use-package)
(require 'use-package)

(zenit-deftest zenit--deferred-packages-alist
  (:doc "`zenit--deferred-packages-alist' is defined")
  (should (boundp 'zenit--deferred-packages-alist)))

(zenit-deftest zenit--resolve-load-path-from-containg-file-a
  (:doc "`zenit--resolve-load-path-from-containg-file-a' advises `use-package-normalize-paths'")
  (progn
    (should (fboundp 'zenit--resolve-load-path-from-containg-file-a))
    (should (advice-member-p 'zenit--resolve-load-path-from-containg-file-a #'use-package-normalize-paths))))

(zenit-deftest use-package-handler/:defer-incrementally
  (:doc "`use-package-handler/:defer-incrementally' is defined")
  (should (fboundp 'use-package-handler/:defer-incrementally)))

(zenit-deftest use-package-handler/:after-call
  (:doc "`use-package-handler/:after-call' is defined")
  (should (fboundp 'use-package-handler/:after-call)))

(zenit-deftest use-package!
  (:doc "`use-package!' is defined")
  (should (fboundp 'use-package!)))

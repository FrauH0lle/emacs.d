;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-core-zenit-use-package.el

(describe "core/zenit-use-package"

  (load! "zenit-use-package" zenit-core-dir)
  (require 'use-package)

  (describe "zenit--resolve-load-path-from-containg-file-a"
    (it "is defined"
      (expect (fboundp 'zenit--resolve-load-path-from-containg-file-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p 'zenit--resolve-load-path-from-containg-file-a 'use-package-normalize-paths) :to-be-truthy)))


  (describe "use-package!"
    (it "expands into protected use-package form"
      (expect '(use-package! test-package)
              :to-expand-into
              '(protect-macros-maybe! test-package
                 (use-package test-package))))

    (it "ignores disabled packages"
      (defvar zenit-disabled-packages ())
      (cl-pushnew 'test-package zenit-disabled-packages)
      (expect '(use-package! test-package)
              :to-expand-into
              nil))))

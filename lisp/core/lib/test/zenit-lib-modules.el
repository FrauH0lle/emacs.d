;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-modules.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'modules)

(zenit-deftest zenit-modules
  (:doc "`zenit-modules' is defined")
  (should (boundp 'zenit-modules)))

(zenit-deftest zenit-inhibit-module-warnings
  (:doc "`zenit-inhibit-module-warnings' is defined")
  (should (boundp 'zenit-inhibit-module-warnings)))

(zenit-deftest zenit-modules-initialize
  (:doc "`zenit-modules-initialize' is defined")
  (should (fboundp 'zenit-modules-initialize)))


(zenit-deftest zenit-module-mplist-map
  (:vars ((test-results nil)))
  ,test
  (test)
  :doc "`zenit-module-mplist-map' applies function to each module"
  (let ((mplist '(:cat1 mod1 mod2 :cat2 mod3)))
    (setq test-results nil)
    (zenit-module-mplist-map
     (lambda (category module &rest plist)
       (push (list category module (plist-get plist :flags)) test-results))
     mplist)
    (should (equal '((:cat1 mod1 nil)
                     (:cat1 mod2 nil)
                     (:cat2 mod3 nil))
                   (reverse test-results))))

  :doc "`zenit-module-mplist-map' handles module flags"
  (let ((mplist '(:cat1 (mod1 +flag1 +flag2) mod2 :cat2 (mod3 +flag3))))
    (setq test-results nil)
    (zenit-module-mplist-map
     (lambda (category module &rest plist)
       (push (list category module (plist-get plist :flags)) test-results))
     mplist)
    (should (equal '((:cat1 mod1 (+flag1 +flag2))
                     (:cat1 mod2 nil)
                     (:cat2 mod3 (+flag3)))
                   (reverse test-results)))))

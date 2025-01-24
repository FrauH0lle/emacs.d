;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-store.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'store)
(zenit-require 'zenit-lib 'files)

(zenit-deftest zenit-store-dir
  (:doc "`zenit-store-dir' is defined")
  (should (boundp 'zenit-store-dir)))

(zenit-deftest zenit-store-persist-alist
  (:doc "`zenit-store-persist-alist' is defined")
  (should (boundp 'zenit-store-persist-alist)))

(zenit-deftest zenit-store-location
  (:doc "`zenit-store-location' is defined")
  (should (boundp 'zenit-store-location)))

(zenit-deftest zenit--store-table
  (:doc "`zenit--store-table' is defined")
  (should (boundp 'zenit--store-table)))

(zenit-deftest zenit-save-persistent-store-h
  (:doc "`zenit-save-persistent-store-h' is defined")
  (progn
    (should (fboundp 'zenit-save-persistent-store-h))
    (should (member 'zenit-save-persistent-store-h kill-emacs-hook))))

(zenit-deftest zenit-store-persist
  (:doc "`zenit-store-persist' stores variables in `zenit-store-persist-alist'")
  (let ((zenit-store-persist-alist nil))
    (zenit-store-persist "test" '(kill-emacs-hook zenit-store-dir))
    (should (zenit-test-contains-items-p
             '(("test" kill-emacs-hook zenit-store-dir))
             zenit-store-persist-alist
             :test #'equal))))

(zenit-deftest zenit-store-desist
  (:doc "`zenit-store-desist' remove variables from `zenit-store-persist-alist'")
  (let ((zenit-store-persist-alist '(("test-1" kill-emacs-hook zenit-store-dir)
                                     ("test-2" tab-width))))
    (zenit-store-desist "test-1" '(zenit-store-dir))
    (should (zenit-test-contains-items-p
             '(("test-1" kill-emacs-hook)
               ("test-2" tab-width))
             zenit-store-persist-alist
             :test #'equal))))

(zenit-deftest zenit--store-init
  (:doc "`zenit--store-init' initializes a store"
   :after-each (setq zenit--store-table (make-hash-table :test 'equal)))
  (progn
    (zenit--store-init)
    (should (gethash "default" zenit--store-table))))

(zenit-deftest zenit--store-expired-p
  (:doc "`zenit--store-expired-p' returns t is data is expired")
  (let ((data (cons (time-add (current-time) -500) "test-val")))
    (should (zenit--store-expired-p "test-val" data))))

(zenit-deftest zenit--store-flush
  (:doc "`zenit--store-flush' writes file to disk")
  (let ((zenit-store-dir (zenit-test-make-temp-file t)))
    (zenit--store-flush zenit-store-location)
    (should (file-exists-p (expand-file-name zenit-store-location zenit-store-dir)))
    (delete-directory zenit-store-dir t)))

(zenit-deftest zenit-store-get
  (:doc "`zenit-store-get' retrieves key from store"
   :after-each (setq zenit--store-table (make-hash-table :test 'equal)))
  (progn
    (zenit-store-put "test-key" "test-val" nil "test" t)
    (should (equal "test-val" (zenit-store-get "test-key" "test" nil t)))))

(zenit-deftest zenit-store-put
  (:doc "`zenit-store-put' sets a key in store"
   :after-each (setq zenit--store-table (make-hash-table :test 'equal)))
  (progn
    (zenit-store-put "test-key" "test-val" nil "test" t)
    (should (equal "test-val" (zenit-store-get "test-key" "test" nil t)))))

(zenit-deftest zenit-store-rem
  (:doc "`zenit-store-rem' removes key from store"
   :after-each (setq zenit--store-table (make-hash-table :test 'equal)))
  (progn
    (zenit-store-put "test-key" "test-val" nil "test" t)
    (zenit-store-rem "test-key" "test" t)
    (should (equal nil (zenit-store-get "test-key" "test" nil t)))))

(zenit-deftest zenit-store-member-p
  (:doc "`zenit-store-member-p' returns t if key in location exists"
   :after-each (setq zenit--store-table (make-hash-table :test 'equal)))
  (progn
    (zenit-store-put "test-key" "test-val" nil "test" t)
    (should (zenit-store-member-p "test-key" "test"))))

(zenit-deftest zenit-store-clear
  (:doc "`zenit-store-clear' clears the store at location"
   :after-each (setq zenit--store-table (make-hash-table :test 'equal)))
  (let ((zenit-store-dir (zenit-test-make-temp-file t)))
    (zenit-store-put "test-key" "test-val" nil "test")
    (should (file-exists-p (expand-file-name "test" zenit-store-dir)))
    (should (zenit-store-clear "test"))
    (should-not (file-exists-p (expand-file-name "test" zenit-store-dir)))
    (delete-directory zenit-store-dir t)))

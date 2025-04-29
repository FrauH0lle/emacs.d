;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/zenit-start.el

(require 'zenit-test)
(require 'zenit-start)

(zenit-deftest zenit-first-input-hook
  (:doc "`zenit-first-input-hook' is defined")
  (should (boundp 'zenit-first-input-hook)))

(zenit-deftest zenit-first-file-hook
  (:doc "`zenit-first-file-hook' is defined")
  (should (boundp 'zenit-first-file-hook)))

(zenit-deftest zenit-first-buffer-hook
  (:doc "`zenit-first-buffer-hook' is defined")
  (should (boundp 'zenit-first-buffer-hook)))

(zenit-deftest zenit-inhibit-local-var-hooks
  (:doc "`zenit-inhibit-local-var-hooks' is defined")
  (should (boundp 'zenit-inhibit-local-var-hooks)))

(zenit-deftest zenit-run-local-var-hooks-h
  (:doc "`zenit-run-local-var-hooks-h' is defined")
  (should (fboundp 'zenit-run-local-var-hooks-h)))

(zenit-deftest zenit-run-local-var-hooks-maybe-h
  (:doc "`zenit-run-local-var-hooks-maybe-h' is defined")
  (should (fboundp 'zenit-run-local-var-hooks-maybe-h)))

(zenit-deftest zenit-incremental-packages
  (:doc "`zenit-incremental-packages' is defined")
  (should (boundp 'zenit-incremental-packages)))

(zenit-deftest zenit-incremental-first-idle-timer
  (:doc "`zenit-incremental-first-idle-timer' is defined")
  (should (boundp 'zenit-incremental-first-idle-timer)))

(zenit-deftest zenit-incremental-idle-timer
  (:doc "`zenit-incremental-idle-timer' is defined")
  (should (boundp 'zenit-incremental-idle-timer)))

(zenit-deftest zenit-load-packages-incrementally
  (:doc "`zenit-load-packages-incrementally' is defined")
  (should (fboundp 'zenit-load-packages-incrementally)))

(zenit-deftest zenit-load-packages-incrementally-h
  (:doc "`zenit-load-packages-incrementally-h' is defined")
  (progn
    (should (fboundp 'zenit-load-packages-incrementally-h))
    (should (member 'zenit-load-packages-incrementally-h zenit-after-init-hook))))

(zenit-deftest zenit-display-benchmark-h
  (:doc "`zenit-display-benchmark-h' is defined")
  (progn
    (should (fboundp 'zenit-display-benchmark-h))
    (should (member 'zenit-display-benchmark-h zenit-after-init-hook))))

(zenit-deftest zenit-run-first-hooks-if-files-open-h
  (:doc "`zenit-run-first-hooks-if-files-open-h' is a member of `zenit-after-init-hook'")
  (progn
    (should (fboundp 'zenit-run-first-hooks-if-files-open-h))
    (should (member #'zenit-run-first-hooks-if-files-open-h zenit-after-init-hook))))

(zenit-deftest zenit-init-local-var-hooks-h
  (:doc "`zenit-init-local-var-hooks-h' is a member of `after-init-hook'")
  (progn
    (should (fboundp 'zenit-init-local-var-hooks-h))
    (should (member #'zenit-init-local-var-hooks-h after-init-hook))))

(zenit-deftest startup--load-user-init-file@init-zenit
  (:doc "`startup--load-user-init-file@init-zenit' advises `startup--load-user-init-file'")
  (progn
    (should (fboundp 'startup--load-user-init-file@init-zenit))
    (should (advice-member-p 'startup--load-user-init-file@init-zenit #'startup--load-user-init-file))))

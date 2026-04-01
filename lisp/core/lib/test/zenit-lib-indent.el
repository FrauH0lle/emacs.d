;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-indent.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'indent)

(zenit-deftest zenit-indent-excluded-modes
  (:doc "`zenit-indent-excluded-modes' is defined and contains expected modes")
  ,test
  (test)
  (should (boundp 'zenit-indent-excluded-modes))
  (should (memq 'special-mode zenit-indent-excluded-modes))
  (should (memq 'org-mode zenit-indent-excluded-modes)))

(zenit-deftest set-indent-vars!
  (:vars ((test-mode (gensym "test-mode-")))
   :after-each
   (put test-mode 'indent-vars nil))
  ,test
  (test)
  :doc "`set-indent-vars!' registers indent variables for a single mode"
  (progn
    (set-indent-vars! test-mode 'test-indent-var)
    (should (equal '(test-indent-var) (get test-mode 'indent-vars))))

  :doc "`set-indent-vars!' registers multiple indent variables for a mode"
  (progn
    (set-indent-vars! test-mode '(test-indent-var1 test-indent-var2))
    (should (equal '(test-indent-var1 test-indent-var2) (get test-mode 'indent-vars))))

  :doc "`set-indent-vars!' registers indent variables for all modes when modes is t"
  (progn
    (set-indent-vars! t 'test-indent-var)
    (should (equal '(test-indent-var) (get 'tab-width 'indent-vars)))))

(zenit-deftest zenit-indent-var-for-mode
  (:doc "`zenit-indent-var-for-mode' returns nil for excluded and fundamental modes")
  ,test
  (test)
  :doc "`zenit-indent-var-for-mode' returns nil for excluded modes"
  (should-not (zenit-indent-var-for-mode 'special-mode))
  (should-not (zenit-indent-var-for-mode 'org-mode))

  :doc "`zenit-indent-var-for-mode' returns nil for fundamental-mode"
  (should-not (zenit-indent-var-for-mode 'fundamental-mode)))

(zenit-deftest zenit-indent-vars-for-mode
  (:vars ((test-mode (gensym "test-mode-")))
   :after-each
   (put test-mode 'indent-vars nil))
  ,test
  (test)
  :doc "`zenit-indent-vars-for-mode' returns nil for fundamental-mode"
  (should-not (zenit-indent-vars-for-mode 'fundamental-mode))

  :doc "`zenit-indent-vars-for-mode' returns registered indent-vars"
  (progn
    (set-indent-vars! test-mode 'test-indent-var)
    (should (equal '(test-indent-var) (zenit-indent-vars-for-mode test-mode))))

  :doc "`zenit-indent-vars-for-mode' returns list from registered indent vars"
  (progn
    (set-indent-vars! test-mode '(test-indent-var1 test-indent-var2))
    (should (equal '(test-indent-var1 test-indent-var2) (zenit-indent-vars-for-mode test-mode)))))

(zenit-deftest zenit-set-indent
  (:vars ((test-buffer (get-buffer-create "test-buffer"))
          (test-mode (gensym "test-mode-")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (progn
     (kill-buffer test-buffer)
     (put test-mode 'indent-vars nil)))
  ,test
  (test)
  :doc "`zenit-set-indent' sets tab-width and standard-indent from mode's indent variable"
  (with-current-buffer test-buffer
    ;; Create a variable and register it
    (setq-local test-indent-var 4)
    (set-indent-vars! test-mode 'test-indent-var)
    (setq major-mode test-mode)
    (zenit-set-indent)
    (should (= 4 tab-width))
    (should (= 4 standard-indent)))

  :doc "`zenit-set-indent' handles multiple indent variables"
  (with-current-buffer test-buffer
    (setq-local test-indent-var1 2)
    (setq-local test-indent-var2 4)
    (set-indent-vars! test-mode '(test-indent-var1 test-indent-var2))
    (setq major-mode test-mode)
    (zenit-set-indent)
    ;; should use the first variable's value
    (should (= 2 tab-width))
    (should (= 2 standard-indent))))

(zenit-deftest zenit/set-indent-width
  (:vars ((test-buffer (get-buffer-create "test-buffer"))
          (test-mode (gensym "test-mode-")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (progn
     (kill-buffer test-buffer)
     (put test-mode 'indent-vars nil)))
  ,test
  (test)
  :doc "`zenit/set-indent-width' sets tab-width to the specified value"
  (with-current-buffer test-buffer
    ;; Set up a mode with indent variables
    (setq-local test-indent-var 8)
    (set-indent-vars! test-mode 'test-indent-var)
    (setq major-mode test-mode)
    (quiet!!
      (zenit/set-indent-width 2))
    (should (= 2 tab-width)))

  :doc "`zenit/set-indent-width' sets standard-indent to the specified value"
  (with-current-buffer test-buffer
    ;; Set up a mode with indent variables
    (setq-local test-indent-var 8)
    (set-indent-vars! test-mode 'test-indent-var)
    (setq major-mode test-mode)
    (quiet!!
      (zenit/set-indent-width 4))
    (should (= 4 standard-indent))))

(zenit-deftest zenit/toggle-indent-style
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit/toggle-indent-style' toggles indent-tabs-mode"
  (with-current-buffer test-buffer
    (let ((initial-value indent-tabs-mode))
      (quiet!!
        (zenit/toggle-indent-style))
      (should-not (equal initial-value indent-tabs-mode))
      (quiet!!
        (zenit/toggle-indent-style))
      (should (equal initial-value indent-tabs-mode)))))

(zenit-deftest zenit/retab
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit/retab' converts tabs to spaces when indent-tabs-mode is nil"
  (with-current-buffer test-buffer
    (insert "First line\n\tSecond line")
    (setq tab-width 4)
    (let ((indent-tabs-mode nil))
      (zenit/retab nil (point-min) (point-max))
      (should (equal "First line\n    Second line" (buffer-string)))))

  :doc "`zenit/retab' with ARG uses opposite indentation style"
  (with-current-buffer test-buffer
    (insert "First line\n    Second line")
    (setq tab-width 4
          indent-tabs-mode nil)
    (zenit/retab t (point-min) (point-max))
    (should (equal "First line\n\tSecond line" (buffer-string)))))

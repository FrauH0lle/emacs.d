;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; editor/word-wrap/test/autoload.el

(require 'cl-lib)
(require 'zenit-test)
(zenit-require 'zenit-lib 'modules)
(zenit-modules-initialize)
(zenit-load (zenit-module-locate-path :editor 'word-wrap "autoload.el"))

(defvar +word-wrap-extra-indent)
(defvar test-indent-offset)

(zenit-deftest +word-wrap--major-mode-indent-width
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`+word-wrap--major-mode-indent-width' accepts a list of indent vars"
  (with-current-buffer test-buffer
    (setq-local test-indent-offset 2
                tab-width 8
                +word-wrap--major-mode-indent-var
                '(missing-indent-var test-indent-offset tab-width))
    (should (= 2 (+word-wrap--major-mode-indent-width))))

  :doc "`+word-wrap--major-mode-indent-width' falls back to tab-width"
  (with-current-buffer test-buffer
    (setq-local tab-width 4
                +word-wrap--major-mode-indent-var '(missing-indent-var))
    (should (= 4 (+word-wrap--major-mode-indent-width)))))

(zenit-deftest +word-wrap--calc-extra-indent
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`+word-wrap--calc-extra-indent' handles list-backed indent vars"
  (with-current-buffer test-buffer
    (setq-local test-indent-offset 3
                tab-width 8
                +word-wrap--major-mode-indent-var
                '(missing-indent-var test-indent-offset tab-width)
                +word-wrap--major-mode-is-text nil)
    (let ((+word-wrap-extra-indent 'double))
      (cl-letf (((symbol-function 'sp-point-in-string-or-comment)
                 (lambda (_) nil)))
        (should (= 6 (+word-wrap--calc-extra-indent (point-min))))))))

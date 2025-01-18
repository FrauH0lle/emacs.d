;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-help.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'help)

(zenit-deftest zenit-docs-dir
  (:doc "`zenit-docs-dir' is defined")
  (should (boundp 'zenit-docs-dir)))

(zenit-deftest zenit--help-major-mode-module-alist
  (:doc "`zenit--help-major-mode-module-alist' is defined")
  (should (boundp 'zenit--help-major-mode-module-alist)))

(zenit-deftest zenit-active-minor-modes
  (:doc "`zenit-active-minor-modes' is defined")
  (let ((buffer (get-buffer-create "test")))
    (with-current-buffer buffer
      (prog-mode)
      (global-eldoc-mode +1)
      (line-number-mode +1)
      (should (zenit-test-contains-items-p
               '(global-eldoc-mode line-number-mode) (zenit-active-minor-modes))))
    (kill-buffer buffer)))

(zenit-deftest zenit/describe-active-minor-mode
  (:doc "`zenit/describe-active-minor-mode' is defined")
  (should (fboundp 'zenit/describe-active-minor-mode)))

(zenit-deftest zenit--org-headings
  (:doc "`zenit--org-headings' is defined")
  (should (fboundp 'zenit--org-headings)))

(zenit-deftest zenit-completing-read-org-headings
  (:doc "`zenit-completing-read-org-headings' is defined")
  (should (fboundp 'zenit-completing-read-org-headings)))

(zenit-deftest zenit/homepage
  (:doc "`zenit/homepage' is defined")
  (should (fboundp 'zenit/homepage)))

(zenit-deftest zenit/issue-tracker
  (:doc "`zenit/issue-tracker' is defined")
  (should (fboundp 'zenit/issue-tracker)))

(zenit-deftest zenit/report-bug
  (:doc "`zenit/report-bug' is defined")
  (should (fboundp 'zenit/report-bug)))

(zenit-deftest zenit/help-search-headings
  (:doc "`zenit/help-search-headings' is defined")
  (should (fboundp 'zenit/help-search-headings)))

(zenit-deftest zenit/help-search
  (:doc "`zenit/help-search' is defined")
  (should (fboundp 'zenit/help-search)))

(zenit-deftest zenit/help-autodefs
  (:doc "`zenit/help-autodefs' is defined")
  (should (fboundp 'zenit/help-autodefs)))

(zenit-deftest zenit--help-modules-list
  (:doc "`zenit--help-modules-list' is defined")
  (should (fboundp 'zenit--help-modules-list)))

(zenit-deftest zenit--help-current-module-str
  (:doc "`zenit--help-current-module-str' is defined")
  (should (fboundp 'zenit--help-current-module-str)))

(zenit-deftest zenit/help-modules
  (:doc "`zenit/help-modules' is defined")
  (should (fboundp 'zenit/help-modules)))

(zenit-deftest zenit--help-variable-p
  (:doc "`zenit--help-variable-p' is defined")
  (should (fboundp 'zenit--help-variable-p)))

(zenit-deftest zenit/help-custom-variable
  (:doc "`zenit/help-custom-variable' is defined")
  (should (fboundp 'zenit/help-custom-variable)))

(zenit-deftest zenit--help-insert-button
  (:doc "`zenit--help-insert-button' is defined")
  (should (fboundp 'zenit--help-insert-button)))

(zenit-deftest zenit--help-package-configs
  (:doc "`zenit--help-package-configs' is defined")
  (should (fboundp 'zenit--help-package-configs)))

(zenit-deftest zenit--help-packages-list
  (:doc "`zenit--help-packages-list' is defined")
  (should (boundp 'zenit--help-packages-list)))

(zenit-deftest zenit/help-packages
  (:doc "`zenit/help-packages' is defined")
  (should (fboundp 'zenit/help-packages)))

(zenit-deftest zenit--package-cache
  (:doc "`zenit--package-cache' is defined")
  (should (boundp 'zenit--package-cache)))

(zenit-deftest zenit--package-list
  (:doc "`zenit--package-list' is defined")
  (should (fboundp 'zenit--package-list)))

(zenit-deftest zenit/help-package-config
  (:doc "`zenit/help-package-config' is defined")
  (should (fboundp 'zenit/help-package-config)))

(zenit-deftest zenit--help-search-prompt
  (:doc "`zenit--help-search-prompt' is defined")
  (should (fboundp 'zenit--help-search-prompt)))

(zenit-deftest zenit--help-search
  (:doc "`zenit--help-search' is defined")
  (should (fboundp 'zenit--help-search)))

(zenit-deftest zenit/help-search-load-path
  (:doc "`zenit/help-search-load-path' is defined")
  (should (fboundp 'zenit/help-search-load-path)))

(zenit-deftest zenit/help-search-loaded-files
  (:doc "`zenit/help-search-loaded-files' is defined")
  (should (fboundp 'zenit/help-search-loaded-files)))

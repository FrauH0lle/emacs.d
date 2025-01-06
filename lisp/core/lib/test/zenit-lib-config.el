;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-config.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'config)

(zenit-deftest zenit/open-local-config
  (:doc "`zenit/open-local-config' is defined")
  (should (fboundp 'zenit/open-local-config)))

(zenit-deftest zenit/find-file-in-local-config
  (:doc "`zenit/find-file-in-local-config' is defined")
  (should (fboundp 'zenit/find-file-in-local-config)))

(zenit-deftest zenit/goto-local-init-file
  (:doc "`zenit/goto-local-init-file' is defined")
  (should (fboundp 'zenit/goto-local-init-file)))

(zenit-deftest zenit/goto-local-config-file
  (:doc "`zenit/goto-local-config-file' is defined")
  (should (fboundp 'zenit/goto-local-config-file)))

(zenit-deftest zenit/goto-local-packages-file
  (:doc "`zenit/goto-local-packages-file' is defined")
  (should (fboundp 'zenit/goto-local-packages-file)))

(zenit-deftest zenit/open-in-magit
  (:doc "`zenit/open-in-magit' is defined")
  (should (fboundp 'zenit/open-in-magit)))

(zenit-deftest zenit/search-in-emacsd
  (:doc "`zenit/search-in-emacsd' is defined")
  (should (fboundp 'zenit/search-in-emacsd)))

(zenit-deftest zenit/browse-emacsd
  (:doc "`zenit/browse-emacsd' is defined")
  (should (fboundp 'zenit/browse-emacsd)))

(zenit-deftest zenit/find-in-emacsd
  (:doc "`zenit/find-in-emacsd' is defined")
  (should (fboundp 'zenit/find-in-emacsd)))

(zenit-deftest zenit/insert-date ()
  (should (string-match-p ,out (with-temp-buffer
                                 (zenit/insert-date ,in)
                                 (buffer-string))))
  (in out)
  nil "[0-9]\\{4\\}\\-[0-9]\\{2\\}\\-[0-9]\\{2\\}"
  t "[0-9]\\{2\\}\\.[0-9]\\{2\\}\\.[0-9]\\{4\\}")

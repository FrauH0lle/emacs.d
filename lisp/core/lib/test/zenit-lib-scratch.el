;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-scratch.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'scratch)
(zenit-require 'zenit-lib 'files)

(zenit-deftest zenit-scratch-default-file
  (:doc "`zenit-scratch-default-file' is defined")
  (should (boundp 'zenit-scratch-default-file)))

(zenit-deftest zenit-scratch-dir
  (:doc "`zenit-scratch-dir' is defined")
  (should (boundp 'zenit-scratch-dir)))

(zenit-deftest zenit-scratch-initial-major-mode
  (:doc "`zenit-scratch-initial-major-mode' is defined")
  (should (boundp 'zenit-scratch-initial-major-mode)))

(zenit-deftest zenit-scratch-buffers
  (:doc "`zenit-scratch-buffers' is defined")
  (should (boundp 'zenit-scratch-buffers)))

(zenit-deftest zenit-scratch-current-project
  (:doc "`zenit-scratch-current-project' is defined")
  (should (boundp 'zenit-scratch-current-project)))

(zenit-deftest zenit-scratch-buffer-hook
  (:doc "`zenit-scratch-buffer-hook' is defined")
  (should (boundp 'zenit-scratch-buffer-hook)))

(zenit-deftest zenit--load-persistent-scratch-buffer
  (:doc "`zenit--load-persistent-scratch-buffer' loads persisted scratch buffer")
  (let* ((buffer (get-buffer-create "test"))
         (test-dir (zenit-test-make-temp-file t))
         (test-file (file-name-concat test-dir (concat zenit-scratch-default-file ".el")))
         (zenit-scratch-dir test-dir))
    (zenit-file-write test-file `(("Hello World\n" ,(point-min) ,major-mode)))
    (with-current-buffer buffer
      (zenit--load-persistent-scratch-buffer nil)
      (should (equal "Hello World\n" (buffer-string))))
    (kill-buffer buffer)
    (delete-directory test-dir t)))

(zenit-deftest zenit-scratch-buffer
  (:doc "`zenit-scratch-buffer' is defined")
  (should (fboundp 'zenit-scratch-buffer)))

(zenit-deftest zenit-persist-scratch-buffer-h
  (:doc "`zenit-persist-scratch-buffer-h' saves current buffer to scratch directory")
  (let* ((buffer (get-buffer-create "test"))
         (test-dir (zenit-test-make-temp-file t))
         (test-file (file-name-concat test-dir (concat zenit-scratch-default-file ".el")))
         (zenit-scratch-dir test-dir))
    (with-current-buffer buffer
      (insert "(message \"Hello World\")")
      (goto-char (point-min))
      (zenit-persist-scratch-buffer-h))
    (should (equal '("(message \"Hello World\")" 1 fundamental-mode) (zenit-file-read test-file :by 'read)))
    (kill-buffer buffer)
    (delete-directory test-dir t)))

(zenit-deftest zenit--persist-scratch-buffers-h
  (:doc "`zenit--persist-scratch-buffers-h' is defined")
  (should (fboundp 'zenit--persist-scratch-buffers-h)))

(zenit-deftest zenit-persist-scratch-buffers-after-switch-h
  (:doc "`zenit-persist-scratch-buffers-after-switch-h' is defined")
  (should (fboundp 'zenit-persist-scratch-buffers-after-switch-h)))

(zenit-deftest zenit/open-scratch-buffer
  (:doc "`zenit/open-scratch-buffer' is defined")
  (should (fboundp 'zenit/open-scratch-buffer)))

(zenit-deftest zenit/switch-to-scratch-buffer
  (:doc "`zenit/switch-to-scratch-buffer' is defined")
  (should (fboundp 'zenit/switch-to-scratch-buffer)))

(zenit-deftest zenit/open-project-scratch-buffer
  (:doc "`zenit/open-project-scratch-buffer' is defined")
  (should (fboundp 'zenit/open-project-scratch-buffer)))

(zenit-deftest zenit/switch-to-project-scratch-buffer
  (:doc "`zenit/switch-to-project-scratch-buffer' is defined")
  (should (fboundp 'zenit/switch-to-project-scratch-buffer)))

(zenit-deftest zenit/revert-scratch-buffer
  (:doc "`zenit/revert-scratch-buffer' is defined")
  (should (fboundp 'zenit/revert-scratch-buffer)))

(zenit-deftest zenit/delete-persistent-scratch-file
  (:doc "`zenit/delete-persistent-scratch-file' is defined")
  (should (fboundp 'zenit/delete-persistent-scratch-file)))

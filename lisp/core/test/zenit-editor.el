;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/zenit-editor.el

(require 'zenit-test)
(require 'zenit-use-package)
(zenit-require 'zenit-lib 'ui)
(require 'gcmh)
(require 'zenit-editor)

(zenit-deftest zenit-detect-indentation-excluded-modes
  (:doc "`zenit-detect-indentation-excluded-modes' is defined")
  (should (boundp 'zenit-detect-indentation-excluded-modes)))

(zenit-deftest zenit-inhibit-indent-detection
  (:doc "`zenit-inhibit-indent-detection' is defined")
  (should (boundp 'zenit-inhibit-indent-detection)))

(zenit-deftest zenit-inhibit-large-file-detection
  (:doc "`zenit-inhibit-large-file-detection' is defined")
  (should (boundp 'zenit-inhibit-large-file-detection)))

(zenit-deftest zenit-large-file-size-alist
  (:doc "`zenit-large-file-size-alist' is defined")
  (should (boundp 'zenit-large-file-size-alist)))

(zenit-deftest zenit--prepare-for-large-files-a
  (:doc "`zenit--prepare-for-large-files-a' advises `abort-if-file-too-large'")
  (progn
    (should (fboundp 'zenit--prepare-for-large-files-a))
    (should (advice-member-p 'zenit--prepare-for-large-files-a #'abort-if-file-too-large))))

(zenit-deftest zenit-optimize-for-large-files-h
  (:doc "`zenit-optimize-for-large-files-h' is a member of `find-file-hook'")
  (progn
    (should (fboundp 'zenit-optimize-for-large-files-h))
    (should (member #'zenit-optimize-for-large-files-h find-file-hook))))

(zenit-deftest zenit--symlink-origin
  (:doc "`zenit--symlink-origin' is defined")
  (should (boundp 'zenit--symlink-origin)))

(zenit-deftest zenit--record-symlink-origin-a
  (:doc "`zenit--record-symlink-origin-a' advises `vc-follow-link'")
  (progn
    (should (fboundp 'zenit--record-symlink-origin-a))
    (should (advice-member-p 'zenit--record-symlink-origin-a #'vc-follow-link))))

(zenit-deftest zenit-create-missing-directories-h
  (:doc "`zenit-create-missing-directories-h' is a member of `find-file-not-found-functions'")
  (progn
    (should (fboundp 'zenit-create-missing-directories-h))
    (should (member #'zenit-create-missing-directories-h find-file-not-found-functions))))

(zenit-deftest zenit-guess-mode-h
  (:doc "`zenit-guess-mode-h' is a member of `after-save-hook'")
  (progn
    (should (fboundp 'zenit-guess-mode-h))
    (should (member #'zenit-guess-mode-h after-save-hook))))

(zenit-deftest zenit--shut-up-autosave-a
  (:doc "`zenit--shut-up-autosave-a' advises `after-find-file'")
  (progn
    (should (fboundp 'zenit--shut-up-autosave-a))
    (should (advice-member-p 'zenit--shut-up-autosave-a #'after-find-file))))

(zenit-deftest zenit-make-hashed-auto-save-file-name-a
  (:doc "`zenit-make-hashed-auto-save-file-name-a' advises `make-auto-save-file-name'")
  (progn
    (should (fboundp 'zenit-make-hashed-auto-save-file-name-a))
    (should (advice-member-p 'zenit-make-hashed-auto-save-file-name-a #'make-auto-save-file-name))
    (let ((temp-file (zenit-test-make-temp-file)))
      (with-current-buffer (find-file-noselect temp-file)
        (should (equal (concat "#" (sha1 buffer-file-name) "#")
                       (file-name-base (make-auto-save-file-name))))))))

(zenit-deftest zenit-make-hashed-backup-file-name-a
  (:doc "`zenit-make-hashed-backup-file-name-a' advises `make-auto-save-file-name'")
  (progn
    (should (fboundp 'zenit-make-hashed-backup-file-name-a))
    (should (advice-member-p 'zenit-make-hashed-backup-file-name-a #'make-backup-file-name-1))
    (let ((temp-file (zenit-test-make-temp-file)))
      (with-current-buffer (find-file-noselect temp-file)
        (advice-remove 'make-backup-file-name-1 #'zenit-make-hashed-backup-file-name-a)
        (let ((orig (make-backup-file-name-1 temp-file)))
          (advice-add 'make-backup-file-name-1 :around #'zenit-make-hashed-backup-file-name-a)
          (should (equal (sha1 (file-name-nondirectory orig))
                         (file-name-base (make-backup-file-name-1 temp-file)))))))))

(zenit-deftest zenit-cache-hashed-backup-file-name-a
  (:doc "`zenit-cache-hashed-backup-file-name-a' advises `backup-buffer'")
  (progn
    (should (fboundp 'zenit-cache-hashed-backup-file-name-a))
    (should (advice-member-p 'zenit-cache-hashed-backup-file-name-a #'backup-buffer))))

(zenit-deftest zenit-auto-revert-buffer-h
  (:doc "`zenit-auto-revert-buffer-h' is defined")
  (should (fboundp 'zenit-auto-revert-buffer-h)))

(zenit-deftest zenit-auto-revert-buffers-h
  (:doc "`zenit-auto-revert-buffers-h' is defined")
  (should (fboundp 'zenit-auto-revert-buffers-h)))

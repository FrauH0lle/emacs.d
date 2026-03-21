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

(zenit-deftest zenit-file-lines-threshold-alist
  (:doc "`zenit-file-lines-threshold-alist' is defined")
  (should (boundp 'zenit-file-lines-threshold-alist)))

(zenit-deftest zenit-so-long-p
  (:doc "`zenit-so-long-p' is defined and set as `so-long-predicate'")
  (progn
    (require' so-long)
    (should (fboundp 'zenit-so-long-p))
    (should (eq so-long-predicate #'zenit-so-long-p))))

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

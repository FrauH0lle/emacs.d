;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-backups.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'backups)

(zenit-deftest +backups--full-version-number
  (:doc "Test version number extraction from filenames")
  (should (equal ,out (+backups--full-version-number ,@in)))
  (in out)
  ("file~123~" 4) 123
  ("file~1~" 4) 1
  ("file~1~" 0) 0
  ("file~123abc~" 4) 123
  ("file~123~456~" 4) 123)

(zenit-deftest +backups--make-version-number
  (:doc "Test version number extraction from filenames")
  (should (equal ,out (+backups--make-version-number ,@in)))
  (in out)
  ("file~123~") 123
  ("file~1~") 1
  ("file~123abc~") nil
  ("file~123~456~") 456
  ("file") nil)

(zenit-deftest +backups--file-sort-p
  (:doc "Test backup file version comparison")
  (should (equal ,out (+backups--file-sort-p ,@in)))
  (in out)
  ("file~123~" "file~122~") t
  ("file~123~" "file~123~") nil
  ("file~122~" "file~123~") nil)

(zenit-deftest +backups--get-last-modified
  (:doc "Test last modified date retrieval in the format \"%Y-%m-%d %T\"")
  (let ((test-file (zenit-test--make-temp-file)))
    (should (string-match-p
             (rx (repeat 4 digit) "-" (repeat 2 digit) "-" (repeat 2 digit)
                 space
                 (repeat 2 digit) ":" (repeat 2 digit) ":" (repeat 2 digit))
             (+backups--get-last-modified test-file)))
    (should-not (+backups--get-last-modified
                 (md5 (format "%s%s%s%s" (system-name) (emacs-pid)
                              (current-time) (random)))))
    (delete-file test-file)))

(zenit-deftest +backups--make-file
  (:doc "Test file info structure creation")
  (let* ((test-file (zenit-test--make-temp-file nil "~123~"))
         (result (+backups--make-file test-file)))
    (should (equal (length result) 3))
    (should (numberp (nth 0 result)))
    (should (stringp (nth 1 result)))
    (should (stringp (nth 2 result)))
    (delete-file test-file)))

(zenit-deftest +backups--backup-files
  (:doc "Test backup file listing")
  (let* ((backup-directory-alist `(("." . ,temporary-file-directory)))
         (test-file (zenit-test--make-temp-file))
         (backup-file (make-backup-file-name test-file)))
    (with-temp-file backup-file
      (insert "Hello World!"))

    (should (member backup-file (+backups--backup-files test-file)))

    (delete-file test-file)
    (delete-file backup-file)))

(zenit-deftest +backups--get-sorted-backups
  (:doc "Test sorted backup retrieval")
  (let* ((backup-directory-alist `(("." . ,temporary-file-directory)))
         (test-file (zenit-test--make-temp-file))
         (backup-file (make-backup-file-name test-file))
         (backup1 (concat backup-file "1~"))
         (backup2 (concat backup-file "5~")))
    (with-temp-file backup1
      (insert "Hello World 1!"))
    (with-temp-file backup2
      (insert "Hello World 2!"))

    (let ((result (+backups--get-sorted-backups test-file)))
      (message "%s" result)
      (should (equal (length result) 3))
      (should (> (nth 0 (nth 1 result)) (nth 0 (nth 2 result)))))

    (delete-file test-file)
    (delete-file backup1)
    (delete-file backup2)))

;; (zenit-deftest +backups/list-backups
;;   (:doc "Test backup listing functionality")
;;   (let* ((test-file (make-temp-file "zenit-test"))
;;          (backup-file (concat test-file "~1~")))
;;     (write-region "" nil backup-file)
;;     (with-current-buffer (find-file-noselect test-file)
;;       (should (get-buffer (+backups/list-backups)))
;;       (kill-buffer))
;;     (delete-file test-file)
;;     (delete-file backup-file)))

;; (zenit-deftest +backups/view
;;   (:doc "Test backup file viewing")
;;   (let* ((test-file (make-temp-file "zenit-test"))
;;          (backup-file (concat test-file "~1~")))
;;     (write-region "" nil backup-file)
;;     (with-current-buffer (find-file-noselect test-file)
;;       (let ((backups-buffer (+backups/list-backups)))
;;         (with-current-buffer backups-buffer
;;           (goto-char (point-min))
;;           (forward-line)
;;           (should (get-buffer (+backups/view)))
;;           (kill-buffer))))
;;     (delete-file test-file)
;;     (delete-file backup-file)))

;; (zenit-deftest +backups/revert
;;   (:doc "Test file reversion from backup")
;;   (let* ((test-file (make-temp-file "zenit-test"))
;;          (backup-file (concat test-file "~1~"))
;;          (test-content "test content"))
;;     (write-region test-content nil test-file)
;;     (write-region "backup content" nil backup-file)
;;     (with-current-buffer (find-file-noselect test-file)
;;       (let ((backups-buffer (+backups/list-backups)))
;;         (with-current-buffer backups-buffer
;;           (goto-char (point-min))
;;           (forward-line 2)
;;           (+backups/revert)
;;           (should (string= (buffer-string) "backup content"))
;;           (kill-buffer))))
;;     (delete-file test-file)
;;     (delete-file backup-file)))

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
    (should (equal 3 (length result)))
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
      (should (equal 3 (length result)))
      (should (> (nth 0 (nth 1 result)) (nth 0 (nth 2 result)))))

    (delete-file test-file)
    (delete-file backup1)
    (delete-file backup2)))

(zenit-deftest +backups--get-backups ()
  (should (equal ',out (+backups--get-backups ',in)))
  (in out)
  :doc "Test that it returns :backups from the data alist"
  ((:original-file . "file.txt")
   (:backups . ((1 "2022-01-01" "file.txt.~1~")
                (2 "2022-02-01" "file.txt.~2~"))))
  ((1 "2022-01-01" "file.txt.~1~" )
   (2 "2022-02-01" "file.txt.~2~"))
  :doc "Test that it returns nil if the backups key is not found"
  ((:original-file . "file.txt")) nil)

(zenit-deftest +backups--get-version ()
  (should (equal ',out (+backups--get-version ',in)))
  (in out)
  :doc "Test that it returns the first element of the list when called on a file"
  (1 "2022-01-01" "file.txt.~1~") 1
  :doc "Test that it returns nil when called on an empty list"
  () nil)

(zenit-deftest +backups--get-last-modified-date ()
  (should (equal ',out (+backups--get-last-modified-date ',in)))
  (in out)
  :doc "Test that it returns the second element of the list when called on a file"
  (1 "2022-01-01" "file.txt.~1~") "2022-01-01"
  :doc "Test that it returns nil when called on an empty list"
  () nil)

(zenit-deftest +backups--get-file-name ()
  (should (equal ',out (+backups--get-file-name ',in)))
  (in out)
  :doc "Test that it returns the third element of the list when called on a file"
  (1 "2022-01-01" "file.txt.~1~") "file.txt.~1~"
  :doc "Test that it returns nil when called on an empty list"
  () nil)

(zenit-deftest +backups--list-backups-from-file
  (:doc "Test backup listing functionality"
   :vars (orig-file backup-file +backups--file-info-alist)
   :before-each (progn
                  (setq orig-file (zenit-test--make-temp-file)
                        backup-file (concat (make-backup-file-name orig-file) "1~")
                        +backups--file-info-alist nil)
                  (with-temp-file backup-file
                    (insert "Hello World!")))
   :after-each (progn
                 (delete-file orig-file)
                 (delete-file backup-file)))
  (let ((buf-name (format "*Backups: %s*" (buffer-name (get-file-buffer orig-file)))))
    (+backups--list-backups-from-file orig-file)
    (with-current-buffer buf-name
      (should ,assert))
    (kill-buffer buf-name))
  (assert)
  :doc "Test adding information to `+backups--file-info-alist'"
  (equal
   `((:backups-buffer . ,(current-buffer))
     (:backups . ,(+backups--get-sorted-backups orig-file +backups-files-function))
     (:original-file . ,orig-file))
   +backups--file-info-alist)

  :doc "Test populating `+backups--assoc-files-alist'"
  (equal
   `(,backup-file ,orig-file)
   +backups--assoc-files-alist)

  :doc "Test creating buffer with content"
  (let (orig-entry backup-entry)
    (goto-char (point-min))
    (save-excursion
      (setq orig-entry (re-search-forward
                        (format "  %-6s\t%s\n" "current"
                                (format-time-string "%Y-%m-%d %T"
                                                    (nth 5 (file-attributes orig-file)))))))
    (save-excursion
      (setq backup-entry (re-search-forward
                          (format "  %-6s\t%s\n" "current"
                                  (format-time-string "%Y-%m-%d %T"
                                                      (nth 5 (file-attributes backup-file)))))))
    (and orig-entry backup-entry)))

(zenit-deftest +backups--cleanup-and-close
  (:vars
   (orig-file file1 file2 file3 a b c d +backups--assoc-files-alist +backups--saved-wconf)
   :before-each
   (setq orig-file (zenit-test--make-temp-file)
         file1 (zenit-test--make-temp-file)
         file2 (zenit-test--make-temp-file)
         file3 (zenit-test--make-temp-file)
         a (find-file-noselect orig-file)
         b (find-file-noselect file1)
         c (find-file-noselect file2)
         d (find-file-noselect file3)
         +backups--assoc-files-alist (list orig-file file1 file2 file3)
         +backups--saved-wconf (current-window-configuration))
   :after-each
   (progn
     (kill-buffer a)
     (kill-buffer b)
     (kill-buffer c)
     (kill-buffer d)
     (delete-file orig-file)
     (delete-file file1)
     (delete-file file2)
     (delete-file file3)))
  ,assert
  (assert)
  :doc "Test killing all buffers associated with backups"
  (progn
    (+backups--cleanup-and-close)
    (dolist (buf (list b c d))
      (should-not (buffer-live-p buf)))))

(zenit-deftest +backups--get-file-name-from-index
  (:doc "Test that it returns the name of the backup file at index")
  (let ((+backups--file-info-alist '((:original-file . "file.txt")
                                     (:backups . ((1 "2022-01-01" "file.txt.~1~")
                                                  (2 "2022-02-01" "file.txt.~2~"))))))
    (should (equal ',out (+backups--get-file-name-from-index ,in))))
  (in out)
  1 "file.txt.~2~"
  0 "file.txt.~1~")

(zenit-deftest +backups--get-index-number
  (:doc "Test that it returns the index number of the backup file")
  (should (equal ,out (+backups--get-index-number ,in)))
  (in out)
  3 1
  4 2
  1 0)

(zenit-deftest +backups--get-line-number
  (:doc "Test that it returns the line number of the backup file")
  (should (equal ,out (+backups--get-line-number ,in)))
  (in out)
  1 3
  2 4
  -5 0)

(zenit-deftest +backups--get-original-file
  (:doc "Test that it returns the original file of the backup file in the backups list")
  (should (equal "file.txt"
                 (+backups--get-original-file '((:original-file . "file.txt")
                                                (:backups . ((1 "2022-01-01" "file.txt.~1~")
                                                             (2 "2022-02-01" "file.txt.~2~"))))))))

(zenit-deftest +backups--revert-backup-from-file
  (:doc "Test that it reverts the original file to the backup file")
  (let ((orig-file (zenit-test--make-temp-file nil nil "foo"))
        (backup-file (zenit-test--make-temp-file nil nil "bar")))
    (should (equal "foo" (with-temp-buffer
                           (insert-file-contents orig-file)
                           (buffer-string))))
    (should (equal "bar" (with-temp-buffer
                           (insert-file-contents backup-file)
                           (buffer-string))))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
      (+backups--revert-backup-from-file orig-file backup-file))
    (should (equal "bar" (with-temp-buffer
                           (insert-file-contents orig-file)
                           (buffer-string))))
    (should (equal "bar" (with-temp-buffer
                           (insert-file-contents backup-file)
                           (buffer-string))))
    (delete-file orig-file)
    (delete-file backup-file)))

(zenit-deftest +backups--guess-mode
  (:doc "Test that it guesses file mode based on the file extension")
  (should (equal ,out (+backups--guess-mode ,in)))
  (in out)
  "foo.el" 'emacs-lisp-mode
  "foo.txt" 'text-mode
  "foo.conf" 'conf-mode-maybe)

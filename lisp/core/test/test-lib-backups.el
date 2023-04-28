;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/tests/test-lib-backups.el

(describe "core/lib/backups"

  (load! "lib/backups" zenit-core-dir)

  (describe "+backups--full-version-number"
    :var (file-name)
    (it "returns the correct version number for a file name"
      (setq file-name "file~1~")
      (expect (+backups--full-version-number file-name (string-match "~[0-9]+~$" file-name)) :to-equal 1)
      (setq file-name "file~22~")
      (expect (+backups--full-version-number file-name (string-match "~[0-9]+~$" file-name)) :to-equal 22)
      (setq file-name "file~333~")
      (expect (+backups--full-version-number file-name (string-match "~[0-9]+~$" file-name)) :to-equal 333))

    (it "returns 0 for an invalid version number"
      (setq file-name "file~1~")
      (expect (+backups--full-version-number file-name 0) :to-equal 0)))


  (describe "+backups--make-version-number"
    (it "returns the correct version number for a file name"
      (expect (+backups--make-version-number "file~1~") :to-equal 1)
      (expect (+backups--make-version-number "file~100~") :to-equal 100)
      (expect (+backups--make-version-number "file~123~") :to-equal 123))

    (it "returns nil for a file name without a version number"
      (expect (+backups--make-version-number "file") :to-be nil)))


  (describe "+backups--file-sort-p"
    (it "returns t for file names with a higher version number"
      (expect (+backups--file-sort-p "file~2~" "file~1~") :to-be t)
      (expect (+backups--file-sort-p "file~100~" "file~1~") :to-be t))

    (it "returns nil for file names with a lower or equal version number"
      (expect (+backups--file-sort-p "file~1~" "file~2~") :to-be nil)
      (expect (+backups--file-sort-p "file~1~" "file~100~") :to-be nil)
      (expect (+backups--file-sort-p "file~1~" "file~1~") :to-be nil)))


  (describe "+backups--get-last-modified"
    :var ((file (make-temp-file "file")))
    (it "returns the last modified date of a file in the format \"%Y-%m-%d %T\""
      (expect (+backups--get-last-modified file)
              :to-match
              (rx (repeat 4 digit) "-" (repeat 2 digit) "-" (repeat 2 digit)
                  space
                  (repeat 2 digit) ":" (repeat 2 digit) ":" (repeat 2 digit))))

    (it "handles the case where the file's attributes cannot be retrieved"
      (expect (+backups--get-last-modified (md5 (format "%s%s%s%s" (system-name) (emacs-pid) (current-time) (random)))) :to-be nil)))


  (describe "+backups--make-file"
    :var ((file (make-temp-file "file" nil "~42~")))
    (it "returns a list with the version number, last modified date, and filename"
      (expect (+backups--make-file file)
              :to-equal `(42 ,(format-time-string "%Y-%m-%d %T" (nth 5 (file-attributes file))) ,file))))


  (describe "+backups--backup-files"
    (it "returns a list of backup files for the given original file"
      (let* ((backup-directory-alist `(("." . ,temporary-file-directory)))
             (orig-file (make-temp-file "file"))
             (backup-file (make-backup-file-name orig-file)))

        (with-temp-buffer (write-file (concat backup-file "1~") nil))
        (expect (+backups--backup-files orig-file)
                :to-equal `(,(concat backup-file "1~"))))))


  (describe "+backups--get-sorted-backups"
    (it "returns a sorted list of backup files, including the original file"
      (let* ((backup-directory-alist `(("." . ,temporary-file-directory)))
             (orig-file (make-temp-file "file"))
             (backup-file (make-backup-file-name orig-file)))

        (with-temp-buffer (write-file (concat backup-file "1~") nil))
        (with-temp-buffer (write-file (concat backup-file "5~") nil))
        (expect (+backups--get-sorted-backups orig-file)
                :to-equal `((nil ,(format-time-string "%Y-%m-%d %T" (nth 5 (file-attributes orig-file))) ,orig-file)
                            (5 ,(format-time-string "%Y-%m-%d %T" (nth 5 (file-attributes (concat backup-file "1~")))) ,(concat backup-file "5~"))
                            (1 ,(format-time-string "%Y-%m-%d %T" (nth 5 (file-attributes (concat backup-file "5~")))) ,(concat backup-file "1~"))))))

    (it "handles the case where no backups are found"
      (expect (+backups--get-sorted-backups "foo.el")
              :to-equal '((nil nil "foo.el")))))


  (describe "+backups--get-backups"
    :var (data)

    (after-each
      (setq data nil))

    (it "returns the backups from the data alist"
      (setq data '((:original-file . "file.txt")
                   (:backups . ((1 "2022-01-01" "file.txt.~1~")
                                (2 "2022-02-01" "file.txt.~2~")))))
      (expect (+backups--get-backups data)
              :to-equal '((1 "2022-01-01" "file.txt.~1~" )
                          (2 "2022-02-01" "file.txt.~2~"))))

    (it "returns nil if the backups key is not found in the data alist"
      (setq data '((:original-file . "file.txt")))
      (expect (+backups--get-backups data)
              :to-be nil)))


  (describe "+backups--get-version"
    (it "returns the first element of the list when called on a file"
      (expect (+backups--get-version '(1 "2022-01-01" "file.txt.~1~")) :to-be 1))
    (it "returns nil when called on an empty list"
      (expect (+backups--get-version '()) :to-be nil)))


  (describe "+backups--get-last-modified-date"
    (it "should return the second element of the input list"
      (expect (+backups--get-last-modified-date '(1 "date" "filename")) :to-equal "date"))
    (it "returns nil when called on an empty list"
      (expect (+backups--get-version '()) :to-be nil)))


  (describe "+backups--list-backups-from-file"
    :var (orig-file backup-file +backups--file-info-alist)

    (before-each
      (setq orig-file (make-temp-file "orig-file")
            backup-file (concat (make-backup-file-name orig-file) "1~")
            +backups--file-info-alist nil)
      (with-temp-buffer (write-file backup-file nil)))

    (after-each
      (ignore-errors (delete-file orig-file))
      (ignore-errors (delete-file backup-file)))

    (it "adds information to +backups--file-info-alist"
      (+backups--list-backups-from-file orig-file)
      (let ((buf-name (format "*Backups: %s*" (buffer-name (get-file-buffer orig-file)))))
        (with-current-buffer buf-name
          (expect +backups--file-info-alist :to-have-same-items-as
                  `((:backups-buffer . ,(current-buffer))
                    (:backups . ,(+backups--get-sorted-backups orig-file +backups-files-function))
                    (:original-file . ,orig-file)))
          (kill-buffer (current-buffer)))))

    (it "populates +backups--assoc-files-alist"
      (+backups--list-backups-from-file orig-file)
      (let ((buf-name (format "*Backups: %s*" (buffer-name (get-file-buffer orig-file)))))
        (with-current-buffer buf-name
          (expect +backups--assoc-files-alist :to-have-same-items-as
                  `(,orig-file ,backup-file))
          (kill-buffer (current-buffer)))))

    (it "creates buffer with content"
      (+backups--list-backups-from-file orig-file)
      (let ((buf-name (format "*Backups: %s*" (buffer-name (get-file-buffer orig-file)))))
        (with-current-buffer buf-name
          (expect (buffer-string) :to-match
                  (format "  %-6s\t%s\n" "current" (format-time-string "%Y-%m-%d %T" (nth 5 (file-attributes orig-file)))))
          (expect (buffer-string) :to-match
                  (format "  %-6s\t%s\n" "current" (format-time-string "%Y-%m-%d %T" (nth 5 (file-attributes backup-file)))))
          (kill-buffer (current-buffer))))))


  (describe "+backups--cleanup-and-close"
    :var (orig-file file1 file2 file3 a b c d +backups--assoc-files-alist)
    (before-each
      (spy-on 'kill-buffer)
      (spy-on 'kill-buffer-and-window)
      (spy-on 'set-window-configuration)
      (setq orig-file (make-temp-file "orig-file")
            file1 (make-temp-file "file1")
            file2 (make-temp-file "file2")
            file3 (make-temp-file "file2"))

      (setq a (find-file-noselect orig-file)
            b (find-file-noselect file1)
            c (find-file-noselect file2)
            d (find-file-noselect file3))
      (setq +backups--assoc-files-alist (list orig-file file1 file2 file3)))

    (after-each
      (kill-buffer a)
      (kill-buffer b)
      (kill-buffer c)
      (kill-buffer d)
      (ignore-errors (delete-file orig-file))
      (ignore-errors (delete-file file1))
      (ignore-errors (delete-file file2))
      (ignore-errors (delete-file file3)))

    (it "kills all buffers associated with backups"
      (+backups--cleanup-and-close)
      (expect 'kill-buffer :to-have-been-called-with b)
      (expect 'kill-buffer :to-have-been-called-with c)
      (expect 'kill-buffer :to-have-been-called-with d))

    (it "kills the backup information buffer"
      (+backups--cleanup-and-close)
      (expect 'kill-buffer-and-window :to-have-been-called))

    (it "restores the previous window configuration"
      (+backups--cleanup-and-close)
      (expect 'set-window-configuration :to-have-been-called)))


  (describe "zenit-file-backups-mode"

    (it "sets the buffer-local variables to the correct initial values"
      (with-temp-buffer
        (zenit-file-backups-mode)
        (expect +backups--file-info-alist :to-be nil)
        (expect +backups--first-diff-index :to-be nil)
        (expect +backups--assoc-files-alist :to-be nil)))

    (it "disables undo in the buffer"
      (with-temp-buffer
        (zenit-file-backups-mode)
        (expect buffer-undo-list :to-be t)))

    (it "sets the header-line-format variable to the correct value"
      (with-temp-buffer
        (zenit-file-backups-mode)
        (expect header-line-format :to-equal "<return> view backup, <d> + <d> diff, <R> revert, <q> quit"))))


  (describe "+backups/close-view-buffer"
    (before-each
      (spy-on 'kill-buffer-and-window :and-call-through)
      (spy-on 'set-window-configuration :and-call-through))

    (it "closes the buffer and restores the previous window configuration"
      (let ((+backups--minor-saved-wconf (current-window-configuration)))
        ;; Create a temporary buffer and window
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (let ((temp-buffer (current-buffer)))
            ;; Call the function under test
            (+backups/close-view-buffer)
            ;; Check if kill-buffer-and-window was called
            (expect 'kill-buffer-and-window :to-have-been-called)
            ;; Check if set-window-configuration was called with the correct argument
            (expect 'set-window-configuration :to-have-been-called-with +backups--minor-saved-wconf)
            ;; Check if the temporary buffer was killed
            (expect (buffer-live-p temp-buffer) :not :to-be-truthy))))))


  (describe "zenit-view-backup-file-mode"
    (it "activates the minor mode"
      (with-temp-buffer
        (zenit-view-backup-file-mode +1)
        (expect zenit-view-backup-file-mode :to-be-truthy)))

    (it "sets the header-line-format to Press <q> to exit buffer."
      (with-temp-buffer
        (zenit-view-backup-file-mode)
        (expect header-line-format :to-equal "Press <q> to exit buffer."))))


  (describe "+backups--get-file-name"
    (it "returns the name of the backup file"
      (expect (+backups--get-file-name '(1 "2022-01-01" "file.txt.~1~")) :to-equal "file.txt.~1~")))


  (describe "+backups--get-file-name-from-index"
    (it "returns the name of the backup file at index"
      (let ((+backups--file-info-alist '((:original-file . "file.txt")
                                         (:backups . ((1 "2022-01-01" "file.txt.~1~")
                                                      (2 "2022-02-01" "file.txt.~2~"))))))
        (expect (+backups--get-file-name-from-index 1) :to-equal "file.txt.~2~"))))


  (describe "+backups--get-index-number"
    (it "returns the index number of the backup file in the backups list"
      (expect (+backups--get-index-number 3) :to-equal 1)))


  (describe "+backups--get-line-number"
    (it "returns the index number of the backup file in the backups list"
      (expect (+backups--get-line-number 3) :to-equal 5)))


  (describe "+backups--get-original-file"
    (it "returns the index number of the backup file in the backups list"
      (expect (+backups--get-original-file '((:original-file . "file.txt")
                                             (:backups . ((1 "2022-01-01" "file.txt.~1~")
                                                          (2 "2022-02-01" "file.txt.~2~")))))
              :to-equal "file.txt")))


  (describe "+backups/diff"
    (before-each
      (setq +backups--file-info-alist nil)
      (setq +backups--first-diff-index nil)
      (spy-on 'ediff))

    (it "diffs two versions of the file"
      (setq orig-file (make-temp-file "orig-file"))
      (setq a (find-file-noselect orig-file))

      (with-current-buffer a
        (setq +backups--file-info-alist `((:original-file . ,orig-file)
                                          (:backups . ((nil "2022-01-01" "file.txt")
                                                       (1 "2022-01-01" "file.txt.~1~")
                                                       (2 "2022-02-01" "file.txt.~2~")))))
        (setq +backups--first-diff-index 1)

        (insert (format "%s\n" orig-file))
        (insert (format "  %-6s\t%s\n" "current" "2022-01-01 01:01:01"))
        (insert (format "  %-6s\t%s\n" "2" "2022-02-01 01:01:01"))
        (insert (format "  %-6s\t%s\n" "1" "2022-01-01 01:01:01"))
        (goto-char (point-max))
        (forward-line -1)
        (funcall #'+backups/diff)
        (expect +backups--first-diff-index :to-equal nil)
        (expect 'ediff :to-have-been-called-with "file.txt.~1~" "file.txt.~2~"))

      (kill-buffer a)
      (ignore-errors (delete-file orig-file)))

    (it "errors if no file is selected"
      (let ((buffer (get-buffer-create "file.txt")))
        (with-current-buffer buffer
          (let ((+backups--file-info-alist '((:original-file . "file.txt")
                                             (:backups . ((1 "2022-01-01" "file.txt.~1~")
                                                          (2 "2022-02-01" "file.txt.~2~"))))))
            (expect (funcall #'+backups/diff) :to-throw 'error)))
        (kill-buffer buffer))))


  (describe "+backups--revert-backup-from-file"
    :var (orig-file backup-file)
    (before-each
      (spy-on 'y-or-n-p :and-return-value t)
      (spy-on 'copy-file :and-call-through)
      (spy-on 'delete-file)

      (setq orig-file (make-temp-file "orig-file")
            backup-file (make-temp-file "file1"))

      (with-temp-file orig-file
        (insert "bar"))
      (with-temp-file backup-file
        (insert "foo")))

    (after-each
      (ignore-errors (delete-file orig-file))
      (ignore-errors (delete-file backup-file)))

    (it "asks for confirmation before reverting the file"
      (+backups--revert-backup-from-file orig-file backup-file)
      (expect 'y-or-n-p :to-have-been-called))

    (it "creates a temporary copy of the backup file"
      (+backups--revert-backup-from-file orig-file backup-file)
      (expect 'copy-file :to-have-been-called-with
              backup-file (concat backup-file "#temp#") t))

    (it "copies the temporary copy over the original file"
      (+backups--revert-backup-from-file orig-file backup-file)
      (expect (with-temp-buffer
                (insert-file-contents orig-file)
                (buffer-string))
              :to-equal "foo"))

    (it "removes the temporary copy of the backup file"
      (+backups--revert-backup-from-file orig-file backup-file)
      (expect 'delete-file :to-have-been-called-with
              (concat backup-file "#temp#"))))


  (describe "+backups/revert"
    (it "reverts the current file"
      (setq +backups--file-info-alist nil)
      (spy-on '+backups--revert-backup-from-file)

      (with-temp-buffer
        (setq +backups--file-info-alist `((:original-file . "file.txt")
                                          (:backups . ((nil "2022-01-01" "file.txt")
                                                       (1 "2022-01-01" "file.txt.~1~")
                                                       (2 "2022-02-01" "file.txt.~2~")))))

        (insert (format "%s\n" orig-file))
        (insert (format "  %-6s\t%s\n" "current" "2022-01-01 01:01:01"))
        (insert (format "  %-6s\t%s\n" "2" "2022-02-01 01:01:01"))
        (insert (format "  %-6s\t%s\n" "1" "2022-01-01 01:01:01"))
        (goto-char (point-max))
        (forward-line -1)
        (funcall #'+backups/revert)
        (expect '+backups--revert-backup-from-file :to-have-been-called-with "file.txt" "file.txt.~2~"))))


  (describe "+backups--guess-mode"
    (it "guesses file mode based on file extension"
      (expect (+backups--guess-mode "foo.el") :to-be 'emacs-lisp-mode)
      (expect (+backups--guess-mode "foo.txt") :to-be 'text-mode)
      (expect (+backups--guess-mode "foo.conf") :to-be 'conf-mode-maybe)))


  (describe "+backups/view"
    (it "views the current backup file"
      (setq +backups--file-info-alist nil)
      (spy-on 'find-file-read-only-other-window)
      (spy-on 'zenit-view-backup-file-mode)
      (spy-on 'text-mode)

      (with-temp-buffer
        (setq +backups--file-info-alist `((:original-file . "file.txt")
                                          (:backups . ((nil "2022-01-01" "file.txt")
                                                       (1 "2022-01-01" "file.txt.~1~")
                                                       (2 "2022-02-01" "file.txt.~2~")))))
        (insert (format "%s\n" orig-file))
        (insert (format "  %-6s\t%s\n" "current" "2022-01-01 01:01:01"))
        (insert (format "  %-6s\t%s\n" "2" "2022-02-01 01:01:01"))
        (insert (format "  %-6s\t%s\n" "1" "2022-01-01 01:01:01"))
        (goto-char (point-max))
        (forward-line -1)
        (funcall #'+backups/view)
        (expect 'find-file-read-only-other-window :to-have-been-called-with "file.txt.~2~")
        (expect 'zenit-view-backup-file-mode :to-have-been-called)
        (expect 'text-mode :to-have-been-called))))


  (describe "+backups/list-backups"
    (it "lists backups of current buffer"
      (spy-on '+backups--list-backups-from-file)
      (spy-on 'buffer-file-name :and-return-value "foo.el")
      (with-temp-buffer
        (+backups/list-backups)
        (expect '+backups--list-backups-from-file :to-have-been-called-with "foo.el"))))


  ;; TODO
  (describe "+backups--collect-orphans"
    (it "is defined"
      (expect (fboundp '+backups--collect-orphans) :to-be t))
    (xit "needs tests"))
  (describe "+backups/list-orphaned"
    (it "is defined"
      (expect (fboundp '+backups/list-orphaned) :to-be t))
    (xit "needs tests"))
  (describe "+backups/remove-orphaned"
    (it "is defined"
      (expect (fboundp '+backups/remove-orphaned) :to-be t))
    (xit "needs tests")))

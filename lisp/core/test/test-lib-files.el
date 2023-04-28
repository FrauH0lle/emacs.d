;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-lib-files.el

(describe "core/lib/files"

  (load! "lib/files" zenit-core-dir)
  (require 'zenit-modules)

  (describe "zenit--resolve-path-forms"
    ;; This is a bit complicated but works
    (it "handles simple forms"
      (expect (prin1-to-string (zenit--resolve-path-forms "test.txt"))
              :to-equal
              (prin1-to-string
               '(let ((file "test.txt"))
                  (and (stringp file) (file-exists-p file) file)))))

    (it "handles or forms"
      (expect (prin1-to-string
               (zenit--resolve-path-forms '(or "test1.txt" "test2.txt")))
              :to-equal
              (prin1-to-string
               '(or (let ((file "test1.txt"))
                      (and (stringp file) (file-exists-p file) file))
                    (let ((file "test2.txt"))
                      (and (stringp file) (file-exists-p file) file))))))

    (it "handles and forms"
      (expect (prin1-to-string
               (zenit--resolve-path-forms '(and "test1.txt" "test2.txt")))
              :to-equal
              (prin1-to-string
               '(and (let ((file "test1.txt"))
                       (and (stringp file) (file-exists-p file) file))
                     (let ((file "test2.txt"))
                       (and (stringp file) (file-exists-p file) file))))))

    (it "handles nested forms"
      (expect (prin1-to-string
               (zenit--resolve-path-forms '(or A (and B C))))
              :to-equal
              (prin1-to-string
               '(or (let ((file A))
                      (and (stringp file) (file-exists-p file) file))
                    (and (let ((file B)) (and (stringp file) (file-exists-p file) file))
                         (let ((file C)) (and (stringp file) (file-exists-p file) file)))))))

    (it "handles directory parameter"
      (let ((file "test.txt")
            (directory "~"))
        (expect (prin1-to-string (zenit--resolve-path-forms "test.txt" "~"))
                :to-equal
                (prin1-to-string
                 '(let ((file "test.txt"))
                    (and (stringp file)
                         (let ((default-directory "~"))
                           (file-exists-p file)) file)))))))


  (describe "zenit--path"
    (it "joins the segments into a single file path"
      (expect (zenit--path "~" "test" "file.txt")
              :to-equal (expand-file-name "~/test/file.txt")))

    (it "expands each segment"
      (expect (zenit--path "~" ".." "file.txt")
              :to-equal (expand-file-name "~/../file.txt")))

    (it "ignores nil segments"
      (expect (zenit--path "~" nil "file.txt")
              :to-equal (expand-file-name "~/file.txt"))))


  (describe "zenit-glob"
    :var (file1 file2)

    (before-each
      (setq file1 (make-temp-file "glob-file1" nil ".el"))
      (setq file2 (make-temp-file "glob-file2" nil ".el")))

    (after-each
      (delete-file file1)
      (delete-file file2))

    (it "constructs a path from segments and expands glob patterns"
      (expect (zenit-glob temporary-file-directory "*glob-file*.el") :to-equal (list file1 file2)))

    (it "returns nil if the path doesn't exist"
      (expect (zenit-glob "nonexistent") :to-be nil))

    (it "ignores nil elements in segments"
      (expect (zenit-glob temporary-file-directory nil "*glob-file*.el") :to-equal (list file1 file2))))


  (describe "zenit-path"
    :var (buf file1)

    (before-each
      (setq file1 (make-temp-file "file1" nil ".el"))
      (setq buf (find-file file1)))

    (after-each
      (kill-buffer buf)
      (delete-file file1))

    (it "joins the segments into a single file path"
      (expect (zenit-path "~" "test" "file.txt")
              :to-equal (expand-file-name "~/test/file.txt")))

    (it "ignores nil segments"
      (expect (zenit-path "~" nil "file.txt")
              :to-equal (expand-file-name "~/file.txt")))

    (it "returns buffer files name if segments is nil"
      (expect (with-current-buffer buf (zenit-path))
              :to-equal (expand-file-name file1))))


  (describe "zenit-files-in"
    :var (test-dir file-a file-b)

    (before-each
      (setq test-dir (make-temp-file "zenit-files-in-test" t)))

    (after-each
      (delete-directory test-dir t))

    (it "returns files in a directory"
      (let ((file-a (expand-file-name "a.txt" test-dir))
            (file-b (expand-file-name "b.txt" test-dir)))
        (write-region "" nil file-a)
        (write-region "" nil file-b)
        (expect (zenit-files-in test-dir) :to-equal (list (file-name-concat test-dir "b.txt")
                                                          (file-name-concat test-dir "a.txt")))))

    (it "returns directories in a directory"
      (let ((sub-dir (expand-file-name "sub-dir" test-dir)))
        (make-directory sub-dir)
        (expect (zenit-files-in test-dir :type 'dirs) :to-equal (list sub-dir))))

    (it "filters files based on a filter function"
      (let ((file-a (expand-file-name "a.txt" test-dir))
            (file-b (expand-file-name "b.txt" test-dir)))
        (write-region "" nil file-a)
        (write-region "" nil file-b)
        (expect (zenit-files-in test-dir :filter (lambda (file) (equal file file-a)))
                :to-equal (list (file-name-concat test-dir "b.txt")))))

    (it "maps files based on a map function"
      (let ((file-a (expand-file-name "a.txt" test-dir))
            (file-b (expand-file-name "b.txt" test-dir)))
        (write-region "" nil file-a)
        (write-region "" nil file-b)
        (expect (zenit-files-in test-dir :map (lambda (file) (concat file ".bak")))
                :to-equal (list (file-name-concat test-dir "b.txt.bak")
                                (file-name-concat test-dir "a.txt.bak"))))))


  (describe "zenit-file-cookie-p"
    :var (test-file)

    (before-each
      (setq test-file (make-temp-file "test")))

    (after-each
      (delete-file test-file))

    (it "evaluates file cookie"
      (write-region ";;;###if (> 2 1)" nil test-file)
      (expect (zenit-file-cookie-p test-file "if" nil) :to-be t))

    (it "returns null-value if file cookie is not found"
      (write-region ";;;###when (> 2 1)" nil test-file)
      (expect (zenit-file-cookie-p test-file "if" 5) :to-be 5)))


  (describe "file-exists-p!"
    :var (test-file)

    (after-each
      (ignore-errors (delete-file test-file)))

    (it "is a (quasi) drop-in replacement for file-exists-p"
      (expect (and (file-exists-p! (file!))
                   (file-exists-p (file!)))
              :to-be t))

    (it "returns the file path if it exists"
      (setq test-file (make-temp-file "test"))
      (expect (file-exists-p! test-file) :to-be (expand-file-name test-file))))


  (describe "zenit-file-size"
    :var (test-file)

    (after-each
      (delete-file test-file))

    (it "returns file size in bytes"
      (setq test-file (make-temp-file "test"))
      (write-region "foo bar baz" nil test-file)
      (expect (zenit-file-size test-file) :to-be (file-attribute-size
                                                  (file-attributes test-file)))))


  (describe "zenit-emacs-directory-size"
    (before-each
      (spy-on 'executable-find :and-return-value nil)
      (setq test-dir (make-temp-file "zenit-test-dir" t))
      (mkdir (concat test-dir "/subdir") t)
      (write-region "foo" nil (concat test-dir "/file.txt")))

    (after-each
      (delete-directory test-dir t))

    (it "calculates the size of a directory in kilobytes"
      (expect (zenit-emacs-directory-size test-dir) :to-be-close-to
              (let ((sum 0.0))
                (dolist (attrs (directory-files-and-attributes test-dir nil nil t) sum)
                  (unless (member (car attrs) '("." ".."))
                    (cl-incf
                     sum (if (eq (nth 1 attrs) t)
                             (zenit-emacs-directory-size (expand-file-name (car attrs) test-dir))
                           (/ (nth 8 attrs) 1024.0))))))
              0)))


  (describe "zenit--update-files"
    :var (buf recentf-mode projectile-mode save-place-mode)

    (before-each
      (setq buf (get-buffer-create "test"))
      (setq-default recentf-mode t
                    projectile-mode t
                    save-place-mode t))

    (after-each
      (kill-buffer buf))

    (it "updates the files in `recentf`, `magit`, and `save-place`"
      (spy-on 'featurep :and-return-value t)
      (spy-on 'vc-file-clearprops)
      (spy-on 'get-file-buffer :and-return-value buf)
      (spy-on 'vc-refresh-state)
      (spy-on 'magit-toplevel :and-return-value "/path/to/toplevel")
      (spy-on 'recentf-remove-if-non-kept)
      (spy-on 'zenit-project-p :and-return-value t)
      (spy-on 'zenit-project-root :and-return-value "/path/to/project")
      (spy-on 'projectile-file-cached-p :and-return-value t)
      (spy-on 'projectile-purge-file-from-cache)
      (spy-on 'magit-refresh)
      (spy-on 'save-place-forget-unreadable-files)
      (zenit--update-files "/path/to/file1" "/path/to/file2")

      (expect 'vc-file-clearprops :to-have-been-called-times 2)
      (expect 'get-file-buffer :to-have-been-called-times 2)
      (expect 'vc-refresh-state :to-have-been-called-times 2)
      (expect 'magit-toplevel :to-have-been-called-times 2)
      (expect 'recentf-remove-if-non-kept :to-have-been-called-times 2)
      (expect 'zenit-project-p :to-have-been-called)
      (expect 'zenit-project-root :to-have-been-called)
      (expect 'projectile-file-cached-p :to-have-been-called-times 2)
      (expect 'projectile-purge-file-from-cache :to-have-been-called-times 2)
      (expect 'magit-refresh :to-have-been-called)
      (expect 'save-place-forget-unreadable-files :to-have-been-called)))


  (describe "zenit/delete-this-file"
    :var (file1 buf)

    (before-each
      (setq file1 (make-temp-file "file1")
            buf (find-file file1))
      (spy-on 'y-or-n-p :and-return-value t)
      (spy-on 'zenit/kill-this-buffer-in-all-windows)
      (spy-on 'zenit--update-files))

    (after-each
      (delete-file file1)
      (kill-buffer buf))

    (it "deletes the specified file and calls functions to update related information"
      (let ((inhibit-message t))
        (zenit/delete-this-file file1))
      (expect (file-exists-p file1) :to-be nil)
      (expect 'zenit/kill-this-buffer-in-all-windows :to-have-been-called)
      (expect 'zenit--update-files :to-have-been-called-with file1))

    (it "prompts for confirmation before deleting the file if force-p is nil"
      (let ((inhibit-message t))
        (zenit/delete-this-file file1 nil))
      (expect 'y-or-n-p :to-have-been-called)))


  (describe "zenit/copy-this-file"
    :var (buf file1 target)

    (before-each
      (setq file1 (make-temp-file "file1")
            buf (find-file file1)
            target (file-name-concat temporary-file-directory "foo1"))
      (spy-on 'zenit--update-files))

    (after-each
      (delete-file file1)
      (kill-buffer buf))

    (after-all
      (delete-file target))


    (it "copies the current buffer's file to the specified location"
      (ignore-errors (delete-file target))
      (expect (file-exists-p target) :to-be nil)
      (with-current-buffer buf
        (let ((inhibit-message t))
          (zenit/copy-this-file target)))
      (expect (file-exists-p target) :to-be t)
      (expect 'zenit--update-files :to-have-been-called))

    (it "overwrites the destination file if it exists and force-p is t"
      (spy-on 'copy-file :and-call-through)
      (expect (file-exists-p target) :to-be t)
      (with-current-buffer buf
        (let ((inhibit-message t))
          (zenit/copy-this-file target t)))
      (expect 'copy-file :to-have-been-called-with file1 target t))

    (it "prompts for confirmation before overwriting the destination file if it exists and force-p is not t"
      (spy-on 'copy-file)
      (with-current-buffer buf
        (let ((inhibit-message t))
          (zenit/copy-this-file target)))
      (expect 'copy-file :to-have-been-called-with file1 target 1))

    (it "throws an error if the buffer is not visiting any file"
      (with-current-buffer buf
        (setq buffer-file-name nil)
        (expect (zenit/copy-this-file target) :to-throw 'user-error))))


  (describe "zenit/move-this-file"
    :var (buf file1 target)

    (before-each
      (setq file1 (make-temp-file "file1")
            buf (find-file file1)
            target (file-name-concat temporary-file-directory "foo1"))
      (spy-on 'zenit--update-files))

    (after-each
      (delete-file file1)
      (kill-buffer buf))

    (after-all
      (delete-file target))

    (it "copies the current buffer's file to the specified location"
      (ignore-errors (delete-file target))
      (expect (file-exists-p target) :to-be nil)
      (with-current-buffer buf
        (let ((inhibit-message t))
          (zenit/move-this-file target)))
      (expect (file-exists-p file1) :to-be nil)
      (expect (file-exists-p target) :to-be t)
      (expect 'zenit--update-files :to-have-been-called))

    (it "overwrites the destination file if it exists and force-p is t"
      (spy-on 'rename-file :and-call-through)
      (expect (file-exists-p target) :to-be t)
      (with-current-buffer buf
        (let ((inhibit-message t))
          (zenit/move-this-file target t)))
      (expect 'rename-file :to-have-been-called-with file1 target t))

    (it "prompts for confirmation before overwriting the destination file if it exists and force-p is not t"
      (spy-on 'rename-file)
      (with-current-buffer buf
        (let ((inhibit-message t))
          (zenit/move-this-file target)))
      (expect 'rename-file :to-have-been-called-with file1 target 1))

    (it "throws an error if the buffer is not visiting any file"
      (with-current-buffer buf
        (setq buffer-file-name nil)
        (expect (zenit/move-this-file target) :to-throw 'user-error))))


  (describe "zenit--sudo-file-path"
    (it "returns the correct sudo path for a local file"
      (expect (zenit--sudo-file-path "/tmp/test.txt") :to-equal "/sudo:root@localhost:/tmp/test.txt")))


  (describe "zenit/sudo-find-file"
    (it "opens a file as root when given a local file path"
      (spy-on 'find-file)
      (zenit/sudo-find-file "/foo/bar")
      (expect 'find-file :to-have-been-called-with "/sudo:root@localhost:/foo/bar")))


  (describe "zenit/sudo-this-file"
    :var (buf file1)

    (before-each
      (setq file1 (make-temp-file "file1")
            buf (find-file file1)))

    (after-each
      (delete-file file1)
      (kill-buffer buf))

    (it "opens the current buffer as root"
      (spy-on 'find-file)
      (with-current-buffer buf
        (zenit/sudo-this-file))
      (expect 'find-file :to-have-been-called-with (concat "/sudo:root@localhost:" file1))))


  (describe "zenit/sudo-save-buffer"
    :var (buf file1)

    (before-each
      (setq file1 (make-temp-file "file1")
            buf (find-file file1)))

    (after-each
      (delete-file file1)
      (kill-buffer buf))

    (it "saves the current buffer as root"
      (spy-on 'find-file-noselect :and-call-fake
              (lambda (filename &optional nowarn rawfile wildcards) (generate-new-buffer filename)))
      (spy-on 'copy-to-buffer)
      (spy-on 'save-buffer)
      (with-current-buffer buf
        (zenit/sudo-save-buffer))
      (expect 'find-file-noselect :to-have-been-called-with (concat "/sudo:root@localhost:" file1))
      (expect 'copy-to-buffer :to-have-been-called)
      (expect 'save-buffer :to-have-been-called)))


  (describe "zenit/toggle-symlink"
    :var (buf file1 ln zenit--symlink-origin)

    (before-each
      (setq file1 (make-temp-file "file1")
            ln (concat file1 "-link"))
      (make-symbolic-link file1 ln t)
      (spy-on 'kill-buffer :and-call-through)
      (spy-on 'switch-to-buffer))

    (after-each
      (delete-file file1)
      (delete-file ln)
      (kill-buffer buf))

    (it "toggles to the true file when the buffer is visiting a symlink"
      (setq buf (find-file ln))
      (with-current-buffer buf
        (zenit/toggle-symlink)
        (expect 'kill-buffer :to-have-been-called)
        (expect 'switch-to-buffer :to-have-been-called-with (find-file-noselect file1))
        (expect (buffer-local-value 'zenit--symlink-origin (current-buffer)) :to-be ln)))

    (it "toggles back to the symlink when the buffer is visiting the true file"
      (setq buf (find-file file1))
      (with-current-buffer buf
        (setq-local zenit--symlink-origin ln)
        (zenit/toggle-symlink)
        (expect 'kill-buffer :to-have-been-called)
        (expect 'switch-to-buffer :to-have-been-called-with (find-file-noselect ln))
        (expect (buffer-local-value 'zenit--symlink-origin (current-buffer)) :to-be nil)))

    (it "displays a message when it cannot detect a symlink"
      (spy-on 'message)
      (with-current-buffer (get-buffer-create "test")
        (setq buffer-file-truename "/path/to/true/file")
        (setq buffer-file-name "/path/to/file")
        (zenit/toggle-symlink)
        (expect 'message :to-have-been-called-with "Could not detect symlink.")))))

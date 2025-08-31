;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-files.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'files)
(require 'zenit-modules)

(zenit-deftest zenit--resolve-path-forms
  (:doc "`zenit--resolve-path-forms' resolves nested forms")
  (should (equal
           (prin1-to-string
            '(or
              (let ((file A))
                (and (stringp file)
                     (let ((default-directory "~"))
                       (file-exists-p file))
                     file))
              (and (let ((file B))
                     (and (stringp file)
                          (let ((default-directory "~"))
                            (file-exists-p file))
                          file))
                   (let ((file C))
                     (and (stringp file)
                          (let ((default-directory "~"))
                            (file-exists-p file))
                          file)))))
           (prin1-to-string
            (zenit--resolve-path-forms
             '(or A (and B C))
             "~")))))

(zenit-deftest zenit-path
  (:doc "`zenit-path' returns a path from segments")
  (should (equal ,out (zenit-path ,@in)))
  (in out)
  ("/tmp" "foo" "bar.txt") "/tmp/foo/bar.txt"
  ("foo") (expand-file-name "foo")
  ("/tmp" "foo" nil "bar.txt") "/tmp/foo/bar.txt")

(zenit-deftest zenit-glob
  (:doc "`zenit-glob' returns a list with matching paths from segments")
  (let ((default-directory (dir!)))
    (should (zenit-test-same-items-p ,out (zenit-glob ,@in) :test #'equal)))
  (in out)
  ("*") (directory-files default-directory nil directory-files-no-dot-files-regexp)
  (default-directory "*") (directory-files default-directory t directory-files-no-dot-files-regexp))

(zenit-deftest zenit-files-in
  (:doc "`zenit-files-in' returns files/dirs matching criteria"
   :vars ((test-dir (zenit-test-make-temp-file t)))
   :before-each
   (progn
     (make-directory (expand-file-name "subdir" test-dir) t)
     (make-directory (expand-file-name "subdir/.hidden" test-dir) t)
     (write-region "" nil (expand-file-name "file1.txt" test-dir))
     (write-region "" nil (expand-file-name "file2.txt" test-dir))
     (write-region "" nil (expand-file-name ".hidden.txt" test-dir))
     (write-region "" nil (expand-file-name "subdir/file3.txt" test-dir)))
   :after-each
   (delete-directory test-dir t))
  (should (zenit-test-same-items-p ,out ,in :test #'equal))
  (out in)
  (list (expand-file-name "file1.txt" test-dir)
        (expand-file-name "file2.txt" test-dir)
        (expand-file-name ".hidden.txt" test-dir)
        (expand-file-name "subdir/file3.txt" test-dir))
  (zenit-files-in test-dir :type 'files)

  (list (expand-file-name "subdir" test-dir)
        (expand-file-name "subdir/.hidden" test-dir))
  (zenit-files-in test-dir :type 'dirs)

  (list (expand-file-name "file1.txt" test-dir)
        (expand-file-name "file2.txt" test-dir)
        (expand-file-name ".hidden.txt" test-dir)
        (expand-file-name "subdir" test-dir))
  (zenit-files-in test-dir :type t :depth 0)

  (list (expand-file-name "file1.txt" test-dir))
  (zenit-files-in test-dir :type 'files :match "file1")

  (list (expand-file-name "file1.txt" test-dir)
        (expand-file-name "file2.txt" test-dir)
        (expand-file-name ".hidden.txt" test-dir))
  (zenit-files-in test-dir :filter (lambda (f) (string-match-p "subdir" f))))

(zenit-deftest zenit-file-cookie-p
  (:doc "`zenit-file-cookie-p' returns the evaluated result a ;;;###COOKIE FORM of a file.")
  (let ((test-file (zenit-test-make-temp-file nil ".el" ,fcookie)))
    (,assert (zenit-file-cookie-p test-file ,tcookie ,null-value))
    (delete-file test-file))
  (assert fcookie tcookie null-value)
  should ";;;###if (equal \"test\" \"test\")" "if" nil
  should ";;;###foo-test (equal \"test\" \"test\")" "foo-test" nil
  should ";;;###foo-test (equal \"test\" \"test\")" "if" t
  should-not ";;;###foo-test (equal \"test\" \"test\")" "if" nil)

(zenit-deftest file-exists-p!
  (:doc "`file-exists-p!' tests if one or more files exist.")
  ,test
  (test)
  (should (file-exists-p! (file!)))
  (let ((test-file (zenit-test-make-temp-file)))
    (should (equal (expand-file-name test-file) (file-exists-p! test-file)))
    (delete-file test-file))
  (let ((test-file (zenit-test-make-temp-file)))
    (should (file-exists-p! (and (file!) test-file)))
    (delete-file test-file)))

(zenit-deftest zenit-file-size
  (:doc "`zenit-file-size' is defined"
   :vars ((test-file (zenit-test-make-temp-file nil nil "Hello World")))
   :after-each (delete-file test-file))
  (should (numberp (zenit-file-size test-file))))

(zenit-deftest zenit-directory-size
  (:doc "`zenit-directory-size' is defined")
  (should (fboundp 'zenit-directory-size)))

(zenit-deftest zenit--with-prepared-file-buffer
  (:doc "`zenit--with-prepared-file-buffer' is defined")
  (should (fboundp 'zenit--with-prepared-file-buffer)))

(zenit-deftest zenit-file-read
  (:doc "`zenit-file-read' Reads a file and return its contents"
   :vars ((test-file (zenit-test-make-temp-file nil ".el" "\"Hello World\"\n(progn (message \"Test\"))\n(message \"Done\")")))
   :after-each (delete-file test-file))
  ,test
  (test)
  (should (equal "\"Hello World\"\n(progn (message \"Test\"))\n(message \"Done\")"
                 (zenit-file-read test-file)))
  (should (equal "\"Hello World\"\n(progn (message \"Test\"))\n(message \"Done\")"
                 (with-temp-buffer
                   (zenit-file-read test-file :by 'insert)
                   (buffer-string))))
  (should (equal "Hello World"
                 (zenit-file-read test-file :by 'read)))
  (should (equal '("Hello World" (progn (message "Test")) (message "Done"))
                 (zenit-file-read test-file :by 'read*)))
  (should (equal '("Hello World" (progn (message "Test")))
                 (zenit-file-read test-file :by '(read . 2)))))

(zenit-deftest zenit-file-write
  (:doc "`zenit-file-write' writes contents into a file")
  (let ((test-file (expand-file-name "test-file.el" temporary-file-directory)))
    (zenit-file-write test-file '("Hello World" (progn (message "Test")) (message "Done")))
    (should (file-exists-p test-file))
    (should (equal '(Hello World (progn (message "Test")) (message "Done"))
                   (zenit-file-read test-file :by 'read*)))
    (delete-file test-file)))

(zenit-deftest with-file-contents!
  (:doc "`with-file-contents!' is defined")
  (should (fboundp 'with-file-contents!)))

(zenit-deftest with-file!
  (:doc "`with-file!' is defined")
  (should (fboundp 'with-file!)))

(zenit-deftest zenit--update-files
  (:doc "`zenit--update-files' is defined")
  (should (fboundp 'zenit--update-files)))

(zenit-deftest zenit/delete-this-file
  (:doc "`zenit/delete-this-file' is defined")
  (should (fboundp 'zenit/delete-this-file)))

(zenit-deftest zenit/copy-this-file
  (:doc "`zenit/copy-this-file' is defined")
  (should (fboundp 'zenit/copy-this-file)))

(zenit-deftest zenit/move-this-file
  (:doc "`zenit/move-this-file' is defined")
  (should (fboundp 'zenit/move-this-file)))

(zenit-deftest zenit--sudo-file-path
  (:doc "`zenit--sudo-file-path' is defined")
  (should (fboundp 'zenit--sudo-file-path)))

(zenit-deftest zenit/sudo-find-file
  (:doc "`zenit/sudo-find-file' is defined")
  (should (fboundp 'zenit/sudo-find-file)))

(zenit-deftest zenit/sudo-this-file
  (:doc "`zenit/sudo-this-file' is defined")
  (should (fboundp 'zenit/sudo-this-file)))

(zenit-deftest zenit/sudo-save-buffer
  (:doc "`zenit/sudo-save-buffer' is defined")
  (should (fboundp 'zenit/sudo-save-buffer)))

(zenit-deftest zenit/toggle-symlink
  (:doc "`zenit/toggle-symlink' is defined")
  (should (fboundp 'zenit/toggle-symlink)))

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
    (should (equal (expand-file-name test-file) (file-exists-p! test-file))))
  (let ((test-file (zenit-test-make-temp-file)))
    (should (file-exists-p! (and (file!) test-file)))))

(zenit-deftest zenit-file-size
  (:doc "`zenit-file-size' is defined"
   :vars ((test-file (zenit-test-make-temp-file nil nil "Hello World"))))
  (should (numberp (zenit-file-size test-file))))

(zenit-deftest zenit-emacs-directory-size
  (:doc "`zenit-emacs-directory-size' is defined")
  (should (fboundp 'zenit-emacs-directory-size)))

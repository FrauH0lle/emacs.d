;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-files.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'files)

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

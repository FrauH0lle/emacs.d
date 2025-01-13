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

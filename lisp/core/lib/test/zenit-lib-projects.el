;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-projects.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'projects)
(require 'projectile)
(zenit-require 'zenit-lib 'files)

(zenit-deftest set-project-type!
  (:doc "`set-project-type!' adds a project type to `projectile-project-types'")
  (progn
    (set-project-type!
        'test-name :predicate "test.json" :compile "make compile" :run "make run" :test "make test")
    (should (zenit-test-contains-items-p
             '((test-name marker-files "test.json" compilation-dir nil
               configure-command nil compile-command "make compile"
               test-command "make test" run-command "make run"))
             projectile-project-types :test #'equal))))

(zenit-deftest project-file-exists-p!
  (:doc "`project-file-exists-p!' checks if file exists in project root directory")
  (should (project-file-exists-p! "README.md" user-emacs-directory)))

(zenit-deftest zenit/find-file-in-other-project
  (:doc "`zenit/find-file-in-other-project' is defined")
  (should (fboundp 'zenit/find-file-in-other-project)))

(zenit-deftest zenit/browse-in-other-project
  (:doc "`zenit/browse-in-other-project' is defined")
  (should (fboundp 'zenit/browse-in-other-project)))

(zenit-deftest zenit/add-directory-as-project
  (:doc "`zenit/add-directory-as-project' is defined")
  (should (fboundp 'zenit/add-directory-as-project)))

(zenit-deftest zenit-project-p
  (:doc "`zenit-project-p' return t if directory is a valid project")
  (should (eq ,out (zenit-project-p ,dir)))
  (dir out)
  user-emacs-directory t
  temporary-file-directory nil)

(zenit-deftest zenit-project-name
  (:doc "`zenit-project-name' returns the name of the current project")
  (should (equal ".emacs.d" (zenit-project-name user-emacs-directory))))

(zenit-deftest zenit-project-expand
  (:doc "`zenit-project-expand' expands name to project root")
  (should (equal (file-name-concat user-emacs-directory "test")
                 (zenit-project-expand "test" user-emacs-directory))))

(zenit-deftest zenit-project-find-file
  (:doc "`zenit-project-find-file' is defined")
  (should (fboundp 'zenit-project-find-file)))

(zenit-deftest zenit-project-browse
  (:doc "`zenit-project-browse' is defined")
  (should (fboundp 'zenit-project-browse)))

(zenit-deftest zenit-project-ignored-p
  (:doc "`zenit-project-ignored-p' returns non-nil if project should be ignored")
  (should (eq ,out (zenit-project-ignored-p ,dir)))
  (dir out)
  user-emacs-directory nil
  temporary-file-directory t)

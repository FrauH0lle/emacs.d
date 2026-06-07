;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-packages.el

(require 'zenit-test)
(require 'straight)
(zenit-require 'zenit-lib 'packages)

(zenit-deftest zenit/search-versions
  (:doc "`zenit/search-versions' is defined")
  (should (fboundp 'zenit/search-versions)))

(zenit-deftest zenit/find-duplicate-package-versions
  (:doc "`zenit/find-duplicate-package-versions' is defined")
  (should (fboundp 'zenit/find-duplicate-package-versions)))

(zenit-deftest zenit/bump-package ()
  ,test
  (test)
  :doc "`zenit/bump-package' is defined"
  (should (fboundp 'zenit/bump-package))

  :doc "`zenit/bump-package' dispatches prefix args to bump actions"
  (let (calls)
    (letf! ((defun zenit-initialize-packages (&optional _force-p))
            (defun zenit-packages--bump-recipe-repos ())
            (defun zenit-packages--bump-package (&rest args)
              (push args calls)))
      (let (current-prefix-arg)
        (zenit/bump-package 'pkg))
      (let ((current-prefix-arg '(4)))
        (zenit/bump-package 'pkg))
      (let ((current-prefix-arg '(16)))
        (zenit/bump-package 'pkg))
      (let (current-prefix-arg)
        (zenit/bump-package 'pkg "commit")))
    (should
     (equal '((pkg nil nil select)
              (pkg nil nil latest)
              (pkg nil nil read)
              (pkg "commit" nil select))
            (nreverse calls))))

  :doc "`zenit/bump-package' resolves prefix actions without extra prompts"
  (let (calls)
    (letf! ((defun zenit-packages--bump-latest-commit (_recipe)
              (push 'latest calls)
              "latest-commit")
            (defun zenit-packages--bump-read-commit (_package)
              (push 'read calls)
              "raw-commit")
            (defun zenit-packages--bump-select-commit (_package _local-repo _lockfiles _recipe)
              (push 'select calls)
              "selected-commit"))
      (should (equal "selected-commit"
                     (zenit-packages--bump-resolve-commit 'pkg "repo" '("core.el") 'recipe 'select)))
      (should (equal "latest-commit"
                     (zenit-packages--bump-resolve-commit 'pkg "repo" '("core.el") 'recipe 'latest)))
      (should (equal "raw-commit"
                     (zenit-packages--bump-resolve-commit 'pkg "repo" '("core.el") 'recipe 'read))))
    (should (equal '(select latest read) (nreverse calls))))

  :doc "`zenit/bump-package' resolves latest commits with `git ls-remote'"
  (let (process-args fetched)
    (letf! ((defun straight-vc-git--encode-url (_repo _host _protocol)
              "https://example.invalid/repo.git")
            (defun straight--process-run (&rest args)
              (setq process-args args)
              '(0 "abcdef123456\trefs/heads/main\n" ""))
            (defun zenit-packages--bump-with-fetched-repo (&rest _args)
              (setq fetched t)))
      (should (equal "abcdef123456"
                     (zenit-packages--bump-latest-commit
                      '(:host github :repo "owner/repo" :branch "main"))))
      (should (equal '("git" "ls-remote" "https://example.invalid/repo.git" "refs/heads/main")
                     process-args))
      (should-not fetched)))

  :doc "`zenit/bump-package' uses a temporary clone for missing repos"
  (let (cloned fetched processed deleted called)
    (letf! ((defun straight--repos-dir (_local-repo) "/missing/repo")
            (defun file-exists-p (_file) nil)
            (defun straight-vc-fetch-from-remote (_recipe)
              (push t fetched))
            (defun straight-vc-git--encode-url (_repo _host _protocol)
              "https://example.invalid/repo.git")
            (defun straight-vc-git--clone-internal (&rest args)
              (setq cloned args))
            (defun straight--process-run (&rest args)
              (push args processed))
            (defun file-directory-p (_dir) t)
            (defun delete-directory (dir recursive)
              (push (list dir recursive) deleted)))
      (zenit-packages--bump-with-fetched-repo
       '(:local-repo "repo" :repo "owner/repo" :remote "origin" :branch "main")
       (lambda (_repo-dir local-repo)
         (setq called local-repo))))
    (should (equal "repo" called))
    (should-not fetched)
    (should (member :repo-dir cloned))
    (should (equal '("git" "fetch" "--unshallow" "--filter=tree:0" "--tags")
                   (car processed)))
    (should deleted))

  :doc "`zenit/bump-package' does not abort when temporary clone cleanup fails"
  (let ((attempts 0)
        messages)
    (letf! ((defun file-directory-p (_dir) t)
            (defun delete-directory (_dir _recursive)
              (cl-incf attempts)
              (signal 'file-error '("Removing directory" "Directory not empty" "/tmp/repo")))
            (defun message (format &rest args)
              (push (apply #'format format args) messages)))
      (zenit-packages--bump-delete-temp-repo "/tmp/repo"))
    (should (= 2 attempts))
    (should (string-match-p "couldn't delete temporary package repo"
                            (car messages)))))

(zenit-deftest zenit/bump-local-conf-package ()
  ,test
  (test)
  :doc "`zenit/bump-local-conf-package' is defined"
  (should (fboundp 'zenit/bump-local-conf-package))

  :doc "`zenit/bump-local-conf-package' delegates with local-conf and preserves prefix args"
  (let (calls)
    (letf! ((defun zenit/bump-package (package commit local-conf)
              (push (list package commit local-conf current-prefix-arg) calls)))
      (let ((current-prefix-arg '(4)))
        (zenit/bump-local-conf-package 'pkg))
      (let ((current-prefix-arg '(16)))
        (zenit/bump-local-conf-package 'pkg "commit")))
    (should
     (equal '((pkg nil local-conf (4))
              (pkg "commit" local-conf (16)))
            (nreverse calls)))))

(zenit-deftest zenit/bump-module ()
  ,test
  (test)
  :doc "`zenit/bump-module' is defined"
  (should (fboundp 'zenit/bump-module))

  :doc "`zenit/bump-module' propagates prefix args to package bumps"
  (let (calls)
    (letf! ((defun zenit-initialize-packages (&optional _force-p))
            (defun zenit-package-list (_module-list)
              '((pkg-a) (pkg-b)))
            (defun zenit/bump-package (package &optional _commit _local-conf)
              (push (list package current-prefix-arg) calls)))
      (let ((current-prefix-arg '(4)))
        (zenit/bump-module :category 'module)))
    (should (equal '((pkg-a (4))
                     (pkg-b (4)))
                   (nreverse calls))))

  :doc "`zenit/bump-module' reports empty modules"
  (let (messages)
    (letf! ((defun zenit-initialize-packages (&optional _force-p))
            (defun zenit-package-list (_module-list) nil)
            (defun message (format &rest args)
              (push (apply #'format format args) messages)))
      (zenit/bump-module :category 'module))
    (should (equal '("Module (:category . module) has no packages") messages))))

(zenit-deftest zenit/bump-all-packages ()
  ,test
  (test)
  :doc "`zenit/bump-all-packages' is defined"
  (should (fboundp 'zenit/bump-all-packages))

  :doc "`zenit/bump-all-packages' selects enabled packages by default and all packages with prefix"
  (let (args messages)
    (letf! ((defun zenit-initialize-packages (&optional _force-p))
            (defun zenit-packages--bump-recipe-repos ())
            (defun zenit-package-list (&optional module-list)
              (push module-list args)
              nil)
            (defun message (format &rest rest)
              (push (apply #'format format rest) messages)))
      (zenit/bump-all-packages)
      (zenit/bump-all-packages '(4)))
    (should (equal '(nil all) (nreverse args)))
    (should (equal '("Updating package recipe repositories..."
                     "Bumped 0 package lockfile entries"
                     "Updating package recipe repositories..."
                     "Bumped 0 package lockfile entries")
                   (nreverse messages))))

  :doc "`zenit/bump-all-packages' skips built-in/untracked packages and deduplicates lockfile writes"
  (let ((straight-profiles '((nil . "default.el")
                             (core . "core.el")
                             (other . "other.el")
                             (local . "local.el")))
        latest
        writes
        messages)
    (letf! ((defun zenit-initialize-packages (&optional _force-p))
            (defun zenit-packages--bump-recipe-repos ())
            (defun zenit-package-list (&optional _module-list)
              '((pkg-a :lockfile (core) :recipe (:local-repo "repo-a") :modules ((:core)))
                (pkg-alias :lockfile (core) :recipe (:local-repo "repo-a") :modules ((:core)))
                (pkg-b :lockfile (other) :recipe (:local-repo "repo-b") :modules ((:core)))
                (pkg-default :recipe (:local-repo "repo-default") :modules ((:core)))
                (pkg-default-t :lockfile (t) :recipe (:local-repo "repo-default-t") :modules ((:core)))
                (pkg-built-in :built-in t :lockfile (core) :recipe (:local-repo "repo-built-in") :modules ((:core)))
                (pkg-untracked :lockfile (ignore) :recipe (:local-repo "repo-untracked") :modules ((:core)))))
            (defun zenit-packages--bump-recipe (_package plist)
              (plist-get plist :recipe))
            (defun zenit-packages--bump-latest-commit (recipe)
              (push (plist-get recipe :local-repo) latest)
              (format "%s-commit" (plist-get recipe :local-repo)))
            (defun zenit-packages--bump-write-lockfile (local-repo lockfile commit local-conf)
              (push (list local-repo lockfile commit local-conf) writes))
            (defun message (format &rest rest)
              (push (apply #'format format rest) messages)))
      (zenit/bump-all-packages))
    (should (equal '("repo-a" "repo-b" "repo-default" "repo-default-t") (nreverse latest)))
    (should
     (equal '(("repo-a" "core.el" "repo-a-commit" nil)
              ("repo-b" "other.el" "repo-b-commit" nil)
              ("repo-default" "default.el" "repo-default-commit" nil)
              ("repo-default-t" "default.el" "repo-default-t-commit" nil))
            (nreverse writes)))
    (should (equal '("Updating package recipe repositories..."
                     "Bumping package 1/4: pkg-a, pkg-alias (repo-a)"
                     "Bumping package 2/4: pkg-b (repo-b)"
                     "Bumping package 3/4: pkg-default (repo-default)"
                     "Bumping package 4/4: pkg-default-t (repo-default-t)"
                     "Bumped 4 package lockfile entries")
                   (nreverse messages)))))

(zenit-deftest zenit-package-homepage
  (:doc "`zenit-package-homepage' is defined")
  (should (fboundp 'zenit-package-homepage)))

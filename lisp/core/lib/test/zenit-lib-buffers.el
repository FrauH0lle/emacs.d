;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-buffers.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'buffers)
(zenit-require 'zenit-lib 'projects)

(zenit-deftest zenit-real-buffer-functions
  (:doc "Test if `zenit-real-buffer-functions' is defined")
  (should (boundp 'zenit-real-buffer-functions)))

(zenit-deftest zenit-unreal-buffer-functions
  (:doc "Test if `zenit-unreal-buffer-functions' is defined")
  (should (boundp 'zenit-unreal-buffer-functions)))

(zenit-deftest zenit-real-buffer-p
  (:doc "Test if `zenit-real-buffer-p' is defined")
  (should (boundp 'zenit-real-buffer-p)))

(zenit-deftest zenit-fallback-buffer-name
  (:doc "Test if `zenit-fallback-buffer-name' is defined")
  (should (boundp 'zenit-fallback-buffer-name)))

(zenit-deftest zenit-buffer-frame-predicate
  (:doc "Test if `zenit-buffer-frame-predicate' returns t if given a real buffer")
  (let ((a (get-buffer-create "a")))
    (with-current-buffer a
      (setq zenit-real-buffer-p t)
      (should (eq t (zenit-buffer-frame-predicate a))))
    (kill-buffer a)
    (should (eq t (zenit-buffer-frame-predicate (zenit-fallback-buffer))))))

(zenit-deftest zenit-fallback-buffer
  (:doc "Test if `zenit-fallback-buffer' is creates the buffer"
   :vars ((zenit-fallback-buffer-name "*test-fallback*"))
   :after-each (kill-buffer zenit-fallback-buffer-name))
  (let ((buffer-list-update-hook nil))
    (zenit-fallback-buffer)
    (should (buffer-live-p (get-buffer zenit-fallback-buffer-name)))
    (with-current-buffer zenit-fallback-buffer-name
      (should (equal zenit-fallback-buffer-name (buffer-name))))))

(zenit-deftest zenit-buffer-list
  (:doc "Test if `zenit-buffer-list' is defined")
  (should (fboundp 'zenit-buffer-list)))

(zenit-deftest zenit-project-buffer-list
  (:vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b")))
   :before-each
   (progn
     (require 'projectile)
     (with-current-buffer a (setq default-directory zenit-emacs-dir))
     (with-current-buffer b (setq default-directory zenit-core-dir))
     (projectile-mode +1))
   :after-each
   (progn
     (projectile-mode -1)
     (unload-feature 'projectile t)
     (mapc #'kill-buffer (list a b))))
  ,test
  (test)
  :doc "`zenit-project-buffer-list' returns buffers in the same project"
  (with-current-buffer a
    (should-not (cl-set-difference (list a b) (zenit-project-buffer-list))))
  :doc "`zenit-project-buffer-list' returns all buffers if not in a project"
  (should-not (cl-set-difference (buffer-list) (zenit-project-buffer-list))))

(zenit-deftest zenit-open-projects
  (:vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b")))
   :before-each
   (progn
     (require 'projectile)
     (with-current-buffer a (setq default-directory zenit-emacs-dir
                                  zenit-real-buffer-p t))
     (with-current-buffer b (setq default-directory temporary-file-directory
                                  zenit-real-buffer-p t))
     (projectile-mode +1))
   :after-each
   (progn
     (projectile-mode -1)
     (unload-feature 'projectile t)
     (mapc #'kill-buffer (list a b))))
  ,test
  (test)
  :doc "`zenit-open-projects' returns a list of projects with open buffers"
  (with-current-buffer a
    (should (equal (list (expand-file-name zenit-emacs-dir))
                   (mapcar #'expand-file-name (zenit-open-projects)))))
  :doc "`zenit-open-projects' returns nil if no projects with open buffers exist"
  (with-current-buffer b
    (kill-buffer a)
    (should (equal nil (zenit-open-projects)))))

(zenit-deftest zenit-dired-buffer-p
  (:doc "Test if `zenit-dired-buffer-p' detects `dired' buffers")
  (let ((dired-buf (dired ".")))
    (with-temp-buffer
      (should-not (zenit-dired-buffer-p (current-buffer))))
    (should (equal 'dired-mode (zenit-dired-buffer-p dired-buf)))
    (kill-buffer dired-buf)))

(zenit-deftest zenit-special-buffer-p
  (:doc "Test if `zenit-special-buffer-p' detects special buffers")
  ,test
  (test)
  (let ((buf (get-buffer-create "*test-buffer")))
    (should (zenit-special-buffer-p buf))
    (kill-buffer buf))
  (let ((buf (get-buffer-create "test-buffer")))
    (should-not (zenit-special-buffer-p buf))
    (kill-buffer buf)))

(zenit-deftest zenit-temp-buffer-p
  (:doc "Test if `zenit-temp-buffer-p' detects temporary buffers")
  ,test
  (test)
  (let ((buf (get-buffer-create " *test-buffer")))
    (should (zenit-temp-buffer-p buf))
    (kill-buffer buf))
  (let ((buf (get-buffer-create "* test-buffer")))
    (should-not (zenit-temp-buffer-p buf))
    (kill-buffer buf)))

(zenit-deftest zenit-visible-buffer-p
  (:doc "Test if `zenit-visible-buffer-p' detects if buffer is visible")
  (let ((buf (get-buffer-create "test-buffer")))
    (switch-to-buffer buf)
    (should (zenit-visible-buffer-p buf))
    (kill-buffer buf)))

(zenit-deftest zenit-buried-buffer-p
  (:doc "Test if `zenit-buried-buffer-p' detects if buffer is not visible")
  (let ((buf (get-buffer-create "test-buffer")))
    (should (zenit-buried-buffer-p buf))
    (kill-buffer buf)))

(zenit-deftest zenit-non-file-visiting-buffer-p
  (:doc "Test if `zenit-non-file-visiting-buffer-p' detects non-file-visiting buffers")
  ,test
  (test)
  (with-temp-buffer
    (should (zenit-non-file-visiting-buffer-p (current-buffer))))
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (should-not (zenit-non-file-visiting-buffer-p (current-buffer)))))

(zenit-deftest zenit-real-buffer-list
  (:doc "`zenit-real-buffer-list' returns a list of real buffers"
   :vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b"))
    (c (get-buffer-create "c")))
   :before-each
   (progn
     (with-current-buffer a (setq zenit-real-buffer-p t))
     (with-current-buffer b (setq zenit-real-buffer-p nil))
     (with-current-buffer c (setq zenit-real-buffer-p t)))
   :after-each
   (mapc #'kill-buffer (list a b c)))
  (should (equal (list a c) (zenit-real-buffer-list (list a b c)))))

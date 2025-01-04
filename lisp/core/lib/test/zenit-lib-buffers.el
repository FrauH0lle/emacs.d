;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-buffers.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'buffers)
(zenit-require 'zenit-lib 'projects)

(zenit-deftest zenit-real-buffer-functions
  (:doc "`zenit-real-buffer-functions' is defined")
  (should (boundp 'zenit-real-buffer-functions)))

(zenit-deftest zenit-unreal-buffer-functions
  (:doc "`zenit-unreal-buffer-functions' is defined")
  (should (boundp 'zenit-unreal-buffer-functions)))

(zenit-deftest zenit-real-buffer-p
  (:doc "`zenit-real-buffer-p' is defined")
  (should (boundp 'zenit-real-buffer-p)))

(zenit-deftest zenit-fallback-buffer-name
  (:doc "`zenit-fallback-buffer-name' is defined")
  (should (boundp 'zenit-fallback-buffer-name)))

(zenit-deftest zenit-buffer-frame-predicate
  (:doc "`zenit-buffer-frame-predicate' returns t if given a real buffer")
  (let ((a (get-buffer-create "a")))
    (with-current-buffer a
      (setq zenit-real-buffer-p t)
      (should (eq t (zenit-buffer-frame-predicate a))))
    (kill-buffer a)
    (should (eq t (zenit-buffer-frame-predicate (zenit-fallback-buffer))))))

(zenit-deftest zenit-fallback-buffer
  (:doc "`zenit-fallback-buffer' is creates the buffer"
   :vars ((zenit-fallback-buffer-name "*test-fallback*"))
   :after-each (kill-buffer zenit-fallback-buffer-name))
  (let ((buffer-list-update-hook nil))
    (zenit-fallback-buffer)
    (should (buffer-live-p (get-buffer zenit-fallback-buffer-name)))
    (with-current-buffer zenit-fallback-buffer-name
      (should (equal zenit-fallback-buffer-name (buffer-name))))))

(zenit-deftest zenit-buffer-list
  (:doc "`zenit-buffer-list' is defined")
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
  (:doc "`zenit-dired-buffer-p' detects `dired' buffers")
  (let ((dired-buf (dired ".")))
    (with-temp-buffer
      (should-not (zenit-dired-buffer-p (current-buffer))))
    (should (equal 'dired-mode (zenit-dired-buffer-p dired-buf)))
    (kill-buffer dired-buf)))

(zenit-deftest zenit-special-buffer-p
  (:doc "`zenit-special-buffer-p' detects special buffers")
  ,test
  (test)
  (let ((buf (get-buffer-create "*test-buffer")))
    (should (zenit-special-buffer-p buf))
    (kill-buffer buf))
  (let ((buf (get-buffer-create "test-buffer")))
    (should-not (zenit-special-buffer-p buf))
    (kill-buffer buf)))

(zenit-deftest zenit-temp-buffer-p
  (:doc "`zenit-temp-buffer-p' detects temporary buffers")
  ,test
  (test)
  (let ((buf (get-buffer-create " *test-buffer")))
    (should (zenit-temp-buffer-p buf))
    (kill-buffer buf))
  (let ((buf (get-buffer-create "* test-buffer")))
    (should-not (zenit-temp-buffer-p buf))
    (kill-buffer buf)))

(zenit-deftest zenit-visible-buffer-p
  (:doc "`zenit-visible-buffer-p' detects if buffer is visible")
  (let ((buf (get-buffer-create "test-buffer")))
    (switch-to-buffer buf)
    (should (zenit-visible-buffer-p buf))
    (kill-buffer buf)))

(zenit-deftest zenit-buried-buffer-p
  (:doc "`zenit-buried-buffer-p' detects if buffer is not visible")
  (let ((buf (get-buffer-create "test-buffer")))
    (should (zenit-buried-buffer-p buf))
    (kill-buffer buf)))

(zenit-deftest zenit-non-file-visiting-buffer-p
  (:doc "`zenit-non-file-visiting-buffer-p' detects non-file-visiting buffers")
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

(zenit-deftest zenit-real-buffer-p
  (:vars (zenit-real-buffer-functions
          zenit-unreal-buffer-functions))
  ,test
  (test)
  :doc "`zenit-real-buffer-p' returns nil if the buffer is not live"
  (let ((buf (get-buffer-create "test-buffer")))
    (kill-buffer buf)
    (should-not (zenit-real-buffer-p buf)))
  :doc "`zenit-real-buffer-p' returns nil if the buffer is a temp buffer"
  (let ((buf (get-buffer-create " test-buffer")))
    (should-not (zenit-real-buffer-p buf))
    (kill-buffer buf))
  :doc "`zenit-real-buffer-p' returns t if the buffer's `zenit-real-buffer-p' value is t"
  (let ((buf (get-buffer-create "test-buffer")))
    (with-current-buffer buf
      (setq zenit-real-buffer-p t))
    (should (zenit-real-buffer-p buf))
    (kill-buffer buf))
  :doc "`zenit-real-buffer-p' returns t if any of the functions in `zenit-real-buffer-functions' return non-nil"
  (let ((buf (get-buffer-create "test-buffer")))
    (add-hook 'zenit-real-buffer-functions (lambda (b) t))
    (should (zenit-real-buffer-p buf))
    (remove-hook 'zenit-real-buffer-functions (lambda (b) t))
    (kill-buffer buf))
  :doc "`zenit-real-buffer-p' returns nil if any of the functions in `zenit-unreal-buffer-functions' return non-nil"
  (let ((buf (get-buffer-create "test-buffer")))
    (add-hook 'zenit-unreal-buffer-functions (lambda (b) t))
    (should-not (zenit-real-buffer-p buf))
    (remove-hook 'zenit-unreal-buffer-functions (lambda (b) t))
    (kill-buffer buf)))

(zenit-deftest zenit-unreal-buffer-p
  (:vars (zenit-real-buffer-functions
          zenit-unreal-buffer-functions))
  ,test
  (test)
  :doc "`zenit-unreal-buffer-p' returns t if the buffer is not live"
  (let ((buf (get-buffer-create "test-buffer")))
    (kill-buffer buf)
    (should (zenit-unreal-buffer-p buf)))
  :doc "`zenit-unreal-buffer-p' returns t if the buffer is a temp buffer"
  (let ((buf (get-buffer-create " test-buffer")))
    (should (zenit-unreal-buffer-p buf))
    (kill-buffer buf))
  :doc "`zenit-unreal-buffer-p' returns nil if the buffer's `zenit-unreal-buffer-p' value is t"
  (let ((buf (get-buffer-create "test-buffer")))
    (with-current-buffer buf
      (setq zenit-unreal-buffer-p t))
    (should-not (zenit-unreal-buffer-p buf))
    (kill-buffer buf))
  :doc "`zenit-unreal-buffer-p' returns nil if any of the functions in `zenit-real-buffer-functions' return non-nil"
  (let ((buf (get-buffer-create "test-buffer")))
    (add-hook 'zenit-real-buffer-functions (lambda (b) t))
    (should-not (zenit-unreal-buffer-p buf))
    (remove-hook 'zenit-real-buffer-functions (lambda (b) t))
    (kill-buffer buf))
  :doc "`zenit-unreal-buffer-p' returns t if any of the functions in `zenit-unreal-buffer-functions' return non-nil"
  (let ((buf (get-buffer-create "test-buffer")))
    (add-hook 'zenit-unreal-buffer-functions (lambda (b) t))
    (should (zenit-unreal-buffer-p buf))
    (remove-hook 'zenit-unreal-buffer-functions (lambda (b) t))
    (kill-buffer buf)))

(zenit-deftest zenit-buffers-in-mode
  (:vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b"))
    (c (get-buffer-create "c")))
   :before-each
   (progn
     (with-current-buffer a
       (text-mode))
     (with-current-buffer b
       (prog-mode))
     (with-current-buffer c
       (text-mode)))
   :after-each
   (mapc #'kill-buffer (list a b c)))
  ,test
  (test)
  :doc "`zenit-buffers-in-mode' returns a list of buffers whose major-mode is eq to mode"
  (should (equal (list a c) (zenit-buffers-in-mode 'text-mode (list a b c))))
  :doc "`zenit-buffers-in-mode' returns a list of buffers whose major-mode is eq to modes"
  (should (equal (list a b c) (zenit-buffers-in-mode '(text-mode prog-mode) (list a b c))))
  :doc "`zenit-buffers-in-mode' returns a list of buffers whose derived-mode-p is eq to mode"
  (with-current-buffer a
    (emacs-lisp-mode)
    (should (equal (list a b) (zenit-buffers-in-mode 'prog-mode (list a b c) 'derived-p)))))

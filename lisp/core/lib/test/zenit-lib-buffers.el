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
    (b (get-buffer-create "b"))
    (c (get-buffer-create "c")))
   :before-each
   (progn
     (require 'projectile)
     (with-current-buffer a (setq default-directory zenit-emacs-dir))
     (with-current-buffer b (setq default-directory zenit-emacs-dir))
     (with-current-buffer c (setq default-directory temporary-file-directory))
     (projectile-mode +1))
   :after-each
   (progn
     (projectile-mode -1)
     (unload-feature 'projectile t)
     (mapc #'kill-buffer (list a b c))))
  ,test
  (test)
  :doc "`zenit-project-buffer-list' returns buffers in the same project"
  (with-current-buffer a
    (should (zenit-test-same-items-p (list a b) (zenit-project-buffer-list))))
  :doc "`zenit-project-buffer-list' returns all buffers if not in a project"
  (with-current-buffer c
    (should (zenit-test-same-items-p (buffer-list) (zenit-project-buffer-list)))))

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

(zenit-deftest zenit-visible-windows
  (:vars*
   ((window-1 (selected-window))
    (window-2 (split-window-right))
    (window-3 (split-window-right))
    (win-list (list window-1 window-2 window-3)))
   :after-each
   (delete-other-windows window-1))
  ,test
  (test)
  :doc "`zenit-visible-windows' returns only the visible windows"
  (progn
    (set-window-dedicated-p window-1 t)
    (set-window-dedicated-p window-2 nil)
    (delete-window window-3)
    (should (equal (list window-2) (zenit-visible-windows))))
  :doc "`zenit-visible-windows' returns all windows if all are visible"
  (should (zenit-test-same-items-p win-list (zenit-visible-windows)))
  :doc "`zenit-visible-windows' returns an empty list if all windows are dedicated"
  (progn
    (set-window-dedicated-p window-1 t)
    (set-window-dedicated-p window-2 t)
    (set-window-dedicated-p window-3 t)
    (should (equal nil (zenit-visible-windows win-list)))))

(zenit-deftest zenit-visible-buffers
  (:doc "`zenit-visible-buffers' returns a list of visible buffers"
   :vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b"))
    (c (get-buffer-create "c")))
   :after-each
   (progn
     (mapc #'kill-buffer (list a b c))
     (delete-other-windows)))
  (progn
    (set-window-buffer (selected-window) a)
    (split-window-right)
    (set-window-buffer (next-window) c)
    (bury-buffer b)
    (should (equal (list a c) (zenit-visible-buffers)))))

(zenit-deftest zenit-buried-buffers
  (:doc "`zenit-buried-buffers' returns a list of buried buffers"
   :vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b"))
    (c (get-buffer-create "c"))
    (d (get-buffer-create "d")))
   :after-each
   (progn
     (mapc #'kill-buffer (list a b c d))
     (delete-other-windows)))
  (progn
    (set-window-buffer (selected-window) a)
    (split-window-right)
    (set-window-buffer (next-window) c)
    (bury-buffer b)
    (bury-buffer d)
    (should (zenit-test-contains-items-p (list b d) (zenit-buried-buffers)))
    (should-not (zenit-test-contains-items-p (list a c) (zenit-buried-buffers)))))

(zenit-deftest zenit-matching-buffers
  (:doc "`zenit-matching-buffers' returns a list of buffers matching a pattern"
   :vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b"))
    (c (get-buffer-create "c"))
    (d (get-buffer-create "d")))
   :after-each
   (mapc #'kill-buffer (list a b c d)))
  (progn
    (should (equal (list b d) (zenit-matching-buffers "^[bd]$" (list a b c d))))
    (should (equal (list a c) (zenit-matching-buffers "^[ac]$" (list a b c d))))))

(zenit-deftest zenit-set-buffer-real
  (:doc "`zenit-set-buffer-real' sets zenit-real-buffer-p buffer-locally"
   :vars
   ((a (get-buffer-create "a")))
   :after-each
   (mapc #'kill-buffer (list a)))
  (progn
    (should (equal nil (buffer-local-value 'zenit-real-buffer-p a)))
    (zenit-set-buffer-real a t)
    (should (buffer-local-value 'zenit-real-buffer-p a))))

(zenit-deftest zenit-kill-buffer-and-windows
  (:vars
   ((a (get-buffer-create "a")))
   :after-each
   (progn
     (mapc #'kill-buffer (list a))
     (delete-other-windows)))
  ,test
  (test)
  :doc "`zenit-kill-buffer-and-windows' kills the buffer and deletes all windows it's displayed in"
  (progn
    (split-window-right)
    (set-window-buffer (next-window) a)
    (should (get-buffer-window-list a))
    (zenit-kill-buffer-and-windows a)
    (should-error (get-buffer-window-list "a")))
  :doc "`zenit-kill-buffer-and-windows' does not delete the window if it's the only window"
  (progn
    (set-window-buffer (next-window) a)
    (should (one-window-p t))
    (zenit-kill-buffer-and-windows a)
    (should-not (get-buffer "a"))))

(zenit-deftest zenit-fixup-windows
  (:doc "`zenit-fixup-windows' shows a real or the fallback buffer"
   :vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b")))
   :after-each
   (progn
     (mapc #'kill-buffer (list a b))
     (delete-other-windows)))
  (progn
    (set-window-buffer (selected-window) a)
    (split-window-right)
    (set-window-buffer (next-window) b)
    (with-current-buffer a
      (zenit-set-buffer-real (current-buffer) t))
    (zenit-fixup-windows (list (selected-window) (next-window)))
    (should (equal a (window-buffer (selected-window))))
    (kill-buffer a)
    (zenit-fixup-windows (list (selected-window) (next-window)))
    (should (equal (zenit-fallback-buffer) (window-buffer (next-window))))))

(zenit-deftest zenit-kill-buffer-fixup-windows
  (:doc "`zenit-kill-buffer-fixup-windows' kills the buffer and shows a real or the fallback buffer"
   :vars
   ((a (get-buffer-create "a")))
   :after-each
   (progn
     (mapc #'kill-buffer (list a))
     (delete-other-windows)))
  (progn
    (set-window-buffer (selected-window) a)
    (split-window-right)
    (set-window-buffer (next-window) a)
    (zenit-kill-buffer-fixup-windows a)
    (should (equal (zenit-fallback-buffer) (window-buffer (selected-window))))
    (should (equal (zenit-fallback-buffer) (window-buffer (next-window))))))

(zenit-deftest zenit-kill-buffers-fixup-windows
  (:doc "`zenit-kill-buffers-fixup-windows' kills the buffers and shows a real or the fallback buffer"
   :vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b")))
   :after-each
   (progn
     (mapc #'kill-buffer (list a b))
     (delete-other-windows)))
  (progn
    (set-window-buffer (selected-window) a)
    (split-window-right)
    (set-window-buffer (next-window) b)
    (zenit-kill-buffers-fixup-windows (list a b))
    (should (equal (zenit-fallback-buffer) (window-buffer (selected-window))))
    (should (equal (zenit-fallback-buffer) (window-buffer (next-window))))))

(zenit-deftest zenit-kill-matching-buffers
  (:doc "`zenit-kill-matching-buffers' kills the buffers matching a pattern"
   :vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b")))
   :after-each
   (progn
     (mapc #'kill-buffer (list a b))))
  (progn
    (should (equal 2 (zenit-kill-matching-buffers "^[ab]$" (list a b))))))

(zenit-deftest zenit-mark-buffer-as-real-h
  (:doc "`zenit-mark-buffer-as-real-h' marks buffer as real"
   :vars
   ((a (get-buffer-create "a")))
   :after-each
   (mapc #'kill-buffer (list a)))
  (with-current-buffer a
    (should-not (buffer-local-value 'zenit-real-buffer-p a))
    (zenit-mark-buffer-as-real-h)
    (should (buffer-local-value 'zenit-real-buffer-p a))))

(zenit-deftest zenit/save-and-kill-buffer
  (:doc "`zenit/save-and-kill-buffer' is defined")
  (should (fboundp 'zenit/save-and-kill-buffer)))

(zenit-deftest zenit/kill-this-buffer-in-all-windows
  (:doc "`zenit/kill-this-buffer-in-all-windows' is defined")
  (should (fboundp 'zenit/kill-this-buffer-in-all-windows)))

(zenit-deftest zenit--message-or-count
  (:doc "`zenit--message-or-count' displays the message with count")
  (should (equal ,out
                 (let ((inhibit-message t)
                       (message-log-max nil))
                   (zenit--message-or-count ,@in))))
  (in out)
  (nil "hello%s" 3) 3
  (t "hello%s" 3) "hello3")

(zenit-deftest zenit/kill-all-buffers
  (:doc "`zenit/kill-all-buffers' is defined")
  (should (fboundp 'zenit/kill-all-buffers)))

(zenit-deftest zenit/kill-other-buffers
  (:doc "`zenit/kill-other-buffers' is defined")
  (should (fboundp 'zenit/kill-other-buffers)))

(zenit-deftest zenit/kill-matching-buffers
  (:doc "`zenit/kill-matching-buffers' is defined")
  (should (fboundp 'zenit/kill-matching-buffers)))

(zenit-deftest zenit/kill-buried-buffers
  (:doc "`zenit/kill-buried-buffers' is defined")
  (should (fboundp 'zenit/kill-buried-buffers)))

(zenit-deftest zenit/kill-project-buffers
  (:doc "`zenit/kill-project-buffers' is defined")
  (should (fboundp 'zenit/kill-project-buffers)))

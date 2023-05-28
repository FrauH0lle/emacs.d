;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-lib-buffers.el

(describe "core/lib/buffers"

  (load! "lib/buffers" zenit-core-dir)
  (load! "lib/projects" zenit-core-dir)

  (require 'zenit-use-package)
  (require 'zenit-projects)

  (before-each
    (delete-other-windows)
    (setq a (switch-to-buffer (get-buffer-create "a"))
          b (get-buffer-create "b")
          c (get-buffer-create "c")
          d (get-buffer-create "d")
          buffer-list (list a b c d)))

  (after-each
    (mapc #'kill-buffer buffer-list))


  (describe "zenit-buffer-frame-predicate"
    (it "returns t if `zenit-real-buffer-p' returns t"
      (with-current-buffer a
        (setq zenit-real-buffer-p t)
        (expect (zenit-buffer-frame-predicate a) :to-be t)
        (kill-current-buffer)))

    (it "returns t if buffer is equal to the value of `zenit-fallback-buffer'"
      (with-current-buffer (zenit-fallback-buffer)
        (expect (zenit-buffer-frame-predicate (current-buffer)) :to-be t)
        (kill-current-buffer)))

    (it "returns nil otherwise"
      (expect (zenit-buffer-frame-predicate a) :to-be nil)))


  (describe "zenit-fallback-buffer"
    :var (zenit-fallback-buffer-name)

    (before-each
      (setq zenit-fallback-buffer-name "*test-fallback*"))

    (after-each
      (kill-buffer zenit-fallback-buffer-name))

    (it "creates a new fallback buffer if it doesn't exist"
      (let ((buffer-list-update-hook nil))
        (with-current-buffer (zenit-fallback-buffer)
          (expect (buffer-name) :to-equal zenit-fallback-buffer-name))))

    (it "returns the existing fallback buffer if it exists"
      (let ((buffer-list-update-hook nil))
        (get-buffer-create zenit-fallback-buffer-name)
        (with-current-buffer (zenit-fallback-buffer)
          (expect (buffer-name) :to-equal zenit-fallback-buffer-name)))))


  (describe "zenit-project-buffer-list"
    :var (projectile-projects-cache-time projectile-projects-cache)

    (before-all (require 'projectile))
    (after-all  (unload-feature 'projectile t))

    (before-each
      (with-current-buffer a (setq default-directory zenit-emacs-dir))
      (with-current-buffer b (setq default-directory zenit-core-dir))
      (with-current-buffer c (setq default-directory "/tmp/"))
      (with-current-buffer d (setq default-directory "~"))
      (projectile-mode +1))
    (after-each
      (projectile-mode -1))

    (it "returns buffers in the same project"
      (with-current-buffer a
        (expect (zenit-project-buffer-list)
                :to-contain-items (list a b))))

    (it "returns all buffers if not in a project"
      (with-current-buffer c
        (expect (zenit-project-buffer-list)
                :to-have-same-items-as (buffer-list)))))


  (describe "zenit-open-projects"
    (before-each
      (with-current-buffer a (setq default-directory zenit-emacs-dir
                                   zenit-real-buffer-p t))
      (with-current-buffer b (setq default-directory zenit-core-dir
                                   zenit-real-buffer-p t))
      (with-current-buffer c (setq default-directory "/tmp/"
                                   zenit-real-buffer-p t))
      (with-current-buffer d (setq default-directory "~"
                                   zenit-real-buffer-p t)))

    (it "returns a list of projects with open buffers"
      (with-current-buffer a
        (expect (mapcar #'expand-file-name (zenit-open-projects))
                :to-have-same-items-as (mapcar #'expand-file-name (list zenit-emacs-dir))))))

  
  (describe "zenit-dired-buffer-p"
    (it "returns nil if buffer is not a dired buffer"
      (with-temp-buffer
        (expect (zenit-dired-buffer-p (current-buffer)) :to-be nil)))

    (it "returns non-nil if buffer is a dired buffer"
      (let ((dired-buf (dired ".")))
        (expect (zenit-dired-buffer-p dired-buf) :to-be 'dired-mode)
        (kill-buffer dired-buf))))


  (describe "zenit-special-buffer-p"
    (it "should return t if the buffer name starts and ends with an *"
      (let ((buf (get-buffer-create "*special-buffer*")))
        (expect (zenit-special-buffer-p buf) :to-be t)
        (kill-buffer buf)))

    (it "should return nil if the buffer name does not start and end with an *"
      (expect (zenit-special-buffer-p a) :to-be nil)))


  (describe "zenit-temp-buffer-p"
    (it "returns non-nil if BUF is temporary"
      (let ((buf (generate-new-buffer " buf")))
        (expect (zenit-temp-buffer-p buf) :to-be t)
        (kill-buffer buf)))

    (it "returns nil if BUF is not temporary"
      (expect (zenit-temp-buffer-p a) :to-be nil)))



  (describe "zenit-non-file-visiting-buffer-p"
    (it "returns non-nil if the buffer does not have a value for `buffer-file-name`"
      (with-temp-buffer
        (expect (zenit-non-file-visiting-buffer-p (current-buffer)) :to-be t)))

    (it "returns nil if the buffer has a value for `buffer-file-name`"
      (with-temp-buffer
        (setq buffer-file-name "file.txt")
        (expect (zenit-non-file-visiting-buffer-p (current-buffer)) :to-be nil))))


  (describe "zenit-real-buffer-p"
    :var (zenit-real-buffer-functions zenit-unreal-buffer-functions)

    (it "returns nil if the buffer is not live"
      (kill-buffer a)
      (expect (zenit-real-buffer-p a) :to-be nil))

    (it "returns nil if the buffer is a temp buffer"
      (let ((buf (get-buffer-create " foo")))
        (expect (zenit-real-buffer-p buf) :to-be nil)
        (kill-buffer buf)))

    (it "returns t if the buffer is a real buffer according to `zenit-real-buffer-p`"
      (let ((buf (get-buffer-create "foo")))
        (with-current-buffer buf
          (setq-local zenit-real-buffer-p t))
        (expect (zenit-real-buffer-p buf) :to-be t)
        (kill-buffer buf)))

    (it "returns t if any of the functions in `zenit-real-buffer-functions` return non-nil"
      (add-hook 'zenit-real-buffer-functions (lambda (b) t))
      (expect (zenit-real-buffer-p a) :to-be t)
      (remove-hook 'zenit-real-buffer-functions (lambda (b) t)))

    (it "returns nil if any of the functions in `zenit-unreal-buffer-functions` return non-nil"
      (add-hook 'zenit-unreal-buffer-functions (lambda (b) t))
      (expect (zenit-real-buffer-p a) :to-be nil)
      (remove-hook 'zenit-unreal-buffer-functions (lambda (b) t))))


  (describe "zenit-unreal-buffer-p"
    :var (zenit-real-buffer-functions zenit-unreal-buffer-functions)

    (it "returns t if the buffer is not live"
      (kill-buffer a)
      (expect (zenit-unreal-buffer-p a) :to-be t))

    (it "returns t if the buffer is a temp buffer"
      (let ((buf (get-buffer-create " foo")))
        (expect (zenit-unreal-buffer-p buf) :to-be t)
        (kill-buffer buf)))

    (it "returns t if the buffer is a real buffer according to `zenit-real-buffer-p`"
      (let ((buf (get-buffer-create "foo")))
        (with-current-buffer buf
          (setq-local zenit-real-buffer-p nil))
        (expect (zenit-unreal-buffer-p buf) :to-be t)
        (kill-buffer buf)))

    (it "returns nil if any of the functions in `zenit-real-buffer-functions` return t"
      (add-hook 'zenit-real-buffer-functions (lambda (b) t))
      (expect (zenit-unreal-buffer-p a) :to-be nil)
      (remove-hook 'zenit-real-buffer-functions (lambda (b) t)))

    (it "returns t if any of the functions in `zenit-unreal-buffer-functions` return non-nil"
      (add-hook 'zenit-unreal-buffer-functions (lambda (b) t))
      (expect (zenit-unreal-buffer-p a) :to-be t)
      (remove-hook 'zenit-unreal-buffer-functions (lambda (b) t))))


  (describe "zenit-buffers-in-mode"
    :var (buffer-list)

    (before-each
      (setq buffer-list (list a b c d))

      (let ((inc 0))
        (mapc (lambda (buf)
                (with-current-buffer buf
                  (funcall (if (cl-oddp inc)
                               'text-mode
                             'prog-mode)))
                (setq inc (1+ inc)))
              buffer-list)))

    (it "returns a list of buffers whose major-mode is eq to mode(s)"
      (expect (zenit-buffers-in-mode 'text-mode buffer-list)
              :to-equal (list b d))
      (expect (zenit-buffers-in-mode 'prog-mode buffer-list)
              :to-equal (list a c)))

    (it "returns a list of buffers whose derived-mode-p is eq to mode(s)"
      (expect (zenit-buffers-in-mode 'text-mode buffer-list t)
              :to-equal (list b d))))


  (describe "zenit-visible-windows"
    :var (window-1 window-2 window-3)

    (before-each
      (setq window-1 (selected-window)
            window-2 (split-window-right)
            window-3 (split-window-right)))

    (after-each
      (delete-other-windows window-1))

    (it "returns only the visible windows"
      (set-window-dedicated-p window-1 t)
      (set-window-dedicated-p window-2 nil)
      (delete-window window-3)
      (expect (zenit-visible-windows) :to-equal (list window-2)))

    (it "returns all windows if all are visible"
      (set-window-dedicated-p window-1 nil)
      (set-window-dedicated-p window-2 nil)
      (expect (zenit-visible-windows) :to-have-same-items-as (list window-1 window-2 window-3)))

    (it "returns an empty list if no windows are visible"
      (set-window-dedicated-p window-1 t)
      (set-window-dedicated-p window-2 t)
      (delete-window window-3)
      (expect (zenit-visible-windows) :to-equal nil))

    (it "accepts a list of windows as an argument"
      (set-window-dedicated-p window-1 t)
      (set-window-dedicated-p window-2 nil)
      (delete-window window-3)
      (expect (zenit-visible-windows (list window-1 window-2)) :to-equal (list window-2))))


  (describe "zenit-visible-buffers"
    (it "returns a list of visible buffers"
      (set-window-buffer (selected-window) a)
      (split-window-right)
      (set-window-buffer (next-window) b)
      (bury-buffer c)
      (expect (zenit-visible-buffers) :to-equal (list a b))))


  (describe "zenit-buried-buffers"
    (it "returns a list of visible buffers"
      (set-window-buffer (selected-window) b)
      (split-window-right)
      (set-window-buffer (next-window) d)
      (bury-buffer a)
      (bury-buffer c)
      (expect (zenit-buried-buffers) :to-contain-items (list a c))
      (expect (zenit-buried-buffers) :not :to-contain-items (list b d))))


  (describe "zenit-matching-buffers"
    (it "returns a list of buffers matching the pattern"
      (expect (zenit-matching-buffers "^[ac]$") :to-equal (list a c)))

    (it "accepts a list of buffers as an argument"
      (expect (zenit-matching-buffers "^[bd]$" (list a b c d)) :to-equal (list b d)))

    (it "returns an empty list if no buffers match the pattern"
      (expect (zenit-matching-buffers "^non-existent-buffer$") :to-equal nil)))

  (describe "zenit-set-buffer-real"
    (it "sets zenit-real-buffer-p buffer-locally"
      (expect (buffer-local-value 'zenit-real-buffer-p a) :to-be nil)
      (zenit-set-buffer-real a t)
      (expect (buffer-local-value 'zenit-real-buffer-p a) :to-be t)))


  (describe "zenit-kill-buffer-and-windows"
    (it "kills the buffer and deletes all windows it's displayed in"
      (set-window-buffer (selected-window) a)
      (split-window-right)
      (set-window-buffer (next-window) a)
      (expect (get-buffer-window-list a) :not :to-be nil)
      (zenit-kill-buffer-and-windows a)
      (expect (get-buffer-window-list "a") :to-throw 'error)
      (expect (get-buffer "a") :to-be nil))

    (it "does not delete the window if it's the only window"
      (set-window-buffer (selected-window) a)
      (expect (one-window-p t) :to-be t)
      (zenit-kill-buffer-and-windows a)
      (expect (get-buffer "a") :to-be nil)))


  (describe "zenit-fixup-windows"
    (it "shows the fallback buffer for unreal buffers"
      (set-window-buffer (selected-window) a)
      (split-window-right)
      (set-window-buffer (next-window) b)
      (zenit-fixup-windows (list (selected-window) (next-window)))
      (expect (window-buffer (selected-window)) :to-equal (zenit-fallback-buffer))
      (expect (window-buffer (next-window)) :to-equal (zenit-fallback-buffer)))

    (it "does not show the fallback buffer if the buffer is real"
      (set-window-buffer (selected-window) a)
      (split-window-right)
      (set-window-buffer (next-window) b)
      (with-current-buffer a
        (zenit-set-buffer-real (current-buffer) t))
      (zenit-fixup-windows (list (selected-window) (next-window)))
      (expect (window-buffer (selected-window)) :to-equal a)
      (expect (window-buffer (next-window)) :to-equal a)))


  (describe "zenit-kill-buffer-fixup-windows"
    (it "kills the specified buffer"
      (zenit-kill-buffer-fixup-windows a)
      (expect (get-buffer "a") :to-be nil))

    (it "fixes up the windows that were displaying the buffer"
      (spy-on 'zenit-fixup-windows :and-call-through)
      (zenit-kill-buffer-fixup-windows a)
      (expect 'zenit-fixup-windows :to-have-been-called)))


  (describe "zenit-kill-buffers-fixup-windows"
    :var (zenit-fallback-buffer-name)

    (before-each
      (setq zenit-fallback-buffer-name "*test-fallback*")
      (setq window1 (selected-window))
      (setq window2 (split-window))
      (set-window-buffer window1 a)
      (set-window-buffer window2 b)
      (with-current-buffer b
        (zenit-set-buffer-real (current-buffer) t)))

    (it "kills the specified buffers and ensures all windows display a real buffer or the fallback buffer"
      (expect (zenit-kill-buffers-fixup-windows (list a c)) :not :to-throw)
      (expect (get-buffer "a") :to-be nil)
      (expect (get-buffer "b") :to-be-truthy)
      (expect (buffer-name (window-buffer window1)) :not :to-equal "a")
      (expect (buffer-name (window-buffer window1)) :to-equal zenit-fallback-buffer-name)
      (expect (buffer-name (window-buffer window2)) :to-equal "b")))


  (describe "zenit-kill-matching-buffers"
    (it "kills buffers and returns the number of killed buffers"
      (expect (zenit-kill-matching-buffers "^[ac]$") :to-equal 2)
      (expect (get-buffer "a") :to-be nil)
      (expect (get-buffer "c") :to-be nil)))


  (describe "zenit-mark-buffer-as-real-h"
    (it "sets zenit-real-buffer-p to t"
      (with-current-buffer a
        (zenit-mark-buffer-as-real-h))
      (expect (zenit-real-buffer-p a) :to-be t)))


  (describe "zenit/save-and-kill-buffer"
    :var (buf-file)

    (before-each
      (setq buf-file (make-temp-file "foo")))

    (it "saves the current buffer to file and kills it"
      (with-current-buffer a
        (setq buffer-file-name buf-file)
        (insert "test")
        (expect (buffer-string) :to-match "test")
        (zenit/save-and-kill-buffer))

      (expect (get-buffer "a") :to-be nil)
      (expect (with-temp-buffer (insert-file-contents buf-file) (buffer-string)) :to-match "test")))


  (describe "zenit/kill-this-buffer-in-all-windows"
    :var (zenit-fallback-buffer-name)

    (before-each
      (setq zenit-fallback-buffer-name "*test-fallback*")
      (setq window1 (selected-window))
      (setq window2 (split-window))
      (set-window-buffer window1 a)
      (set-window-buffer window2 a))

    (it "kills the specified buffer and ensures all windows display a real buffer or the fallback buffer"
      (zenit/kill-this-buffer-in-all-windows a)
      (expect (get-buffer "a") :to-be nil)
      (expect (buffer-name (window-buffer window1)) :to-equal zenit-fallback-buffer-name)
      (expect (buffer-name (window-buffer window2)) :to-equal zenit-fallback-buffer-name)))


  (describe "zenit--message-or-count"
    (it "displays message with count if interactive is non-nil"
      (let ((inhibit-message t))
        (expect (zenit--message-or-count t "The count is: %d" 5) :to-match "The count is: 5")))

    (it "only returns count if interactive is nil"
      (expect (zenit--message-or-count nil "The count is: %d" 5) :to-be 5)))


  (describe "zenit/kill-other-buffers"
    (it "kills all other buffers"
      (with-current-buffer a
        (zenit/kill-other-buffers (list b c d)))
      (expect (get-buffer "a") :to-be-truthy)
      (expect (get-buffer "b") :to-be nil)
      (expect (get-buffer "c") :to-be nil)
      (expect (get-buffer "d") :to-be nil))

    (it "displays a message with number of buffers killed"
      (with-current-buffer a
        (let ((inhibit-message t))
          (expect (zenit/kill-other-buffers (list b c d) t) :to-match "Killed 3 other buffers")))))


  (describe "zenit/kill-matching-buffers"
    (it "kills all matching buffers"
      (zenit/kill-matching-buffers "^[bd]$" (list a b c d))
      (expect (get-buffer "a") :to-be-truthy)
      (expect (get-buffer "b") :to-be nil)
      (expect (get-buffer "c") :to-be-truthy)
      (expect (get-buffer "d") :to-be nil))

    (it "displays a message with number of buffers killed"
      (let ((inhibit-message t))
        (expect (zenit/kill-matching-buffers "^[bd]$" (list a b c d) t) :to-match "Killed 2 matching buffers"))))


  (describe "zenit/kill-buried-buffers"
    (before-each
      (bury-buffer a)
      (bury-buffer b)
      (set-window-buffer (selected-window) c)
      (split-window-right)
      (set-window-buffer (next-window) d))

    (it "kills all buried buffers"
      (zenit/kill-buried-buffers (zenit-buried-buffers (list a b c d)))
      (expect (get-buffer "a") :to-be nil)
      (expect (get-buffer "b") :to-be nil)
      (expect (get-buffer "c") :to-be-truthy)
      (expect (get-buffer "d") :to-be-truthy))

    (it "displays a message with number of buffers killed"
      (let ((inhibit-message t))
        (expect (zenit/kill-buried-buffers (zenit-buried-buffers (list a b c d)) t) :to-match "Killed 2 buried buffers"))))


  (describe "zenit/kill-project-buffers"
    :var (projectile-projects-cache-time projectile-projects-cache)

    (before-all (require 'projectile))
    (after-all  (unload-feature 'projectile t))

    (before-each
      (with-current-buffer a (setq default-directory zenit-emacs-dir))
      (with-current-buffer b (setq default-directory zenit-core-dir))
      (with-current-buffer c (setq default-directory "/tmp/"))
      (with-current-buffer d (setq default-directory "~"))
      (projectile-mode +1))
    (after-each
      (projectile-mode -1))

    (it "kills all buffers in a project"
      (with-current-buffer a
        (zenit/kill-project-buffers (zenit-project-root)))
      (expect (get-buffer "a") :to-be nil)
      (expect (get-buffer "b") :to-be nil)
      (expect (get-buffer "c") :to-be-truthy)
      (expect (get-buffer "d") :to-be-truthy))

    (it "displays a message with number of buffers killed"
      (let ((inhibit-message t))
        (expect (with-current-buffer a
                  (zenit/kill-project-buffers (zenit-project-root) t))
                :to-match "Killed 2 project buffers")))))

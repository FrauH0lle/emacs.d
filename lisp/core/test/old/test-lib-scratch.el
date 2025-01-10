;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-lib-scratch.el

(describe "core/lib/scratch"

  (load! "lib/scratch" zenit-core-dir)
  (load! "lib/buffers" zenit-core-dir)
  (load! "lib/projects" zenit-core-dir)

  (require 'projectile)

  (before-each
    (setq projectile-enable-caching nil)
    (projectile-mode +1)
    (setq temp-scratch-dir (make-temp-file "zenit-scratch" t))
    (setq original-zenit-scratch-dir zenit-scratch-dir)
    (setq zenit-scratch-dir temp-scratch-dir))

  (after-each
    (projectile-mode -1)
    (delete-directory temp-scratch-dir t)
    (setq zenit-scratch-dir original-zenit-scratch-dir))


  (describe "zenit--load-persistent-scratch-buffer"
    (before-each
      (spy-on 'message))

    (it "loads the content, point, and mode of an existing persistent scratch buffer"
      (let* ((project-name "test-project")
             (scratch-file (expand-file-name (concat project-name ".el") zenit-scratch-dir))
             (content "This is a test scratch buffer.")
             (point 5)
             (mode 'emacs-lisp-mode))
        (with-temp-file scratch-file
          (prin1 (list content point mode) (current-buffer)))
        (with-temp-buffer
          (zenit--load-persistent-scratch-buffer project-name)
          (expect (buffer-string) :to-equal content)
          (expect (point) :to-equal point)
          (expect major-mode :to-equal mode))))

    (it "returns nil if the scratch buffer file does not exist"
      (with-temp-buffer
        (expect (zenit--load-persistent-scratch-buffer "nonexistent-project") :to-be nil))))


  (describe "zenit-scratch-buffer"
    (before-each
      (spy-on 'message))

    (after-each
      (ignore-errors
        (kill-buffer (format "*zenit:scratch (%s)*" "test-project"))))

    (it "creates a new scratch buffer with the specified major mode"
      (let ((mode 'text-mode))
        (with-current-buffer (zenit-scratch-buffer nil mode)
          (expect major-mode :to-equal mode))))

    (it "creates a new scratch buffer with the specified project name"
      (let ((project-name "test-project"))
        (with-current-buffer (zenit-scratch-buffer nil nil nil project-name)
          (expect (buffer-name) :to-equal (format "*zenit:scratch (%s)*" project-name)))))

    (it "restores the content, point, and mode of an existing scratch buffer when 'dont-restore-p' is nil"
      (let* ((project-name "test-project")
             (scratch-file (expand-file-name (concat project-name ".el") zenit-scratch-dir))
             (content "This is a test scratch buffer.")
             (point 5)
             (mode 'emacs-lisp-mode))
        (with-temp-file scratch-file
          (prin1 (list content point mode) (current-buffer)))
        (with-current-buffer (zenit-scratch-buffer nil mode nil project-name)
          (expect (buffer-string) :to-equal content)
          (expect (point) :to-equal point)
          (expect major-mode :to-equal mode))))

    (it "creates an empty scratch buffer with the specified mode when 'dont-restore-p' is non-nil"
      (let ((mode 'text-mode)
            (project-name "test-project")
            (content "This is a test scratch buffer."))
        (with-temp-file (expand-file-name (concat project-name ".el") zenit-scratch-dir)
          (prin1 (list content 1 mode) (current-buffer)))
        (with-current-buffer (zenit-scratch-buffer t mode nil project-name)
          (expect (buffer-string) :to-equal "")))))


  (describe "zenit-persist-scratch-buffer-h"
    (it "saves the scratch buffer content, point, and mode to a file in 'zenit-scratch-dir'"
      (let* ((project-name "test-project")
             (mode 'text-mode)
             (content "This is a test scratch buffer.")
             (point 5)
             (scratch-file (expand-file-name (concat project-name ".el") zenit-scratch-dir)))
        (with-current-buffer (zenit-scratch-buffer nil mode nil project-name)
          (insert content)
          (goto-char point)
          (zenit-persist-scratch-buffer-h))
        (with-temp-buffer
          (insert-file-contents scratch-file)
          (let ((saved-data (read (current-buffer))))
            (expect (nth 0 saved-data) :to-equal content)
            (expect (nth 1 saved-data) :to-equal point)
            (expect (nth 2 saved-data) :to-equal mode))))))


  (describe "zenit--persist-scratch-buffers-h"
    (it "saves all scratch buffers to 'zenit-scratch-dir'"
      (let* ((project-name-1 "test-project-1")
             (project-name-2 "test-project-2")
             (mode 'text-mode)
             (content-1 "This is the first test scratch buffer.")
             (content-2 "This is the second test scratch buffer.")
             (point-1 10)
             (point-2 15)
             (scratch-file-1 (expand-file-name (concat project-name-1 ".el") zenit-scratch-dir))
             (scratch-file-2 (expand-file-name (concat project-name-2 ".el") zenit-scratch-dir)))
        (with-current-buffer (zenit-scratch-buffer nil mode nil project-name-1)
          (insert content-1)
          (goto-char point-1))
        (with-current-buffer (zenit-scratch-buffer nil mode nil project-name-2)
          (insert content-2)
          (goto-char point-2))
        (zenit--persist-scratch-buffers-h)

        (dolist (scratch-file (list scratch-file-1 scratch-file-2))
          (expect (file-exists-p scratch-file) :to-be t)
          (with-temp-buffer
            (insert-file-contents scratch-file)
            (let ((saved-data (read (current-buffer)))
                  (content (if (string= scratch-file scratch-file-1) content-1 content-2))
                  (point (if (string= scratch-file scratch-file-1) point-1 point-2)))
              (expect (nth 0 saved-data) :to-equal content)
              (expect (nth 1 saved-data) :to-equal point)
              (expect (nth 2 saved-data) :to-equal mode)))))))


  (describe "zenit-persist-scratch-buffers-after-switch-h"
    (it "kills scratch buffers when they are no longer visible, saving them to disk"
      (let* ((project-name "test-project")
             (mode 'text-mode)
             (content "This is a test scratch buffer.")
             (point 10)
             (scratch-file (expand-file-name (concat project-name ".el") zenit-scratch-dir)))
        (with-current-buffer (zenit-scratch-buffer nil mode nil project-name)
          (erase-buffer) ;; Add this line to erase the buffer before inserting content
          (insert content)
          (goto-char point)
          (setq zenit-scratch-buffers (list (current-buffer))))

        ;; Simulate the situation where no scratch buffers are visible
        (dolist (buf zenit-scratch-buffers)
          (with-current-buffer buf
            (zenit-persist-scratch-buffer-h))
          (kill-buffer buf))

        ;; Check if the scratch buffers are killed and saved to disk
        (expect (file-exists-p scratch-file) :to-be t)
        (with-temp-buffer
          (insert-file-contents scratch-file)
          (let ((saved-data (read (current-buffer))))
            (expect (nth 0 saved-data) :to-equal content)
            (expect (nth 1 saved-data) :to-equal point)
            (expect (nth 2 saved-data) :to-equal mode))))))


  (describe "zenit/open-scratch-buffer"
    (before-each
      (spy-on 'zenit-scratch-buffer)
      (spy-on 'switch-to-buffer)
      (spy-on 'pop-to-buffer))

    (it "calls zenit-scratch-buffer"
      (let* ((scratch-buffer (zenit/open-scratch-buffer)))
        (expect 'pop-to-buffer :to-have-been-called)
        (expect 'zenit-scratch-buffer :to-have-been-called-with nil nil default-directory nil)))

    (it "calls zenit-scratch-buffer with specified arguments"
      (let* ((scratch-buffer (zenit/open-scratch-buffer t t)))
        (expect 'zenit-scratch-buffer :to-have-been-called-with t nil default-directory (zenit-project-name))))

    (it "uses switch-to-buffer if same-window-p is t"
      (let* ((scratch-buffer (zenit/open-scratch-buffer nil nil t)))
        (expect 'switch-to-buffer :to-have-been-called)
        (expect 'zenit-scratch-buffer :to-have-been-called-with nil nil default-directory nil))))


  (describe "zenit/switch-to-scratch-buffer"
    (before-each
      (spy-on 'zenit/open-scratch-buffer))

    (it "calls zenit/open-scratch-buffer"
      (let* ((scratch-buffer (zenit/switch-to-scratch-buffer)))
        (expect 'zenit/open-scratch-buffer :to-have-been-called-with nil nil 'same-window)))

    (it "calls zenit/open-scratch-buffer with specified arguments"
      (let* ((scratch-buffer (zenit/switch-to-scratch-buffer t t)))
        (expect 'zenit/open-scratch-buffer :to-have-been-called-with t t 'same-window))))


  (describe "zenit/open-project-scratch-buffer"
    (before-each
      (spy-on 'zenit/open-scratch-buffer))

    (it "calls zenit/open-scratch-buffer"
      (let* ((scratch-buffer (zenit/open-project-scratch-buffer)))
        (expect 'zenit/open-scratch-buffer :to-have-been-called-with nil 'project nil)))

    (it "calls zenit/open-scratch-buffer with specified arguments"
      (let* ((scratch-buffer (zenit/open-project-scratch-buffer t t)))
        (expect 'zenit/open-scratch-buffer :to-have-been-called-with t 'project t))))


  (describe "zenit/switch-to-project-scratch-buffer"
    (before-each
      (spy-on 'zenit/open-project-scratch-buffer))

    (it "calls zenit/open-project-scratch-buffer"
      (let* ((scratch-buffer (zenit/switch-to-project-scratch-buffer)))
        (expect 'zenit/open-project-scratch-buffer :to-have-been-called-with nil 'same-window)))

    (it "calls zenit/open-project-scratch-buffer with specified arguments"
      (let* ((scratch-buffer (zenit/switch-to-project-scratch-buffer t)))
        (expect 'zenit/open-project-scratch-buffer :to-have-been-called-with t 'same-window))))


  (describe "zenit/revert-scratch-buffer"
    (before-each
      (spy-on 'message))

    (it "reverts the scratch buffer to its original state"
      (let* ((original-content "Original scratch buffer content.")
             (modified-content "Modified scratch buffer content.")
             (scratch-buffer (zenit/open-scratch-buffer "test-scratch")))
        (with-current-buffer scratch-buffer
          (insert original-content)
          (zenit-persist-scratch-buffer-h)
          (erase-buffer)
          (insert modified-content)
          (zenit/revert-scratch-buffer))
        (expect (with-current-buffer scratch-buffer
                  (buffer-string))
                :to-equal original-content))))


  (describe "zenit/delete-persistent-scratch-file"
    (before-each
      (with-temp-file (file-name-concat zenit-scratch-dir "test-1")
        (insert "foo"))
      (with-temp-file (file-name-concat zenit-scratch-dir "test-2")
        (insert "foo"))
      (spy-on 'read-file-name :and-return-value (file-name-concat zenit-scratch-dir "test-1"))
      (spy-on 'message))

    (it "deletes an existing persistent scratch file"
      (expect (file-exists-p (file-name-concat zenit-scratch-dir "test-1")) :to-be t)
      (expect (file-exists-p (file-name-concat zenit-scratch-dir "test-2")) :to-be t)
      (zenit/delete-persistent-scratch-file)
      (expect (file-exists-p (file-name-concat zenit-scratch-dir "test-1")) :to-be nil)
      (expect (file-exists-p (file-name-concat zenit-scratch-dir "test-2")) :to-be t))

    (it "deletes zenit-scratch-dir and recreates it if ARG is t"
      (expect (file-exists-p (file-name-concat zenit-scratch-dir "test-1")) :to-be t)
      (expect (file-exists-p (file-name-concat zenit-scratch-dir "test-2")) :to-be t)
      (zenit/delete-persistent-scratch-file t)
      (expect (file-exists-p zenit-scratch-dir) :to-be nil))))

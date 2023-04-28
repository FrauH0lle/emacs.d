;; tools/magit/autoload.el -*- lexical-binding: t; -*-

;; HACK Magit complains loudly (but harmlessly) when it can't determine its own
;;      version in a sparse clone. Since I'd rather not compromise on shallow
;;      clones, I've gimped `magit-version' so it doesn't complain (unless
;;      called interactively).
;;;###autoload
(defadvice! +magit--ignore-version-a (fn &rest args)
  :around #'magit-version
  (let ((inhibit-message (not (called-interactively-p 'any))))
    (apply fn args)))

;;;###autoload
(defun +magit-display-buffer (buffer)
  "Same as `magit-display-buffer-traditional', except...

- If opened from a commit window, it will open below it.
- Magit process windows are always opened in small windows below the current.
- Everything else will reuse the same window."
  (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
    (display-buffer
     buffer (cond
             ((and (eq buffer-mode 'magit-status-mode)
                   (get-buffer-window buffer))
              '(display-buffer-reuse-window))
             ;; Any magit buffers opened from a commit window should open below
             ;; it. Also open magit process windows below.
             ((or (bound-and-true-p git-commit-mode)
                  (eq buffer-mode 'magit-process-mode))
              (let ((size (if (eq buffer-mode 'magit-process-mode)
                              0.35
                            0.7)))
                `(display-buffer-below-selected
                  . ((window-height . ,(truncate (* (window-height) size)))))))

             ;; Everything else should reuse the current window.
             ((or (not (derived-mode-p 'magit-mode))
                  (not (memq (with-current-buffer buffer major-mode)
                             '(magit-process-mode
                               magit-revision-mode
                               magit-diff-mode
                               magit-stash-mode
                               magit-status-mode))))
              '(display-buffer-same-window))))))


;;
;;; Auto-revert

(defun +magit--refresh-vc-in-buffer (buffer)
  (with-current-buffer buffer
    (when (and vc-mode (fboundp 'vc-refresh-state))
      (vc-refresh-state))
    (when (and (bound-and-true-p git-gutter-mode)
               (fboundp '+version-control--update-git-gutter))
      (+version-control--update-git-gutter))
    (setq +magit--vc-is-stale-p nil)))

;;;###autoload
(defvar-local +magit--vc-is-stale-p nil)

;;;###autoload
(defun +magit--refresh-vc-state-maybe-h ()
  "Update `vc' and `git-gutter' if out of date."
  (when +magit--vc-is-stale-p
    (+magit--refresh-vc-in-buffer (current-buffer))))

;;;###autoload
(add-hook '+emacs-switch-buffer-hook #'+magit--refresh-vc-state-maybe-h)


;;
;;; Commands

;;;###autoload
(defun +magit/quit (&optional kill-buffer)
  "Bury the current magit buffer.

If KILL-BUFFER, kill this buffer instead of burying it.
If the buried/killed magit buffer was the last magit buffer open for this repo,
kill all magit buffers for this repo."
  (interactive "P")
  (let ((topdir (magit-toplevel)))
    (funcall magit-bury-buffer-function kill-buffer)
    (or (cl-find-if (lambda (win)
                      (with-selected-window win
                        (and (derived-mode-p 'magit-mode)
                             (equal magit--default-directory topdir))))
                    (window-list))
        (+magit/quit-all))))

;;;###autoload
(defun +magit/quit-all ()
  "Kill all magit buffers for the current repository."
  (interactive)
  (mapc #'+magit--kill-buffer (magit-mode-get-buffers)))

(defun +magit--kill-buffer (buf)
  "TODO"
  (when (and (bufferp buf) (buffer-live-p buf))
    (let ((process (get-buffer-process buf)))
      (if (not (processp process))
          (kill-buffer buf)
        (with-current-buffer buf
          (if (process-live-p process)
              (run-with-timer 5 nil #'+magit--kill-buffer buf)
            (kill-process process)
            (kill-buffer buf)))))))

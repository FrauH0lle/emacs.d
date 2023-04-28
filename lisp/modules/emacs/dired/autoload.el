;; emacs/dired/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +dired/quit (&optional arg)
  "Kill current `dired-mode' buffer. If ARG, kill all
`dired-mode' buffers."
  (interactive "P")
  (if arg
      (progn
        (mapc #'kill-buffer (zenit-buffers-in-mode 'dired-mode))
        (message "Killed all dired buffers"))
    (message "Killed '%s' dired buffer"
             (current-buffer))
    (kill-buffer)))

;;;###autoload
(defun +dired--enable-git-info-h ()
  "Enable `dired-git-info-mode' in git repos."
  (and (not (file-remote-p default-directory))
       (locate-dominating-file "." ".git")
       (dired-git-info-mode 1)))

;;;###autoload
(defun +dired/toggle-hidden-files ()
  "Toggle display of hidden files."
  (interactive)
  (let* ((old-switches (split-string dired-actual-switches " "))
         (new-switches (if (string= (car old-switches) "-ahl")
                           (string-join (append '("-hl") (cdr old-switches)) " ")
                         (string-join (append '("-ahl") (cdr old-switches)) " "))))
    (setq-local dired-actual-switches new-switches)
    (revert-buffer)))

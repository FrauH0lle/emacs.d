;; completion/corfu/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +corfu-dabbrev-friend-buffer-p (other-buffer)
  (< (buffer-size other-buffer) +corfu-buffer-scanning-size-limit))


;;
;;; Commands

;;;###autoload
(defun +corfu/move-to-minibuffer ()
  "Move the current list of candidates to the of minibuffer completion UI."
  (interactive)
 (unless completion-in-region--data
    (user-error "No completion active"))
  (pcase-let ((`(,beg ,end ,table ,pred ,extras)
               completion-in-region--data))
    (let ((completion-extra-properties extras)
          completion-cycle-threshold
          completion-cycling)
      (cond ((and (modulep! :completion vertico)
                  (fboundp #'consult-completion-in-region))
             (consult-completion-in-region beg end table pred))
            ((user-error "No minibuffer completion UI available for moving to!"))))))

;;;###autoload
(defun +corfu/smart-sep-toggle-escape ()
  "Insert `corfu-separator' or toggle escape if it's already there."
  (interactive)
  (cond ((and (char-equal (char-before) corfu-separator)
              (char-equal (char-before (1- (point))) ?\\))
         (save-excursion (delete-char -2)))
        ((char-equal (char-before) corfu-separator)
         (save-excursion (backward-char 1)
                         (insert-char ?\\)))
        (t (call-interactively #'corfu-insert-separator))))

;;;###autoload
(defun +corfu/dabbrev-this-buffer ()
  "Like `cape-dabbrev', but only scans current buffer."
  (interactive)
  (require 'cape)
  (let ((cape-dabbrev-buffer-function #'current-buffer))
    (cape-dabbrev t)))

;;;###autoload
(defun +corfu/toggle-auto-complete (&optional interactive)
  "Toggle as-you-type completion in Corfu."
  (interactive (list 'interactive))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when corfu-mode
        (if corfu-auto
            (remove-hook 'post-command-hook #'corfu-auto--post-command 'local)
          (add-hook 'post-command-hook #'corfu-auto--post-command nil 'local)))))
  (when interactive
    (message "Corfu auto-complete %s" (if corfu-auto "disabled" "enabled")))
  (setq corfu-auto (not corfu-auto)))

;;;###autoload
(defun +corfu/dabbrev-or-next (&optional arg)
  "Invoke `cape-dabbrev' but respect `evil-complete-all-buffers'.

Intended to mimic `evil-complete-next', unless the popup is
already open."
  (interactive "p")
  (if corfu--candidates
      (corfu-next arg)
    (require 'cape)
    (let ((cape-dabbrev-buffer-function
           (if (bound-and-true-p evil-complete-all-buffers)
               #'cape-same-mode-buffers
             #'current-buffer)))
      (cape-dabbrev t)
      (when (> corfu--total 0)
        (corfu--goto (or arg 0))))))

;;;###autoload
(defun +corfu/dabbrev-or-last (&optional arg)
  "Invoke `cape-dabbrev' but respect `evil-complete-all-buffers'.

Intended to mimic `evil-complete-previous', unless the popup is
already open."
  (interactive "p")
  (if corfu--candidates
      (corfu-previous arg)
    (require 'cape)
    (let ((cape-dabbrev-buffer-function
           (if (bound-and-true-p evil-complete-all-buffers)
               #'cape-same-mode-buffers
             #'current-buffer)))
      (cape-dabbrev t)
      (when (> corfu--total 0)
        (corfu--goto (- corfu--total (or arg 1)))))))

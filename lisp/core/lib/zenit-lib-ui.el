;; lisp/core/lib/zenit-lib-ui.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

;; `cl-seq'
(declare-function cl-remove-if "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `display-line-numbers'
(defvar display-line-numbers-type)

;; `zenit-lib-buffers'
(declare-function zenit-real-buffer-list "zenit-lib-buffers" (&optional buffer-list))

;; `zenit-lib-text.el'
(declare-function zenit-region-beginning "zenit-lib-text")
(declare-function zenit-region-end "zenit-lib-text")

;; `ui/workspaces'
(declare-function +popup-window-p "../../modules/ui/popup/autoload/popup.el" (&optional window))


;;
;;; Public library

;;;###autoload
(defun zenit-resize-window (window new-size &optional horizontal force-p)
  "Resize a window to NEW-SIZE. If HORIZONTAL, do it width-wise.
If FORCE-P is omitted when `window-size-fixed' is non-nil,
resizing will fail."
  (with-selected-window (or window (selected-window))
    (let ((window-size-fixed (unless force-p window-size-fixed)))
      (enlarge-window (- new-size (if horizontal (window-width) (window-height)))
                      horizontal))))

;;;###autoload
(defun zenit-quit-p (&optional prompt)
  "Prompt the user for confirmation when killing Emacs.
Returns t if it is safe to kill this session. Does not prompt if
no real buffers are open."
  (or (not (ignore-errors (zenit-real-buffer-list)))
      (yes-or-no-p (format "››› %s" (or prompt "Quit Emacs?")))
      (ignore (message "Aborted"))))


;;
;;; Advice

;;;###autoload
(defun zenit-recenter-a (&rest _)
  "Generic advisor for recentering window (typically :after other functions)."
  (recenter))

;;;###autoload
(defun zenit-shut-up-a (orig-fn &rest args)
  "Generic advisor for silencing noisy functions.

In interactive Emacs, this just inhibits messages from appearing
in the minibuffer. They are still logged to *Messages*.

In tty Emacs, messages are suppressed completely."
  (quiet! (apply orig-fn args)))


;;
;;; Hooks

;;;###autoload
(defun zenit-disable-show-paren-mode-h ()
  "Turn off `show-paren-mode' buffer-locally."
  (setq-local show-paren-mode nil))


;;
;;; Commands

;;;###autoload
(defun zenit/toggle-line-numbers ()
  "Toggle line numbers.
Cycles through regular, relative and no line numbers. The order
depends on what `display-line-numbers-type' is set to. If you're
using Emacs 26+, and visual-line-mode is on, this skips relative
and uses visual instead.  See `display-line-numbers' for what
these values mean."
  (interactive)
  (defvar zenit--line-number-style display-line-numbers-type)
  (let* ((styles `(t ,(if visual-line-mode 'visual 'relative) nil))
         (order (cons display-line-numbers-type (remq display-line-numbers-type styles)))
         (queue (memq zenit--line-number-style order))
         (next (if (= (length queue) 1)
                   (car order)
                 (car (cdr queue)))))
    (setq zenit--line-number-style next)
    (setq display-line-numbers next)
    (message "Switched to %s line numbers"
             (pcase next
               (`t "normal")
               (`nil "disabled")
               (_ (symbol-name next))))))

;;;###autoload
(defun zenit/delete-frame-with-prompt ()
  "Delete the current frame, but ask for confirmation if it isn't
empty."
  (interactive)
  (if (cdr (frame-list))
      (when (zenit-quit-p "Close frame?")
        (delete-frame))
    (save-buffers-kill-emacs)))

(defun zenit--enlargened-forget-last-wconf-h ()
  (set-frame-parameter nil 'zenit--maximize-last-wconf nil)
  (set-frame-parameter nil 'zenit--enlargen-last-wconf nil)
  (remove-hook 'zenit-switch-window-hook #'zenit--enlargened-forget-last-wconf-h))

;;;###autoload
(defun zenit/window-maximize-buffer (&optional arg)
  "Close other windows to focus on this one.
Activate again to undo this. If prefix ARG is non-nil, don't
restore the last window configuration and re-maximize the current
window. Alternatively, use `zenit/window-enlargen'."
  (interactive "P")
  (let ((param 'zenit--maximize-last-wconf))
    (cl-destructuring-bind (window . wconf)
        (or (frame-parameter nil param)
            (cons nil nil))
      (set-frame-parameter
       nil param
       (if (and (equal window (selected-window))
                (not arg)
                (null (cdr (cl-remove-if #'window-dedicated-p (window-list))))
                wconf)
           (ignore
            (let ((source-window (selected-window)))
              (set-window-configuration wconf)
              (when (window-live-p source-window)
                (select-window source-window))))
         (when (and (bound-and-true-p +popup-mode)
                    (+popup-window-p))
           (user-error "Cannot maximize a popup, use `+popup/raise' first or use `zenit/window-enlargen' instead"))
         (prog1 (cons (selected-window) (or wconf (current-window-configuration)))
           (delete-other-windows)
           (add-hook 'zenit-switch-window-hook #'zenit--enlargened-forget-last-wconf-h)))))))

;;;###autoload
(defun zenit/window-enlargen (&optional arg)
  "Enlargen the current window to focus on this one. Does not close other
windows (unlike `zenit/window-maximize-buffer') Activate again to undo."
  (interactive "P")
  (let ((param 'zenit--enlargen-last-wconf))
    (cl-destructuring-bind (window . wconf)
        (or (frame-parameter nil param)
            (cons nil nil))
      (set-frame-parameter
       nil param
       (if (and (equal window (selected-window))
                (not arg)
                wconf)
           (ignore
            (let ((source-window (selected-window)))
              (set-window-configuration wconf)
              (when (window-live-p source-window)
                (select-window source-window))))
         (prog1 (cons (selected-window) (or wconf (current-window-configuration)))
           (let* ((window (selected-window))
                  (dedicated-p (window-dedicated-p window))
                  (preserved-p (window-parameter window 'window-preserved-size))
                  (ignore-window-parameters t)
                  (window-resize-pixelwise nil)
                  (frame-resize-pixelwise nil))
             (unwind-protect
                 (progn
                   (when dedicated-p
                     (set-window-dedicated-p window nil))
                   (when preserved-p
                     (set-window-parameter window 'window-preserved-size nil))
                   (maximize-window window))
               (set-window-dedicated-p window dedicated-p)
               (when preserved-p
                 (set-window-parameter window 'window-preserved-size preserved-p))
               (add-hook 'zenit-switch-window-hook #'zenit--enlargened-forget-last-wconf-h)))))))))

;;;###autoload
(defun zenit/window-maximize-horizontally ()
  "Delete all windows to the left and right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (ignore-errors (windmove-left)) (delete-window))
    (while (ignore-errors (windmove-right)) (delete-window))))

;;;###autoload
(defun zenit/window-maximize-vertically ()
  "Delete all windows above and below the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (ignore-errors (windmove-up)) (delete-window))
    (while (ignore-errors (windmove-down)) (delete-window))))

;;;###autoload
(defun zenit/set-frame-opacity (opacity &optional frames)
  "Interactively change the current frame's opacity.

OPACITY is an integer between 0 to 100, inclusive. FRAMES is a list of
frames to apply the change to or `t' (meaning all open and future
frames). If called interactively, FRAMES defaults to the current
frame (or `t' with the prefix arg)."
  (interactive
   (list 'interactive (if current-prefix-arg t (list (selected-frame)))))
  (let* ((parameter
          (if (eq window-system 'pgtk)
              'alpha-background
            'alpha))
         (opacity
          (if (eq opacity 'interactive)
              (read-number "Opacity (0-100): "
                           (or (frame-parameter nil parameter)
                               100))
            opacity))
         (alist `((,parameter . ,opacity))))
    (if (eq frames t)
        (modify-all-frames-parameters alist)
      (dolist (frame frames)
        (modify-frame-parameters frame alist)))))

(defvar zenit--narrowed-base-buffer nil
  "Variable to store the base buffer of an indirectly narrowed
buffer.")
;;;###autoload
(defun zenit/narrow-buffer-indirectly (beg end)
  "Restrict editing in this buffer to the current region, indirectly.
This recursively creates indirect clones of the current buffer so
that the narrowing doesn't affect other windows displaying the
same buffer. Call `zenit/widen-indirectly-narrowed-buffer' to undo
it (incrementally). Inspired from
http://demonastery.org/2013/04/emacs-evil-narrow-region/"
  (interactive (if (region-active-p)
                   (list (zenit-region-beginning) (zenit-region-end))
                 (list (pos-bol) (pos-eol))))
  (deactivate-mark)
  (let ((orig-buffer (current-buffer)))
    (with-current-buffer (switch-to-buffer (clone-indirect-buffer nil nil))
      (narrow-to-region beg end)
      (setq-local zenit--narrowed-base-buffer orig-buffer))))

;;;###autoload
(defun zenit/widen-indirectly-narrowed-buffer (&optional arg)
  "Widens narrowed buffers. This command will incrementally kill
indirect buffers (under the assumption they were created by
`zenit/narrow-buffer-indirectly') and switch to their base
buffer. If ARG, then kill all indirect buffers, return the base
buffer and widen it.  If the current buffer is not an indirect
buffer, it is `widen'ed."
  (interactive "P")
  (unless (buffer-narrowed-p)
    (user-error "Buffer isn't narrowed"))
  (let ((orig-buffer (current-buffer))
        (base-buffer zenit--narrowed-base-buffer))
    (cond ((or (not base-buffer)
               (not (buffer-live-p base-buffer)))
           (widen))
          (arg
           (let ((buffer orig-buffer)
                 (buffers-to-kill (list orig-buffer)))
             (while (setq buffer (buffer-local-value 'zenit--narrowed-base-buffer buffer))
               (push buffer buffers-to-kill))
             (switch-to-buffer (buffer-base-buffer))
             (mapc #'kill-buffer (remove (current-buffer) buffers-to-kill))))
          ((switch-to-buffer base-buffer)
           (kill-buffer orig-buffer)))))

;;;###autoload
(defun zenit/toggle-narrow-buffer (beg end)
  "Narrow the buffer to BEG END. If narrowed, widen it."
  (interactive (if (region-active-p)
                   (list (zenit-region-beginning) (zenit-region-end))
                 (list (pos-bol) (pos-eol))))
  (if (buffer-narrowed-p)
      (widen)
    (narrow-to-region beg end)))

(provide 'zenit-lib '(ui))

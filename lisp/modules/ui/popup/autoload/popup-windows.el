;; ui/popup/autoload/popup-windows.el -*- lexical-binding: t; -*-

(defun +popup--delete-popup (win)
  "Delete popup window WIN, respecting its window type.
If WIN is an atomic window, delete it properly preserving window
layout.
If WIN is in a child frame, delete the frame.
Otherwise delete the window using standard window deletion."
  (when (window-valid-p win)
    (cond
     ;; For windows in child frames, delete the frame
     ((frame-parent)
      (delete-frame))

     ;; For windows in the main frame
     ((window-parent win)
      (let ((side (window-parameter win 'window-side)))
        ;; Handle atomic windows properly
        (when (window-parameter win 'window-atom)
          ;; Save window layout if needed
          (let ((window-combination-resize t))
            (set-window-parameter win 'window-atom nil)
            ;; Give sibling windows a chance to resize properly
            (balance-windows (window-parent win))))
        ;; Use consistent window deletion strategy
        (if side
            ;; Side windows should use delete-window to maintain side window
            ;; layout
            (delete-window win)
          ;; Regular windows use quit-window to handle buffers properly
          (quit-window nil win))))

     ;; Fallback for any other case
     (t (quit-window nil win)))))

;;;###autoload
(defun +popup-window-p (&optional window)
  "Return non-nil if WINDOW is a popup window. Defaults to the
current window."
  (when +popup-mode
    (let* ((window (or window (selected-window))))
      (and (windowp window)
           (window-live-p window)
           (window-parameter window 'popup)
           window))))

;;;###autoload
(defun +popup-windows ()
  "Returns a list of all popup windows."
  (cl-remove-if-not #'+popup-window-p (window-list)))

;;;###autoload
(defun +popup-shrink-to-fit (&optional window)
  "Shrinks WINDOW to fit the buffer contents, if the buffer isn't
empty.

Uses `shrink-window-if-larger-than-buffer'."
  (unless window
    (setq window (selected-window)))
  (unless (= (- (point-max) (point-min)) 0)
    (shrink-window-if-larger-than-buffer window)))

;;;###autoload
(defun +popup-alist-from-window-state (state)
  "Convert window STATE (from `window-state-get') to a
`display-buffer' alist."
  (let* ((params (alist-get 'parameters state)))
    `((side          . ,(alist-get 'window-side params))
      (window-width  . ,(alist-get 'total-width state))
      (window-height . ,(alist-get 'total-height state))
      (window-parameters ,@params))))

;; ui/popup/autoload/popup.el -*- lexical-binding: t; -*-

(defvar +popup--internal nil)

(defun +popup--remember (windows)
  "Remember WINDOWS (a list of windows) for later restoration."
  (cl-assert (cl-every #'windowp windows) t)
  (setq +popup--last
        (cl-loop for w in windows
                 collect (cons (window-buffer w)
                               (window-state-get w)))))

(defun +popup--kill-buffer (buffer ttl)
  "Tries to kill BUFFER, as was requested by a transient timer. If
it fails, eg. the buffer is visible, then set another timer and
try again later."
  (let ((inhibit-quit t))
    (cond ((not (buffer-live-p buffer)))
          ((not (get-buffer-window buffer t))
           (with-demoted-errors "Error killing transient buffer: %s"
             (with-current-buffer buffer
               (let ((kill-buffer-hook (remq '+popup-kill-buffer-hook-h kill-buffer-hook))
                     confirm-kill-processes)
                 (when-let (process (get-buffer-process buffer))
                   (when (eq (process-type process) 'real)
                     (kill-process process)))
                 (let (kill-buffer-query-functions)
                   ;; HACK The debugger backtrace buffer, when killed, called
                   ;;      `top-level'. This causes jumpiness when the popup
                   ;;      manager tries to clean it up.
                   (cl-letf (((symbol-function #'top-level) #'ignore))
                     (kill-buffer buffer)))))))
          ((let ((ttl (if (= ttl 0)
                          (or (plist-get +popup-defaults :ttl) 3)
                        ttl)))
             (with-current-buffer buffer
               (setq +popup--timer
                     (run-at-time ttl nil #'+popup--kill-buffer buffer ttl))))))))

(defun +popup--delete-window (window)
  "Do housekeeping before destroying a popup window.

+ Disables `+popup-buffer-mode' so that any hooks attached to it
  get a chance to run and do cleanup of its own.

+ Either kills the buffer or sets a transient timer, if the
  window has a `transient' window parameter (see
  `+popup-window-parameters').

+ And finally deletes the window!"
  (let ((buffer (window-buffer window))
        (inhibit-quit t))
    (and (or (buffer-file-name buffer)
             (if-let (base-buffer (buffer-base-buffer buffer))
                 (buffer-file-name base-buffer)))
         (buffer-modified-p buffer)
         (let ((autosave (+popup-parameter 'autosave window)))
           (cond ((eq autosave 't))
                 ((null autosave)
                  (y-or-n-p "Popup buffer is modified. Save it?"))
                 ((functionp autosave)
                  (funcall autosave buffer))))
         (with-current-buffer buffer (save-buffer)))
    (let ((ignore-window-parameters t))
      (if-let (wconf (window-parameter window 'saved-wconf))
          (set-window-configuration wconf)
        (+popup--delete-popup window)))
    (unless (window-live-p window)
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (+popup-buffer-mode -1)
        (unless +popup--inhibit-transient
          (let ((ttl (+popup-parameter 'ttl window)))
            (when (eq ttl 't)
              (setq ttl (plist-get +popup-defaults :ttl)))
            (cond ((null ttl))
                  ((functionp ttl)
                   (funcall ttl buffer))
                  ((not (integerp ttl))
                   (signal 'wrong-type-argument (list 'integerp ttl)))
                  ((= ttl 0)
                   (+popup--kill-buffer buffer 0))
                  ((add-hook 'kill-buffer-hook #'+popup-kill-buffer-hook-h nil t)
                   (setq +popup--timer
                         (run-at-time ttl nil #'+popup--kill-buffer
                                      buffer ttl))))))))))

(defun +popup--delete-other-windows (window)
  "Fixes `delete-other-windows' when used from a popup window."
  (when-let (window (ignore-errors (+popup/raise window)))
    (let ((ignore-window-parameters t))
      (delete-other-windows window)))
  nil)

(defun +popup--delete-popup (&optional window)
  "Delete popup window WINDOW in a manner appropriate to its type."
  (let ((window (or window (selected-window))))
    (when (window-valid-p window)
      (cond
       ((window-parent window)
        ;; Kludge. Side windows and regular windows are handled differently. The
        ;; latter is still somewhat broken. This is a bad idea.
        (if (window-parameter window 'window-side)
            (delete-window window)
          (quit-window nil window)))
       ((frame-parent) (delete-frame))
       (t (quit-window nil window))))))

(defun +popup--maybe-select-window (window origin)
  "Select a window based on `+popup--inhibit-select' and this
window's `select' parameter."
  (unless +popup--inhibit-select
    (let ((select (+popup-parameter 'select window)))
      (if (functionp select)
          (funcall select window origin)
        (select-window (if select window origin))))))

(defun +popup--normalize-alist (alist)
  "Merge `+popup-default-alist' and `+popup-default-parameters'with
ALIST."
  (when alist
    (let ((alist  ; handle defaults
           (cl-remove-duplicates
            (append alist +popup-default-alist)
            :key #'car-safe :from-end t))
          (parameters
           (cl-remove-duplicates
            (append (cdr (assq 'window-parameters alist))
                    +popup-default-parameters)
            :key #'car-safe :from-end t)))
      ;; handle `size'
      (when-let* ((size  (cdr (assq 'size alist)))
                  (side  (or (cdr (assq 'side alist)) 'bottom))
                  (param (if (memq side '(left right))
                             'window-width
                           'window-height)))
        (setq list (assq-delete-all 'size alist))
        (setf (alist-get param alist) size))
      (setf (alist-get 'window-parameters alist)
            parameters)
      ;; Fixes #1305: addresses an edge case where a popup with a :size, :width
      ;; or :height greater than the current frame's dimensions causes
      ;; hanging/freezing (a bug in Emacs' `display-buffer' API perhaps?)
      (let ((width  (cdr (assq 'window-width  alist)))
            (height (cdr (assq 'window-height alist))))
        (setf (alist-get 'window-width alist)
              (if (numberp width)
                  (min width (frame-width))
                width))
        (setf (alist-get 'window-height alist)
              (if (numberp height)
                  (min height (frame-height))
                height))
        alist))))

;;;###autoload
(defun +popup--init (window &optional alist)
  "Initializes a popup window. Run any time a popup is opened. It
sets the default window parameters for popup windows, clears
leftover transient timers and enables `+popup-buffer-mode'."
  (with-selected-window window
    (setq alist (delq (assq 'actions alist) alist))
    (set-window-parameter window 'popup t)
    ;; (set-window-parameter window 'split-window #'+popup--split-window)
    (set-window-parameter window 'delete-window #'+popup--delete-window)
    (set-window-parameter window 'delete-other-windows #'+popup--delete-other-windows)
    (set-window-dedicated-p window 'side)
    (window-preserve-size
     window (memq (window-parameter window 'window-side)
                  '(left right))
     t)
    (+popup-buffer-mode +1)
    (run-hooks '+popup-create-window-hook)))

(defun +popup--find-popups (&optional buf-list)
  "Return an alist of popup buffers.

Each element of the alist is a cons cell of the form (window .
buffer). BUF-LIST defaults to `buffer-list'."
  (let ((buf-list (or buf-list (buffer-list)))
        open-popups)
    (dolist (buffer buf-list open-popups)
      (let ((popup-status (buffer-local-value '+popup--popup-status buffer)))
        (when (and (not (minibufferp buffer))
                   (not (eq popup-status 'raised))
                   (or (eq popup-status 'popup)
                       (+popup-handle-buffer-p buffer)))
          (with-current-buffer buffer
            (setq +popup--popup-status (or popup-status 'popup)))
          (push (cons (get-buffer-window buffer) buffer)
                open-popups))))))

(defun +popup--find-buried-popups ()
  "Return the list of currently buried popups.

 Meant to be run when `+popup-mode' is enabled."
  (let ((buried-popups (+popup--find-popups
                        (cl-set-difference
                         (buffer-list)
                         (mapcar #'window-buffer
                                 (window-list))))))
    buried-popups))


;;
;;; Public library

(defun +popup-handle-buffer-p (&optional buffer)
  "Predicate to test if BUFFER qualifies for popup handling."
  (when +popup-mode
    (let ((buffer (or buffer (current-buffer))))
      (seq-some (lambda (x) (cl-destructuring-bind (buf-regexp . ignore) x
                        (and (string-match-p buf-regexp (buffer-name buffer))
                             (not ignore))))
                +popup--buffer-alist))))

;;;###autoload
(defun +popup-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is a popup buffer. Defaults to the
current buffer."
  (when +popup-mode
    (let ((buffer (or buffer (current-buffer))))
      (and (bufferp buffer)
           (buffer-live-p buffer)
           (buffer-local-value '+popup-buffer-mode buffer)
           (+popup-handle-buffer-p buffer)
           buffer))))

;;;###autoload
(defun +popup-window-p (&optional window)
  "Return non-nil if WINDOW is a popup window. Defaults to the
current window."
  (when +popup-mode
    (let ((window (or window (selected-window))))
      (and (assoc window (+popup--find-popups (buffer-list)))
           window))))

;;;###autoload
(defun +popup-buffer (buffer &optional alist)
  "Open BUFFER in a popup window. ALIST describes its features."
  (let* ((origin (selected-window))
         (window-min-height 3)
         (alist (+popup--normalize-alist alist))
         (actions (or (cdr (assq 'actions alist))
                      +popup-default-display-buffer-actions)))
    (or (let* ((alist (remove (assq 'window-width alist) alist))
               (alist (remove (assq 'window-height alist) alist))
               (window (display-buffer-reuse-window buffer alist)))
          (when window
            (+popup--maybe-select-window window origin)
            window))
        (when-let (popup (cl-loop for func in actions
                                  if (funcall func buffer alist)
                                  return it))
          (+popup--init popup alist)
          (+popup--maybe-select-window popup origin)
          popup))))

;;;###autoload
(defun +popup-parameter (parameter &optional window)
  "Fetch the window PARAMETER (symbol) of WINDOW"
  (window-parameter (or window (selected-window)) parameter))

;;;###autoload
(defun +popup-parameter-fn (parameter &optional window &rest args)
  "Fetch the window PARAMETER (symbol) of WINDOW. If it is a
function, run it with ARGS to get its return value."
  (let ((val (+popup-parameter parameter window)))
    (if (functionp val)
        (apply val args)
      val)))

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


;;
;;; Hooks

;;;###autoload
(defun +popup-update-popups-h ()
  "Update the lists of currently open and buried popups.

 Intended to be added to `window-configuration-change-hook'."
  (unless (frame-parent)
    (setq +popup-open-popup-alist (+popup--find-popups (mapcar #'window-buffer (window-list nil 'no-mini)))
          +popup-buried-popup-alist (+popup--find-buried-popups))
    ;; FIXME
    (cl-loop for (_ . buf) in +popup-open-popup-alist do
             (with-current-buffer buf
               (setq mode-line-format (append (cl-subseq (default-value 'mode-line-format) 0 0)
                                              (list '(:eval (propertize " POP-TEST" 'face 'mode-line-emphasis))
                                                    (nthcdr 0
                                                            (default-value 'mode-line-format)))))))))

;;;###autoload
(defun +popup-adjust-fringes-h ()
  "Hides the fringe in popup windows, restoring them if
`+popup-buffer-mode' is disabled."
  (let ((f (if (bound-and-true-p +popup-buffer-mode) 0)))
    (set-window-fringes nil f f fringes-outside-margins)))

;;;###autoload
(defun +popup-adjust-margins-h ()
  "Creates padding for the popup window determined by
`+popup-margin-width', restoring it if `+popup-buffer-mode' is
disabled."
  (when +popup-margin-width
    (unless (memq (window-parameter nil 'window-side) '(left right))
      (let ((m (if (bound-and-true-p +popup-buffer-mode) +popup-margin-width)))
        (set-window-margins nil m m)))))

(defvar hide-mode-line-format)
;;;###autoload
(defun +popup-set-modeline-on-enable-h ()
  "Don't show modeline in popup windows without a `modeline'
window-parameter.

Possible values for this parameter are:

  t            show the mode-line as normal
  nil          hide the modeline entirely (the default)
  a function   `mode-line-format' is set to its return value

Any non-nil value besides the above will be used as the raw value
for `mode-line-format'."
  (when (bound-and-true-p +popup-buffer-mode)
    (let ((modeline (+popup-parameter 'modeline)))
      (cond ((eq modeline 't))
            ((null modeline)
             ;; TODO use `mode-line-format' window parameter instead (emacs 26+)
             (hide-mode-line-mode +1))
            ((let ((hide-mode-line-format
                    (if (functionp modeline)
                        (funcall modeline)
                      modeline)))
               (hide-mode-line-mode +1)))))))
(put '+popup-set-modeline-on-enable-h 'permanent-local-hook t)

;;;###autoload
(defun +popup-unset-modeline-on-disable-h ()
  "Restore the modeline when `+popup-buffer-mode' is deactivated."
  (when (and (not (bound-and-true-p +popup-buffer-mode))
             (bound-and-true-p hide-mode-line-mode))
    (hide-mode-line-mode -1)))

;;;###autoload
(defun +popup-close-on-escape-h ()
  "If called inside a popup, try to close that popup window (see
`+popup/close'). If this popup window cannot be closed due to its
`quit' window parameter, try to close all other popup windows. If
called outside, try to close all popup windows (see
`+popup/close-all')."
  (if (+popup-window-p)
      (or (+popup/close)
          (+popup/close-all))
    (+popup/close-all)))

;;;###autoload
(defun +popup-cleanup-rules-h ()
  "Cleans up any duplicate popup rules."
  (interactive)
  (setq +popup--display-buffer-alist
        (cl-delete-duplicates +popup--display-buffer-alist
                              :key #'car :test #'equal :from-end t)
        +popup--buffer-alist
        (cl-delete-duplicates +popup--buffer-alist
                              :key #'car :test #'equal :from-end t))
  (when +popup-mode
    (setq display-buffer-alist +popup--display-buffer-alist)))

;;;###autoload
(defun +popup-kill-buffer-hook-h ()
  "TODO"
  (when-let (window (get-buffer-window))
    (when (+popup-window-p window)
      (let ((+popup--inhibit-transient t))
        (+popup--delete-window window)))))


;;
;;; Commands

;;;###autoload
(defalias 'other-popup #'+popup/other)

;;;###autoload
(defun +popup/buffer ()
  "Open this buffer in a popup window."
  (interactive)
  (let ((+popup-default-display-buffer-actions
         '(+popup-display-buffer-stacked-side-window-fn))
        (display-buffer-alist +popup--display-buffer-alist)
        (buffer (current-buffer)))
    (push (+popup-make-rule "." +popup-defaults) display-buffer-alist)
    (bury-buffer)
    (with-current-buffer buffer
      (setq +popup--popup-status 'popup)
      (pop-to-buffer buffer))
    (+popup-update-popups-h)))

;;;###autoload
(defun +popup/other ()
  "Cycle through popup windows, like `other-window'. Ignores regular
windows."
  (interactive)
  (if-let ((popups (mapcar #'car +popup-open-popup-alist)))
      (select-window (if (+popup-window-p)
                         (let ((window (selected-window)))
                           (or (car-safe (cdr (memq window popups)))
                               (car (delq window popups))
                               (car popups)))
                       (car popups)))
    (user-error "No popups are open")))

;;;###autoload
(defun +popup/close (&optional window force-p)
  "Close WINDOW, if it's a popup window.

This will do nothing if the popup's `quit' window parameter is
either nil or 'other. This window parameter is ignored if FORCE-P
is non-nil."
  (interactive
   (list (selected-window)
         current-prefix-arg))
  (let ((window (or window (selected-window))))
    (when (and (+popup-window-p window)
               (or force-p
                   (memq (+popup-parameter-fn 'quit window window)
                         '(t current))))
      (when +popup--remember-last
        (+popup--remember (list window)))
      (+popup--delete-popup window)
      t)))

;;;###autoload
(defun +popup/close-all (&optional force-p)
  "Close all open popup windows.

This will ignore popups with an `quit' parameter that is either
nil or 'current. This window parameter is ignored if FORCE-P is
non-nil."
  (interactive "P")
  (let (targets +popup--remember-last)
    (dolist (window (+popup-windows))
      (when (or force-p
                (memq (+popup-parameter-fn 'quit window window)
                      '(t other)))
        (push window targets)))
    (when targets
      (+popup--remember targets)
      (mapc #'+popup--delete-popup targets)
      t)))

;;;###autoload
(defun +popup/toggle ()
  "Toggle any visible popups.
If no popups are available, display the *Messages* buffer in a
popup window."
  (interactive)
  (let ((+popup--inhibit-transient t))
    (cond ((+popup-windows) (+popup/close-all t))
          ((ignore-errors (+popup/restore)))
          ((display-buffer (get-buffer "*Messages*"))))))

;;;###autoload
(defun +popup/restore ()
  "Restore the last popups that were closed, if any."
  (interactive)
  (unless +popup--last
    (error "No popups to restore"))
  (cl-loop for (buffer . state) in +popup--last
           if (buffer-live-p buffer)
           do (+popup-buffer buffer (+popup-alist-from-window-state state)))
  (setq +popup--last nil)
  t)

;;;###autoload
(defun +popup/raise (window)
  "Raise the current popup window into a regular window and return
it.

If the window has been raised already, lower it back to a popup."
  (interactive
   (list (selected-window)))
  (cl-check-type window window)
  (let* ((buffer (current-buffer))
         (+popup--inhibit-transient t)
         (popup-status (buffer-local-value '+popup--popup-status buffer))
         +popup--remember-last)
    (unless popup-status
      (user-error "Cannot raise or lower a non-popup window"))
    (+popup--delete-popup window)
    (with-current-buffer buffer
      (if (eq +popup--popup-status 'popup)
          (progn
            (setq +popup--popup-status 'raised)
            (switch-to-buffer buffer))
        (setq +popup--popup-status 'popup)
        (pop-to-buffer buffer)))
    (+popup-update-popups-h)
    (selected-window)))

;;;###autoload
(defun +popup/diagnose ()
  "Reveal what popup rule will be used for the current buffer."
  (interactive)
  (if-let (rule (cl-loop with bname = (buffer-name)
                         for (pred . action) in display-buffer-alist
                         if (and (functionp pred) (funcall pred bname action))
                         return (cons pred action)
                         else if (and (stringp pred) (string-match-p pred bname))
                         return (cons pred action)))
      (message "Rule matches: %s" rule)
    (message "No popup rule for this buffer")))


;;
;;; Advice

;;;###autoload
(defun +popup-close-a (&rest _)
  "TODO"
  (+popup/close nil t))

;;;###autoload
(defun +popup-save-a (fn &rest args)
  "Sets aside all popups before executing the original function,
usually to prevent the popup(s) from messing up the UI (or vice
versa)."
  (save-popups! (apply fn args)))

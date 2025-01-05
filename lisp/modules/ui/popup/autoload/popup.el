;; ui/popup/autoload/popup.el -*- lexical-binding: t; -*-

(defvar +popup--reference-modes nil
  "List of buffer names whose windows are treated as popups.")
(defvar +popup--reference-names nil
  "List of buffer major-modes whose buffers are treated as popups.")
(defvar +popup--reference-predicates nil
  "List of predicates to test if a buffer is treated as a popup.

Each predicate takes a buffer as an argument and returns t if it
should be considered a popup.")

(defvar +popup--suppressed-names nil
  "List of buffer names which will be suppressed.")

(defvar +popup--suppressed-modes nil
  "List of buffer major-modes which will be suppressed.")

(defvar +popup--suppressed-predicates nil
  "List of predicates to test if a buffer will be suppressed popup
buffers.")


;;
;;; Internal functions

(defun +popup--find-popup-buffers (buf-list)
  "Find popup buffers in BUF-List and return them.

This is determined by the variable `+popup-buffer-status'. The
resulting list is sorted by buffer access time."
  (let* (open-popups)
    (dolist (b buf-list open-popups)
      (let ((popup-status (plist-get (buffer-local-value '+popup-buffer-status b) :status)))
        (when (and (not (minibufferp b))
                   (not (eq popup-status 'raised))
                   (or (member popup-status '(popup user-popup))
                       (+popup-buffer-p b)))
          (with-current-buffer b
            (setq +popup-buffer-status (plist-put
                                        +popup-buffer-status
                                        :status
                                        (cond ((+popup-suppress-p b) 'suppressed)
                                              (t (or popup-status 'popup))))))
          (push (cons (get-buffer-window b) b)
                open-popups))))
    (cl-sort open-popups
             (lambda (a b) (time-less-p b a))
             :key (lambda (p)
                    (buffer-local-value 'buffer-display-time
                                        (cdr p))))))

(defun +popup--find-buried-popup-buffers ()
  "Update the list of currently buried popups.

 Meant to be run when starting command `+popup-mode'."
  (let ((buried-popups (+popup--find-popup-buffers
                        (cl-set-difference
                         (buffer-list)
                         (mapcar #'window-buffer
                                 (window-list))))))
    (if +popup-group-function
        (let (result)
          (cl-loop for (win . buf) in buried-popups do
                   (push (cons win buf)
                         (alist-get
                          (with-current-buffer buf
                            (funcall +popup-group-function))
                          result
                          nil nil 'equal)))
          result)
      (list (cons nil buried-popups)))))

(defun +popup--find-open-popup-buffers ()
  "Update the list of currently open popups."
  (let* ((open-buffers (mapcar #'window-buffer (window-list nil 'no-mini)))
         (open-popups (+popup--find-popup-buffers open-buffers)))
    open-popups))

(defun +popup--update-buried-popup-list (group-name buf action)
  "Update `+popup-buried-buffers-alist' for GROUP-NAME and BUF.
ACTION can be \\='remove or \\='add."
  (let* ((group-popups (cdr (assoc group-name +popup-buried-buffers-alist #'equal)))
         (window (get-buffer-window buf))
         (popup-entry (cons window buf)))
    (setf (alist-get group-name +popup-buried-buffers-alist nil nil #'equal)
          (cl-remove-if-not
           #'buffer-live-p
           (if (eq action 'remove)
               (cl-remove buf group-popups :key #'cdr)
             (append (list popup-entry)
                     (cl-remove popup-entry group-popups :key #'cdr)))
           :key #'cdr))))

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
                   ;;   `top-level'. This causes jumpiness when the popup
                   ;;   manager tries to clean it up.
                   (cl-letf (((symbol-function #'top-level) #'ignore))
                     (kill-buffer buffer)))))))
          ((let ((ttl (if (= ttl 0)
                          (or (plist-get +popup-defaults :ttl) 3)
                        ttl)))
             (with-current-buffer buffer
               (setq +popup--timer
                     (run-at-time ttl nil #'+popup--kill-buffer buffer ttl))))))))

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

(defun +popup--delete-window (buffer-or-window)
  "Do housekeeping before destroying a popup window.

+ Disables `+popup-buffer-mode' so that any hooks attached to it
  get a chance to run and do cleanup of its own.
+ Either kills the buffer or sets a transient timer, if the
  window has a `transient' window parameter (see
  `+popup-window-parameters').
+ And finally deletes the window!"
  (let ((buffer (or (and (bufferp buffer-or-window) buffer-or-window)
                    (window-buffer buffer-or-window)))
        (window (and (windowp buffer-or-window) buffer-or-window))
        (inhibit-quit t))
    (letf! ((defun +popup--autosave-buffer (buffer)
              (and (or (buffer-file-name buffer)
                       (if-let (base-buffer (buffer-base-buffer buffer))
                           (buffer-file-name base-buffer)))
                   (buffer-modified-p buffer)
                   (let ((autosave (+popup-parameter 'autosave buffer)))
                     (cond ((eq autosave 't))
                           ((null autosave)
                            (y-or-n-p "Popup buffer is modified. Save it?"))
                           ((functionp autosave)
                            (funcall autosave buffer))))
                   (with-current-buffer buffer (save-buffer))))
            (defun +popup--destroy-buffer (buffer)
              (with-current-buffer buffer
                (set-buffer-modified-p nil)
                (+popup-buffer-mode -1)
                (unless +popup--inhibit-transient
                  (let ((ttl (+popup-parameter 'ttl buffer)))
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
                                              buffer ttl)))))))))
      (+popup--autosave-buffer buffer)
      (let ((ignore-window-parameters t))
        (unless (+popup-parameter 'tabbed)
          (if-let (wconf (window-parameter window 'saved-wconf))
              (set-window-configuration wconf)
            (+popup--delete-popup window))))
      (unless (window-live-p window)
        (+popup--destroy-buffer buffer)))))

(defun +popup--delete-other-windows (window)
  "Fixes `delete-other-windows' when used from a popup window."
  (when-let (window (ignore-errors (+popup/raise window)))
    (let ((ignore-window-parameters t))
      (delete-other-windows window)))
  nil)

(defun +popup--normalize-alist (alist)
  "Merge `+popup-default-alist' and `+popup-default-parameters' with
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
        (setq alist (assq-delete-all 'size alist))
        (setf (alist-get param alist) size))
      (setf (alist-get 'window-parameters alist)
            parameters)
      ;; Addresses an edge case where a popup with a :size, :width or :height
      ;; greater than the current frame's dimensions causes hanging/freezing (a
      ;; bug in Emacs' `display-buffer' API perhaps?)
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

(defun +popup--split-window (window size side)
  "Ensure a non-dedicated/popup window is selected when splitting a
window."
  (unless +popup--internal
    (cl-loop for win
             in (cons (or window (selected-window))
                      (window-list nil 0 window))
             unless (+popup-window-p win)
             return (setq window win)))
  (let ((ignore-window-parameters t))
    (split-window window size side)))

(defun +popup--maybe-select-window (window origin)
  "Select a window based on `+popup--inhibit-select' and this
window's `select' parameter."
  (unless +popup--inhibit-select
    ;; REVIEW: Once our minimum version is bumped up to Emacs 30.x, replace this
    ;;   with `post-command-select-window' window parameter.
    (let ((select (+popup-parameter 'select window)))
      (if (functionp select)
          (funcall select window origin)
        (select-window (if select window origin))))))

;;;###autoload
(defun +popup--init (window &optional alist)
  "Initializes a popup window.
Run any time a popup is opened. It sets the default window
parameters for popup windows, clears leftover transient timers
and enables `+popup-buffer-mode'."
  (with-selected-window window
    (setq alist (delq (assq 'actions alist) alist))
    (set-window-parameter window 'popup t)
    (set-window-parameter window 'split-window #'+popup--split-window)
    (set-window-parameter window 'delete-window #'+popup--delete-window)
    (set-window-parameter window 'delete-other-windows #'+popup--delete-other-windows)
    (set-window-dedicated-p window 'popup)
    (window-preserve-size
     window (memq (window-parameter window 'window-side)
                  '(left right))
     t)
    (+popup-buffer-mode +1)
    (run-hooks '+popup-create-window-hook)))


;;
;;; Public library

;;;###autoload
(defun +popup-update-reference-vars ()
  "Update internal reference lists from `+popup-reference-buffers'."
  ;; Reset all variables
  (setq +popup--reference-modes nil
        +popup--reference-names nil
        +popup--reference-predicates nil
        +popup--suppressed-names nil
        +popup--suppressed-modes nil
        +popup--suppressed-predicates nil)

  (cl-loop for entry in +popup-reference-buffers do
           (pcase-exhaustive entry
             ;; Handle suppressed entries up front
             (`(,e . hide)
              (pcase-exhaustive e
                ((pred stringp)
                 (cl-pushnew e +popup--suppressed-names :test #'equal))
                ((and (pred symbolp)
                      (guard (or (get e 'derived-mode-parent)
                                 (get e 'mode-class)
                                 (string-suffix-p "-mode" (symbol-name e)))))
                 (cl-pushnew e +popup--suppressed-modes))
                ((pred functionp)
                 (cl-pushnew e +popup--suppressed-predicates))))
             ;; Handle regular entries
             ((pred stringp)
              (cl-pushnew entry +popup--reference-names :test #'equal))
             ((and (pred symbolp)
                   (guard (or (get entry 'derived-mode-parent)
                              (get entry 'mode-class)
                              (string-suffix-p "-mode" (symbol-name entry)))))
              (cl-pushnew entry +popup--reference-modes))
             ((pred functionp)
              (cl-pushnew entry +popup--reference-predicates)))))

;;;###autoload
(defun +popup-buffer-p (buf)
  "Predicate to test if buffer BUF qualifies for popup handling.
Criteria are listed in `+popup-reference-buffers'."
  (or (seq-some (lambda (buf-regexp)
                  (string-match-p buf-regexp (buffer-name buf)))
                +popup--reference-names)
      (member (buffer-local-value 'major-mode buf) +popup--reference-modes)
      (seq-some (lambda (pred) (funcall pred buf)) +popup--reference-predicates)))

;;;###autoload
(defun +popup-suppress-p (buf)
  "Predicate to check if popup buffer BUF needs to be suppressed."
  (or (seq-some (lambda (buf-regexp)
                  (string-match-p buf-regexp (buffer-name buf)))
                +popup--suppressed-names)
      (member (buffer-local-value 'major-mode buf) +popup--suppressed-modes)
      (seq-some (lambda (pred) (funcall pred buf)) +popup--suppressed-predicates)))

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
(defun +popup-buffer (buffer &optional alist)
  "Open BUFFER in a popup window. ALIST describes its features."
  (let* ((origin (selected-window))
         (window-min-height 3)
         (alist (+popup--normalize-alist alist))
         (actions (or (cdr (assq 'actions alist))
                      +popup-default-display-buffer-actions))
         (ttl (alist-get 'ttl (assq 'window-parameters alist)))
         (quit (alist-get 'quit (assq 'window-parameters alist)))
         (select (alist-get 'select (assq 'window-parameters alist)))
         (modeline (alist-get 'modeline (assq 'window-parameters alist)))
         (autosave (alist-get 'autosave (assq 'window-parameters alist)))
         (tabbed (alist-get 'tabbed (assq 'window-parameters alist)))
         (side (alist-get 'side alist)))
    (letf! ((defun set-popup-buffer-status (buffer side ttl quit select modeline autosave)
              (with-current-buffer buffer
                (setq +popup-buffer-status (plist-put +popup-buffer-status :tabbed side)
                      +popup-buffer-status (plist-put +popup-buffer-status :ttl ttl)
                      +popup-buffer-status (plist-put +popup-buffer-status :quit quit)
                      +popup-buffer-status (plist-put +popup-buffer-status :select select)
                      +popup-buffer-status (plist-put +popup-buffer-status :modeline modeline)
                      +popup-buffer-status (plist-put +popup-buffer-status :autosave autosave)))))
      (or (when tabbed
            (when-let* ((tab-win (cl-loop for (win . buf) in +popup-open-buffers-alist
                                          for pop-side = (+popup-parameter 'window-side win)
                                          for pop-tabbed = (+popup-parameter 'tabbed win)
                                          if (and (eq pop-side side)
                                                  pop-tabbed)
                                          return win)))
              (with-selected-window tab-win
                (switch-to-buffer buffer)
                (set-popup-buffer-status buffer side ttl quit select modeline autosave)
                (+popup--init tab-win alist))
              (+popup--maybe-select-window tab-win origin)))

          (let* ((alist (remove (assq 'window-width alist) alist))
                 (alist (remove (assq 'window-height alist) alist))
                 (window (display-buffer-reuse-window buffer alist)))
            (when window
              (set-popup-buffer-status buffer side ttl quit select modeline autosave)
              (+popup--maybe-select-window window origin)
              window))
          (when-let (popup (cl-loop for func in actions
                                    if (funcall func buffer alist)
                                    return it))
            (set-popup-buffer-status buffer side ttl quit select modeline autosave)
            (+popup--record-parent (or (caar (window-prev-buffers popup)) (window-buffer origin)) buffer)
            (+popup--init popup alist)
            (+popup--maybe-select-window popup origin)
            popup)))))

;;;###autoload
(defun +popup-parameter (parameter &optional buffer-or-window)
  "Fetch the PARAMETER (symbol) of BUFFER-OR-WINDOW.

BUFFER-OR-WINDOW defaults to the `selected-window'."
  (let ((buffer-or-window (or buffer-or-window (selected-window))))
    (if (bufferp buffer-or-window)
        (plist-get (buffer-local-value '+popup-buffer-status buffer-or-window)
                   (zenit-keyword-intern (symbol-name parameter)))
      (window-parameter buffer-or-window parameter))))

;;;###autoload
(defun +popup-parameter-fn (parameter &optional buffer-or-window &rest args)
  "Fetch the PARAMETER (symbol) of BUFFER-OR-WINDOW.

If it is a function, run it with ARGS to get its return value."
  (let ((val (+popup-parameter parameter buffer-or-window)))
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
;;; Macros

;;;###autoload
(defmacro with-popup-rules! (rules &rest body)
  "Evaluate BODY with popup RULES. RULES is a list of popup rules.
Each rule should match the arguments of `+popup-define' or the
:popup setting."
  (declare (indent defun))
  `(let ((+popup--display-buffer-alist +popup--old-display-buffer-alist)
         (+popup--reference-buffers +popup--old-reference-buffers)
         display-buffer-alist +popup-reference-buffers)
     (set-popup-rules! ,rules)
     (when (bound-and-true-p +popup-mode)
       (setq display-buffer-alist +popup--display-buffer-alist
             +popup-reference-buffers +popup--reference-buffers)
       (+popup-update-reference-vars))
     ,@body))

;;;###autoload
(defmacro save-popups! (&rest body)
  "Set aside all popups before executing BODY.

Usually to prevent the popup(s) from messing up the UI (or vice
versa)."
  `(let* ((in-popup-p (+popup-buffer-p (current-buffer)))
          (popups (+popup-windows))
          (+popup--inhibit-transient t)
          buffer-list-update-hook
          +popup--last)
     (dolist (p popups)
       (+popup/close p 'force))
     (unwind-protect
         (progn ,@body)
       (when popups
         (let ((origin (selected-window)))
           (+popup/restore)
           (unless in-popup-p
             (select-window origin)))))))


;;
;;; Hooks

;;;###autoload
(defun +popup-update-popup-alists-h ()
  "Update the lists of current popups.

This function will update the variables
`+popup-open-buffers-alist' and `+popup-buried-buffers-alist'.

 Intended to be added to `window-configuration-change-hook'."
  (unless (frame-parent)
    (let ((open-popups (+popup--find-open-popup-buffers)))
      (if (not +popup-buried-buffers-alist)
          (setq +popup-buried-buffers-alist (+popup--find-buried-popup-buffers))
        (let ((closed-popups (cl-remove-if-not
                              (lambda (win-buf)
                                (memq (plist-get (buffer-local-value '+popup-buffer-status (cdr win-buf)) :status)
                                      '(popup user-popup)))
                              (cl-set-difference +popup-open-buffers-alist open-popups :key #'cdr))))
          ;; Handle newly opened and closed popups
          (dolist (popup-entry (append open-popups closed-popups))
            (let* ((buf (cdr popup-entry))
                   (group-name (when +popup-group-function
                                 (with-current-buffer buf
                                   (funcall +popup-group-function)))))
              (+popup--update-buried-popup-list
               group-name buf
               (if (or (not (buffer-live-p buf))
                       (member popup-entry open-popups))
                   'remove 'add))))))
      (setq +popup-open-buffers-alist open-popups))))

;;;###autoload
(defun +popup-suppress-popups-h ()
  "Suppress open popups defined in `+popup-reference-buffers'.
This should run after `+popup-update-popup-alists-h' in
`window-configuration-change-hook'."
  ;; Check if popup-status for any open popup is 'suppressed. If yes, change its
  ;; popup-status to 'popup and hide it.
  (let ((configuration-changed-p))
    (cl-loop for (win . buf) in +popup-open-buffers-alist do
             (when (eq (plist-get (buffer-local-value '+popup-buffer-status buf) :status) 'suppressed)
               (setq configuration-changed-p t)
               (with-selected-window win
                 (setq-local +popup-buffer-status (plist-put +popup-buffer-status :status 'popup))
                 ;; Leave the buffer alive
                 (set-window-parameter win 'ttl nil)
                 ;; If window was previously showing a different buffer, switch
                 ;; to it
                 (if-let ((wpb (window-prev-buffers win))
                          (switch-to-buffer-preserve-window-point t))
                     (switch-to-buffer (caar wpb))
                   ;; otherwise kill this window/frame
                   (+popup--delete-popup win))
                 (message (format "Popup suppressed: %s" (buffer-name buf))))))
    (when configuration-changed-p
      (+popup-update-popup-alists-h))))

;;;###autoload
(defun +popup-adjust-fringes-h ()
  "Hides the fringe in popup windows, restoring them if
`+popup-buffer-mode' is disabled."
  (let ((f (if (bound-and-true-p +popup-buffer-mode) 0)))
    (set-window-fringes nil f f fringes-outside-margins)))

;;;###autoload
(defun +popup-adjust-margins-h ()
  "Creates padding for the popup window determined by
`+popup-margin-width',restoring it if `+popup-buffer-mode' is
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
             (bound-and-true-p hide-mode-line-mode)
             (not (bound-and-true-p global-hide-mode-line-mode)))
    (hide-mode-line-mode -1)))

;;;###autoload
(defun +popup-close-on-escape-h ()
  "If called inside a popup, try to close that popup window (see
`+popup/close'). If called outside, try to close all popup
windows (see `+popup/close-all')."
  (if (+popup-window-p)
      (+popup/close)
    (+popup/close-all)))

;;;###autoload
(defun +popup-cleanup-rules-h ()
  "Cleans up any duplicate popup rules.
The last modification is kept."
  (interactive)
  (setq +popup--display-buffer-alist
        (cl-delete-duplicates +popup--display-buffer-alist
                              :key #'car :test #'equal :from-end t)
        +popup--reference-buffers
        (cl-delete-duplicates +popup--reference-buffers
                              :test (lambda (x y)
                                      (cond ((consp x) (equal (car x) y))
                                            ((consp y) (equal x (car y)))
                                            (t (equal x y))))
                              :from-end t))
  (when (bound-and-true-p +popup-mode)
    (setq display-buffer-alist +popup--display-buffer-alist
          +popup-reference-buffers +popup--reference-buffers)))

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
        (+popup-reference-buffers +popup--reference-buffers)
        (buffer (current-buffer)))
    (unless (+popup-buffer-p buffer)
      (push (+popup-make-rule "." (plist-put +popup-defaults :ttl nil)) display-buffer-alist))
    (bury-buffer)
    (with-current-buffer buffer
      (setq +popup-buffer-status
            (plist-put +popup-buffer-status :status (if (+popup-buffer-p buffer)
                                                        'popup
                                                      'user-popup)))
      (pop-to-buffer buffer))))

;;;###autoload
(defun +popup/other ()
  "Cycle through popup windows, like `other-window'.

Ignores regular windows."
  (interactive)
  (if-let (popups (cl-remove-if-not
                   (lambda (w) (or (+popup-window-p w)
                                   ;; This command should be able to hop between
                                   ;; windows with a `no-other-window'
                                   ;; parameter, since `other-window' won't.
                                   (window-parameter w 'no-other-window)))
                   (window-list)))
      (select-window (if (or (+popup-window-p)
                             (window-parameter nil 'no-other-window))
                         (let ((window (selected-window)))
                           (or (car-safe (cdr (memq window popups)))
                               (car (delq window popups))
                               (car popups)))
                       (car popups)))
    (user-error "No popups are open")))

(defun +popup--reap-parents (window keep)
  "Close all parent popup buffers of WINDOW.
Windows with a `quit' window parameter matching the list KEEP
will be ignored."
  (when-let ((parents (with-current-buffer (window-buffer window) +popup--parents)))
    (dolist (parent parents)
      (unless (memq (cdr parent) keep)
        (+popup--kill-buffer (car parent) 1)))))
// Add a docstring to +popup--close-tab-current-window as well as detailed explanations as code comments to every line. AI!
(defun +popup--close-tab-current-window (window &optional force-p)
  (let ((buffer (window-buffer window))
        next-buf)
    (with-current-buffer buffer
      (when (or force-p
                (memq (+popup-parameter 'quit buffer)
                      '(t current)))
        (setq next-buf (car-safe (remq buffer (+popup-tabs-fn))))
        (setq +popup-buffer-status (plist-put +popup-buffer-status :tabbed nil))
        (tab-line-mode -1)
        (if next-buf
            (when (switch-to-buffer next-buf)
              ;; HACK 2025-01-05: For whatever reason this is necessary as
              ;;   otherwise the window with the tabs won't stay dedicated.
              (set-window-dedicated-p window 'popup)
              (+popup--delete-window buffer))
          (set-window-parameter window 'tabbed nil)
          (+popup--delete-window window))))))

(defun +popup--close-tabs-current-window (window &optional force-p)
  (with-selected-window window
    (let ((tabs (+popup-tabs-fn))
          tab
          remaining)
      (while tabs
        (setq tab (pop tabs))
        (with-current-buffer tab
          (if (and (not force-p)
                   (not (memq (+popup-parameter 'quit tab)
                              '(t other))))
              (push tab remaining)
            (setq +popup-buffer-status (plist-put +popup-buffer-status :tabbed nil))
            (tab-line-mode -1)
            (bury-buffer tab)
            (+popup--delete-window tab))))
      (if remaining
          (when (switch-to-buffer (car remaining))
            ;; HACK 2025-01-05: For whatever reason this is necessary as
            ;;   otherwise the window with the tabs won't stay dedicated.
            (set-window-dedicated-p window 'popup))
        (set-window-parameter window 'tabbed nil)
        (+popup--delete-window window)))))

;;;###autoload
(defun +popup/close (&optional window force-p)
  "Close WINDOW, if it's a popup window.

This will do nothing if the popup's `quit' window parameter is
either nil or \\='other. This window parameter is ignored if
FORCE-P is non-nil."
  (interactive
   (list (selected-window)
         current-prefix-arg))
  (let ((window (or window (selected-window))))
    (when (+popup-window-p window)
      (when +popup--remember-last
        (+popup--remember (list window)))
      (if (+popup-parameter 'tabbed window)
          (+popup--close-tab-current-window window force-p)
        (when (or force-p
                  (memq (+popup-parameter-fn 'quit window window)
                        '(t current)))
          (+popup--reap-parents window '(nil other))
          (+popup--delete-window window)
          t)))))

;;;###autoload
(defun +popup/close-all (&optional force-p)
  "Close all open popup windows.

This will ignore popups with an `quit' parameter that is either
nil or \\='current. This window parameter is ignored if FORCE-P
is non-nil."
  (interactive "P")
  (let (targets +popup--remember-last)
    (dolist (window (+popup-windows))
      (when (or force-p
                (+popup-parameter 'tabbed window)
                (memq (+popup-parameter-fn 'quit window window)
                      '(t other)))
        (push window targets)))
    (when targets
      (+popup--remember targets)
      (mapc (lambda (target)
              (if (and (not force-p)
                       (+popup-parameter 'tabbed target))
                  (+popup--close-tabs-current-window target)
                (+popup--reap-parents target '(nil current))
                (+popup--delete-window target)))
            targets)
      t)))

;;;###autoload
(defun +popup/toggle ()
  "Toggle any visible popups.
If no popups are available, display the *Messages* buffer in a popup window."
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
(defun +popup/raise (window &optional arg)
  "Raise the current popup window into a regular window and
return it. If prefix ARG, raise the current popup into a new
window and return that window."
  (interactive
   (list (selected-window) current-prefix-arg))
  (cl-check-type window window)
  (unless (+popup-window-p window)
    (user-error "Cannot raise a non-popup window"))
  (let ((buffer (current-buffer))
        (+popup--inhibit-transient t)
        +popup--remember-last)
    (+popup/close window 'force)
    (let (display-buffer-alist)
      (if arg
          (pop-to-buffer buffer)
        (switch-to-buffer buffer))
      (with-current-buffer buffer
        (setq +popup-buffer-status
              (plist-put +popup-buffer-status :status
                         (when (+popup-buffer-p buffer) 'raised)))
        (+popup-buffer-mode -1)))
    (selected-window)))

;;;###autoload
(defun +popup/toggle-type (window &optional arg)
  (interactive
   (list (selected-window) current-prefix-arg))
  (cl-check-type window window)
  (let ((buffer (current-buffer)))
    (with-current-buffer buffer
      (pcase (plist-get +popup-buffer-status :status)
        ((or 'popup 'user-popup) (+popup/raise window arg))
        (_ (+popup/buffer))))))

;;;###autoload
(defun +popup/diagnose ()
  "Reveal what popup rule will be used for the current buffer."
  (interactive)
  (if-let (rule (cl-loop with bname = (buffer-name)
                         for (pred . action) in display-buffer-alist
                         if (and (functionp pred) (funcall pred bname action))
                         return (cons pred action)
                         else if (and (stringp pred) (string-match-p pred bname))
                         return (cons pred action)
                         else if (and (consp pred) (buffer-match-p pred bname))
                         return (cons pred action)))
      (message "Rule matches: %s" rule)
    (message "No popup rule for this buffer")))


;;
;;; Advice

(defun +popup--record-parent (parent-buffer &optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (let ((parents (buffer-local-value '+popup--parents parent-buffer))
            (quit (+popup-parameter 'quit parent-buffer)))
        (when (and (+popup-buffer-p buffer)
                   ;; Prevent self-reference
                   (not (eq buffer parent-buffer))
                   ;; Avoid duplicate parents
                   (not (assq parent-buffer parents))
                   (not +popup--ignore-parent))
          (push (cons parent-buffer quit) parents)
          (setq +popup--parents parents))))))

;;;###autoload
(defun +popup-close-a (&rest _)
  "TODO"
  (+popup/close nil t))

;;;###autoload
(defun +popup-save-a (fn &rest args)
  "Sets aside all popups before executing the original
function, usually to prevent the popup(s) from messing up the
UI (or vice versa)."
  (save-popups! (apply fn args)))

;;; tab-line

;;;###autoload
(defun +popup-tabs-fn ()
  (let ((grp (when +popup-group-function
               (funcall +popup-group-function)))
        (side (plist-get (buffer-local-value '+popup-buffer-status (current-buffer)) :tabbed)))
    (when side
      (cl-sort
       (cons (current-buffer)
             (delete-dups
              (cl-delete-if-not
               (lambda (b)
                 (and (eq (plist-get (buffer-local-value '+popup-buffer-status b) :tabbed) side)
                      (buffer-live-p b)))
               (mapcar #'cdr (alist-get grp +popup-buried-buffers-alist nil nil #'equal)))))
       #'string< :key #'buffer-name))))

(defun +popup--single-tab-p ()
  (length= (+popup-tabs-fn) 1))

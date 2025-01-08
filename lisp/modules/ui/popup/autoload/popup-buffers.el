;; ui/popup/autoload/popup-buffers.el -*- lexical-binding: t; -*-

(defun +popup--find-popup-buffers (buf-list)
  "Find popup buffers in BUF-List and return them.

This is determined by the variable `+popup-buffer-status'. The
resulting list is sorted by buffer access time."
  (let* (open-popups)
    (dolist (b buf-list open-popups)
      (let ((popup-status (+popup-buffer-parameter 'status b)))
        (when (and (not (minibufferp b))
                   (not (eq popup-status 'raised))
                   (or (member popup-status '(popup user-popup))
                       (+popup-buffer-p b)))
          (+popup-buffer-set-status b :status
                                    (cond ((+popup-buffer-suppress-p b) 'suppressed)
                                          (t (or popup-status 'popup))))
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

(defun +popup--kill-buffer (buffer ttl)
  "Tries to kill BUFFER, as was requested by a transient timer.
If it fails, eg. the buffer is visible, then set another timer
and try again later."
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

;;;###autoload
(defun +popup-buffer-p (buf)
  "Predicate to test if buffer BUF qualifies for popup handling.
Criteria are listed in `+popup-reference-buffers' and :status in
`+popup-buffer-status'."
  (or (member (+popup-buffer-parameter 'status buf)
              '(popup raised user-popup))
      (seq-some (lambda (buf-regexp)
                  (string-match-p buf-regexp (buffer-name buf)))
                +popup--reference-names)
      (member (buffer-local-value 'major-mode buf) +popup--reference-modes)
      (seq-some (lambda (pred) (funcall pred buf)) +popup--reference-predicates)))

;;;###autoload
(defun +popup-buffer-suppress-p (buf)
  "Predicate to check if popup buffer BUF needs to be suppressed."
  (or (eq (+popup-buffer-parameter 'status buf)
          'suppressed)
      (seq-some (lambda (buf-regexp)
                  (string-match-p buf-regexp (buffer-name buf)))
                +popup--suppressed-names)
      (member (buffer-local-value 'major-mode buf) +popup--suppressed-modes)
      (seq-some (lambda (pred) (funcall pred buf)) +popup--suppressed-predicates)))

;;;###autoload
(defun +popup-buffer-set-status (buffer &rest plist)
  "Set plist `+popup-buffer-status'."
  (with-current-buffer buffer
    (setq +popup-buffer-status (zenit-plist-merge plist +popup-buffer-status))))

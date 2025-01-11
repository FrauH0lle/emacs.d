;; lisp/core/lib/zenit-lib-process.el -*- lexical-binding: t; -*-

;; `print!'
(autoload #'print! "zenit-lib-print" nil nil 'macro)


;;;###autoload
(defun zenit-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Returns (STATUS . OUTPUT) when it is done, where STATUS is the
returned error code of the process and OUTPUT is its stdout
output."
  (with-temp-buffer
    (cons (or (apply #'call-process command nil t nil args)
              -1)
          (string-trim (buffer-string)))))

;;;###autoload
(defun zenit-exec-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Unlike `zenit-call-process', this pipes output to
`standard-output' on the fly to simulate \\='exec\\=' in the
shell, so batch scripts could run external programs synchronously
without sacrificing their output.

Warning: freezes indefinitely on any stdin prompt."
  ;; FIXME Is there any way to handle prompts?
  (with-temp-buffer
    (cons (let ((process
                 (make-process :name "zenit-sh"
                               :buffer (current-buffer)
                               :command (cons command (remq nil args))
                               :connection-type 'pipe))
                done-p)
            (set-process-filter
             process (lambda (_process output)
                       (princ output (current-buffer))
                       (print! output)))
            (set-process-sentinel
             process (lambda (process _event)
                       (when (memq (process-status process) '(exit stop))
                         (setq done-p t))))
            (while (not done-p)
              (sit-for 0.1))
            (process-exit-status process))
          (string-trim (buffer-string)))))

(provide 'zenit-lib '(process))

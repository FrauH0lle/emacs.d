;; lang/ess/patches/ess-rdired.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'el-patch)
  (require 'ess-rdired))

;; PATCH `ess-rdired'
(el-patch-defun ess-rdired-refresh ()
  "Refresh the `ess-rdired' buffer."
  (let* ((buff (get-buffer-create ess-rdired-buffer))
         (proc-name (buffer-local-value 'ess-local-process-name buff))
         (proc (get-process proc-name))
         (out-buff (get-buffer-create " *ess-rdired-output*"))
         text)
    (when (and proc-name proc
               (not (process-get proc 'busy)))
      (ess--foreground-command ess-rdired-objects out-buff nil nil nil proc)
      (with-current-buffer out-buff
        (goto-char (point-min))
        ;; Delete two lines. One filled with +'s from R's prompt
        ;; printing, the other with the header info from the data.frame
        (el-patch-remove
          (delete-region (point-min) (1+ (line-end-position 2)))
          (setq text (split-string (buffer-string) "\n" t "\n"))
          (erase-buffer))
        (el-patch-add
          (when (> (count-lines (point-min) (point-max)) 2)
            (delete-region (point-min) (1+ (line-end-position 2)))
            (setq text (split-string (buffer-string) "\n" t "\n"))
            (erase-buffer))))
      (with-current-buffer buff
        (setq tabulated-list-entries
              (mapcar #'ess-rdired--tabulated-list-entries text))
        (let ((entry (tabulated-list-get-id))
              (col (current-column)))
          (tabulated-list-print)
          (while (not (equal entry (tabulated-list-get-id)))
            (forward-line))
          (move-to-column col))))))

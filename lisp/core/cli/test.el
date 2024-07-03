;; lisp/core/cli/test.el -*- lexical-binding: t; -*-

(defun zenit--emacs-binary ()
  (let ((emacs-binary-path (zenit-path invocation-directory invocation-name))
        (runemacs-binary-path (if zenit--system-windows-p (zenit-path invocation-directory "runemacs.exe"))))
    (if (and runemacs-binary-path (file-exists-p runemacs-binary-path))
        runemacs-binary-path
      emacs-binary-path)))

(defun zenit-cli--tests-cleanup ()
  "Cleanup artifacts from tests."
  (let ((artifact-files
         (list
          (zenit-glob zenit-emacs-dir "projectile-bookmarks.eld")))
        (artifact-dirs
         (list
          (zenit-glob zenit-emacs-dir "eln-cache"))))
    (dolist (f artifact-files)
      (when (and f (file-exists-p f))
        (delete-file f)))
    (dolist (d artifact-dirs)
      (when (and d (file-exists-p d))
        (delete-directory d t)))))

(defun zenit-cli-test (&optional files)
  (require 'ansi-color)
  (print! (start "Running unit tests..."))
  (print-group!
   (with-temp-buffer
     (let ((files (or files (zenit-glob zenit-core-dir "test/test-*.el")))
           read-files)
       (dolist (file files)
         (cl-destructuring-bind (_status . output)
             (apply #'zenit-exec-process
                    (zenit--emacs-binary)
                    "-Q"
                    "--batch"
                    (concat "--init-directory=" (expand-file-name user-emacs-directory))
                    "--eval" (prin1-to-string '(setq load-prefer-newer t))
                    "-l" (concat zenit-core-dir "zenit-core.el")
                    "--eval" (prin1-to-string '(zenit-context-push 'init))
                    "-l" (file-name-concat zenit-local-dir (car (mapcar #'car zenit-cache-generators)))
                    "--eval" (prin1-to-string '(zenit-context-push 'tests))
                    "-l" (concat zenit-core-dir "test/helpers.el")
                    (list "-l" file
                          "-f" "buttercup-run"))
           (insert (replace-regexp-in-string ansi-color-control-seq-regexp "" output))
           (terpri)
           (push file read-files)))

       (let ((total 0)
             (total-failed 0)
             (i 0))
         (print! "\n----------------------------------------\nTests finished")
         (print-group!
          (goto-char (point-min))
          (while (re-search-forward "^Ran \\([0-9]+\\) specs, \\([0-9]+\\) failed," nil t)
            (let ((ran (string-to-number (match-string 1)))
                  (failed (string-to-number (match-string 2))))
              (when (> failed 0)
                (terpri)
                (print! (warn "(%s) Failed %d/%d tests")
                        (path (nth i read-files))
                        failed ran)
                (save-excursion
                  (print-group!
                   (print!
                    "%s" (string-trim
                          (buffer-substring
                           (match-beginning 0)
                           (dotimes (_ failed (point))
                             (search-backward "========================================"))))))))
              (cl-incf total ran)
              (cl-incf total-failed failed)
              (cl-incf i))))
         (terpri)
         (zenit-cli--tests-cleanup)
         (if (= total-failed 0)
             (print! (success "Ran %d tests successfully." total))
           (print! (error "Ran %d tests, %d failed") total total-failed)
           (kill-emacs 1)))))))

;; lisp/core/cli/freeze.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defun zenit-cli-freeze-packages ()
  "Freeze packages and write lockfiles."
  (print! (start "Freezing your package versions..."))
  ;; First we must be sure that configs have been fully loaded. Which usually
  ;; aren't so in an noninteractive session.
  (let ((noninteractive nil))
    (zenit-initialize)
    (zenit-initialize-packages)
    (load (concat user-emacs-directory "lisp/init-core-interactive")
          nil 'nomessage)
    (load (concat user-emacs-directory "lisp/init-core-modules")
          nil 'nomessage)
    (zenit-initialize-modules))
  (straight-x-freeze-versions))

;; lisp/core/cli/freeze.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defun zenit-cli-freeze-packages ()
  "Freeze packages and write lockfiles."
  (zenit-initialize-packages)
  (zenit--cli-recipes-update)
  (let ((esc (unless init-file-debug "\033[1A")))
    (zenit--with-package-recipes (zenit-package-recipe-list)
        (recipe package type local-repo)
      (when local-repo
        (print! (start "\033[KNormalize %s...%s") package esc)
        (let ((straight--default-directory (straight--repos-dir local-repo)))
          (straight-vc 'normalize type recipe))))
    (delete-directory (straight--modified-dir) 'recursive)
    (print! (start "Freezing package versions..."))
    (straight-freeze-versions t)))

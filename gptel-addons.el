;; gptel-addons.el -*- lexical-binding: t; -*-


(defun +gptel-add-commit-conventions ()
  (interactive)
  (let ((file (file-name-concat (zenit-project-root) "docs" "commit_messages.md")))
    (if (file-exists-p file)
        (cl-pushnew (ensure-list file) gptel-context--alist :test #'equal)
      (user-error "%s does not exist" file))))

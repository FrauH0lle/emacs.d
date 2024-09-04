;; completion/vertico/autoload/workspaces.el -*- lexical-binding: t; -*-
;;;###if (modulep! :ui workspaces)

;;;###autoload
(defun +vertico/embark-open-in-new-workspace (file)
  "Open file in a new workspace."
  (interactive "GFile:")
  (+workspace/new)
  (find-file file))

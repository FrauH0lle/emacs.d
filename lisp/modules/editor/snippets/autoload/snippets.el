;; editor/snippets/autoload/snippets.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +snippets-activate-extra-mode (mode)
  "Add MODE to `+snippets--extra-modes'"
  (when mode
    (add-to-list '+snippets--extra-modes mode)))

;;;###autoload
(defun +snippets-deactivate-extra-mode (mode)
  "Remove MODE from `+snippets--extra-modes'"
  (when mode
    (setq-local +snippets--extra-modes
                (remove mode +snippets--extra-modes))))


;;
;;; Hooks

;;;###autoload
(defun +snippets-enable-project-modes-h (mode &rest _)
  "Automatically enable snippet libraries for project minor modes defined with
`def-project-mode!'."
  (if (symbol-value mode)
      (+snippets-activate-extra-mode mode)
    (+snippets-deactivate-extra-mode mode)))

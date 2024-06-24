;; checkers/spell/autoload/jinx.el -*- lexical-binding: t; -*-
;;;###if (executable-find "enchant-2")

;;;###autoload
(defalias '+spell/correct #'jinx-correct)

;;;###autoload
(defun +spell/add-word (word &optional scope)
  "Add WORD to your personal dictionary, within SCOPE.

SCOPE can be `buffer' or `session' to exclude words only from the
current buffer or session. Otherwise, the addition is permanent."
  (interactive)
  (user-error "Not supported yet"))

;;;###autoload
(defun +spell/remove-word (word &optional _scope)
  "Remove WORD from your personal dictionary."
  (interactive)
  (user-error "Not supported yet"))

;;;###autoload
(defalias '+spell/next-error #'jinx-next)

;;;###autoload
(defalias '+spell/previous-error #'jinx-previous)

;; tools/tree-sitter/autoload-treesit.el -*- lexical-binding: t; -*-


;; tools/tree-sitter/autoload.el -*- lexical-binding: t; -*-

;;;###autodef (fset 'set-tree-sitter-lang! #'ignore)
(defun set-tree-sitter-lang! (langs &optional remove)
  "Enable tree-sitter for LANGS.

LANGS can be a symbol or list of symbols. The symbols in LANGS
should correspond to the `:lang' recipe argument for
`treesit-auto-recipe-list'.

If REMOVE is non-nil, the tree-sitter will be disabled for these
langs."
  (let ((langs (ensure-list langs)))
    ;; The execution order matters here. The language needs to be present in
    ;; `treesit-auto-langs' before it can be added to `auto-mode-alist' and vice
    ;; versa before removal.
    (cond (remove
           (when (featurep 'treesit-auto)
             (dolist (r (treesit-auto--filter-recipes-with-langs langs (treesit-auto--selected-recipes)))
               (setq auto-mode-alist
                     (cl-remove (cons (treesit-auto-recipe-ext r) (treesit-auto-recipe-ts-mode r))
                                auto-mode-alist :test #'equal))))
           (dolist (lang langs)
             (delq! lang treesit-auto-langs)))
          (t
           (dolist (lang langs)
             (cl-pushnew lang treesit-auto-langs))
           (when (featurep 'treesit-auto)
             (treesit-auto-add-to-auto-mode-alist langs))))))

;;;###autoload
(defun +tree-sitter-get-textobj (group &optional query)
  "A wrapper around `evil-textobj-tree-sitter-get-textobj' to
prevent eager expansion."
  (eval `(evil-textobj-tree-sitter-get-textobj ,group ,query)))

;;;###autoload
(defun +tree-sitter-goto-textobj (group &optional previous end query)
  "Thin wrapper that returns the symbol of a named function, used in
keybindings."
  (let ((sym (intern (format "+goto%s%s-%s" (if previous "-previous" "") (if end "-end" "") group))))
    (fset sym (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj group previous end query)))
    sym))

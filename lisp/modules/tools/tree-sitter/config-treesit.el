;; tools/tree-sitter/config-treesit.el -*- lexical-binding: t; -*-

;; tools/tree-sitter/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(use-package! treesit
  :config
  (cl-pushnew (file-name-concat zenit-data-dir "tree-sitter") treesit-extra-load-path :test #'equal))


(use-package! treesit-auto
  :commands treesit-auto-install-all treesit-auto-mode treesit-auto-add-to-auto-mode-alist
  :hook (zenit-first-buffer . global-treesit-auto-mode)
  :init
  (defvar treesit-auto-langs '())
  :config
  ;; PATCH 2024-07-11: From https://github.com/renzmann/treesit-auto/pull/87
  (defun +treesit-auto--build-major-mode-remap-alist (&optional mode)
    "Construct `major-mode-remap-alist' using all known recipes.
When MODE is specified, only add to `major-mode-remap-alist' for
recipes that have the specified MODE in the :remap value of the
recipe."
    (append major-mode-remap-alist
            (let ((remap-alist '()))
              (cl-loop for recipe in (treesit-auto--selected-recipes)
                       for ts-mode = (treesit-auto-recipe-ts-mode recipe)
                       for remap-modes = (ensure-list
                                          (treesit-auto-recipe-remap recipe))
                       when (and (or (null mode) (memq mode remap-modes))
                                 (treesit-auto--ready-p ts-mode))
                       do (dolist (remap-mode remap-modes)
                            (push (cons remap-mode ts-mode) remap-alist))
                       finally return remap-alist))))

  (defun +treesit-auto--set-major-remap-a (fn mode &rest args)
    "Locally bind `major-mode-remap-alist' around FN.
Search recipes for those matching MODE (as the :remap value) and
add them to `major-mode-remap-alist' when calling FN. Call
FN with MODE and ARGS."
    (let ((major-mode-remap-alist (+treesit-auto--build-major-mode-remap-alist mode)))
        (apply fn mode args)))

  (define-globalized-minor-mode global-treesit-auto-mode treesit-auto-mode
    treesit-auto--on
    :group 'treesit
    :predicate
    (let ((modes '()))
      (cl-loop for recipe in (treesit-auto--selected-recipes)
               do (push (treesit-auto-recipe-ts-mode recipe) modes)
               do (dolist (mode (ensure-list (treesit-auto-recipe-remap recipe)))
                    (push mode modes))
               finally return modes))
    (if global-treesit-auto-mode
        (advice-add #'set-auto-mode-0 :around #'+treesit-auto--set-major-remap-a)
      (advice-remove #'set-auto-mode-0 #'+treesit-auto--set-major-remap-a)))

  ;; Prompt before installing a language
  (setq! treesit-auto-install 'prompt)
  ;; Enable langs in `treesit-auto-langs'
  (treesit-auto-add-to-auto-mode-alist 'all))


(use-package! evil-textobj-tree-sitter
  :when (modulep! :editor evil)
  :defer t
  :init (after! (evil treesit) (require 'evil-textobj-tree-sitter))
  :config
  (after! evil
    (defvar +tree-sitter-inner-text-objects-map (make-sparse-keymap))
    (defvar +tree-sitter-outer-text-objects-map (make-sparse-keymap))
    (defvar +tree-sitter-goto-previous-map (make-sparse-keymap))
    (defvar +tree-sitter-goto-next-map (make-sparse-keymap))

    (evil-define-key '(visual operator) 'tree-sitter-mode
      "i" +tree-sitter-inner-text-objects-map
      "a" +tree-sitter-outer-text-objects-map)
    (evil-define-key 'normal 'tree-sitter-mode
      "[g" +tree-sitter-goto-previous-map
      "]g" +tree-sitter-goto-next-map)

    (map! (:map +tree-sitter-inner-text-objects-map
                "A" (+tree-sitter-get-textobj '("parameter.inner" "call.inner"))
                "f" (+tree-sitter-get-textobj "function.inner")
                "F" (+tree-sitter-get-textobj "call.inner")
                "C" (+tree-sitter-get-textobj "class.inner")
                "v" (+tree-sitter-get-textobj "conditional.inner")
                "l" (+tree-sitter-get-textobj "loop.inner"))
          (:map +tree-sitter-outer-text-objects-map
                "A" (+tree-sitter-get-textobj '("parameter.outer" "call.outer"))
                "f" (+tree-sitter-get-textobj "function.outer")
                "F" (+tree-sitter-get-textobj "call.outer")
                "C" (+tree-sitter-get-textobj "class.outer")
                "c" (+tree-sitter-get-textobj "comment.outer")
                "v" (+tree-sitter-get-textobj "conditional.outer")
                "l" (+tree-sitter-get-textobj "loop.outer"))

          (:map +tree-sitter-goto-previous-map
                "a" (+tree-sitter-goto-textobj "parameter.outer" t)
                "f" (+tree-sitter-goto-textobj "function.outer" t)
                "F" (+tree-sitter-goto-textobj "call.outer" t)
                "C" (+tree-sitter-goto-textobj "class.outer" t)
                "c" (+tree-sitter-goto-textobj "comment.outer" t)
                "v" (+tree-sitter-goto-textobj "conditional.outer" t)
                "l" (+tree-sitter-goto-textobj "loop.outer" t))
          (:map +tree-sitter-goto-next-map
                "a" (+tree-sitter-goto-textobj "parameter.outer")
                "f" (+tree-sitter-goto-textobj "function.outer")
                "F" (+tree-sitter-goto-textobj "call.outer")
                "C" (+tree-sitter-goto-textobj "class.outer")
                "c" (+tree-sitter-goto-textobj "comment.outer")
                "v" (+tree-sitter-goto-textobj "conditional.outer")
                "l" (+tree-sitter-goto-textobj "loop.outer")))

    (after! which-key
      (setq which-key-allow-multiple-replacements t)
      (pushnew!
       which-key-replacement-alist
       '(("" . "\\`+?evil-textobj-tree-sitter-function--\\(.*\\)\\(?:.inner\\|.outer\\)") . (nil . "\\1"))))))

;; -*- no-byte-compile: t; -*-
;; tools/tree-sitter/packages.el

(package! tree-sitter
  :lockfile tools_tree-sitter)

;; NOTE 2025-09-03: `tree-sitter-langs' is only used for the queries folder
;;   which replaces the one provided by `treesit-langs'. We don't know which
;;   package gets installed first, so the :post-build step is added to both.
(package! treesit-langs
  :recipe
  (:host github :repo "kiennq/treesit-langs" :files ("treesit-*.el")
   :post-build
   (let* ((treesit-langs-dir (straight--build-dir "treesit-langs"))
          (treesit-langs-queries-dir (file-name-concat treesit-langs-dir "queries"))
          (tree-sitter-langs-dir (straight--repos-dir "tree-sitter-langs"))
          (tree-sitter-langs-queries-dir (file-name-concat tree-sitter-langs-dir "queries")))
     (when (and (file-exists-p tree-sitter-langs-queries-dir)
                (not (file-exists-p treesit-langs-queries-dir)))
       (straight--symlink-recursively tree-sitter-langs-queries-dir treesit-langs-queries-dir)))))

(package! tree-sitter-langs
  :recipe (:files ("queries")
           :post-build
           (let* ((treesit-langs-dir (straight--build-dir "treesit-langs"))
                  (treesit-langs-queries-dir (file-name-concat treesit-langs-dir "queries"))
                  (tree-sitter-langs-dir (straight--repos-dir "tree-sitter-langs"))
                  (tree-sitter-langs-queries-dir (file-name-concat tree-sitter-langs-dir "queries")))
             (when (and (file-exists-p tree-sitter-langs-queries-dir)
                        (not (file-exists-p treesit-langs-queries-dir)))
               (straight--symlink-recursively tree-sitter-langs-queries-dir treesit-langs-queries-dir))))
  :lockfile tools_tree-sitter)

(when (modulep! :editor evil)
  (package! evil-textobj-tree-sitter
    :lockfile tools_tree-sitter))

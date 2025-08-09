;; lang/data/config.el -*- lexical-binding: t; -*-

(use-package! nxml-mode
  :mode "\\.p\\(?:list\\|om\\)\\'" ; plist, pom
  :mode "\\.xs\\(?:d\\|lt\\)\\'"   ; xslt, xsd
  :mode "\\.rss\\'"
  :config
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t)
  ;; https://github.com/Fuco1/smartparens/issues/397#issuecomment-501059014
  (after! smartparens
    (sp-local-pair 'nxml-mode "<" ">" :post-handlers '(("[d1]" "/"))))
  (setq-hook! 'nxml-mode-hook tab-width nxml-child-indent)
  (set-formatter! 'xmllint '("xmllint" "--format" "-") :modes '(nxml-mode)))


;;;###package csv-mode
(map! :after csv-mode
      :localleader
      :map csv-mode-map
      "a" #'csv-align-fields
      "u" #'csv-unalign-fields
      "s" #'csv-sort-fields
      "S" #'csv-sort-numeric-fields
      "k" #'csv-kill-fields
      "t" #'csv-transpose)


(use-package! json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"
  :config
  (set-electric! 'json-mode :chars '(?\n ?: ?{ ?}))

  (eval-when! (modulep! +lsp)
    (add-hook 'json-mode-local-vars-hook #'lsp! 'append))

  (map! :after json-mode
        :map json-mode-map
        :localleader
        :desc "Copy path" "p" #'json-mode-show-path
        "t" #'json-toggle-boolean
        "d" #'json-mode-kill-path
        "x" #'json-nullify-sexp
        "+" #'json-increment-number-at-point
        "-" #'json-decrement-number-at-point
        "f" #'json-mode-beautify))


(use-package! json-ts-mode
  :when (modulep! +tree-sitter)
  :when (fboundp 'json-ts-mode)
  :defer t
  :init
  (set-tree-sitter! 'json-mode 'json-ts-mode
    '((json :url "https://github.com/tree-sitter/tree-sitter-json"
            :rev "v0.24.8")))
  :config
  ;; HACK: Rely on `major-mode-remap-defaults'.
  (cl-callf2 assq-delete-all 'json-ts-mode auto-mode-alist)

  (eval-when! (modulep! +lsp)
    (add-hook 'json-ts-mode-local-vars-hook #'lsp! 'append)))


(use-package! yaml-mode
  :mode "Procfile\\'"
  :config
  (when (modulep! +lsp)
    (add-hook 'yaml-mode-local-vars-hook #'lsp! 'append))
  (setq-hook! 'yaml-mode-hook tab-width yaml-indent-offset))


(use-package! yaml-ts-mode
  :when (modulep! +tree-sitter)
  :when (fboundp 'yaml-ts-mode) ; 29.1+ only
  :init
  (set-tree-sitter! 'yaml-mode 'yaml-ts-mode
                    '((yaml :url "https://github.com/tree-sitter-grammars/tree-sitter-yaml"
                       :rev "v0.7.0")))
  :config
  ;; HACK: Rely on `major-mode-remap-defaults'.
  (cl-callf2 rassq-delete-all 'yaml-ts-mode auto-mode-alist)

  (when (modulep! +lsp)
    (add-hook 'yaml-ts-mode-local-vars-hook #'lsp! 'append)))

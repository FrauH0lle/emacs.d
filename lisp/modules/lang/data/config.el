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
  :init
  (eval-when! (modulep! +lsp)
    (add-hook 'json-mode-local-vars-hook #'lsp! 'append))
  (eval-when! (modulep! +tree-sitter)
    (add-hook! '(json-mode-local-vars-hook
                 jsonc-mode-local-vars-hook)
               :append #'tree-sitter!))
  :config
  (set-electric! 'json-mode :chars '(?\n ?: ?{ ?}))

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

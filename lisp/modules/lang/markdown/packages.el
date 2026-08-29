;; -*- no-byte-compile: t; -*-
;; lang/markdown/packages.el

(package! markdown-mode :lockfile lang_markdown)
(package! markdown-toc :lockfile lang_markdown)
;; Required by `markdown-mode', or it will install it via package.el if it isn't
;; present when you call `markdown-edit-code-block'.
(package! edit-indirect :lockfile lang_markdown)

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! markdown-ts-mode
    :built-in 'prefer))

(when (modulep! :editor evil)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :lockfile lang_markdown))

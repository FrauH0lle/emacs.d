;; -*- no-byte-compile: t; -*-
;; lang/markdown/packages.el

(package! markdown-mode :lockfile lang-markdown)
(package! markdown-toc :lockfile lang-markdown)
;; Required by `markdown-mode', or it will install it via package.el if it isn't
;; present when you call `markdown-edit-code-block'.
(package! edit-indirect :lockfile lang-markdown)

(when (modulep! :editor evil)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :lockfile lang-markdown-evil))

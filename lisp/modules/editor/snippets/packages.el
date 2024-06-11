;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; editor/snippets/packages.el

(package! tempel :lockfile editor-snippets)
(when (modulep! :tools lsp)
  (package! lsp-snippet
    :recipe (:type git
             :host github
             :repo "svaante/lsp-snippet")
    :lockfile editor-snippets))

;; -*- no-byte-compile: t; -*-
;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :lockfile tools-lsp-eglot)
      (when (modulep! :completion vertico)
        (package! consult-eglot :lockfile tools-lsp-eglot-vertico)))
  (package! lsp-mode :lockfile tools-lsp)
  (package! lsp-ui :lockfile tools-lsp)
  (when (modulep! :completion vertico)
    (package! consult-lsp :lockfile tools-lsp-vertico)))

;; -*- no-byte-compile: t; -*-
;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :lockfile tools_lsp_eglot)
      (when (modulep! :completion vertico)
        (package! consult-eglot :lockfile tools_lsp_eglot_vertico)))
  (package! lsp-mode :lockfile tools_lsp)
  (package! lsp-ui :lockfile tools_lsp)
  (when (modulep! :completion vertico)
    (package! consult-lsp :lockfile tools_lsp_vertico)))

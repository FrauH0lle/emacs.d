;; -*- no-byte-compile: t; -*-
;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :lockfile tools_lsp)
      (when (modulep! :completion vertico)
        (package! consult-eglot :lockfile tools_lsp))
      (when (modulep! :checkers syntax -flymake)
        (package! flycheck-eglot :lockfile tools_lsp)))

  ;; `lsp-mode' must be rebuilt if this variable is changed, so expose it here
  ;; so it can be changed from site-lisp/packages.el.
  (defvar lsp-use-plists t)
  (package! lsp-mode
    :lockfile tools_lsp
    :env `(("LSP_USE_PLISTS" . ,(and lsp-use-plists "1"))))
  (package! lsp-ui :lockfile tools_lsp)
  (when (modulep! :completion vertico)
    (package! consult-lsp :lockfile tools_lsp)))

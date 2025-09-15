;; -*- no-byte-compile: t; -*-
;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :lockfile tools_lsp)
      (when (modulep! :completion vertico)
        (package! consult-eglot :lockfile tools_lsp))
      (when (modulep! :checkers syntax -flymake)
        (package! flycheck-eglot :lockfile tools_lsp)))

  ;; Ensure lsp-mode is built with lsp-use-plists on, but also that
  ;; `lsp-use-plists' isn't set *before* the package is rebuilt (which would
  ;; break things).
  (defvar lsp-use-plists t)
  (add-hook 'straight-use-package-pre-build-functions
            (lambda (package)
              (when (equal package "lsp-mode")
                (let ((default-directory zenit-local-dir)
                      (gen-file "01-modules-lsp-use-plists.el"))
                  (if (not lsp-use-plists)
                      (when (file-exists-p gen-file)
                        (delete-file gen-file))
                    ;; Ensure the setting propagates to child processes
                    (setenv "LSP_USE_PLISTS" "1")
                    (add-to-list 'zenit-init-generators
                                 (cons gen-file (fn! `((setenv "LSP_USE_PLISTS" "1"))))))))))
  (package! lsp-mode :lockfile tools_lsp)
  (package! lsp-ui :lockfile tools_lsp)
  (when (modulep! :completion vertico)
    (package! consult-lsp :lockfile tools_lsp)))

;; os/gentoo/patches.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'el-patch)
  (require 'company-ebuild))

;; PATCH 2024-07-26: Remove `company' dependency
(el-patch-defun company-ebuild-setup ()
  "Setup for Company-Ebuild.

To setup the integration correctly, add this function to ‘ebuild-mode-hook’
in your config:
\(add-hook 'ebuild-mode-hook 'company-ebuild-setup)
or `require' Company-Ebuild:
\(require 'company-ebuild)"
  ;; Force-enable `company-mode'.
  (el-patch-remove
    (when (null company-mode)
      (company-mode +1)))
  ;; Regenerate dynamic keywords.
  (company-ebuild--regenerate-dynamic-keywords)
  ;; Add the `company-ebuild' backend.
  (el-patch-remove
    (setq-local company-backends
                `((company-ebuild
                   company-capf  ; standard fallback
                   ,@(cond
                      ((fboundp 'company-yasnippet)  ; YAS for easier setup
                       '(company-yasnippet))
                      (t
                       '())))
                  ,@company-backends))
    (setq-local company-require-match nil)))

;; -*- no-byte-compile: t; -*-
;; checkers/syntax/packages.el

(unless (modulep! +flymake)
  (package! flycheck :lockfile checkers_syntax)
  (package! flycheck-popup-tip :lockfile checkers_syntax))

(when (or (modulep! +flymake)
          (modulep! :tools lsp +lsp-flymake))
  (package! flymake-popon
    :recipe (:host github :repo "doomelpa/flymake-popon")
    :lockfile checkers_syntax))

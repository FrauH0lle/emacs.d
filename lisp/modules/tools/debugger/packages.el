;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; tools/debugger/packages.el

(package! dape :lockfile debugger)

(when (modulep! +lsp)
  (package! dap-mode :lockfile debugger_lsp)
  (package! posframe :lockfile debugger_lsp))

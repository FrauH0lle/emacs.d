;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; tools/debugger/packages.el

(package! dape :lockfile debugger)

(when (modulep! +lsp)
  (package! dap-mode :lockfile debugger)
  (package! posframe :lockfile debugger))

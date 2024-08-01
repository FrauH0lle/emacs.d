;; -*- no-byte-compile: t; -*-
;; lisp/packages.el

;; zenit-core.el
(package! auto-minor-mode :lockfile core)
(package! gcmh :lockfile core)

;; zenit-ui.el
(package! nerd-icons :lockfile core)
(package! hide-mode-line :lockfile core)
(package! highlight-numbers :lockfile core)
(package! rainbow-delimiters :lockfile core)
(package! restart-emacs :lockfile core)

;; zenit-editor.el
(package! better-jumper :lockfile core)
(package! copy-as-format :lockfile core)
(package! dtrt-indent :lockfile core)
(package! expand-region :lockfile core)
(package! helpful :lockfile core)
(package! smartparens :lockfile core)
(package! undo-tree :lockfile core)
(package! ws-butler :lockfile core)

;; core-modules.el
(package! el-patch :lockfile core)

;; core-packages.el

;; core-projects.el
(package! projectile :lockfile core)

;; core-keybinds.el
(package! general :lockfile core)
(package! which-key :lockfile core)

;; tests
(package! buttercup :lockfile core)

(package! benchmark-init)
(package! esup)

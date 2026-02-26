;; -*- no-byte-compile: t; -*-
;; lisp/packages.el

(package! straight :lockfile core)
(package! async :lockfile core)

;; zenit-core.el
(package! auto-minor-mode :lockfile core)
(package! gcmh :lockfile core)
(package! compat
  :recipe (:host github :repo "emacs-compat/compat")
  :lockfile core)

;; zenit-ui.el
(package! nerd-icons :lockfile core)
(package! hide-mode-line :lockfile core)
(package! highlight-numbers :lockfile core)
(package! rainbow-delimiters :lockfile core)

;; zenit-editor.el
(package! better-jumper :lockfile core)
(package! copy-as-format :lockfile core)
(package! dtrt-indent :lockfile core)
(package! expand-region :lockfile core)
(package! smartparens :lockfile core)
(package! undo-tree :lockfile core)
(package! ws-butler :lockfile core
  :recipe
  (:host github
   :repo "emacsmirror/nongnu_elpa"
   :branch "elpa/ws-butler"
   :local-repo "ws-butler"))

;; core-modules.el
(package! el-patch :lockfile core)

;; core-packages.el

;; core-projects.el
(package! projectile :lockfile core)
(package! project :lockfile core)

;; core-keybinds.el
(package! general :lockfile core)
(package! which-key :lockfile core :built-in t)

(package! benchmark-init :lockfile core)
(package! esup)

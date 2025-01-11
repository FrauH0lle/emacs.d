;; lisp/core/init.el -*- lexical-binding: t; -*-

(require 'zenit-use-package)
(require 'zenit-el-patch)
(require 'zenit-keybinds)
(require 'zenit-ui)
(require 'zenit-projects)
(require 'zenit-editor)

;; Ensure .dir-locals.el in `zenit-emacs-dir' and `zenit-local-conf-dir' are
;; always respected
(add-to-list 'safe-local-variable-directories zenit-emacs-dir)
(add-to-list 'safe-local-variable-directories zenit-local-conf-dir)

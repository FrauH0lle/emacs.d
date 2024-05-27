;; lisp/core/init.el -*- lexical-binding: t; -*-

(require 'zenit-use-package)

;; We need `el-patch' only during compilation
(eval-when-compile
  (require 'zenit-el-patch))

(require 'zenit-keybinds)
(require 'zenit-ui)
(require 'zenit-projects)
(require 'zenit-editor)

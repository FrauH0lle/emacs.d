;; lisp/core/init.el -*- lexical-binding: t; -*-

(require 'zenit-use-package)
(require 'zenit-el-patch)
(require 'zenit-keybinds)
(require 'zenit-ui)
(require 'zenit-projects)
(require 'zenit-editor)

;; DEPRECATED 2024-09-15: `safe-local-variable-directories' was introduced in
;;   Emacs 30.1
(eval-when! (< emacs-major-version 30)
  (unless (boundp 'safe-local-variable-directories)
    (defvar safe-local-variable-directories ())
    (define-advice hack-local-variables-filter (:around (fn variables dir-name) respect)
      (let ((enable-local-variables
             (if (delq nil (mapcar (lambda (dir)
                                     (and dir-name dir
                                          (file-equal-p dir dir-name)))
                                   safe-local-variable-directories))
                 :all
               enable-local-variables)))
        (funcall fn variables dir-name)))))

;; Ensure .dir-locals.el in `zenit-emacs-dir' and `zenit-local-conf-dir' are
;; always respected
(add-to-list 'safe-local-variable-directories zenit-emacs-dir)
(add-to-list 'safe-local-variable-directories zenit-local-conf-dir)

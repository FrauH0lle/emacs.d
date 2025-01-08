;; lisp/core/zenit-compat.el -*- lexical-binding: t; -*-

;; DEPRECATED 2024-09-15: `safe-local-variable-directories' was introduced in
;;   Emacs 30.1
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
      (funcall fn variables dir-name))))

(provide 'zenit-compat)

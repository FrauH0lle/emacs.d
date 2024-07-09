;; lisp/core/lib/regexp.el -*- lexical-binding: t; -*-

;;;###autoload
(defun zenit-pcre-quote (str)
  "Like `reqexp-quote', but for PCREs."
  (let ((special '(?. ?^ ?$ ?* ?+ ?? ?{ ?\\ ?\[ ?\| ?\())
        (quoted nil))
    (mapc (lambda (c)
            (when (memq c special)
              (push ?\\ quoted))
            (push c quoted))
          str)
    (concat (nreverse quoted))))

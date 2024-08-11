;; lisp/core/lib/zenit-lib-alist.el -*- lexical-binding: t; -*-

;;;###autoload
(defun zenit-alist-set (key val alist &optional symbol)
  "Set property KEY to VAL in ALIST and return the modified alist.

- If an association with KEY exists in ALIST, set its value to
  VAL.

- If no such association exists, add a new one to the front of
  ALIST.

By default, key comparison is done using `equal'. If SYMBOL is
non-nil, use `eq' instead.

Note that this function may modify the original ALIST, but the
return value should be used instead of the original to ensure
correct results.

Adapted from URL `https://emacs.stackexchange.com/a/33893'."
  (if-let ((pair (if symbol (assq key alist) (assoc key alist))))
      (setcdr pair val)
    (push (cons key val) alist))
  alist)

(provide 'zenit-lib '(alist))

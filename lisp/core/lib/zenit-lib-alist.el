;; lisp/core/lib/zenit-lib-alist.el -*- lexical-binding: t; -*-

;;;###autoload
(defun zenit-alist-set (key val alist &optional symbol)
  "Set property KEY to VAL in ALIST and return ALIST.

If ALIST contains KEY, update its value to VAL. If not, prepend a
new (KEY . VAL) pair to ALIST.

KEY comparison uses `equal' by default. If SYMBOL is non-nil,
uses `eq' instead.

Returns the modified ALIST. Note: May mutate the original ALIST,
so always use the return value rather than assuming the original
was modified in-place.

Example:
  (setq my-alist \\='((\"a\" . 1) (\"b\" . 2)))
  (zenit-alist-set \"a\" 3 my-alist)  ; => ((\"a\" . 3) (\"b\" . 2))
  (zenit-alist-set \"c\" 4 my-alist)  ; => ((\"c\" . 4) (\"a\" . 3) (\"b\" . 2))

Adapted from URL `https://emacs.stackexchange.com/a/33893'."
  (if-let ((pair (if symbol (assq key alist) (assoc key alist))))
      (setcdr pair val)
    (push (cons key val) alist))
  alist)

(provide 'zenit-lib '(alist))

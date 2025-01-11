;; lisp/core/lib/zenit-lib-plist.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))


;;
;;; Library

;;;###autoload
(defalias 'zenit-plist-get #'cl-getf)

;;;###autoload
(defun zenit-plist-map (fn plist)
  "Map FN on each keyword/value pair in PLIST.

FN is a function that takes two arguments: a keyword and value,
and its return values are accumulated ala `mapcar'."
  (cl-loop for (key val) on plist by #'cddr
           while (keywordp key)
           do (plist-put plist key (funcall fn key val))))

;;;###autoload
(defun zenit-plist-map* (fn vplist)
  "Apply FN to each variadic property in VPLIST.

FN is a variadic function, whose first argument is the keyword
and the rest the values that follow (until the next keyword). Its
return value is accumulated ala `mapcar'.

VPLIST is a variadic-property list (a plist whose key may be
followed by one or more values)."
  (let ((vplist (copy-sequence vplist))
        results)
    (while vplist
      (let ((prop (pop vplist))
            vals)
        (while (and vplist (not (keywordp (car vplist))))
          (push (pop vplist) vals))
        (push (funcall fn prop (nreverse vals)) results)))
    (nreverse results)))

;;;###autoload
(defun zenit-plist-merge (from-plist to-plist)
  "Non-destructively merge FROM-PLIST onto TO-PLIST"
  (let ((from-plist (copy-sequence from-plist))
        (to-plist (copy-sequence to-plist)))
    (while from-plist
      (cl-callf plist-put to-plist (pop from-plist) (pop from-plist)))
    to-plist))

;;;###autoload
(defun zenit-plist-delete-nil (plist)
  "Delete `nil' properties from a copy of PLIST."
  (let (p)
    (while plist
      (if (car plist)
          (cl-callf plist-put p (car plist) (nth 1 plist)))
      (setq plist (cddr plist)))
    p))

;;;###autoload
(defun zenit-plist-keys (plist)
  "Return the keys in PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    keys))

;;;###autoload
(defun zenit-plist-values (plist)
  "Return the values in PLIST."
  (let (keys)
    (while plist
      (push (cadr plist) keys)
      (setq plist (cddr plist)))
    keys))


;;
;;; Modified plist

;;;###autoload
(defun zenit-mplist-get-values (mplist prop)
  "Get the values associated to PROP in MPLIST, a modified plist.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the
first property and its values is returned.

Loops infinitely when the list is circular."
  (let ((plist mplist)
        result)
    (while (and (consp plist) (not (eq prop (car plist))))
      (pop plist))
    ;; Pop the found keyword
    (pop plist)
    (while (and (consp plist) (not (keywordp (car plist))))
      (push (pop plist) result))
    (nreverse result)))

(provide 'zenit-lib '(plist))

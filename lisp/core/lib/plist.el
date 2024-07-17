;; lisp/core/lib/plist.el -*- lexical-binding: t; -*-

;;
;;; Library

;;;###autoload
(defun zenit-plist-get (plist prop &optional nil-value)
  "Return PROP in PLIST, if it exists. Otherwise NIL-VALUE."
  (if-let (val (plist-member plist prop))
      (cadr val)
    nil-value))

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
all non-keywords elements that follow it. If there are multiple
properties with the same keyword, only the first property and its
values is returned. Loops infinitely when the list is circular."
  (let ((plist mplist)
        result)
    (while (and (consp plist) (not (eq prop (car plist))))
      (pop plist))
    ;; Pop the found keyword
    (pop plist)
    (while (and (consp plist) (not (keywordp (car plist))))
      (push (pop plist) result))
    (nreverse result)))

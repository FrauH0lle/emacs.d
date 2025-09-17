;; lisp/core/lib/zenit-lib-modules.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))


(defvar zenit-modules nil
  "A hash table of enabled modules.
Set by `zenit-modules-initialize'.")

(defvar zenit-inhibit-module-warnings (not noninteractive)
  "If non-nil, don't emit deprecated or missing module warnings.
Warnings are emitted at startup.")


;;
;;; API

;;;###autoload
(defun zenit-modules-initialize (&optional force-p)
  "Initializes module metadata.

When FORCE-P is non-nil, force initialization."
  (when (or (null zenit-modules) force-p)
    (setq zenit-modules (make-hash-table :test 'equal))
    ;; Register zenit's two virtual module categories, representing zenit's core
    ;; and the user's config; which are always enabled.
    (zenit-module-set :core nil :path zenit-core-dir :depth -110)
    (zenit-module-set :local-conf nil :path zenit-local-conf-dir :depth '(-105 . 105))

    ;; Load site-lisp/init.el, where the user's `modules!' lives, which will
    ;; inform us of all desired modules.
    (zenit-load (file-name-concat zenit-local-conf-dir zenit-module-init-file)
               'noerror)))

(defun zenit-module-mplist-map (fn mplist)
  "Apply FN to each module in MPLIST."
  (let ((mplist (copy-sequence mplist))
        (inhibit-message zenit-inhibit-module-warnings)
        results
        category m)
    (while mplist
      (setq m (pop mplist))
      (cond ((keywordp m)
             (setq category m))
            ((null category)
             (error "No module category specified for %s" m))
            ((and (listp m) (keywordp (car m)))
             (pcase (car m)
               (:cond
                (cl-loop for (cond . mods) in (cdr m)
                         if (eval cond t)
                         return (prependq! mplist mods)))
               (:if (if (eval (cadr m) t)
                        (push (caddr m) mplist)
                      (prependq! mplist (cdddr m))))
               (test (if (xor (eval (cadr m) t)
                              (eq test :unless))
                         (prependq! mplist (cddr m))))))
            ((catch 'zenit-modules
               ;; (let ((module (if (listp m) (car m) m)))
               (let* ((module (if (listp m) (car m) m))
                      (flags  (if (listp m) (cdr m))))
                 (push (funcall fn category module :flags flags)
                       results))))))
    (when noninteractive
      (setq zenit-inhibit-module-warnings t))
    (nreverse results)))

(provide 'zenit-lib '(modules))

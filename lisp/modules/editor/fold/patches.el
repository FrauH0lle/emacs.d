;; editor/fold/patches.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'el-patch)
  (require 'outline-minor-faces))

;; PATCH 2025-08-11: Let `outline-minor-faces--syntactic-matcher' use
;;   `outline-search-function'
(el-patch-defun outline-minor-faces--syntactic-matcher (regexp)
  "Return a matcher that matches REGEXP only outside of strings.

Returns REGEXP directly for modes where `font-lock-keywords-only'
is non-nil because Font Lock does not mark strings and comments
for those modes, and the matcher will not know what is/is not a
string."
  (cond
   ;; Assume that if a mode defines such a function, it likely is
   ;; benefitial to use it.  We know that `elisp-outline-search'
   ;; (added in Emacs 31) is unnecessary here though.  It's purpose
   ;; is to avoid matching parens at the bol inside strings, but we
   ;; don't even try to match parens at all, so that's not relevant.
   (el-patch-remove
     ((and (bound-and-true-p outline-search-function)
         (not (eq outline-search-function 'elisp-outline-search)))
    #'ignore))
   (font-lock-keywords-only regexp)
   ((lambda (limit)
      (and (el-patch-swap
             (re-search-forward regexp limit t)
             (if outline-search-function
                 (funcall outline-search-function limit)
               (re-search-forward regexp limit t)))
           (not (nth 3 (syntax-ppss (match-beginning 0)))))))))

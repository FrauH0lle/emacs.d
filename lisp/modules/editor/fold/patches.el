;; editor/fold/patches.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'el-patch)
  (require 'outline-minor-faces))

;; PATCH 2025-02-10: Let `outline-minor-faces--level' respect
;;   `outline-search-function'
(el-patch-defun outline-minor-faces--level ()
  (save-excursion
    (beginning-of-line)
    (and (el-patch-swap
           (looking-at outline-regexp)
           (if outline-search-function
               (funcall outline-search-function nil nil nil t)
             (looking-at outline-regexp)))
         (funcall outline-level))))

;; PATCH 2025-02-10: Let `outline-minor-faces--syntactic-matcher' respect
;;   `outline-search-function'
(el-patch-defun outline-minor-faces--syntactic-matcher (regexp)
  "Return a matcher that matches REGEXP only outside of strings.

Returns REGEXP directly for modes where `font-lock-keywords-only'
is non-nil because Font Lock does not mark strings and comments
for those modes, and the matcher will not know what is/is not a
string."
  (if font-lock-keywords-only
      regexp
    (lambda (limit)
      (and (el-patch-swap
             (re-search-forward regexp limit t)
             (if outline-search-function
                 (funcall outline-search-function limit)
               (re-search-forward regexp limit t)))
           (not (nth 3 (syntax-ppss (match-beginning 0))))))))

;; (defun outline-minor-faces--level ()
;;   (save-excursion
;;     (beginning-of-line)
;;     (and (if outline-search-function
;;                (funcall outline-search-function nil nil nil t)
;;              (looking-at outline-regexp))
;;          (funcall outline-level))))

;; ;; PATCH 2025-02-10: Let `outline-minor-faces--syntactic-matcher' respect
;; ;;   `outline-search-function'
;; (defun outline-minor-faces--syntactic-matcher (regexp)
;;   "Return a matcher that matches REGEXP only outside of strings.

;; Returns REGEXP directly for modes where `font-lock-keywords-only'
;; is non-nil because Font Lock does not mark strings and comments
;; for those modes, and the matcher will not know what is/is not a
;; string."
;;   (if font-lock-keywords-only
;;       regexp
;;     (lambda (limit)
;;       (and (if outline-search-function
;;                  (funcall outline-search-function limit)
;;                (re-search-forward regexp limit t))
;;            (not (nth 3 (syntax-ppss (match-beginning 0))))))))

emacs-lisp-mode

(foo (p (completing-read "Enter a name: " '("Tom" "Jerry" "Spike" "Tyke")) foo) n
     (s foo) n)
(fn "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")

;; Sections
(section-box
 >
 ";; " (make-string (- fill-column (current-indentation) 3) ?-) n>
 ";; " (r "Title") n>
 ";; " (make-string (- fill-column (current-indentation) 3) ?-) n>
 q)
(section-minor
 >
 ";;" n>
 ";;; " (r "Title") n>
 q)
(section (i section-minor))

;; Tests
;; Simple FN test
(test-fun
 "(zenit-deftest " (r "FUN" fn) n>
 "(:doc \"Test if `" (s fn) "' is defined\")" n>
 "(should (fboundp '" (s fn) ")))" n>
 q)
;; Simple VAR test
(test-var
 "(zenit-deftest " (r "VAR" var) n>
 "(:doc \"Test if `" (s var) "' is defined\")" n>
 "(should (boundp '" (s var) ")))" n>
 q)
;; Simple ADVICE test
(test-adv
 "(zenit-deftest " (r "ADVICE" advice) n>
 "(:doc \"Test if `" (s advice) "' advises `" (p "FN" fn) "'\")" n>
 "(should (advice-member-p '" (s advice) " #'" (s fn)")))" n>
 q)

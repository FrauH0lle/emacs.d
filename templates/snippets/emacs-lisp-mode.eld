emacs-lisp-mode

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
 "(:doc \"`" (s fn) "' is defined\")" n>
 "(should (fboundp '" (s fn) ")))" n>
 q)
;; Simple VAR test
(test-var
 "(zenit-deftest " (r "VAR" var) n>
 "(:doc \"`" (s var) "' is defined\")" n>
 "(should (boundp '" (s var) ")))" n>
 q)
;; Simple ADVICE test
(test-adv
 "(zenit-deftest " (r "ADVICE" advice) n>
 "(:doc \"`" (s advice) "' advises `" (p "FN" fn) "'\")" n>
 "(should (advice-member-p '" (s advice) " #'" (s fn)")))" n>
 q)
;; Simple DEFADVICE test
(test-defadv
 "(zenit-deftest " (r "ADVICE" advice) n>
 "(:doc \"`" (s advice) "' advises `" (p "FN" fn) "'\")" n>
 "(progn" n>
 "(should (fboundp '" (s advice) "))" n>
 "(should (advice-member-p '" (s advice) " #'" (s fn)"))))" n>
 q)
;; Simple HOOK with DEFUN test
(test-defhook
 "(zenit-deftest " (r "FUN" fn) n>
 "(:doc \"`" (s fn) "' is a member of `" (p "HOOK" hook) "'\")" n>
 "(progn" n>
 "(should (fboundp '" (s fn) "))" n>
 "(should (member #'" (s fn) " " (s hook) "))))" n>
 q)
;; Simple MINOR MODE test
(test-minor-mode
 "(zenit-deftest " (r "MINOR-MODE" minor-mode) n>
 "(:doc \"`" (s minor-mode) "' is defined\")" n>
 "(progn" n>
 "(should (fboundp '" (s minor-mode) "))" n>
 "(should (boundp '" (s minor-mode) "))))" n>
 q)

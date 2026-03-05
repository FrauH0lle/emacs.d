emacs-lisp-mode

;; Sections
(section-box
 >
 ";; " (make-string (- fill-column (current-indentation) 3) ?-) n>
 ";; " (r "Title") n>
 ";; " (make-string (- fill-column (current-indentation) 3) ?-) n>
 q
 :ann "Section box"
 :doc "Insert a boxed section header")
(section-minor
 >
 ";;" n>
 ";;; " (r "Title") n>
 q
 :ann "Minor section"
 :doc "Insert a minor section header")
(section (i section-minor)
         :ann "Section"
         :doc "Insert a section header")

;; Tests
;; Simple FN test
(test-fun
 "(zenit-deftest " (r "FUN" fn) n>
 "(:doc \"`" (s fn) "' is defined\")" n>
 "(should (fboundp '" (s fn) ")))" n>
 q
 :ann "Function test"
 :doc "Insert a test template for a function definition")
;; Simple VAR test
(test-var
 "(zenit-deftest " (r "VAR" var) n>
 "(:doc \"`" (s var) "' is defined\")" n>
 "(should (boundp '" (s var) ")))" n>
 q
 :ann "Variable test"
 :doc "Insert a test template for a variable definition")
;; Simple ADVICE test
(test-adv
 "(zenit-deftest " (r "ADVICE" advice) n>
 "(:doc \"`" (s advice) "' advises `" (p "FN" fn) "'\")" n>
 "(should (advice-member-p '" (s advice) " #'" (s fn)")))" n>
 q
 :ann "Advice test"
 :doc "Insert a test template for an advice")
;; Simple DEFADVICE test
(test-defadv
 "(zenit-deftest " (r "ADVICE" advice) n>
 "(:doc \"`" (s advice) "' advises `" (p "FN" fn) "'\")" n>
 "(progn" n>
 "(should (fboundp '" (s advice) "))" n>
 "(should (advice-member-p '" (s advice) " #'" (s fn)"))))" n>
 q
 :ann "Defadvice test"
 :doc "Insert a test template for a defadvice")
;; Simple HOOK with DEFUN test
(test-defhook
 "(zenit-deftest " (r "FUN" fn) n>
 "(:doc \"`" (s fn) "' is a member of `" (p "HOOK" hook) "'\")" n>
 "(progn" n>
 "(should (fboundp '" (s fn) "))" n>
 "(should (member #'" (s fn) " " (s hook) "))))" n>
 q
 :ann "Hook function test"
 :doc "Insert a test template for a hook function")
;; Simple MINOR MODE test
(test-minor-mode
 "(zenit-deftest " (r "MINOR-MODE" minor-mode) n>
 "(:doc \"`" (s minor-mode) "' is defined\")" n>
 "(progn" n>
 "(should (fboundp '" (s minor-mode) "))" n>
 "(should (boundp '" (s minor-mode) "))))" n>
 q
 :ann "Minor mode test"
 :doc "Insert a test template for a minor mode")

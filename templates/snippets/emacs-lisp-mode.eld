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

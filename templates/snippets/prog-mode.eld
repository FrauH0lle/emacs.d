prog-mode

;; Tags
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; "
        (if (string-suffix-p " " comment-start)
            comment-start
          (concat comment-start " ")))
      "TODO " (i today) ": ")
(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; "
         (if (string-suffix-p " " comment-start)
             comment-start
           (concat comment-start " ")))
       "FIXME " (i today) ": ")
(review (if (derived-mode-p 'emacs-lisp-mode) ";; "
          (if (string-suffix-p " " comment-start)
              comment-start
            (concat comment-start " ")))
        "REVIEW " (i today) ": ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; "
        (if (string-suffix-p " " comment-start)
            comment-start
          (concat comment-start " ")))
      "HACK " (i today) ": ")
(patch (if (derived-mode-p 'emacs-lisp-mode) ";; "
         (if (string-suffix-p " " comment-start)
             comment-start
           (concat comment-start " ")))
       "PATCH " (i today) ": ")
(deprecated (if (derived-mode-p 'emacs-lisp-mode) ";; "
              (if (string-suffix-p " " comment-start)
                  comment-start
                (concat comment-start " ")))
            "DEPRECATED " (i today) ": ")
(note (if (derived-mode-p 'emacs-lisp-mode) ";; "
        (if (string-suffix-p " " comment-start)
            comment-start
          (concat comment-start " ")))
      "NOTE " (i today) ": ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; "
       (if (string-suffix-p " " comment-start)
           comment-start
         (concat comment-start " ")))
     "BUG " (i today) ": ")
(xxx (if (derived-mode-p 'emacs-lisp-mode) ";; "
       (if (string-suffix-p " " comment-start)
           comment-start
         (concat comment-start " ")))
     "XXX " (i today) ": ")

;; Sections
(section-box
 ;; Indent line
 >
 ;; Ensure `comment-start` is followed by a space
 (p (if (string-suffix-p " " comment-start)
        comment-start
      (concat comment-start " ")) cs t)
 ;; Create upper line of the box, i.e. # ---------...
 cs (make-string (- fill-column (current-indentation) (length cs)) ?-) n>
 ;; Insert region or default
 cs (r "Title") n>
 ;; Create lower line of the box, i.e. # ---------...
 cs (make-string (- fill-column (current-indentation) (length cs)) ?-) n>
 q)
(section (i section-box))
(section-minor
 ;; Indent line
 >
 ;; Ensure `comment-start` is followed by a space
 (p (if (string-suffix-p " " comment-start)
        comment-start
      (concat comment-start " ")) cs t)
 ;; Create minor indicator, i.e. # --
 cs "--" n>
 ;; Insert region or default
 cs (r "Title") n>
 q)

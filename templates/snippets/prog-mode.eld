prog-mode

(_cs (if (or (derived-mode-p 'emacs-lisp-mode)
             (derived-mode-p 'lisp-mode)
             (derived-mode-p 'lisp-data-mode))
         ";; "
       (if (string-suffix-p " " comment-start)
           comment-start
         (concat comment-start " "))))

;; Tags
(todo (i _cs) "TODO: "
      :ann "TODO tag"
      :doc "Insert a TODO comment tag")
(fixme (i _cs) "FIXME: "
       :ann "FIXME tag"
       :doc "Insert a FIXME comment tag")
(review (i _cs) "REVIEW: "
        :ann "REVIEW tag"
        :doc "Insert a REVIEW comment tag")
(hack (i _cs) "HACK: "
      :ann "HACK tag"
      :doc "Insert a HACK comment tag")
(patch (i _cs) "PATCH: "
       :ann "PATCH tag"
       :doc "Insert a PATCH comment tag")
(deprecated (i _cs) "DEPRECATED: "
            :ann "DEPRECATED tag"
            :doc "Insert a DEPRECATED comment tag")
(note (i _cs) "NOTE: "
      :ann "NOTE tag"
      :doc "Insert a NOTE comment tag")
(bug (i _cs) "BUG: "
     :ann "BUG tag"
     :doc "Insert a BUG comment tag")
(xxx (i _cs) "XXX " (i today) ": "
     :ann "XXX tag with date"
     :doc "Insert an XXX comment tag with today's date")
(todo-date (i _cs) "TODO " (i today) ": "
           :ann "TODO tag with date"
           :doc "Insert a TODO comment tag with today's date")
(fixme-date (i _cs) "FIXME " (i today) ": "
            :ann "FIXME tag with date"
            :doc "Insert a FIXME comment tag with today's date")
(review-date (i _cs) "REVIEW " (i today) ": "
             :ann "REVIEW tag with date"
             :doc "Insert a REVIEW comment tag with today's date")
(hack-date (i _cs) "HACK " (i today) ": "
           :ann "HACK tag with date"
           :doc "Insert a HACK comment tag with today's date")
(patch-date (i _cs) "PATCH " (i today) ": "
            :ann "PATCH tag with date"
            :doc "Insert a PATCH comment tag with today's date")
(deprecated-date (i _cs) "DEPRECATED " (i today) ": "
                 :ann "DEPRECATED tag with date"
                 :doc "Insert a DEPRECATED comment tag with today's date")
(note-date (i _cs) "NOTE " (i today) ": "
           :ann "NOTE tag with date"
           :doc "Insert a NOTE comment tag with today's date")
(bug-date (i _cs) "BUG " (i today) ": "
          :ann "BUG tag with date"
          :doc "Insert a BUG comment tag with today's date")
(xxx-date (i _cs) "XXX " (i today) ": "
          :ann "XXX tag with date"
          :doc "Insert an XXX comment tag with today's date")

;; Sections
(section-box
 ;; Indent line
 >
 ;; Ensure `comment-start` is followed by a space
 (p (if (string-suffix-p " " comment-start)
        comment-start
      (concat comment-start " "))
    cs noinsert)
 ;; Create upper line of the box, i.e. # ---------...
 cs (make-string (- fill-column (current-indentation) (length cs)) ?-) n>
 ;; Insert region or default
 cs (r "Title") n>
 ;; Create lower line of the box, i.e. # ---------...
 cs (make-string (- fill-column (current-indentation) (length cs)) ?-) n>
 q
 :ann "Section box"
 :doc "Insert a boxed section header")
(section (i section-box)
         :ann "Section"
         :doc "Insert a section header")
(section-minor
 ;; Indent line
 >
 ;; Ensure `comment-start` is followed by a space
 (p (if (string-suffix-p " " comment-start)
        comment-start
      (concat comment-start " "))
    cs noinsert)
 ;; Create minor indicator, i.e. # --
 cs "--" n>
 ;; Insert region or default
 cs (r "Title") n>
 q
 :ann "Minor section"
 :doc "Insert a minor section header")

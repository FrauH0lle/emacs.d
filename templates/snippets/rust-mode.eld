rust-mode rust-ts-mode rustic-mode

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
 cs (p (identity "* ") level) (r "Title") n>
 q)

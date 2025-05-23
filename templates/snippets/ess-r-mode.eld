ess-r-mode

(section
 >
 (p (identity "# ") level) (r "Title" title) " " (make-string (- fill-column (current-indentation) (length level) (length title) 1) ?-) n>
 q)

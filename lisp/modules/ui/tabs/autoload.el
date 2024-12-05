;; ui/tabs/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +tab-line-tab-name-fn (buffer &rest _buffers)
  "Create name for tab with padding and truncation.

If buffer name is shorter than `+tab-line-tab-max-width' it gets
centered with spaces, otherwise it is truncated, to preserve
equal width for all tabs. This function also tries to fit as many
tabs in window as possible, so if there are no room for tabs with
maximum width, it calculates new width for each tab and truncates
text if needed. Minimal width can be set with
`+tab-line-tab-min-width' variable."
  (with-current-buffer buffer
    (let* ((window-width (window-width (get-buffer-window)))
           (tab-amount (length (tab-line-tabs-window-buffers)))
           (window-max-tab-width (if (>= (* (+ +tab-line-tab-max-width 3) tab-amount) window-width)
                                     (/ window-width tab-amount)
                                   +tab-line-tab-max-width))
           (tab-width (- (cond ((> window-max-tab-width +tab-line-tab-max-width)
                                +tab-line-tab-max-width)
                               ((< window-max-tab-width +tab-line-tab-min-width)
                                +tab-line-tab-min-width)
                               (t window-max-tab-width))
                         3)) ;; compensation for ' x ' button
           (buffer-name (string-trim (buffer-name)))
           (name-width (length buffer-name)))
      (if (>= name-width tab-width)
          (concat  " " (truncate-string-to-width buffer-name (- tab-width 2)) "…")
        (let* ((padding (make-string (+ (/ (- tab-width name-width) 2) 1) ?\s))
               (buffer-name (concat padding buffer-name)))
          (concat buffer-name (make-string (- tab-width (length buffer-name)) ?\s)))))))

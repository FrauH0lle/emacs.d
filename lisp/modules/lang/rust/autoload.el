;; lang/rust/autoload.el -*- lexical-binding: t; -*-

;; TODO (defun +rust/run-cargo () (interactive))

;;;###autoload
(defun +rust-cargo-project-p ()
  "Return t if this is a cargo project."
  (locate-dominating-file buffer-file-name "Cargo.toml"))


;;
;;; Custom Cargo commands

(autoload 'rustic-run-cargo-command "rustic-cargo")
;;;###autoload
(defun +rust/cargo-audit ()
  "Run \\='cargo audit' for the current project."
  (interactive)
  (rustic-run-cargo-command "cargo audit"))

;;
;;; Custom code headings

;;;###autoload
(defun +rust-outline-search-fn (&optional bound move backward looking-at)
  "Search for the next outline heading.
Intended for `outline-search-function'. BOUND limits the search
scope. MOVE if non-nil, moves point even if the search fails.
BACKWARD if non-nil, searches backward. LOOKING-AT if non-nil,
matches at point without moving."
  (interactive)
  (let ((search-fn (if backward 're-search-backward 're-search-forward))
        ;; This matches
        ;; // -- [or more]
        ;; // [* zero more] Section
        (full-heading-regex "^[ \t]*// -\\{2,\\}\n[ \t]*// [^\n]+$")
        ;; This matches
        ;; // [* zero more] Section
        ;; thus, the actual heading
        (actual-heading-regex "^\\(?:[ \t]*// [\\*]*[ ]*[^\n]+\\).*$")
        ;; Map MOVE argument to NOERROR
        (move (if move 'move t)))
    (if looking-at
        ;; Limit the following search to beginning of the previous line
        (let ((limit (save-excursion
                       (forward-line -1)
                       (line-beginning-position))))
          (save-excursion
            ;; Find the start of the heading, e.g.
            ;; // -- [or more]
            (when (re-search-backward "^[ \t]*// -\\{2,\\}$" limit t)
              ;; Go to the beginning of the next line
              (forward-line 1)
              (goto-char (line-beginning-position))
              ;; Check if we are looking at
              ;; // [* zero more] Section
              (looking-at actual-heading-regex))))
      (when (funcall search-fn full-heading-regex bound move)
        ;; First we match the full heading, then the second line of the heading
        ;; which is the actual heading.
        (if (not backward)
            ;; If we go forward, the cursor is at the end of
            ;; // -- [or more]
            ;; // [* zero more] Section | <- Here
            ;; Thus, move to the beginning of the line.
            (goto-char (line-beginning-position))
          ;; If we go backward, the cursor is at the beginning of
          ;; | <- Here // -- [or more]
          ;; // [* zero more] Section
          ;; Thus, move to the next line and go to the end of the line (imitates
          ;; `re-search-backward''s behavior).
          (forward-line 1)
          (goto-char (line-end-position)))
        ;; Finally, match the actual heading we want.
        (funcall search-fn actual-heading-regex bound move)))))

;;;###autoload
(defun +rust-outline-level-fn ()
  "`outline-level' function."
  (if (looking-at "^[ \t]*// \\([\\*]*\\)")
      ;; The first level is without a star, e.g. //. Thus, we add 1 to the
      ;; calculated level.
      (1+ (- (match-end 1) (match-beginning 1)))
    1000))

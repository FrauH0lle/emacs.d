;; tools/llm/autoload/gptel.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +gptel/commit-summary ()
  "Insert a commit message header line in the format I use.

The header is followed by a standard magit (GNU style) changelog.

Don't get the LLM to write the commit message itself, because it's bad
at inferring my intent.

Intended to be placed in `git-commit-setup-hook'."
  (interactive)
  (require 'gptel)
  (gptel-with-preset 'commit-summary
    ;; Commit message buffer
    (let ((commit-buffer (current-buffer)))
      ;; Heuristic for blank message
      (when (looking-at-p "[\n[:blank:]]+")
        (with-temp-buffer
          ;; Insert diff
          (vc-git-command
           (current-buffer) 1 nil
           "diff-index" "--exit-code" "--patch"
           (and (magit-anything-staged-p) "--cached")
           "HEAD" "--")

          ;; Run request on diff buffer contents
          (gptel-request nil
            :context commit-buffer
            :callback
            (lambda (resp info)
              (if (not (stringp resp))
                  (message "Git commit summary generation failed")
                (with-current-buffer (plist-get info :context)
                  (save-excursion
                    (goto-char (point-min))
                    (insert resp "\n\n")
                    (magit-generate-changelog)))))))))))

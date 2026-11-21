;; tools/llm/autoload/niwa.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +niwa/embark-create-reference (cand)
  "Create a niwa reference spanning the whole line of CAND.

CAND is an Embark target of category `consult-location' (e.g. from
`consult-line') or `consult-grep' (e.g. from `consult-ripgrep'). Lines
which already hold a reference are skipped. Unlike
`niwa-create-reference', no tags are prompted for.

Intended for `embark-act' and `embark-act-all'."
  (require 'niwa)
  (when-let* ((marker (car (if (get-text-property 0 'consult-location cand)
                               (consult--get-location cand)
                             (consult--grep-position cand #'find-file-noselect))))
              (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char marker)
        (let ((beg (line-beginning-position))
              (end (line-end-position)))
          (unless (niwa--instructions-in beg end 'reference)
            (niwa--create-reference-in buffer beg end)))))))

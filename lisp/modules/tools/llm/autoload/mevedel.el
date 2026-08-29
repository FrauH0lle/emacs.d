;; tools/llm/autoload/mevedel.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +mevedel/embark-create-reference (cand)
  "Create a mevedel reference spanning the whole line of CAND.

CAND is an Embark target of category `consult-location' (e.g. from
`consult-line') or `consult-grep' (e.g. from `consult-ripgrep'). Lines
which already hold a reference are skipped. Unlike
`mevedel-create-reference', no tags are prompted for.

Intended for `embark-act' and `embark-act-all'."
  (require 'mevedel)
  (when-let* ((marker (car (if (get-text-property 0 'consult-location cand)
                               (consult--get-location cand)
                             (consult--grep-position cand #'find-file-noselect))))
              (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char marker)
        (let ((beg (line-beginning-position))
              (end (line-end-position)))
          (unless (mevedel--instructions-in beg end 'reference)
            (mevedel--create-reference-in buffer beg end)))))))

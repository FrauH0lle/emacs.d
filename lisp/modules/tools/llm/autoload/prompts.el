;; tools/llm/autoload/prompts.el -*- lexical-binding: t; -*-

;;
;;; System prompt generator

(defvar +gptel--prev-system-message nil)

;;;###autoload
(defun +gptel-prev-system-message-a (fn &rest args)
  "Store the previous `gptel--system-message'.
The message is stored in `+gptel--prev-system-message'. This function is
an `:around' advice for `gptel-system-prompt'."
  ;; It is important to check if `gptel--system-message' is a function and
  ;; evaluate it before storing it in `+gptel--prev-system-message'. Otherwise,
  ;; you can run into inifinite recursion.
  (setq +gptel--prev-system-message (if (functionp gptel--system-message)
                                        (funcall gptel--system-message)
                                      gptel--system-message))
  (apply fn args))

;;;###autoload
(defun +llm-compose-gptel-system-prompt (prompt-sources &optional appendp)
  "Compose a gptel system prompt from PROMPT-SOURCES.

PROMPT-SOURCES is a list where each element can be:
- A file path (string) - content will be read from the file
- A raw string - used directly as prompt content
- A list containing (system-prompt . conversation-history)
- A function that returns any of the above

If APPENDP is non-nil, the new content is appended to the current system
prompt. If APPENDP is nil (default), the current system prompt is
overwritten with the new content.

This function handles the three possible types of `gptel--system-message':
1. A raw string.
2. A list containing the system prompt and a conversation history.
3. A function that returns either of the above.

The function returns a new value in the format expected by `gptel' (a
list of the system message string and any existing conversation
history), which can then be used to set the `gptel--system-message'
variable."
  (let* (;; 1. Resolve the current system prompt value, calling it if it's a function.
         (current-prompt-value (if (functionp +gptel--prev-system-message)
                                   (funcall +gptel--prev-system-message)
                                 +gptel--prev-system-message))
         ;; 2. Extract the main system message string and the conversation history.
         (current-system-message (if (listp current-prompt-value)
                                     (or (car current-prompt-value) "")
                                   (if (stringp current-prompt-value) current-prompt-value "")))
         (current-history (if (listp current-prompt-value) (cdr current-prompt-value) nil))
         (snippets-to-add '()))
    ;; 3. Process each prompt source.
    (dolist (source prompt-sources)
      (let ((content nil))
        (cond
         ;; Handle function sources
         ((functionp source)
          (let ((result (funcall source)))
            (cond
             ;; Function returned a file path
             ((and (stringp result) (file-readable-p result))
              (setq content (string-trim (with-temp-buffer
                                           (zenit-file-read result :by 'insert)
                                           (buffer-string)))))
             ;; Function returned a raw string
             ((stringp result)
              (setq content result))
             ;; Function returned a list (system-prompt . history)
             ((listp result)
              (setq content (car result))
              ;; Merge conversation history if present
              (when (cdr result)
                (setq current-history (append current-history (cdr result))))))))
         ;; Handle file path sources
         ((and (stringp source)
               (or (file-readable-p (expand-file-name source +llm-user-prompts-dir))
                   (file-readable-p (expand-file-name source +llm-prompts-dir))))
          (when-let* ((file-path
                       (or (and (file-readable-p (expand-file-name source +llm-user-prompts-dir))
                                (expand-file-name source +llm-user-prompts-dir))
                           (and (file-readable-p (expand-file-name source +llm-prompts-dir))
                                (expand-file-name source +llm-prompts-dir)))))
            (setq content (string-trim (with-temp-buffer
                                         (zenit-file-read file-path :by 'insert)
                                         (buffer-string))))))
         ;; Handle raw string sources
         ((stringp source)
          (setq content source))
         ;; Handle list sources (system-prompt . history)
         ((listp source)
          (setq content (car source))
          ;; Merge conversation history if present
          (when (cdr source)
            (setq current-history (append current-history (cdr source))))))

        ;; 4. Add content only if it's not already present (when appending).
        (when (and content (not (string-empty-p content)))
          (if appendp
              ;; When appending, check for duplicates
              (let ((normalized-content (downcase (replace-regexp-in-string "\\s-+" " " content)))
                    (normalized-system-message (downcase (replace-regexp-in-string "\\s-+" " " current-system-message))))
                (unless (string-match-p (regexp-quote normalized-content) normalized-system-message)
                  (push content snippets-to-add)))
            ;; When not appending, just add the content
            (push content snippets-to-add)))))

    (setq snippets-to-add (cl-remove-duplicates snippets-to-add :test #'string=))

    ;; 5. Build the new system message.
    (let* ((new-snippets-str (string-join (nreverse snippets-to-add) "\n\n"))
           (final-system-message
            (cond
             ;; If not appending, use only the new content
             ((not appendp)
              (if (string-empty-p new-snippets-str)
                  current-system-message
                new-snippets-str))
             ;; If appending, combine with current message
             (t
              (if (string-empty-p new-snippets-str)
                  current-system-message
                (string-trim (concat current-system-message "\n\n" new-snippets-str)))))))
      ;; 6. Return the final structure: a list of (new-message . history) or a string.
      (if current-history
          (cons final-system-message current-history)
        final-system-message))))

;;;###autoload
(defun +llm-get-prompt-from-files (prompt-files)
  "Read content from PROMPT-FILES.

PROMPT-FILES is a list of files, the first found will be used. File path
must be relative to project root."
  (let* ((project-root (zenit-project-root))
         (found-file nil))
    (dolist (file prompt-files)
      (let ((full-path (expand-file-name file project-root)))
        (when (and (not found-file) (file-exists-p full-path))
          (setq found-file full-path))))
    (if found-file
        (with-temp-buffer
          (zenit-file-read found-file :by 'insert)
          (buffer-string))
      (let* ((files prompt-files)
             (formatted-string
              ;; If there are fewer than 2 files, just use the file name itself.
              (if (< (length files) 2)
                  (car files)
                ;; Otherwise, join all but the last with ", " and add the last
                ;; with " or ".
                (concat (string-join (butlast files) ", ") " or " (car (last files))))))

        (user-error "Could not find any of %s in project : %s" formatted-string project-root)))))

;;
;;; Commands

;;;###autoload
(defun +gptel/previous-prompt ()
  "Move point to the end of the previous prompt."
  (interactive)
  (goto-char (line-beginning-position))
  (when (re-search-backward
         (regexp-quote (gptel-prompt-prefix-string)) nil t)
    (goto-char (line-end-position))))

;;;###autoload
(defun +gptel/next-prompt ()
  "Move point to the end of the next prompt."
  (interactive)
  (goto-char (line-end-position))
  (when (re-search-forward
         (regexp-quote (gptel-prompt-prefix-string)) nil t)
    (goto-char (line-end-position))))

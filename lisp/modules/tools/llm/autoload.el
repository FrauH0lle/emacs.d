;; tools/llm/autoload.el -*- lexical-binding: t; -*-

;; From `gptel'
;;;###autoload
(defun +llm-api-key-from-auth-source (host &optional user)
  "Lookup api key for HOST in the auth source.
By default \"apikey\" is used as USER."
  (if-let* ((secret
             (plist-get
              (car (auth-source-search
                    :host host
                    :user (or user "apikey")
                    :require '(:secret)))
              :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)
    (user-error "No API key for %s found in the auth source" host)))

;;;###autoload
(defun +llm/aider-set-model (model)
  "Set the MODEL for Aider.
This function prompts the user to choose a model from a
predefined list, retrieves the corresponding API key from the
auth source, and sets the environment variable with the API key.
It also updates the Aider arguments with the selected model's
configuration."
  (interactive
   (list
    ;; Prompt the user to choose a model from a predefined list
    (completing-read "Choose model: " (mapcar #'car +llm-aider-arg-list) nil t)))
  ;; Retrieve the configuration plist for the selected model
  (let* ((plist (alist-get model +llm-aider-arg-list nil nil #'equal))
         ;; Extract the arguments, host, user, and environment variable name for
         ;; the API key lookup
         (args (plist-get plist :args))
         (host (plist-get plist :host))
         (user (plist-get plist :user))
         (env (plist-get plist :env)))
    ;; Set the environment variable with the API key
    (setenv env (+llm-api-key-from-auth-source host user))
    ;; Update the Aider arguments with the selected model's configuration
    (setq aidermacs-args args)))


;;; Prompts

;; gptel-system-prompt
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

;;;###autoload
(defun +gptel/commit-summary ()
  "Insert a commit message header line in the format I use, followed by a
standard magit (GNU style) changelog.

Don't get the LLM to write the commit message itself, because it's bad
at inferring my intent.

Intended to be placed in `git-commit-setup-hook'."
  (interactive)
  (gptel-with-preset 'commit-summary
    (let ((commit-buffer (current-buffer))) ;commit message buffer

      (when (looking-at-p "[\n[:blank:]]+") ;Heuristic for blank message
        (with-temp-buffer
          (vc-git-command             ;insert diff
           (current-buffer) 1 nil
           "diff-index" "--exit-code" "--patch"
           (and (magit-anything-staged-p) "--cached")
           "HEAD" "--")

          (gptel-request nil          ;Run request on diff buffer contents
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

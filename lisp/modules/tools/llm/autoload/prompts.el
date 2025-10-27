;; tools/llm/autoload/prompts.el -*- lexical-binding: t; -*-

;;
;;; System prompt generator

(defvar +gptel--prev-system-message nil)

;;
;;; Helper functions

(defun +llm--read-file-content (file)
  "Read and return trimmed content from FILE.
Returns nil if FILE cannot be read."
  (when (and file (file-readable-p file))
    (condition-case err
        (with-temp-buffer
          (zenit-file-read file :by 'insert)
          (string-trim (buffer-string)))
      (error
       (message "Error reading file %s: %s" file (error-message-string err))
       nil))))

(defun +llm--resolve-prompt-file (path &optional base-dir)
  "Resolve PATH to an absolute file path.
Checks in order:
1. Absolute path
2. Relative to BASE-DIR (if provided)
3. Relative to `+llm-user-prompts-dir'
4. Relative to `+llm-prompts-dir'

Returns the full path if found and readable, nil otherwise."
  (let ((candidates
         (delq nil
               (list
                ;; Try absolute path
                (and (file-name-absolute-p path) path)
                ;; Try relative to base-dir
                (and base-dir (expand-file-name path base-dir))
                ;; Try user prompts directory
                (expand-file-name path +llm-user-prompts-dir)
                ;; Try system prompts directory
                (expand-file-name path +llm-prompts-dir)))))
    (cl-find-if #'file-readable-p candidates)))

(defun +llm--process-prompt-source (source)
  "Process a single prompt SOURCE and return its content and optional history.
SOURCE can be:
- A file path (string) - content will be read from the file
- A raw string - used directly as prompt content
- A list containing (system-prompt . conversation-history)
- A function that returns any of the above

Returns a cons cell (CONTENT . HISTORY) where:
- CONTENT is the prompt string (or nil if source couldn't be processed)
- HISTORY is conversation history from list sources (or nil)"
  (let ((content nil)
        (history nil))
    (cond
     ;; Handle function sources
     ((functionp source)
      (let ((result (funcall source)))
        (cond
         ;; Function returned a file path
         ((and (stringp result) (file-readable-p result))
          (setq content (+llm--read-file-content result)))
         ;; Function returned a raw string
         ((stringp result)
          (setq content result))
         ;; Function returned a list (system-prompt . history)
         ((listp result)
          (setq content (car result))
          (setq history (cdr result))))))

     ;; Handle string sources (file paths or raw strings)
     ((stringp source)
      (if-let ((file-path (+llm--resolve-prompt-file source)))
          (setq content (+llm--read-file-content file-path))
        ;; Not a file - treat as raw string
        (setq content source)))

     ;; Handle list sources (system-prompt . history)
     ((listp source)
      (setq content (car source))
      (setq history (cdr source))))

    (cons content history)))

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
(defun +llm-compose-gptel-system-prompt (prompt-sources &optional appendp strategy)
  "Compose a gptel system prompt from PROMPT-SOURCES.

PROMPT-SOURCES is a list where each element can be:
- A file path (string) - content will be read from the file
- A raw string - used directly as prompt content
- A list containing (system-prompt . conversation-history)
- A function that returns any of the above

APPENDP determines how to combine with existing prompt:
- If non-nil, the new content is appended to the current system prompt
- If nil (default), the current system prompt is overwritten with new content
- Ignored when STRATEGY is `hierarchical'

STRATEGY determines composition approach (defaults to `+llm-prompt-composition-strategy'):
- `hierarchical': Auto-discover and layer system-prompt + project-context + PROMPT-SOURCES
- `replace': Project context completely replaces system prompt (legacy)
- `context-only': Only use project context, skip system prompt layer

This function handles the three possible types of `gptel--system-message':
1. A raw string.
2. A list containing the system prompt and a conversation history.
3. A function that returns either of the above.

The function returns a new value in the format expected by `gptel' (a
list of the system message string and any existing conversation
history), which can then be used to set the `gptel--system-message'
variable."
  (let* ((strategy (or strategy +llm-prompt-composition-strategy))
         ;; 1. Resolve the current system prompt value, calling it if it's a function.
         (current-prompt-value (if (functionp +gptel--prev-system-message)
                                   (funcall +gptel--prev-system-message)
                                 +gptel--prev-system-message))
         ;; 2. Extract the main system message string and the conversation history.
         (current-system-message (if (listp current-prompt-value)
                                     (or (car current-prompt-value) "")
                                   (if (stringp current-prompt-value) current-prompt-value "")))
         (current-history (if (listp current-prompt-value) (cdr current-prompt-value) nil))
         (snippets-to-add '()))

    ;; Handle hierarchical composition
    (when (eq strategy 'hierarchical)
      ;; Load system prompt if available
      (when-let* ((system-prompt-source (+llm-find-system-prompt))
                  (system-content (+llm-load-project-prompt-content system-prompt-source)))
        (push system-content snippets-to-add))

      ;; Load project context if available
      (when-let* ((context-source (+llm-find-project-context))
                  (context-content (+llm-load-project-prompt-content context-source)))
        (push context-content snippets-to-add)))

    ;; Handle context-only composition
    (when (eq strategy 'context-only)
      ;; Load only project context
      (when-let* ((context-source (+llm-find-project-context))
                  (context-content (+llm-load-project-prompt-content context-source)))
        (push context-content snippets-to-add)))

    ;; 3. Process explicit prompt sources using helper function.
    (dolist (source prompt-sources)
      (let* ((result (+llm--process-prompt-source source))
             (content (car result))
             (history (cdr result)))
        ;; Merge conversation history if present
        (when history
          (setq current-history (append current-history history)))

        ;; 4. Add content only if it's not already present (when appending).
        (when (and content (not (string-empty-p content)))
          (if (and appendp (not (eq strategy 'hierarchical)))
              ;; When appending (non-hierarchical), check for duplicates
              (let ((normalized-content (downcase (replace-regexp-in-string "\\s-+" " " content)))
                    (normalized-system-message (downcase (replace-regexp-in-string "\\s-+" " " current-system-message))))
                (unless (string-match-p (regexp-quote normalized-content) normalized-system-message)
                  (push content snippets-to-add)))
            ;; When not appending or hierarchical, just add the content
            (push content snippets-to-add)))))

    (setq snippets-to-add (cl-remove-duplicates snippets-to-add :test #'string=))

    ;; 5. Build the new system message.
    (let* ((new-snippets-str (string-join (nreverse snippets-to-add) "\n\n"))
           (final-system-message
            (cond
             ;; Hierarchical or replace mode: use only new content
             ((or (eq strategy 'hierarchical)
                  (eq strategy 'context-only)
                  (not appendp))
              (if (string-empty-p new-snippets-str)
                  current-system-message
                new-snippets-str))
             ;; Append mode: combine with current message
             (t
              (if (string-empty-p new-snippets-str)
                  current-system-message
                (string-trim (concat current-system-message "\n\n" new-snippets-str)))))))
      ;; 6. Return the final structure: a list of (new-message . history) or a string.
      (if current-history
          (cons final-system-message current-history)
        final-system-message))))

;;;###autoload
(defun +llm-get-prompt-from-files (prompt-files &optional expand-at-refs)
  "Read content from PROMPT-FILES.

PROMPT-FILES is a list of files, the first found will be used. File path
must be relative to project root.

If EXPAND-AT-REFS is non-nil, expand @file references in the content.

Returns the file content as a string, with @ references expanded if
requested."
  (let* ((project-root (zenit-project-root))
         (found-file nil))
    (dolist (file prompt-files)
      (let ((full-path (expand-file-name file project-root)))
        (when (and (not found-file) (file-exists-p full-path))
          (setq found-file full-path))))
    (if found-file
        (let ((content (+llm--read-file-content found-file)))
          (if expand-at-refs
              (+llm--expand-at-references content (file-name-directory found-file))
            content))
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
;;; @ syntax file inclusion

;;;###autoload
(defun +llm--expand-at-references (content base-dir &optional visited-files)
  "Recursively expand @file references in CONTENT.
BASE-DIR is the directory to resolve relative paths against.
VISITED-FILES tracks files already processed to prevent infinite recursion.
Returns the expanded content with all @ references replaced."
  (let ((visited (or visited-files (make-hash-table :test 'equal)))
        (result content))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      ;; Match @ followed by a file path (allowing various path characters)
      ;; The regex matches:
      ;; - @ symbol
      ;; - Followed by a path (letters, numbers, /, ., -, _, ~, spaces)
      ;; - Ends at whitespace, newline, or end of string
      (while (re-search-forward "@\\([~./]?[[:alnum:]_./~-]\\(?:[[:alnum:]_./~ -]\\)*[[:alnum:]_./~-]\\|[[:alnum:]_./~-]\\)" nil t)
        (let* ((path-raw (match-string 1))
               (path (string-trim path-raw))
               (full-path (expand-file-name path base-dir))
               (match-start (match-beginning 0))
               (match-end (match-end 0)))
          (cond
           ;; Check for circular references
           ((gethash full-path visited)
            (message "Warning: Circular reference detected for %s, skipping" path)
            ;; Replace with a warning comment
            (delete-region match-start match-end)
            (goto-char match-start)
            (insert (format "[Circular reference: %s]" path)))

           ;; File exists and is readable
           ((and (file-exists-p full-path)
                 (file-readable-p full-path)
                 (not (file-directory-p full-path)))
            ;; Mark as visited
            (puthash full-path t visited)
            ;; Read file content using helper
            (let ((file-content (+llm--read-file-content full-path)))
              ;; Recursively expand @ references in the included file
              (when file-content
                (let ((expanded-content
                       (+llm--expand-at-references
                        file-content
                        (file-name-directory full-path)
                        visited)))
                  ;; Replace @path with the expanded content
                  (delete-region match-start match-end)
                  (goto-char match-start)
                  (insert (format "\n# Included from: %s\n\n%s\n"
                                  path expanded-content))))))

           ;; Directory reference
           ((file-directory-p full-path)
            (delete-region match-start match-end)
            (goto-char match-start)
            (insert (format "[Directory listing not supported: %s]" path)))

           ;; File not found
           (t
            (delete-region match-start match-end)
            (goto-char match-start)
            (insert (format "[File not found: %s]" path))))))
      (setq result (buffer-string)))
    result))

;;
;;; Project prompt discovery

;;;###autoload
(defun +llm--match-files-in-directory (dir filter)
  "Return files in DIR matching FILTER.
FILTER can be a regexp string or a predicate function.
Returns a list of absolute file paths."
  (when (file-directory-p dir)
    (let* ((files (directory-files dir t "^[^.]"))  ; exclude dotfiles
           (pred (cond
                  ((stringp filter)
                   (lambda (f) (and (file-regular-p f)
                                    (string-match-p filter (file-name-nondirectory f)))))
                  ((functionp filter)
                   (lambda (f) (and (file-regular-p f)
                                    (funcall filter (file-name-nondirectory f)))))
                  (t (lambda (_) nil)))))
      (seq-filter pred files))))

;;;###autoload
(defun +llm-find-nearest-project-prompt (&optional dir)
  "Find the nearest project prompt file in the directory tree.
Searches upward from DIR (defaults to `default-directory') to the
project root, checking for files/directories in `+llm-project-prompt-files'.
Returns the absolute path to the first found file, or a list of files if a
directory without filter is specified, or nil if none found.

This implements the AGENTS.md spec: nested files take precedence over
root-level files, allowing monorepos to have per-subproject prompts.

Entries in `+llm-project-prompt-files' can be:
- Simple filename strings (e.g., \"AGENTS.md\") - searches for a single file
- Simple directory strings (e.g., \".prompts/\") - aggregates all .md files
- Cons cells (directory . filter) for directory-based searches with filtering"
  (when-let* ((start-dir (or dir buffer-file-name default-directory))
              (project-root (zenit-project-root dir)))
    ;; Try each entry in +llm-project-prompt-files until we find a match
    (cl-some
     (lambda (entry)
       (cond
        ;; Simple filename/directory string
        ((stringp entry)
         (when-let* ((found-dir (locate-dominating-file start-dir entry)))
           ;; Make sure we're within the project root
           (when (file-in-directory-p found-dir project-root)
             (let ((candidate (expand-file-name entry found-dir)))
               (cond
                ;; It's a directory - aggregate all markdown files
                ((file-directory-p candidate)
                 (let ((md-files (+llm--match-files-in-directory candidate "\\.md\\'")))
                   (when md-files
                     ;; Return a cons cell indicating it's a directory aggregation
                     (cons 'directory md-files))))
                ;; It's a file - return it
                ((file-readable-p candidate)
                 candidate))))))

        ;; Cons cell (directory . filter)
        ((consp entry)
         (let ((subdir (car entry))
               (filter (cdr entry)))
           ;; Use predicate to find directory containing matching files
           (when-let* ((found-dir
                        (locate-dominating-file
                         start-dir
                         (lambda (parent)
                           (let ((search-dir (expand-file-name subdir parent)))
                             (and (file-directory-p search-dir)
                                  (+llm--match-files-in-directory search-dir filter)))))))
             ;; Make sure we're within the project root
             (when (file-in-directory-p found-dir project-root)
               (let ((search-dir (expand-file-name subdir found-dir)))
                 ;; Return the first matching file
                 (car (+llm--match-files-in-directory search-dir filter)))))))))
     +llm-project-prompt-files)))

;;;###autoload
(defun +llm-load-project-prompt-content (&optional file-or-files)
  "Load content from project prompt FILE-OR-FILES.

If FILE-OR-FILES is nil, uses `+llm-find-nearest-project-prompt' to
auto-detect. FILE-OR-FILES can be:

- A string path to a single file
- A cons cell (directory . file-list) for aggregated directory content

Returns the file content as a string with @ references expanded, or nil
if no file found."
  (let ((prompt-source (or file-or-files (+llm-find-nearest-project-prompt))))
    (when prompt-source
      (cond
       ;; Directory aggregation case: (directory . (file1 file2 ...))
       ((and (consp prompt-source)
             (eq (car prompt-source) 'directory))
        (let ((files (cdr prompt-source))
              (contents '()))
          (dolist (file (sort files #'string<))
            (when-let* ((content (+llm--read-file-content file))
                        ;; Expand @ references for each file
                        (expanded (+llm--expand-at-references
                                   content
                                   (file-name-directory file))))
              (push (format "# %s\n\n%s"
                            (file-name-nondirectory file)
                            expanded)
                    contents)))
          (when contents
            (string-join (nreverse contents) "\n\n"))))

       ;; Single file case
       ((stringp prompt-source)
        (when-let ((content (+llm--read-file-content prompt-source)))
          ;; Expand @ references
          (+llm--expand-at-references
           content
           (file-name-directory prompt-source))))))))

;;
;;; Hierarchical prompt discovery

;;;###autoload
(defun +llm-find-system-prompt (&optional dir)
  "Find the system prompt file (behavioral instructions) in the directory tree.
Searches upward from DIR (defaults to `default-directory') to the
project root, checking for files/directories in `+llm-system-prompts-files'.
Returns the absolute path to the first found file, or a cons cell
\(directory . file-list) for directory aggregations, or nil if none found.

System prompts define HOW the LLM should behave (tone, style, expertise)."
  (when-let* ((start-dir (or dir buffer-file-name default-directory))
              (project-root (zenit-project-root dir)))
    ;; Try each entry in +llm-system-prompts-files until we find a match
    (cl-some
     (lambda (entry)
       (cond
        ;; Simple filename/directory string
        ((stringp entry)
         (when-let* ((found-dir (locate-dominating-file start-dir entry)))
           ;; Make sure we're within the project root
           (when (file-in-directory-p found-dir project-root)
             (let ((candidate (expand-file-name entry found-dir)))
               (cond
                ;; It's a directory - aggregate all markdown files
                ((file-directory-p candidate)
                 (let ((md-files (+llm--match-files-in-directory candidate "\\.md\\'")))
                   (when md-files
                     ;; Return a cons cell indicating it's a directory aggregation
                     (cons 'directory md-files))))
                ;; It's a file - return it
                ((file-readable-p candidate)
                 candidate))))))

        ;; Cons cell (directory . filter)
        ((consp entry)
         (let ((subdir (car entry))
               (filter (cdr entry)))
           ;; Use predicate to find directory containing matching files
           (when-let* ((found-dir
                        (locate-dominating-file
                         start-dir
                         (lambda (parent)
                           (let ((search-dir (expand-file-name subdir parent)))
                             (and (file-directory-p search-dir)
                                  (+llm--match-files-in-directory search-dir filter)))))))
             ;; Make sure we're within the project root
             (when (file-in-directory-p found-dir project-root)
               (let ((search-dir (expand-file-name subdir found-dir)))
                 ;; Return directory aggregation
                 (when-let ((files (+llm--match-files-in-directory search-dir filter)))
                   (cons 'directory files)))))))))
     +llm-system-prompts-files)))

;;;###autoload
(defun +llm-find-project-context (&optional dir)
  "Find the project context file in the directory tree.
Searches upward from DIR (defaults to `default-directory') to the
project root, checking for files/directories in `+llm-project-context-files'.
Returns the absolute path to the first found file, or a cons cell
\(directory . file-list) for directory aggregations, or nil if none found.

Project context defines WHAT the project is about (commands, architecture, guidelines)."
  (when-let* ((start-dir (or dir buffer-file-name default-directory))
              (project-root (zenit-project-root dir)))
    ;; Try each entry in +llm-project-context-files until we find a match
    (cl-some
     (lambda (entry)
       (cond
        ;; Simple filename/directory string
        ((stringp entry)
         (when-let* ((found-dir (locate-dominating-file start-dir entry)))
           ;; Make sure we're within the project root
           (when (file-in-directory-p found-dir project-root)
             (let ((candidate (expand-file-name entry found-dir)))
               (cond
                ;; It's a directory - aggregate all markdown files
                ((file-directory-p candidate)
                 (let ((md-files (+llm--match-files-in-directory candidate "\\.md\\'")))
                   (when md-files
                     ;; Return a cons cell indicating it's a directory aggregation
                     (cons 'directory md-files))))
                ;; It's a file - return it
                ((file-readable-p candidate)
                 candidate))))))

        ;; Cons cell (directory . filter)
        ((consp entry)
         (let ((subdir (car entry))
               (filter (cdr entry)))
           ;; Use predicate to find directory containing matching files
           (when-let* ((found-dir
                        (locate-dominating-file
                         start-dir
                         (lambda (parent)
                           (let ((search-dir (expand-file-name subdir parent)))
                             (and (file-directory-p search-dir)
                                  (+llm--match-files-in-directory search-dir filter)))))))
             ;; Make sure we're within the project root
             (when (file-in-directory-p found-dir project-root)
               (let ((search-dir (expand-file-name subdir found-dir)))
                 ;; Return directory aggregation
                 (when-let ((files (+llm--match-files-in-directory search-dir filter)))
                   (cons 'directory files)))))))))
     +llm-project-context-files)))


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

;;;###autoload
(defun +gptel/toggle-project-prompt ()
  "Toggle project-specific system prompt for the current buffer.
When enabled, uses hierarchical composition of system prompt and project context.
When disabled, restores the default/global system prompt."
  (interactive)
  (cond
   ;; Currently enabled - disable it
   (+llm-project-prompt-enabled
    (kill-local-variable 'gptel--system-message)
    (setq-local +llm-project-prompt-enabled nil)
    (kill-local-variable '+llm-system-prompt-file)
    (kill-local-variable '+llm-project-context-file)
    (kill-local-variable '+llm-project-prompt-file)
    (message "Project prompt disabled"))

   ;; Currently disabled - enable it
   ((zenit-project-p)
    (let ((strategy +llm-prompt-composition-strategy)
          (system-prompt-source nil)
          (context-source nil)
          (composed-content nil)
          (message-parts '()))

      ;; Discover prompt files based on strategy
      (cond
       ;; Hierarchical: discover both system prompt and context
       ((eq strategy 'hierarchical)
        (setq system-prompt-source (+llm-find-system-prompt))
        (setq context-source (or +llm-project-prompt-file
                                 (+llm-find-project-context))))

       ;; Context-only: discover only context
       ((eq strategy 'context-only)
        (setq context-source (or +llm-project-prompt-file
                                 (+llm-find-project-context))))

       ;; Replace (legacy): use old behavior
       ((eq strategy 'replace)
        (setq context-source (or +llm-project-prompt-file
                                 (+llm-find-nearest-project-prompt)))))

      ;; Check if we found any prompts
      (unless (or system-prompt-source context-source)
        (user-error "No project prompt files found (strategy: %s)" strategy))

      ;; Compose the system message
      (setq composed-content
            (+llm-compose-gptel-system-prompt nil nil strategy))

      (unless composed-content
        (user-error "Could not compose project prompt"))

      ;; Apply the composed prompt
      (setq-local gptel--system-message composed-content)
      (setq-local +llm-project-prompt-enabled t)
      (setq-local +llm-system-prompt-file system-prompt-source)
      (setq-local +llm-project-context-file context-source)
      (setq-local +llm-project-prompt-file context-source)

      ;; Build informative message
      (when system-prompt-source
        (push (format "system: %s"
                      (if (consp system-prompt-source)
                          (format "%d files" (length (cdr system-prompt-source)))
                        (file-name-nondirectory system-prompt-source)))
              message-parts))
      (when context-source
        (push (format "context: %s"
                      (if (consp context-source)
                          (format "%d files" (length (cdr context-source)))
                        (file-name-nondirectory context-source)))
              message-parts))

      (message "Project prompt enabled (%s): %s"
               strategy
               (string-join (nreverse message-parts) ", "))))

   ;; No project
   (t
    (user-error "Not in a project"))))

;;;###autoload
(defun +gptel/show-active-prompts ()
  "Display information about currently active prompt files in the minibuffer.
Shows system prompt, project context, and composition strategy."
  (interactive)
  (if (not +llm-project-prompt-enabled)
      (message "No project prompts active (using default/global system prompt)")
    (let ((info-parts '()))
      ;; Strategy
      (push (format "Strategy: %s" +llm-prompt-composition-strategy) info-parts)

      ;; System prompt
      (when +llm-system-prompt-file
        (push (format "System: %s"
                      (if (consp +llm-system-prompt-file)
                          (format "%d files in %s"
                                  (length (cdr +llm-system-prompt-file))
                                  (file-name-directory (cadr +llm-system-prompt-file)))
                        +llm-system-prompt-file))
              info-parts))

      ;; Project context
      (when +llm-project-context-file
        (push (format "Context: %s"
                      (if (consp +llm-project-context-file)
                          (format "%d files in %s"
                                  (length (cdr +llm-project-context-file))
                                  (file-name-directory (cadr +llm-project-context-file)))
                        +llm-project-context-file))
              info-parts))

      (message "%s" (string-join (nreverse info-parts) "\n")))))

;;;###autoload
(defun +gptel/reload-project-prompts ()
  "Reload project prompt files for the current buffer.
Useful after editing prompt files to pick up changes."
  (interactive)
  (if (not (zenit-project-p))
      (user-error "Not in a project")
    ;; Clear buffer-local variables
    (kill-local-variable 'gptel--system-message)
    (kill-local-variable '+llm-project-prompt-enabled)
    (kill-local-variable '+llm-system-prompt-file)
    (kill-local-variable '+llm-project-context-file)
    (kill-local-variable '+llm-project-prompt-file)
    ;; Re-run setup
    (+llm--setup-project-prompt-h)
    (if +llm-project-prompt-enabled
        (message "Project prompts reloaded")
      (message "No project prompts found"))))

;;;###autoload
(defun +gptel/edit-system-prompt ()
  "Open the system prompt file for editing.
If the file doesn't exist, offer to create it."
  (interactive)
  (if-let* ((system-prompt-source (or +llm-system-prompt-file
                                      (+llm-find-system-prompt))))
      ;; File exists
      (cond
       ;; Directory aggregation
       ((consp system-prompt-source)
        (let ((files (cdr system-prompt-source)))
          (if (= (length files) 1)
              (find-file (car files))
            ;; Multiple files - let user choose
            (find-file (completing-read "Edit system prompt file: " files nil t)))))
       ;; Single file
       (t
        (find-file system-prompt-source)))
    ;; File doesn't exist - offer to create
    (when (y-or-n-p "No system prompt file found. Create one? ")
      (let* ((project-root (zenit-project-root))
             (default-path (expand-file-name ".instructions.d/behavior.md" project-root))
             (path (read-file-name "Create system prompt file: " project-root nil nil
                                   ".instructions.d/behavior.md")))
        ;; Create directory if needed
        (make-directory (file-name-directory path) t)
        ;; Create file with template
        (find-file path)
        (when (zerop (buffer-size))
          (insert "# System Prompt\n\n")
          (insert "<!-- Define HOW the LLM should behave: tone, style, expertise -->\n\n")
          (save-buffer))))))

;;;###autoload
(defun +gptel/edit-project-context ()
  "Open the project context file for editing.
If the file doesn't exist, offer to create it."
  (interactive)
  (if-let* ((context-source (or +llm-project-context-file
                                (+llm-find-project-context))))
      ;; File exists
      (cond
       ;; Directory aggregation
       ((consp context-source)
        (let ((files (cdr context-source)))
          (if (= (length files) 1)
              (find-file (car files))
            ;; Multiple files - let user choose
            (find-file (completing-read "Edit project context file: " files nil t)))))
       ;; Single file
       (t
        (find-file context-source)))
    ;; File doesn't exist - offer to create
    (when (y-or-n-p "No project context file found. Create one? ")
      (let* ((project-root (zenit-project-root))
             (options '("CLAUDE.md" "AGENTS.md" "CONVENTIONS.md"))
             (choice (completing-read "Create context file: " options nil nil))
             (path (expand-file-name choice project-root)))
        (find-file path)
        (when (zerop (buffer-size))
          (insert (format "# %s\n\n" (file-name-base choice)))
          (insert "<!-- Define WHAT the project is about: commands, architecture, guidelines -->\n\n")
          (save-buffer))))))

;;;###autoload
(defun +gptel/set-composition-strategy ()
  "Interactively set the prompt composition strategy for the current buffer."
  (interactive)
  (let* ((strategies '(("hierarchical" . hierarchical)
                       ("replace" . replace)
                       ("context-only" . context-only)))
         (choice (completing-read
                  "Composition strategy: "
                  strategies nil t))
         (strategy (alist-get choice strategies nil nil #'equal)))
    (setq-local +llm-prompt-composition-strategy strategy)
    ;; Reload prompts with new strategy
    (when (and (zenit-project-p) +llm-auto-load-project-prompts)
      (+llm--setup-project-prompt-h))
    (message "Composition strategy set to: %s" strategy)))

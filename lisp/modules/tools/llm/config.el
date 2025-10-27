;; tools/llm/config.el -*- lexical-binding: t; -*-

(defvar +llm-aider-arg-list
  '(("DeepSeek" :args ("--deepseek") :host "api.deepseek.com" :user "apikey"
     :env "DEEPSEEK_API_KEY")
    ("Claude Sonnet" :args ("--sonnet" "--cache-prompts" "--cache-keepalive-pings" "6")
     :host "api.anthropic.com" :user "apikey" :env "ANTHROPIC_API_KEY"))
  "List of arguments for different LLM configurations.")

(defvar +llm-prompts-dir (file-name-concat (dir!) "prompts/")
  "Directory where GPTel prompts are defined, one per file.
Files are expected to be plain text files, e.g. .md or .txt.")

(defvar +llm-user-prompts-dir (file-name-concat zenit-local-conf-dir "prompts/")
  "User directory where GPTel prompts are defined, one per file.
Files are expected to be plain text files, e.g. .md or .txt.")

(defvar +llm-auto-load-project-prompts t
  "When non-nil, automatically load project-specific system prompts.
This searches for prompt files like AGENTS.md, CLAUDE.md, etc. in the
project directory tree, with the nearest file taking precedence. Can be
overridden via dir-locals for specific projects.")

(defvar +llm-system-prompts-files
  '(;; ".instructions.d/behavior.md"
    ;; ".instructions.d/system-prompt.md"
    ;; (".instructions.d/behavior" . "\\.md\\'")
    (".instructions.d" . "\\.md\\'")
    "BEHAVIOR.md")
  "Files or directories containing LLM system prompts (behavioral instructions).
These define HOW the LLM should behave (tone, style, expertise).

Each entry can be:
- A string: filename or directory name to search for
  - If it's a file (e.g., \"BEHAVIOR.md\"): uses that single file
  - If it's a directory: aggregates all .md files within
- A cons cell (DIR . FILTER): directory with a filter where:
  - DIR is a directory path relative to search location
  - FILTER is either:
    - A regexp string to match filenames
    - A function taking a filename and returning non-nil to include it

Files are searched from the current buffer's directory upward to the
project root. The first found file wins (AGENTS.md spec behavior).

Files may include other files using @ syntax (e.g., @path/to/file.md).
These references are recursively expanded when loading prompt content.

Always prepended to the final system prompt if found.")

(defvar +llm-project-context-files
  '("CLAUDE.md"
    "AGENTS.md"
    "CONVENTIONS.md"
    ".github/copilot-instructions.md")
  "Files or directories containing project-specific context.
These define WHAT the project is about (commands, architecture, guidelines).

Each entry can be:
- A string: filename or directory name to search for
  - If it's a file (e.g., \"AGENTS.md\"): uses that single file
  - If it's a directory: aggregates all .md files within
- A cons cell (DIR . FILTER): directory with a filter where:
  - DIR is a directory path relative to search location
  - FILTER is either:
    - A regexp string to match filenames
    - A function taking a filename and returning non-nil to include it

Files are searched from the current buffer's directory upward to the
project root. The first found file wins (AGENTS.md spec behavior).

Files may include other files using @ syntax (e.g., @path/to/file.md).
These references are recursively expanded when loading prompt content.

Always appended after system prompt if found.")

(defvar +llm-prompt-composition-strategy 'hierarchical
  "How to compose system prompts from discovered files.

Strategies:
- `hierarchical' (recommended): Compose in layers:
    1. System prompt (from +llm-system-prompts-files)
    2. Project context (from +llm-project-context-files)
    3. Ephemeral prompts (from presets, inline prompts, etc.)
  All layers are joined with double newlines.

- `replace': Project context completely replaces the system prompt.
  This is the legacy behavior for backward compatibility.

- `context-only': Only use project context, skip system prompt layer.
  Useful when you want project-specific behavior without global defaults.")

(defvar-local +llm-system-prompt-file nil
  "Buffer-local path to the system prompt file being used.
Set automatically by `+llm--setup-project-prompt-h'.")

(defvar-local +llm-project-context-file nil
  "Buffer-local path to the project context file being used.
Set automatically by `+llm--setup-project-prompt-h'.")

(defvar-local +llm-project-prompt-file nil
  "Buffer-local override for which project context file to use.
When set, this file path is used instead of auto-detection.
Legacy variable, prefer +llm-project-context-file.")

(defvar-local +llm-project-prompt-enabled nil
  "Buffer-local flag indicating if project prompt is active.")


(use-package! gptel
  :defer t
  :init
  ;; Project prompt auto-loading
  (defun +llm--setup-project-prompt-h ()
    "Set up buffer-local project prompt if available and enabled.
Uses hierarchical composition strategy by default to layer:
1. System prompt (behavioral instructions)
2. Project context (project-specific information)
3. Preset-specific prompts (if any)"
    (when (and +llm-auto-load-project-prompts
               (featurep 'gptel)
               (zenit-project-p)
               ;; Guard against nil default-directory
               default-directory
               ;; Ensure it's valid
               (file-directory-p default-directory))
      (let ((strategy +llm-prompt-composition-strategy)
            (system-prompt-source nil)
            (context-source nil)
            (composed-content nil))

        ;; Discover prompt files based on strategy
        (cond
         ;; Hierarchical: discover both system prompt and context
         ((eq strategy 'hierarchical)
          (setq system-prompt-source (+llm-find-system-prompt))
          (setq context-source (or +llm-project-prompt-file  ; Allow override
                                   (+llm-find-project-context))))

         ;; Context-only: discover only context
         ((eq strategy 'context-only)
          (setq context-source (or +llm-project-prompt-file
                                   (+llm-find-project-context))))

         ;; Replace (legacy): use old behavior
         ((eq strategy 'replace)
          (setq context-source (or +llm-project-prompt-file
                                   (+llm-find-nearest-project-prompt)))))

        ;; Compose the system message if we found any prompts
        (when (or system-prompt-source context-source)
          (setq composed-content
                (+llm-compose-gptel-system-prompt nil nil strategy)))

        ;; Apply the composed prompt to buffer if we got content
        (when composed-content
          (setq-local gptel--system-message composed-content)
          (setq-local +llm-project-prompt-enabled t)
          (setq-local +llm-system-prompt-file system-prompt-source)
          (setq-local +llm-project-context-file context-source)
          ;; Legacy compatibility
          (setq-local +llm-project-prompt-file context-source)))))

  ;; Hook into file opening
  (add-hook! '(find-file-hook zenit-switch-buffer-hook) #'+llm--setup-project-prompt-h)

  ;; Also set up when gptel-mode is activated in a buffer
  (add-hook 'gptel-mode-hook #'+llm--setup-project-prompt-h)
  :config
  ;; Detect project prompt
  (+llm--setup-project-prompt-h)

  (setq
   ;; Use `org-mode' for the `gptel' buffer
   gptel-default-mode 'org-mode
   ;; Always include tool output
   gptel-include-tool-results t
   ;; Make expert commands available
   gptel-expert-commands t)
  ;; Prettier prompts
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*Prompt*: "
        (alist-get 'org-mode gptel-response-prefix-alist) "*Response*:\n")

  ;; Each org heading is its own conversation
  (after! gptel-org
    (setq-default gptel-org-branching-context t))

  ;; Add `major-mode' -> language mapping
  (static-when (modulep! :lang ess)
    (add-to-list 'gptel--mode-description-alist '(ess-r-mode . "R")))

  ;; Keep previous system message in `+gptel--prev-system-message'
  (advice-add #'gptel-system-prompt :around #'+gptel-prev-system-message-a)

  ;; CLaude
  (gptel-make-anthropic "Claude"
    :stream t
    :key 'gptel-api-key)
  ;; DeepSeek (as default)
  (setq gptel-model 'deepseek-chat
        gptel-backend (gptel-make-deepseek "DeepSeek"
                        :stream t
                        :key 'gptel-api-key))
  ;; GLM
  (gptel-make-openai "GLM"
    :host "api.z.ai"
    :endpoint "/api/paas/v4/chat/completions"
    :stream t
    :key 'gptel-api-key
    :models '((glm-4.6
               :description "High Performance, Strong Reasoning, More Versatile"
               :capabilities (tool-use reasoning)
               :context-window 200
               :input-cost 0.6
               :output-cost 2.2)
              (glm-4.5-x
               :description "High Performance, Strong Reasoning, Ultra-Fast Response"
               :capabilities (tool-use reasoning)
               :context-window 128
               :input-cost 2.2
               :output-cost 8.9)
              (glm-4.5-air
               :description "Cost-Effective, Lightweight, High Performance"
               :capabilities (tool-use)
               :context-window 128
               :input-cost 0.2
               :output-cost 1.1)
              (glm-4.5-airx
               :description "Lightweight, High Performance, Ultra-Fast Response"
               :capabilities (tool-use)
               :context-window 128
               :input-cost 1.1
               :output-cost 4.5)
              (glm-4.5-flash
               :description "Lightweight, High Performance"
               :capabilities (tool-use)
               :context-window 128
               :input-cost 0
               :output-cost 0)))

  (setq gptel-display-buffer-action nil)  ; if user changes this, popup manager will bow out
  (set-popup-rule!
    (lambda (bname &optional _action)
      (and (null gptel-display-buffer-action)
           (buffer-local-value 'gptel-mode (get-buffer bname))))
    :select t
    :size 0.3
    :quit nil
    :ttl nil
    :tabbed t)

  (add-hook! 'gptel-post-response-functions :depth 90
    (cl-defun +gptel-clean-up-refactored-code-h (beg end)
      "Clean up the code responses for refactored code in the current buffer.

The response is placed between BEG and END. The current buffer is
guaranteed to be the response buffer."
      ;; Don't want this to happen in the dedicated buffer.
      (when gptel-mode
        (cl-return-from +gptel-clean-up-refactored-code-h))
      (when (and beg end)
        (save-excursion
          (let ((contents
                 (replace-regexp-in-string
                  "\n*``.*\n*" ""
                  (buffer-substring-no-properties beg end))))
            (delete-region beg end)
            (goto-char beg)
            (insert contents))
          ;; Indent the code to match the buffer indentation if it's messed up.
          (indent-region beg end)
          (pulse-momentary-highlight-region beg end))))
    (defun +gptel-remove-extra-whitespace-h (beg end)
      "Clean up superfluous whitespace."
      (save-excursion
        (goto-char beg)
        (while (re-search-forward "\n\n\n+" end t)
          (replace-match "\n\n"))))
    (defun +gptel-remove-headings-h (beg end)
      "Convert `org-mode' headings to bold text."
      (when (derived-mode-p 'org-mode)
        (save-excursion
          (goto-char beg)
          (while (re-search-forward org-heading-regexp end t)
            (forward-line 0)
            (delete-char (1+ (length (match-string 1))))
            (insert-and-inherit "*")
            (end-of-line)
            (skip-chars-backward " \t\r")
            (insert-and-inherit "*"))))))

  ;; Tools
  (gptel-make-tool
   :name "read_buffer"
   :function (lambda (buffer)
               (unless (buffer-live-p (get-buffer buffer))
                 (error "error: buffer %s is not live." buffer))
               (with-current-buffer buffer
                 (buffer-substring-no-properties (point-min) (point-max))))
   :description "Return the contents of an emacs buffer"
   :args (list '(:name "buffer"
                 :type string
                 :description "The name of the buffer whose contents are to be retrieved"))
   :category "emacs")

  ;; Presets
  (gptel-make-preset 'tool-use
    :description "Use tools"
    :use-tools t
    :system (lambda ()
              (+llm-compose-gptel-system-prompt '("tool-use.md"))))

  (gptel-make-preset 'commit-summary
    :description "For generating commit message summaries"
    :system (+llm-get-prompt-from-files '("commit-summary.md" "docs/commit-summary.md"))
    :include-reasoning nil
    :tools nil)

  ;; Keybinds
  (map! :map gptel-mode-map
        "C-c C-n" #'+gptel/next-prompt
        "C-c C-p" #'+gptel/previous-prompt
        "C-c C-x" #'+gptel/toggle-project-prompt))


(use-package! gptel-quick
  :defer t)


(use-package mcp
  :after gptel
  :config
  (require 'gptel-integrations)
  (require 'mcp-hub)

  (setq! mcp-hub-servers
         `(("sequential-thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking@latest")))
           ("context7" . (:command "npx" :args ("-y" "@upstash/context7-mcp@latest")))
           ("duckduckgo" . (:command "uvx" :args ("duckduckgo-mcp-server")))))

  (after! gptel
    (defvar +gptel--memory-mcp-last-root-dir nil
      "Last used project directory.")
    (gptel-make-preset 'memory-bank
      :description "Use Memory Bank MCP to add and update project context"
      :parents '(tool-use)
      :system (lambda ()
                (+llm-compose-gptel-system-prompt '("memory-bank-mcp.md")))
      :tools '(:append "mcp-memory-bank")
      :pre (lambda ()
             (let* ((root-dir (expand-file-name "memory-bank/" (zenit-project-root))))
               (setf (alist-get "memory-bank" mcp-hub-servers nil nil #'equal)
                     `(:command "npx" :args ("-y" "@allpepper/memory-bank-mcp@latest")
                       :env (:MEMORY_BANK_ROOT ,root-dir)))
               (unless (equal +gptel--memory-mcp-last-root-dir root-dir)
                 (when (mcp--server-running-p "memory-bank")
                   (gptel-mcp-disconnect '("memory-bank"))
                   (mcp-stop-server "memory-bank")))

               (unless (mcp--server-running-p "memory-bank")
                 (gptel-mcp-connect '("memory-bank") 'sync)
                 (setq +gptel--memory-mcp-last-root-dir root-dir))))))

  (mcp-hub-start-all-server))


(use-package! macher
  :after gptel
  :config
  (setq! macher-action-buffer-ui 'org)
  (after! gptel
    (macher-install))

  ;; Load extra tools
  (load! "macher-tools"))


(use-package! mevedel
  :after macher
  :config
  (setq! mevedel-empty-tag-query-matches-all nil)
  (mevedel-install)

  (defun +mevedel-add-org-heading-maybe-h (execution)
    "Ensure an Org heading exists for the current mevedel directive.
If a heading with the current MEVEDELUUID exists, move to its end.
Otherwise, create a new heading at the end of the buffer with a
truncated summary of the execution and set the MEVEDELUUID property."
    (let ((action-buffer (current-buffer))
          (mevedel-uuid (and (boundp 'mevedel--current-directive-uuid)
                             mevedel--current-directive-uuid)))
      (with-current-buffer action-buffer
        (when (and (derived-mode-p 'org-mode) mevedel-uuid)
          (if-let* ((matched-headings (let (matches)
                                        (org-map-entries
                                         (lambda ()
                                           (when (equal (org-entry-get (point) "MEVEDELUUID")
                                                        mevedel-uuid)
                                             ;; Store position
                                             (push (point) matches)))
                                         nil nil 'archive)
                                        matches)))
              ;; Found existing heading with this UUID - append to it
              (let ((target-pos (car matched-headings)))
                (goto-char target-pos)
                ;; There should be a prompt prefix already under existing
                ;; headings
                (org-narrow-to-subtree)
                (goto-char (point-max))
                (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
                  ;; Clean up trailing whitespace after prompt prefix. This
                  ;; should ensure that the new prompt will be inserted right
                  ;; after the prefix.
                  (save-excursion
                    (goto-char (point-max))
                    (skip-chars-backward " \t\r\n")
                    (delete-region (point) (point-max))
                    (insert " "))))
            ;; No existing heading - create new one
            (let* ((summary (and (functionp 'macher-action-execution-summary)
                                 (macher-action-execution-summary execution)))
                   (truncated-summary (if summary
                                          (let* ((lines (split-string summary "\n" t "[[:space:]]*"))
                                                 (first-line (or (car lines) ""))
                                                 (prefix (or (alist-get major-mode gptel-prompt-prefix-alist) ""))
                                                 (used-length (length prefix))
                                                 (available-length (max 10 (- (or fill-column 70) used-length 3))))
                                            (truncate-string-to-width first-line available-length nil nil "..."))
                                        "New directive")))
              (goto-char (point-max))
              (let ((buffer-empty-p (= (point-min) (point-max))))
                ;; Insert prompt prefix only if the buffer is empty. As soon as
                ;; we add a heading, `macher--before-action' will not add a
                ;; prompt anymore.
                (insert (concat (unless buffer-empty-p "\n\n") "* " truncated-summary "\n"
                                (or (alist-get major-mode gptel-prompt-prefix-alist) "")))
                (org-set-property "MEVEDELUUID" mevedel-uuid)
                (org-end-of-subtree))))))))

  (defun +mevedel-fixup-action-buffer-maybe-h (_err _execution fsm)
    "Fixup newly entered response.
Indent the content under the current subtree, widen the buffer if
narrowed and ensure there is newline between the end of the response and
the next heading. The marker info from the gptel FSM is used."
    (let* ((action-buffer (current-buffer))
           ;; Get start and end marker positions from FSM
           (info (gptel-fsm-info fsm))
           (start-marker (plist-get info :position))
           (tracking-marker (plist-get info :tracking-marker))
           (current-marker (or tracking-marker start-marker)))
      (with-current-buffer action-buffer
        ;; Indent the current subtree and remove narrowing
        (save-excursion
          (org-mark-subtree)
          (indent-region (region-beginning) (region-end))
          (deactivate-mark))
        (when (buffer-narrowed-p)
          (widen))
        ;; Add a newline above the following heading as a visual separator. This
        ;; is purely cosmetic.
        (when current-marker
          (save-excursion
            (goto-char current-marker)
            (when (= (org-next-visible-heading 1) 0)
              (beginning-of-line)
              (newline)))))))

  (defadvice! +mevedel-heading-per-directive-a ()
    "Add and manage separate org headings for each directive."
    :after #'macher--action-buffer-setup-basic
    (add-hook 'macher-before-action-functions #'+mevedel-add-org-heading-maybe-h -99 t)
    (add-hook 'macher-after-action-functions #'+mevedel-fixup-action-buffer-maybe-h -99 t)))

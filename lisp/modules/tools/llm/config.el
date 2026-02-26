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


;; PATCH 2026-02-11: `gptel'
(el-patch-feature gptel)
(compile-along! "patches/gptel")
(compile-along! "patches/gptel-anthropic")
(compile-along! "patches/gptel-openai")


(use-package! gptel
  :defer t
  :hook (gptel-mode . gptel-highlight-mode)
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

  ;; ;; Hook into file opening
  ;; (add-hook! '(find-file-hook zenit-switch-buffer-hook) #'+llm--setup-project-prompt-h)

  ;; ;; Also set up when gptel-mode is activated in a buffer
  ;; (add-hook 'gptel-mode-hook #'+llm--setup-project-prompt-h)
  :config
  (load! "patches/gptel")
  (after! gptel-anthropic
    (load! "patches/gptel-anthropic"))
  (after! gptel-openai
    (load! "patches/gptel-openai"))

  ;; Detect project prompt
  (+llm--setup-project-prompt-h)

  (setq
   ;; Use `org-mode' for the `gptel' buffer
   gptel-default-mode 'markdown-mode
   ;; Always include tool output
   gptel-include-tool-results t
   ;; Make expert commands available
   gptel-expert-commands t)
  ;; With `gptel-highlight-mode', prefixes are not neccessary anymore
  (setq gptel-prompt-prefix-alist nil
        gptel-response-prefix-alist nil)

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
  ;; DeepSeek (Anthropic API)
  (gptel-make-anthropic "DeepSeek-Chat-coding"
    :stream t
    :key 'gptel-api-key
    :request-params '(:max_tokens 8192
                      :temperature 0.0
                      :thinking (:type "enabled"))
    :host "api.deepseek.com"
    :endpoint "/anthropic/v1/messages"
    :models '((deepseek-chat
               :capabilities (tool)
               :context-window 128
               :input-cost 0.56
               :output-cost 1.68)))
  (gptel-make-anthropic "DeepSeek-R-coding"
    :stream t
    :key 'gptel-api-key
    :request-params '(:max_tokens 65536
                      :temperature 0.0
                      :thinking (:type "enabled"))
    :host "api.deepseek.com"
    :endpoint "/anthropic/v1/messages"
    :models '((deepseek-reasoner
               :capabilities (tool reasoning)
               :context-window 128
               :input-cost 0.56
               :output-cost 1.68)))

  ;; GLM
  (setq gptel-model 'glm-5
        gptel-backend (gptel-make-glm-openai "GLM-coding"
                        :stream t
                        :key 'gptel-api-key
                        :request-params '(:thinking
                                          (:type "enabled"
                                           :clear_thinking :json-false)
                                          :max_tokens 16384
                                          :temperature 0.7)))

  ;; (setq gptel-display-buffer-action nil)  ; if user changes this, popup manager will bow out
  ;; (set-popup-rule!
  ;;   (lambda (bname &optional _action)
  ;;     (and (null gptel-display-buffer-action)
  ;;          (buffer-local-value 'gptel-mode (get-buffer bname))))
  ;;   :select t
  ;;   :size 0.3
  ;;   :quit nil
  ;;   :ttl nil
  ;;   :tabbed t)

  (set-popup-rule!
    (lambda (bname &optional _action)
      (and (null gptel-display-buffer-action)
           (buffer-local-value 'gptel-mode (get-buffer bname))))
    :actions '(display-buffer-pop-up-frame)
    :frame-parameters '((width . 80)
                        (height . 24)
                        (minibuffer . t))
    :quit nil
    :ttl nil)


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

  ;; Todo management tools
  ;; (load! "tools/todo.el")
  ;; (load! "tools/agent_task_v3.el")

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


(use-package! gptel-agent
  :after gptel)


(use-package! mevedel
  :after gptel
  :config
  (setq! mevedel-empty-tag-query-matches-all nil)
  ;; When `evil' is used, bind RET in normal mode to open the overlay menu
  (static-when (modulep! :editor evil)
    (setq! mevedel-ov-dispatch-key nil)
    (dolist (map mevedel--actions-maps)
      (define-key (symbol-value map)
                  [return]
                  `(menu-item "" mevedel--ov-actions-dispatch
                    :filter ,(lambda (cmd) (when (evil-normal-state-p) cmd))))
      (define-key (symbol-value map)
                  "RET"
                  `(menu-item "" mevedel--ov-actions-dispatch
                    :filter ,(lambda (cmd) (when (evil-normal-state-p) cmd))))))

  (mevedel-install))

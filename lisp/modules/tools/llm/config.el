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
  (mevedel-install)

  ;;   ;; TODO 2025-11-27: Add sequential thinking if model is deepseek-chat
  ;;   (defun +mevedel-system--tool-instructions-seqthink ()
  ;;     "Return instructions for the sequentialthinking tool."
  ;;     "<tool name=\"sequentialthinking\">
  ;; **When to use `sequentialthinking`:**
  ;; - Complex problems requiring multi-step reasoning
  ;; - Design and planning tasks that may need revision during analysis
  ;; - Problems where the full scope isn't clear initially
  ;; - Analysis that might need course correction as understanding deepens
  ;; - Tasks requiring hypothesis generation and verification
  ;; - Situations where you need to filter irrelevant information
  ;; - Problems that benefit from explicit reasoning traces

  ;; **When NOT to use `sequentialthinking`:**
  ;; - Simple, straightforward tasks with obvious solutions
  ;; - Questions that can be answered directly from available information
  ;; - Tasks where reasoning steps are trivial
  ;; - When you're confident in a direct approach

  ;; **How to use `sequentialthinking`:**
  ;; - Start with an estimate of thoughts needed (can adjust later)
  ;; - Each thought builds your understanding incrementally
  ;; - Feel free to revise previous thoughts using `isRevision` and `revisesThought`
  ;; - Branch into alternative approaches using `branchFromThought` and `branchId`
  ;; - Adjust `totalThoughts` up or down as understanding evolves
  ;; - Continue adding thoughts even after initial estimate if needed
  ;; - Express uncertainty and explore alternatives when appropriate
  ;; - Generate hypotheses and verify them in subsequent thoughts
  ;; - Only set `nextThoughtNeeded` to false when satisfied with solution
  ;; - Filter out irrelevant information at each step

  ;; **Flexible thinking process:**
  ;; - Not all thoughts need to build linearly—you can branch or backtrack
  ;; - Question previous decisions when new insights emerge
  ;; - Revise your approach if initial direction proves unproductive
  ;; - Use `needsMoreThoughts` when reaching estimated end but realizing more analysis needed

  ;; **Final output:**
  ;; - Provide a single, well-reasoned answer after thought process completes
  ;; - The answer should synthesize insights from your reasoning chain
  ;; - Ensure the solution is verified against your chain of thought
  ;; </tool>")

  ;;   (gptel-make-preset 'mevedel-discuss-deepseek
  ;;     :description "mevedel-discuss with sequential thinking for deepseek-chat"
  ;;     :parents '(mevedel-discuss)
  ;;     :pre (lambda ()
  ;;            (when-let* ((_ (eq gptel-model 'deepseek-chat))
  ;;                        (chat-buffer (mevedel--chat-buffer (mevedel-workspace))))
  ;;              (with-current-buffer chat-buffer
  ;;                (gptel-mcp-connect '("sequential-thinking") 'sync)
  ;;                (setq-local mevedel-tools--ro-tools (cons "sequentialthinking" mevedel-tools--ro-tools))
  ;;                (setf (alist-get "sequentialthinking" mevedel-system-tool-name-to-instruction-alist nil nil #'equal)
  ;;                      '+mevedel-system--tool-instructions-seqthink))))
  ;;     :tools '(:function (lambda (tools)
  ;;                          (if (eq gptel-model 'deepseek-chat)
  ;;                              (cons
  ;;                               (alist-get
  ;;                                "sequentialthinking"
  ;;                                (alist-get "mcp-sequential-thinking" gptel--known-tools nil nil #'equal)
  ;;                                nil nil #'equal)
  ;;                               tools)
  ;;                            tools)))
  ;;     :system '(:function (lambda (_system)
  ;;                           (mevedel-system-build-prompt mevedel-tools--ro-tools))))
  ;;   (gptel-make-preset 'mevedel-implement-deepseek
  ;;     :description "mevedel-implement with sequential thinking for deepseek-chat"
  ;;     :parents '(mevedel-implement)
  ;;     :pre (lambda ()
  ;;            (when-let* ((_ (eq gptel-model 'deepseek-chat))
  ;;                        (chat-buffer (mevedel--chat-buffer (mevedel-workspace))))
  ;;              (with-current-buffer chat-buffer
  ;;                (gptel-mcp-connect '("sequential-thinking") 'sync)
  ;;                (setq-local mevedel-tools--ro-tools (cons "sequentialthinking" mevedel-tools--ro-tools))
  ;;                (setf (alist-get "sequentialthinking" mevedel-system-tool-name-to-instruction-alist nil nil #'equal)
  ;;                      '+mevedel-system--tool-instructions-seqthink))))
  ;;     :tools '(:function (lambda (tools)
  ;;                          (if (eq gptel-model 'deepseek-chat)
  ;;                              (cons
  ;;                               (alist-get
  ;;                                "sequentialthinking"
  ;;                                (alist-get "mcp-sequential-thinking" gptel--known-tools nil nil #'equal)
  ;;                                nil nil #'equal)
  ;;                               tools)
  ;;                            tools)))
  ;;     :system '(:function (lambda (_system)
  ;;                           (mevedel-system-build-prompt
  ;;                            (append mevedel-tools--ro-tools mevedel-tools--rw-tools)))))

  ;;   (setf (alist-get 'discuss mevedel-action-preset-alist) 'mevedel-discuss-deepseek)
  ;;   (setf (alist-get 'implement mevedel-action-preset-alist) 'mevedel-implement-deepseek)
  )
;; (cl-defmethod gptel--parse-buffer :around ((_backend gptel-deepseek) _max-entries)
;;   "Merge successive prompts in the prompts list that have the same role.

;; The Deepseek API requires strictly alternating roles (user/assistant) in messages."
;;   (let* ((prompts (cl-call-next-method))
;;          (index prompts))
;;     (prog1 prompts
;;       (while index
;;         (let ((p1 (car index))
;;               (p2 (cadr index))
;;               (rest (cdr index)))
;;           ;; Only merge if both messages are simple text messages (no tool calls, reasoning, etc.)
;;           (when (and p2
;;                      (equal (plist-get p1 :role) (plist-get p2 :role))
;;                      ;; Don't merge if either message has special fields
;;                      (not (or (plist-get p1 :tool_calls)
;;                               (plist-get p2 :tool_calls)
;;                               (plist-get p1 :reasoning_content)
;;                               (plist-get p2 :reasoning_content))))
;;             (setf (plist-get p1 :content)
;;                   (concat (plist-get p1 :content) "\n"
;;                           (plist-get p2 :content)))
;;             (setcdr index (cdr rest)))
;;           (setq index (cdr index)))))))

;; (cl-defmethod gptel-curl--parse-stream ((_backend gptel-openai) info)
;;   "Parse an OpenAI API data stream.

;; Return the text response accumulated since the last call to this
;; function.  Additionally, mutate state INFO to add tool-use
;; information if the stream contains it."
;;   (let* ((content-strs))
;;     (condition-case nil
;;         (while (re-search-forward "^data:" nil t)
;;           (save-match-data
;;             (if (looking-at " *\\[DONE\\]")
;;                 ;; The stream has ended, so we do the following thing (if we found tool calls)
;;                 ;; - pack tool calls into the messages prompts list to send (INFO -> :data -> :messages)
;;                 ;; - collect tool calls (formatted differently) into (INFO -> :tool-use)
;;                 (when-let* ((tool-use (plist-get info :tool-use))
;;                             (args (apply #'concat (nreverse (plist-get info :partial_json))))
;;                             (func (plist-get (car tool-use) :function)))
;;                   (plist-put func :arguments args) ;Update arguments for last recorded tool
;;                   (gptel--inject-prompt
;;                    (plist-get info :backend) (plist-get info :data)
;;                    `(:role "assistant" :content :null :tool_calls ,(vconcat tool-use)
;;                      ,@(when-let ((reasoning (plist-get info :reasoning)))
;;                          (list :reasoning_content reasoning)))) ; :refusal :null
;;                   (cl-loop
;;                    for tool-call in tool-use ; Construct the call specs for running the function calls
;;                    for spec = (plist-get tool-call :function)
;;                    collect (list :id (plist-get tool-call :id)
;;                                  :name (plist-get spec :name)
;;                                  :args (ignore-errors (gptel--json-read-string
;;                                                        (plist-get spec :arguments))))
;;                    into call-specs
;;                    finally (plist-put info :tool-use call-specs)))
;;               (when-let* ((response (gptel--json-read))
;;                           (delta (map-nested-elt response '(:choices 0 :delta))))
;;                 (if-let* ((content (plist-get delta :content))
;;                           ((not (or (eq content :null) (string-empty-p content)))))
;;                     (push content content-strs)
;;                   ;; No text content, so look for tool calls
;;                   (when-let* ((tool-call (map-nested-elt delta '(:tool_calls 0)))
;;                               (func (plist-get tool-call :function)))
;;                     (if (and (plist-get func :name)
;;                              ;; TEMP: This check is for litellm compatibility, should be removed
;;                              (not (equal (plist-get func :name) "null"))) ; new tool block begins
;;                         (progn
;;                           ;; If we have accumulated reasoning and a tool call starts, mark reasoning as ended
;;                           (when (and (plist-member info :reasoning)
;;                                      (not (eq (plist-get info :reasoning-block) t))
;;                                      (not (eq (plist-get info :reasoning-block) 'done)))
;;                             (plist-put info :reasoning-block t))
;;                           (when-let* ((partial (plist-get info :partial_json)))
;;                             (let* ((prev-tool-call (car (plist-get info :tool-use)))
;;                                    (prev-func (plist-get prev-tool-call :function)))
;;                               (plist-put prev-func :arguments ;update args for old tool block
;;                                          (apply #'concat (nreverse (plist-get info :partial_json)))))
;;                             (plist-put info :partial_json nil)) ;clear out finished chain of partial args
;;                           ;; Start new chain of partial argument strings
;;                           (plist-put info :partial_json (list (plist-get func :arguments)))
;;                           ;; NOTE: Do NOT use `push' for this, it prepends and we lose the reference
;;                           (plist-put info :tool-use (cons tool-call (plist-get info :tool-use))))
;;                       ;; old tool block continues, so continue collecting arguments in :partial_json
;;                       (push (plist-get func :arguments) (plist-get info :partial_json)))))
;;                 ;; Check for reasoning blocks, currently only used by Openrouter
;;                 (unless (eq (plist-get info :reasoning-block) 'done)
;;                   (if-let* ((reasoning-chunk (or (plist-get delta :reasoning) ;for Openrouter and co
;;                                                  (plist-get delta :reasoning_content))) ;for Deepseek, Llama.cpp
;;                             ((not (or (eq reasoning-chunk :null) (string-empty-p reasoning-chunk)))))
;;                       (plist-put info :reasoning
;;                                  (concat (plist-get info :reasoning) reasoning-chunk))
;;                     ;; Done with reasoning if we get non-empty content
;;                     (if-let* (((plist-member info :reasoning)) ;Is this a reasoning model?
;;                               (c (plist-get delta :content)) ;Started receiving text content?
;;                               ((not (or (eq c :null) (string-blank-p c)))))
;;                         (plist-put info :reasoning-block t)))))))) ;Signal end of reasoning block
;;       (error (goto-char (match-beginning 0))))
;;     (apply #'concat (nreverse content-strs))))

;; (cl-defmethod gptel--parse-buffer ((backend gptel-openai) &optional max-entries)
;;   (let ((prompts) (prev-pt (point)))
;;     (if (or gptel-mode gptel-track-response)
;;         (while (and (or (not max-entries) (>= max-entries 0))
;;                     (/= prev-pt (point-min))
;;                     (goto-char (previous-single-property-change
;;                                 (point) 'gptel nil (point-min))))
;;           (pcase (get-char-property (point) 'gptel)
;;             ('response
;;              (when-let* ((content (gptel--trim-prefixes
;;                                    (buffer-substring-no-properties (point) prev-pt))))
;;                (push (list :role "assistant" :content content) prompts)))
;;             (`(tool . ,id)
;;              (save-excursion
;;                (condition-case nil
;;                    (let* ((tool-call (read (current-buffer)))
;;                           (name (plist-get tool-call :name))
;;                           (arguments (decode-coding-string
;;                                       (gptel--json-encode (plist-get tool-call :args))
;;                                       'utf-8 t))
;;                           (reasoning (get-char-property (point) 'gptel-reasoning)))
;;                      (setq id (gptel--openai-format-tool-id id))
;;                      (plist-put tool-call :id id)
;;                      (plist-put tool-call :result
;;                                 (string-trim (buffer-substring-no-properties
;;                                               (point) prev-pt)))
;;                      (push (car (gptel--parse-tool-results backend (list tool-call)))
;;                            prompts)
;;                      (push `(:role "assistant"
;;                              :tool_calls
;;                              ,(vector (list :type "function"
;;                                             :id id
;;                                             :function `( :name ,name
;;                                                          :arguments ,arguments)))
;;                              ,@(when reasoning
;;                                  (list :reasoning_content reasoning)))
;;                            prompts))
;;                  ((end-of-file invalid-read-syntax)
;;                   (message (format "Could not parse tool-call %s on line %s"
;;                                    id (line-number-at-pos (point))))))))
;;             ('ignore)
;;             ('nil
;;              (and max-entries (cl-decf max-entries))
;;              (if gptel-track-media
;;                  (when-let* ((content (gptel--openai-parse-multipart
;;                                        (gptel--parse-media-links major-mode
;;                                                                  (point) prev-pt))))
;;                    (when (> (length content) 0)
;;                      (push (list :role "user" :content content) prompts)))
;;                (when-let* ((content (gptel--trim-prefixes (buffer-substring-no-properties
;;                                                            (point) prev-pt))))
;;                  (push (list :role "user" :content content) prompts)))))
;;           (setq prev-pt (point)))
;;       (let ((content (string-trim (buffer-substring-no-properties
;;                                     (point-min) (point-max)))))
;;         (push (list :role "user" :content content) prompts)))
;;     prompts))

;; (defun gptel--display-tool-results (tool-results info)
;;   "Insert TOOL-RESULTS into buffer.

;; TOOL-RESULTS is

;;  ((tool args result) ...)

;; for tool call results.  INFO contains the state of the request."
;;   (let* ((start-marker (plist-get info :position))
;;          (tool-marker (plist-get info :tool-marker))
;;          (tracking-marker (plist-get info :tracking-marker)))
;;     ;; Insert tool results
;;     (when gptel-include-tool-results
;;       (with-current-buffer (marker-buffer start-marker)
;;         (cl-loop
;;          for (tool args result) in tool-results
;;          with include-names =
;;          (mapcar #'gptel-tool-name
;;                  (cl-remove-if-not #'gptel-tool-include (plist-get info :tools)))
;;          if (or (eq gptel-include-tool-results t)
;;                 (member (gptel-tool-name tool) include-names))
;;          do (funcall
;;              (plist-get info :callback)
;;              (let* ((name (gptel-tool-name tool))
;;                     (separator        ;Separate from response prefix if required
;;                      (cond ((not tracking-marker)
;;                             (and gptel-mode
;;                                  (not (string-suffix-p
;;                                        "\n" (gptel-response-prefix-string)))
;;                                  "\n"))           ;start of response
;;                            ((not (and tool-marker ;not consecutive tool result blocks
;;                                       (= tracking-marker tool-marker)))
;;                             gptel-response-separator)))
;;                     (tool-use
;;                      ;; TODO(tool) also check args since there may be more than
;;                      ;; one call/result for the same tool
;;                      (cl-find-if
;;                       (lambda (tu) (equal (plist-get tu :name) name))
;;                       (plist-get info :tool-use)))
;;                     (id (plist-get tool-use :id))
;;                     (display-call (format "(%s %s)" name
;;                                           (string-trim (prin1-to-string args) "(" ")")))
;;                     (call (prin1-to-string `(:name ,name :args ,args)))
;;                     (truncated-call
;;                      (string-replace "\n" " "
;;                                      (truncate-string-to-width
;;                                       display-call
;;                                       (floor (* (window-width) 0.6)) 0 nil " ...)"))))
;;                (if (derived-mode-p 'org-mode)
;;                    (concat
;;                     separator
;;                     "#+begin_tool "
;;                     truncated-call
;;                     (propertize
;;                      (concat "\n" call "\n\n" (org-escape-code-in-string result))
;;                      'gptel `(tool . ,id)
;;                      'gptel-reasoning (plist-get info :reasoning))
;;                     "\n#+end_tool\n")
;;                  ;; TODO(tool) else branch is handling all front-ends as markdown.
;;                  ;; At least escape markdown.
;;                  (concat
;;                   separator
;;                   ;; TODO(tool) remove properties and strip instead of ignoring
;;                   (propertize (format "``` tool %s" truncated-call)
;;                               'gptel 'ignore 'keymap gptel--markdown-block-map)
;;                   (propertize
;;                    ;; TODO(tool) escape markdown in result
;;                    (concat "\n" call "\n\n" result)
;;                    'gptel `(tool . ,id)
;;                    'gptel-reasoning (plist-get info :reasoning))
;;                   ;; TODO(tool) remove properties and strip instead of ignoring
;;                   (propertize "\n```\n" 'gptel 'ignore
;;                               'keymap gptel--markdown-block-map))))
;;              info
;;              'raw)
;;          ;; tool-result insertion has updated the tracking marker
;;          (unless tracking-marker
;;            (setq tracking-marker (plist-get info :tracking-marker)))
;;          (if tool-marker
;;                (move-marker tool-marker tracking-marker)
;;              (setq tool-marker (copy-marker tracking-marker nil))
;;              (plist-put info :tool-marker tool-marker))
;;          (ignore-errors                 ;fold drawer
;;            (save-excursion
;;              (goto-char tracking-marker)
;;              (forward-line -1)
;;              (if (derived-mode-p 'org-mode)
;;                  (when (looking-at-p "^#\\+end_tool") (org-cycle))
;;                (when (looking-at-p "^```") (gptel-markdown-cycle-block))))))))))

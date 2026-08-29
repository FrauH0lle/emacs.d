;; tools/llm/config.el -*- lexical-binding: t; -*-

;; PATCH 2026-02-11: `gptel'
(el-patch-feature gptel)
(compile-along! "patches/gptel")
(compile-along! "patches/gptel-anthropic")
(compile-along! "patches/gptel-openai")

(defvar-local +gptel-codex-session-id nil
  "Session identifier sent to the Codex backend.")

(defun +gptel--codex-header (info)
  "Return Codex authentication headers for request INFO."
  (require 'org-id)
  (append (gptel--openai-oauth-header info)
          `(("session-id" .
             ,(with-current-buffer (plist-get info :buffer)
                (or +gptel-codex-session-id
                    (setq +gptel-codex-session-id (org-id-uuid))))))))


(use-package! gptel
  :defer t
  :hook (gptel-mode . gptel-highlight-mode)
  :config
  ;; (load! "patches/gptel")
  ;; (after! gptel-anthropic
  ;;   (load! "patches/gptel-anthropic"))
  ;; (after! gptel-openai
  ;;   (load! "patches/gptel-openai"))

  (set-debug-var! 'gptel-log-level 'debug)

  (setq
   ;; Use `org-mode' for the `gptel' buffer
   gptel-default-mode 'org-mode
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

  ;; CLaude
  (gptel-make-anthropic "Claude"
    :stream t
    :key 'gptel-api-key
    :request-params '(:thinking (:type "adaptive")))

  (gptel-make-deepseek "DeepSeek"
    :stream t
    :key 'gptel-api-key)

  ;; GLM
  (gptel-make-glm-openai "GLM-coding"
    :stream t
    :key 'gptel-api-key
    :request-params '(:thinking
                      (:type "enabled"
                       :clear_thinking :json-false)
                      :max_tokens 16384
                      :temperature 0.7))

  ;; OpenAI Codex
  (setq gptel-model 'gpt-5.6-sol
        gptel-backend (gptel-make-openai-oauth "Codex"
                        :header #'+gptel--codex-header
                        :models '((gpt-5.6-sol
                                   :description "The best model for coding and agentic tasks"
                                   :capabilities (media tool-use json url responses-api)
                                   :reasoning-effort (member none low medium high xhigh max)
                                   :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                                   :context-window 258
                                   :input-cost 5
                                   :output-cost 30
                                   :cutoff-date "2026-02")
                                  (gpt-5.6-terra
                                   :description "Faster, more cost-efficient version of GPT-5.6"
                                   :capabilities (media tool-use json url responses-api)
                                   :reasoning-effort (member none low medium high xhigh max)
                                   :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                                   :context-window 258
                                   :input-cost 5
                                   :output-cost 30
                                   :cutoff-date "2026-02")
                                  (gpt-5.6-luna
                                   :description "Fastest, cheapest version of GPT-5.6"
                                   :capabilities (media tool-use json url responses-api)
                                   :reasoning-effort (member none low medium high xhigh max)
                                   :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                                   :context-window 258
                                   :input-cost 5
                                   :output-cost 30
                                   :cutoff-date "2026-02"))))


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

  ;; Keybinds
  (map! :map gptel-mode-map
        "C-c C-n" #'+gptel/next-prompt
        "C-c C-p" #'+gptel/previous-prompt
        "C-c C-x" #'+gptel/toggle-project-prompt))


(use-package! gptel-quick
  :defer t)


(use-package! mcp
  :defer t
  :init (after! gptel (require 'mcp))
  :config
  (require 'gptel-integrations)
  (require 'mcp-hub)

  (setq! mcp-hub-servers
         `(("context7" . (:command "npx" :args ("-y" "@upstash/context7-mcp@latest")))))

  (mcp-hub-start-all-server))


(use-package! gptel-agent
  :defer t
  :init (after! gptel (require 'gptel-agent)))


(use-package! mevedel
  :defer t
  :init (after! gptel (require 'mevedel))
  :config
  (setq! mevedel-empty-tag-query-matches-all nil)
  ;; The extra indentation look awkward here
  (add-to-list '+word-wrap-text-modes 'mevedel-view-mode)

  ;; Disable `undo-tree' in view buffer
  (add-to-list 'undo-tree-incompatible-major-modes 'mevedel-view-mode)

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

  (setq! mevedel-permission-guardian t
         mevedel-permission-mode 'edits)

  (mevedel-define-preset GPT-5.6
    :description "OpenAI GPT team"
    :parents (mevedel-implement)
    :model-tiers
    ((fast :provider "Codex:gpt-5.6-luna" :effort max)
     (balanced :provider "Codex:gpt-5.6-luna" :effort max)
     (strong :provider "Codex:gpt-5.6-sol" :effort high))
    :model-workloads
    ((planning :provider "Codex:gpt-5.6-sol" :effort high)
     (goal-guardian :provider "Codex:gpt-5.6-luna" :effort max)
     (implementation :provider "Codex:gpt-5.6-luna" :effort max)
     (review :provider "Codex:gpt-5.6-sol" :effort high)))

  (mevedel-install))


(static-when (modulep! :completion vertico)
  (after! embark
    (defvar-keymap +mevedel-embark-consult-map
      :doc "Embark actions for `consult-location' and `consult-grep' targets."
      :parent embark-general-map
      "R" #'+mevedel/embark-create-reference)
    (setf (alist-get 'consult-location embark-keymap-alist) '+mevedel-embark-consult-map
          (alist-get 'consult-grep embark-keymap-alist) '+mevedel-embark-consult-map)))

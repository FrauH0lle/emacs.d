;; tools/llm/config.el -*- lexical-binding: t; -*-

;; PATCH 2026-02-11: `gptel'
(el-patch-feature gptel)
(compile-along! "patches/gptel")
(compile-along! "patches/gptel-anthropic")
(compile-along! "patches/gptel-openai")


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

  ;; REVIEW This is a bug with the OpenAI subscription backend
  (setq gptel-temperature nil)

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

  (gptel-make-deepseek "DeepSeek-4"
    :stream t
    :key 'gptel-api-key
    :request-params '(:reasoning_effort "max")
    ;; :models '((deepseek-v4-pro
    ;;            :capabilities (tool reasoning)
    ;;            :context-window 1000
    ;;            :input-cost 0.56
    ;;            :output-cost 1.68)
    ;;           (deepseek-v4-flash
    ;;            :capabilities (tool reasoning)
    ;;            :context-window 1000
    ;;            :input-cost 0.56
    ;;            :output-cost 1.68))
    )

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
  (setq gptel-model 'gpt-5.5
        gptel-backend (gptel-make-openai-oauth "Codex"
                        :models '((gpt-5.5
                                   :description "The best model for coding and agentic tasks"
                                   :capabilities (media tool-use json url responses-api)
                                   :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                                   :context-window 272
                                   :input-cost 5
                                   :output-cost 30
                                   :cutoff-date "2025-12"))
                        :request-params '(:reasoning
                                          (:effort "high"
                                           :summary "auto"))))


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


(use-package mcp
  :defer t
  :init (after! gptel (require 'mcp nil t))
  :config
  (require 'gptel-integrations)
  (require 'mcp-hub)

  (setq! mcp-hub-servers
         `(("context7" . (:command "npx" :args ("-y" "@upstash/context7-mcp@latest")))))

  (mcp-hub-start-all-server))


(use-package! gptel-agent
  :defer t
  :init (after! gptel (require 'gptel-agent nil t)))


(use-package! mevedel
  :defer t
  :init (after! gptel (require 'mevedel nil t))
  :config
  (setq! mevedel-empty-tag-query-matches-all nil)
  ;; The extra indentation look awkward here
  (add-to-list '+word-wrap-text-modes 'mevedel-view-mode)

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

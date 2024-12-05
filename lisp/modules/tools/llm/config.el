;; tools/llm/config.el -*- lexical-binding: t; -*-

(defvar +llm-aider-arg-list
  '(("DeepSeek" :args ("--deepseek") :host "api.deepseek.com" :user "apikey"
     :env "DEEPSEEK_API_KEY")
    ("Claude Sonnet" :args ("--sonnet" "--cache-prompts" "--cache-keepalive-pings 6")
     :host "api.anthropic.com" :user "apikey" :env "ANTHROPIC_API_KEY"))
  "List of arguments for different LLM configurations.")

(use-package! aider
  :defer t)

(use-package! gptel
  :defer t
  :config
  ;; CLaude Sonnet
  (gptel-make-anthropic "Claude Sonnet"
    :stream t
    :key (gptel-api-key-from-auth-source "api.anthropic.com" "apikey"))
  ;; DeepSeek
  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key (gptel-api-key-from-auth-source "api.deepseek.com" "apikey")
    :models '(deepseek-chat deepseek-coder)))

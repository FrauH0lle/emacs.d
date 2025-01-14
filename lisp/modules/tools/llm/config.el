;; tools/llm/config.el -*- lexical-binding: t; -*-

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

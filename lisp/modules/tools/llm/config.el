;; tools/llm/config.el -*- lexical-binding: t; -*-

(defvar +llm-aider-arg-list
  '(("DeepSeek" :args ("--deepseek") :host "api.deepseek.com" :user "apikey"
     :env "DEEPSEEK_API_KEY")
    ("Claude Sonnet" :args ("--sonnet" "--cache-prompts" "--cache-keepalive-pings" "6")
     :host "api.anthropic.com" :user "apikey" :env "ANTHROPIC_API_KEY"))
  "List of arguments for different LLM configurations.")

(use-package! gptel
  :defer t
  :config
  ;; CLaude Sonnet
  (gptel-make-anthropic "Claude"
    :stream t
    :key (gptel-api-key-from-auth-source "api.anthropic.com" "apikey"))
  ;; DeepSeek
  (gptel-make-deepseek "DeepSeek"
    :stream t
    :key (gptel-api-key-from-auth-source "api.deepseek.com" "apikey"))

  (setq gptel-display-buffer-action nil)  ; if user changes this, popup manager will bow out
  (set-popup-rule!
    (lambda (bname &optional _action)
      (and (null gptel-display-buffer-action)
           (buffer-local-value 'gptel-mode (get-buffer bname))))
    :select t
    :size 0.3
    :quit nil
    :ttl nil))


(use-package! gptel-quick
  :defer t)


(use-package! gptel-magit
  :when (modulep! :tools magit)
  :hook (magit-mode . gptel-magit-install))


(use-package! aidermacs
  :defer t
  :config
  ;; Enable minor mode for prompt files
  (aidermacs-setup-minor-mode)
  ;; Setup API keys
  (if-let* ((key (+llm-api-key-from-auth-source "api.anthropic.com" "apikey")))
      (setenv "ANTHROPIC_API_KEY" key))
  (if-let* ((key (+llm-api-key-from-auth-source "api.deepseek.com" "apikey")))
      (setenv "DEEPSEEK_API_KEY" key))

  (setq! aidermacs-use-architect-mode t
         aidermacs-default-model "sonnet"
         aidermacs-architect-model "r1")
  (setq aidermacs-extra-args '("--cache-prompts" "--cache-keepalive-pings" "6")))

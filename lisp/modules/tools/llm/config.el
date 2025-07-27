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


(use-package! gptel
  :defer t
  :config
  (setq
   ;; Use `org-mode' for the `gptel' buffer
   gptel-default-mode 'org-mode
   ;; Always include tool output
   gptel-include-tool-results t)
  ;; Prettier prompts
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*Prompt*: "
        (alist-get 'org-mode gptel-response-prefix-alist) "*Response*:\n")

  ;; Each org heading is its own conversation
  (after! 'gptel-org
    (setq-default gptel-org-branching-context t))

  ;; Add `major-mode' -> language mapping
  (eval-when! (modulep! :lang ess)
    (add-to-list 'gptel--mode-description-alist '(ess-r-mode . "R")))

  ;; Keep previous system message in `+gptel--prev-system-message'
  (advice-add #'gptel-system-prompt :around #'+gptel-prev-system-message-a)

  ;; CLaude Sonnet
  (gptel-make-anthropic "Claude"
    :stream t
    :key (gptel-api-key-from-auth-source "api.anthropic.com" "apikey"))
  ;; DeepSeek (as default)
  (setq gptel-model 'deepseek-chat
        gptel-backend (gptel-make-deepseek "DeepSeek"
                        :stream t
                        :key (gptel-api-key-from-auth-source "api.deepseek.com" "apikey")))

  (setq gptel-display-buffer-action nil)  ; if user changes this, popup manager will bow out
  (set-popup-rule!
    (lambda (bname &optional _action)
      (and (null gptel-display-buffer-action)
           (buffer-local-value 'gptel-mode (get-buffer bname))))
    :select t
    :size 0.3
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
    :system (lambda ()
              (let* ((project-root (zenit-project-root))
                     (commit-summary-files '("commit-summary.md" "docs/commit-summary.md"))
                     (found-file nil))
                (dolist (file commit-summary-files)
                  (let ((full-path (expand-file-name file project-root)))
                    (when (and (not found-file) (file-exists-p full-path))
                      (setq found-file full-path))))
                (if found-file
                    (with-temp-buffer
                      (zenit-file-read found-file :by 'insert)
                      (buffer-string))
                  (let* ((files commit-summary-files)
                         (formatted-string
                          ;; If there are fewer than 2 files, just use the file
                          ;; name itself.
                          (if (< (length files) 2)
                              (car files)
                            ;; Otherwise, join all but the last with ", " and
                            ;; add the last with " or ".
                            (concat (string-join (butlast files) ", ") " or " (car (last files))))))

                    (user-error "Could not find any of %s in project : %s" formatted-string project-root)))))
    :backend "Claude"
    :model 'claude-3-5-haiku-20241022
    :include-reasoning nil
    :tools nil))


;; PATCH 2025-07-21: `gptel-mcp-connect'
(el-patch-feature gptel-integrations)
(compile-along! "patches")
(after! gptel-integrations
  (load! "patches"))


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
                 (gptel-mcp-connect '("memory-bank") nil nil t)
                 (setq +gptel--memory-mcp-last-root-dir root-dir))))))

  (mcp-hub-start-all-server))


(use-package! macher
  :after gptel
  :config
  (setq! macher-action-buffer-ui 'org)
  (after! gptel
    (macher-install))

  (set-popup-rule!
    "\\*macher:.*\\*"
    :select t
    :size 0.3
    :quit nil
    :ttl nil))





;; (defalias 'my/gptel-easy-page
;;     (let ((map (make-composed-keymap
;;                 (define-keymap
;;                   "RET" 'gptel-end-of-response
;;                   "n"   'gptel-end-of-response
;;                   "p"   'gptel-beginning-of-response)
;;                 my-pager-map))
;;           (scrolling
;;            (propertize  "SCRL" 'face '(:inherit highlight))))
;;       (require 'pixel-scroll)
;;       (lambda ()
;;         (interactive)
;;         (when (eq (window-buffer (selected-window))
;;                   (current-buffer))
;;           (add-to-list 'mode-line-format scrolling)
;;           (set-transient-map
;;            map t
;;            (lambda () (setq mode-line-format
;;                        (delete scrolling mode-line-format))))))))

;;   (defvar-keymap my-pager-map
;;     :doc "Keymap with paging commands"
;;     "SPC" 'scroll-up-command
;;     "C-l" 'recenter-top-bottom
;;     "C-M-v" 'scroll-other-window
;;     "C-M-S-v" 'scroll-other-window-down
;;     "d" (lambda ()
;;           (interactive)
;;           (pixel-scroll-precision-interpolate
;;            (- (floor (window-text-height nil t) 2))
;;            nil 1))

;;     "u" (lambda ()
;;           (interactive)
;;           (pixel-scroll-precision-interpolate
;;            (floor (window-text-height nil t) 2)
;;            nil 1))
;;     "M-o" (if (fboundp 'switchy-window-minor-mode)
;;               'switchy-window 'my/other-window)
;;     "S-SPC" 'scroll-down-command)

;;   (let ((scrolling (propertize  "SCRL" 'face '(:inherit highlight)))
;;         ml-buffer)
;;     (defalias 'my/easy-page
;;       (lambda ()
;;         (interactive)
;;         (when (eq (window-buffer (selected-window))
;;                   (current-buffer))
;;           (setq ml-buffer (current-buffer))
;;           (add-to-list 'mode-line-format scrolling)
;;           (set-transient-map
;;            my-pager-map t
;;            (lambda () (with-current-buffer ml-buffer
;;                    (setq mode-line-format
;;                          (delete scrolling mode-line-format)))))))))

;;   (defun my/gptel-previous-prompt ()
;;     (interactive)
;;     (goto-char (line-beginning-position))
;;     (when (re-search-backward
;;            (regexp-quote (gptel-prompt-prefix-string)) nil t)
;;       (goto-char (line-end-position))))

;;   (defun my/gptel-next-prompt (&rest _ignore)
;;     (interactive)
;;     (goto-char (line-end-position))
;;     (when (re-search-forward
;;            (regexp-quote (gptel-prompt-prefix-string)) nil t)
;;       (goto-char (line-end-position))))

;; tools/llm/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :defer t
  :hook (gptel-mode . gptel-highlight-mode)
  :config
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
         `(("context7" . (:command "npx" :args ("-y" "@upstash/context7-mcp@latest"))))))


(use-package! mevedel
  :defer t
  :init (after! gptel (require 'mevedel))
  :config
  (setq! mevedel-empty-tag-query-matches-all nil
         mevedel-permission-reviewer 'auto
         mevedel-permission-mode 'edits
         mevedel-memory-consolidation-mode 'auto
         mevedel-claude-code-directory (file-name-concat zenit-data-dir "mevedel" "claude-code")
         mevedel-sleep-inhibit t)

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

    (mevedel-install))


(static-when (modulep! :completion vertico)
  (after! embark
    (defvar-keymap +mevedel-embark-consult-map
      :doc "Embark actions for `consult-location' and `consult-grep' targets."
      :parent embark-general-map
      "R" #'+mevedel/embark-create-reference)
    (setf (alist-get 'consult-location embark-keymap-alist) '+mevedel-embark-consult-map
          (alist-get 'consult-grep embark-keymap-alist) '+mevedel-embark-consult-map)))

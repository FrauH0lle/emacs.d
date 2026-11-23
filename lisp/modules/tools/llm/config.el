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


(use-package! niwa
  :defer t
  :init (after! gptel (require 'niwa))
  :config
  (setq! niwa-permission-reviewer 'auto
         niwa-permission-mode 'edits
         niwa-memory-consolidation-mode 'auto
         niwa-claude-code-directory (file-name-concat zenit-data-dir "niwa" "claude-code")
         niwa-sleep-inhibit t)

  ;; The extra indentation look awkward here
  (add-to-list '+word-wrap-text-modes 'niwa-view-mode)

  ;; `niwa' shows its status strip in the tab line. Emacs formats the mode
  ;; line inside every pixel-position query, and the view makes many of them
  ;; while a request runs: `doom-modeline' added ~1 ms and ~95 KB to each. The
  ;; modeline does not add much here so we deactivate it.
  (setq-hook! 'niwa-composer-mode-local-vars-hook mode-line-format nil)

  ;; Disable `undo-tree' in view buffer
  (add-to-list 'undo-tree-incompatible-major-modes 'niwa-view-mode)

  ;; Chat windows are dedicated, so zenit's `kill-current-buffer' advice would
  ;; only delete the window (niwa then buries the chat). Let the kill go through.
  (defadvice! +niwa--kill-chat-buffer-a (&rest _)
    :before-while #'zenit--switch-to-fallback-buffer-maybe-a
    (not (window-parameter nil 'niwa-view-pair--role)))

  ;; When `evil' is used, bind RET in normal mode to open the overlay menu
  (static-when (modulep! :editor evil)
    (setq! niwa-ov-dispatch-key nil)
    (dolist (map niwa--actions-maps)
      (keymap-set (symbol-value map)
                  "<return>"
                  `(menu-item "" niwa--ov-actions-dispatch
                    :filter ,(lambda (cmd) (when (evil-normal-state-p) cmd))))
      (keymap-set (symbol-value map)
                  "RET"
                  `(menu-item "" niwa--ov-actions-dispatch
                    :filter ,(lambda (cmd) (when (evil-normal-state-p) cmd))))))

  (niwa-install)

  (static-when (modulep! :editor evil)
    (niwa-evil-setup)))


(static-when (modulep! :completion vertico)
  (after! embark
    (defvar-keymap +niwa-embark-consult-map
      :doc "Embark actions for `consult-location' and `consult-grep' targets."
      :parent embark-general-map
      "R" #'+niwa/embark-create-reference)
    (setf (alist-get 'consult-location embark-keymap-alist) '+niwa-embark-consult-map
          (alist-get 'consult-grep embark-keymap-alist) '+niwa-embark-consult-map)))

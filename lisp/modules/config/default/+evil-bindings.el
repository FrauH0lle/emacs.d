;; config/default/+bindings.el -*- lexical-binding: t; -*-

(defvar +default-minibuffer-maps
  (append '(minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map
            read-expression-map))
  "A list of all the keymaps used for the minibuffer.")

(eval-when! (modulep! :editor evil)
  ;; NOTE SPC u replaces C-u as the universal argument.

  ;; Minibuffer
  (map! :map (evil-ex-completion-map evil-ex-search-keymap)
        "C-a" #'evil-beginning-of-line
        "C-b" #'evil-backward-char
        "C-f" #'evil-forward-char
        :gi "C-j" #'next-complete-history-element
        :gi "C-k" #'previous-complete-history-element)

  (define-key! :keymaps +default-minibuffer-maps
    [escape] #'abort-recursive-edit
    "C-a"    #'move-beginning-of-line
    "C-r"    #'evil-paste-from-register
    "C-u"    #'evil-delete-back-to-indentation
    "C-v"    #'yank
    "C-w"    #'zenit/delete-backward-word
    "C-z"    (cmd! (ignore-errors (call-interactively #'undo))))

  (define-key! :keymaps +default-minibuffer-maps
    "C-j"    #'next-line
    "C-k"    #'previous-line
    "C-S-j"  #'scroll-up-command
    "C-S-k"  #'scroll-down-command)
  ;; For folks with `evil-collection-setup-minibuffer' enabled
  (define-key! :states 'insert :keymaps +default-minibuffer-maps
    "C-j"    #'next-line
    "C-k"    #'previous-line)
  (define-key! read-expression-map
    "C-j" #'next-line-or-history-element
    "C-k" #'previous-line-or-history-element))


;;
;;; Global keybindings

;; Smart tab, these will only work in GUI Emacs
(map! :i [tab]
      `(menu-item "Evil insert smart tab" nil :filter
        (lambda (cmd)
          (cond
           ((or (zenit-lookup-key [tab] overriding-terminal-local-map)
                (zenit-lookup-key (kbd "TAB") overriding-terminal-local-map))
            cmd)
           ,@(when (modulep! :completion corfu)
               '(((and (bound-and-true-p corfu-mode)
                       corfu--candidates)
                  (if (derived-mode-p 'eshell-mode 'comint-mode)
                      #'completion-at-point
                    #'indent-for-tab-command)))))))
      :m [tab] `(menu-item "Evil motion smart tab" nil :filter
                 (lambda (cmd)
                   (cond
                    ((or (zenit-lookup-key [tab] overriding-terminal-local-map)
                         (zenit-lookup-key (kbd "TAB") overriding-terminal-local-map))
                     cmd)
                    ,@(when (modulep! :editor snippets)
                        '(((and (evil-visual-state-p)
                                (or (eq evil-visual-selection 'line)
                                    (not (memq (char-after)
                                               (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                           #'tempel-insert)))
                    ,@(when (modulep! :editor fold)
                        '(((or
                            (save-excursion (end-of-line) (invisible-p (point)))
                            (+fold--vimish-fold-p)
                            (+fold--outline-fold-p))
                           #'+fold/toggle)))
                    ;; Without this, this tab keybind overrides mode-local ones
                    ;; for modes that don't have an evil keybinding scheme or if
                    ;; :editor evil is notenabled.
                    ((or (zenit-lookup-key
                          [tab]
                          (list (evil-get-auxiliary-keymap (current-local-map)
                                                           evil-state)
                                (current-local-map)))
                         (zenit-lookup-key
                          (kbd "TAB")
                          (list (evil-get-auxiliary-keymap (current-local-map)
                                                           evil-state)))
                         (zenit-lookup-key (kbd "TAB") (list (current-local-map))))
                     cmd)
                    ((fboundp 'evil-jump-item)
                     #'evil-jump-item))))

      ;; Extend smart tab for specific modes. This way, we process the entire
      ;; smart tab logic and only fall back to these commands at the end.
      (:when (modulep! :emacs org)
       (:after org :map org-mode-map
        [remap indent-for-tab-command]
        `(menu-item "Go to the next field" org-table-next-field
          :filter ,(lambda (cmd) (when (org-at-table-p) cmd)))))

      (:after help :map help-mode-map
       :n "o"       #'link-hint-open-link)
      (:after info :map Info-mode-map
       :n "o"       #'link-hint-open-link)
      (:after apropos :map apropos-mode-map
       :n "o"       #'link-hint-open-link
       :n "TAB"     #'forward-button
       :n [tab]     #'forward-button
       :n [backtab] #'backward-button)
      (:after view :map view-mode-map
              [escape]  #'View-quit-all)
      (:after man :map Man-mode-map
       :n "q"    #'kill-current-buffer)
      (:after geiser-doc :map geiser-doc-mode-map
       :n "o"    #'link-hint-open-link)

      (:after (evil-org evil-easymotion)
       :map evil-org-mode-map
       :m "gsh" #'+org/goto-visible)

      (:when (modulep! :editor multiple-cursors)
        :prefix ("gz" . "multiple-cursors")
        :nv "d" #'evil-mc-make-and-goto-next-match
        :nv "D" #'evil-mc-make-and-goto-prev-match
        :nv "s" #'evil-mc-skip-and-goto-next-match
        :nv "S" #'evil-mc-skip-and-goto-prev-match
        :nv "c" #'evil-mc-skip-and-goto-next-cursor
        :nv "C" #'evil-mc-skip-and-goto-prev-cursor
        :nv "j" #'evil-mc-make-cursor-move-next-line
        :nv "k" #'evil-mc-make-cursor-move-prev-line
        :nv "m" #'evil-mc-make-all-cursors
        :nv "n" #'evil-mc-make-and-goto-next-cursor
        :nv "N" #'evil-mc-make-and-goto-last-cursor
        :nv "p" #'evil-mc-make-and-goto-prev-cursor
        :nv "P" #'evil-mc-make-and-goto-first-cursor
        :nv "q" #'evil-mc-undo-all-cursors
        :nv "t" #'+multiple-cursors/evil-mc-toggle-cursors
        :nv "u" #'+multiple-cursors/evil-mc-undo-cursor
        :nv "z" #'+multiple-cursors/evil-mc-toggle-cursor-here
        :v  "I" #'evil-mc-make-cursor-in-visual-selection-beg
        :v  "A" #'evil-mc-make-cursor-in-visual-selection-end)

      ;; misc
      :n "C-S-f"  #'toggle-frame-fullscreen
      :n "C-="    #'zenit/reset-font-size
      ;; Frame-local font resizing
      :n "C-+"    #'zenit/increase-font-size
      :n "C--"    #'zenit/decrease-font-size)


;;
;;; Module keybinds
;;; :completion

(map! (:when (modulep! :completion corfu)
        (:after corfu
                (:map corfu-mode-map
                 :i "C-." #'completion-at-point
                 :i "C-n" #'+corfu/dabbrev-or-next
                 :i "C-p" #'+corfu/dabbrev-or-last
                 :n "C-." (cmd! (call-interactively #'evil-insert-state)
                                (call-interactively #'completion-at-point))
                 :v "C-." (cmd! (call-interactively #'evil-change)
                                (call-interactively #'completion-at-point)))
                (:map corfu-map
                 :i "C-SPC" #'corfu-insert-separator
                 "C-k" #'corfu-previous
                 "C-j" #'corfu-next
                 "C-u" (cmd! (let (corfu-cycle)
                               (funcall-interactively #'corfu-next (- corfu-count))))
                 "C-d" (cmd! (let (corfu-cycle)
                               (funcall-interactively #'corfu-next corfu-count)))))
        (:after corfu-popupinfo
         :map corfu-popupinfo-map
         "C-h"      #'corfu-popupinfo-toggle
         ;; Reversed because popupinfo assumes opposite of what feels intuitive
         ;; with evil.
         "C-S-k"    #'corfu-popupinfo-scroll-down
         "C-S-j"    #'corfu-popupinfo-scroll-up
         "C-<up>"   #'corfu-popupinfo-scroll-down
         "C-<down>" #'corfu-popupinfo-scroll-up
         "C-S-p"    #'corfu-popupinfo-scroll-down
         "C-S-n"    #'corfu-popupinfo-scroll-up
         "C-S-u"    (cmd!! #'corfu-popupinfo-scroll-down nil corfu-popupinfo-min-height)
         "C-S-d"    (cmd!! #'corfu-popupinfo-scroll-up nil corfu-popupinfo-min-height))))


;;; :ui
(map! (:when (modulep! :ui popup)
        "C-`"   #'+popup/toggle
        "C-~"   #'+popup/toggle-type
        "C-x p" #'+popup/other)

      (:when (modulep! :ui transient-state)
        :m "*" #'+symbol-overlay-transient-state
        :m "#" #'+symbol-overlay-transient-state)

      (:when (modulep! :ui workspaces)
        :n "C-t"   #'+workspace/new
        :n "C-S-t" #'+workspace/display
        :g "M-1"   #'+workspace/switch-to-0
        :g "M-2"   #'+workspace/switch-to-1
        :g "M-3"   #'+workspace/switch-to-2
        :g "M-4"   #'+workspace/switch-to-3
        :g "M-5"   #'+workspace/switch-to-4
        :g "M-6"   #'+workspace/switch-to-5
        :g "M-7"   #'+workspace/switch-to-6
        :g "M-8"   #'+workspace/switch-to-7
        :g "M-9"   #'+workspace/switch-to-8
        :g "M-0"   #'+workspace/switch-to-final
        (:when zenit--system-macos-p
          :g "s-t"   #'+workspace/new
          :g "s-T"   #'+workspace/display
          :n "s-1"   #'+workspace/switch-to-0
          :n "s-2"   #'+workspace/switch-to-1
          :n "s-3"   #'+workspace/switch-to-2
          :n "s-4"   #'+workspace/switch-to-3
          :n "s-5"   #'+workspace/switch-to-4
          :n "s-6"   #'+workspace/switch-to-5
          :n "s-7"   #'+workspace/switch-to-6
          :n "s-8"   #'+workspace/switch-to-7
          :n "s-9"   #'+workspace/switch-to-8
          :n "s-0"   #'+workspace/switch-to-final)))

;;; :editor
(map! (:when (modulep! :editor format)
        :n "gQ" #'+format:region)

      (:when (modulep! :editor rotate-text)
        :n "]r"  #'rotate-text
        :n "[r"  #'rotate-text-backward)

      (:when (modulep! :editor multiple-cursors)
        ;; evil-multiedit
        :v  "R"     #'evil-multiedit-match-all
        :n  "M-d"   #'evil-multiedit-match-symbol-and-next
        :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
        :v  "M-d"   #'evil-multiedit-match-and-next
        :v  "M-D"   #'evil-multiedit-match-and-prev
        :nv "C-M-d" #'evil-multiedit-restore
        (:after evil-multiedit
                (:map evil-multiedit-mode-map
                 :nv "M-d" #'evil-multiedit-match-and-next
                 :nv "M-D" #'evil-multiedit-match-and-prev
                 [return]  #'evil-multiedit-toggle-or-restrict-region))))

;;; :tools
(when (modulep! :tools eval)
  (map! "M-r" #'+eval/buffer))


;;
;;; <leader>

;; HACK 2024-08-02: The declaration and autoload are necessary because
;;   `general-simulate-key' calls `evil-set-command-property' wrapped in an
;;   `eval-after-load'. Well, the byte-compiler does not like that, even though
;;   my understanding is that code within `eval-after-load' does not get
;;   compiled ...
(declare-function evil-set-command-property "evil-common")
(autoload #'evil-set-command-property "evil-common" nil t)
;; Make "," in normal and visual mode the localleader key
(map! :nv "," (general-simulate-key "SPC m"))

(map! :leader
      :desc "Eval expression"       ":"    #'pp-eval-expression
      :desc "M-x"                   "SPC"    #'execute-extended-command
      :desc "Pop up scratch buffer" "x"    #'zenit/open-scratch-buffer
      :desc "Org Capture"           "X"    #'org-capture
      ;; C-u is used by evil
      :desc "Universal argument"    "u"    #'universal-argument
      (:after evil
       :desc "window"                "w"    evil-window-map)
      :desc "help"                  "h"    help-map

      (:when (modulep! :ui popup)
        :desc "Toggle last popup"     "~"    #'+popup/toggle)
      :desc "Find file"             "."    #'find-file
      :desc "Switch buffer"         ","    #'switch-to-buffer
      :desc "Switch to last buffer" "`"    #'evil-switch-to-windows-last-buffer
      :desc "Resume last search"    "'"
      (cond ((modulep! :completion vertico)    #'vertico-repeat))

      :desc "Search for symbol in project" "*" #'+default/search-project-for-symbol-at-point
      :desc "Search project"               "/" #'+default/search-project

      :desc "Find file in project"  ";"  #'projectile-find-file
      :desc "Jump to bookmark"      "RET"  #'bookmark-jump

      ;;; <leader> TAB --- workspace
      (:when (modulep! :ui workspaces)
        (:prefix-map ("TAB" . "workspace")
         :desc "Display tab bar"           "TAB" #'+workspace/display
         :desc "Switch workspace"          "."   #'+workspace/switch-to
         :desc "Switch to last workspace"  "`"   #'+workspace/other
         :desc "Move workspace"            "m"   #'+workspace/move
         :desc "New workspace"             "n"   #'+workspace/new
         :desc "New named workspace"       "N"   #'+workspace/new-named
         :desc "New workspace in o/frame"  "f"   #'+workspace/new-other-frame
         :desc "Load workspace from file"  "l"   #'+workspace/load
         :desc "Save workspace to file"    "s"   #'+workspace/save
         :desc "Delete session"            "x"   #'+workspace/kill-session
         :desc "Kill this workspace"       "d"   #'+workspace/kill
         :desc "Rename workspace"          "r"   #'+workspace/rename
         :desc "Restore last session"      "R"   #'+workspace/restore-last-session
         :desc "Next workspace"            "]"   #'+workspace/switch-right
         :desc "Previous workspace"        "["   #'+workspace/switch-left
         :desc "Switch to 1st workspace"   "1"   #'+workspace/switch-to-0
         :desc "Switch to 2nd workspace"   "2"   #'+workspace/switch-to-1
         :desc "Switch to 3rd workspace"   "3"   #'+workspace/switch-to-2
         :desc "Switch to 4th workspace"   "4"   #'+workspace/switch-to-3
         :desc "Switch to 5th workspace"   "5"   #'+workspace/switch-to-4
         :desc "Switch to 6th workspace"   "6"   #'+workspace/switch-to-5
         :desc "Switch to 7th workspace"   "7"   #'+workspace/switch-to-6
         :desc "Switch to 8th workspace"   "8"   #'+workspace/switch-to-7
         :desc "Switch to 9th workspace"   "9"   #'+workspace/switch-to-8
         :desc "Switch to final workspace" "0"   #'+workspace/switch-to-final))

      ;;; <leader> b --- buffer
      (:prefix-map ("b" . "buffer")
       :desc "Toggle narrowing"            "-"   #'zenit/toggle-narrow-buffer
       :desc "Previous buffer"             "["   #'previous-buffer
       :desc "Next buffer"                 "]"   #'next-buffer
       ;; REVIEW 2025-04-06: Check these commands
       ;; (:when (modulep! :ui workspaces)
       ;;   :desc "Switch workspace buffer" "b" #'persp-switch-to-buffer
       ;;   :desc "Switch buffer"           "B" #'switch-to-buffer
       ;;   :desc "ibuffer workspace"       "I" #'+ibuffer/open-for-current-workspace)
       :desc "Switch buffer"               "b"   #'switch-to-buffer
       (:unless (modulep! :ui workspaces)
         :desc "Switch buffer"           "b" #'switch-to-buffer)
       :desc "Clone buffer"                "c"   #'clone-indirect-buffer
       :desc "Clone buffer other window"   "C"   #'clone-indirect-buffer-other-window
       :desc "Kill buffer"                 "d"   #'kill-current-buffer
       :desc "ibuffer"                     "i"   #'ibuffer
       :desc "Kill buffer"                 "k"   #'kill-current-buffer
       :desc "Kill all buffers"            "K"   #'zenit/kill-all-buffers
       :desc "Switch to last buffer"       "l"   #'evil-switch-to-windows-last-buffer
       :desc "Set bookmark"                "m"   #'bookmark-set
       :desc "Delete bookmark"             "M"   #'bookmark-delete
       :desc "Next buffer"                 "n"   #'next-buffer
       :desc "New empty buffer"            "N"   #'evil-buffer-new
       :desc "Kill other buffers"          "O"   #'zenit/kill-other-buffers
       :desc "Previous buffer"             "p"   #'previous-buffer
       :desc "Revert buffer"               "r"   #'revert-buffer
       :desc "Rename buffer"               "R"   #'rename-buffer
       :desc "Save buffer"                 "s"   #'basic-save-buffer
       :desc "Save all buffers"            "S"   #'evil-write-all
       :desc "Save buffer as root"         "u"   #'zenit/sudo-save-buffer
       :desc "Pop up scratch buffer"       "x"   #'zenit/open-scratch-buffer
       :desc "Switch to scratch buffer"    "X"   #'zenit/switch-to-scratch-buffer
       :desc "Yank buffer"                 "y"   #'+default/yank-buffer-contents
       :desc "Bury buffer"                 "z"   #'bury-buffer
       :desc "Kill buried buffers"         "Z"   #'zenit/kill-buried-buffers)

      ;;; <leader> c --- code
      (:prefix-map ("c" . "code")
                   (:when (modulep! :tools lsp -eglot)
                     :desc "LSP Execute code action" "a" #'lsp-execute-code-action
                     :desc "LSP Organize imports" "o" #'lsp-organize-imports
                     (:when (modulep! :completion vertico)
                       :desc "Jump to symbol in current workspace" "j"   #'consult-lsp-symbols
                       :desc "Jump to symbol in any workspace"     "J"   (cmd!! #'consult-lsp-symbols 'all-workspaces))
                     (:when (modulep! :ui treemacs +lsp)
                       :desc "Errors list"                         "X"   #'lsp-treemacs-errors-list
                       :desc "Incoming call hierarchy"             "y"   #'lsp-treemacs-call-hierarchy
                       :desc "Outgoing call hierarchy"             "Y"   (cmd!! #'lsp-treemacs-call-hierarchy t)
                       :desc "References tree"                     "R"   (cmd!! #'lsp-treemacs-references t)
                       :desc "Symbols"                             "S"   #'lsp-treemacs-symbols)
                     :desc "LSP"                                 "l"   #'+default/lsp-command-map
                     :desc "LSP Rename"                          "r"   #'lsp-rename)
                   (:when (modulep! :tools lsp +eglot)
                     :desc "LSP Execute code action" "a" #'eglot-code-actions
                     :desc "LSP Rename" "r" #'eglot-rename
                     :desc "LSP Find declaration"                 "j"   #'eglot-find-declaration
                     (:when (modulep! :completion vertico)
                       :desc "Jump to symbol in current workspace" "j"   #'consult-eglot-symbols))

                   :desc "Compile"                               "c"   #'compile
                   :desc "Recompile"                             "C"   #'recompile
                   :desc "Jump to definition"                    "d"   #'+lookup/definition
                   :desc "Jump to references"                    "D"   #'+lookup/references
                   :desc "Evaluate buffer/region"                "e"   #'+eval/buffer-or-region
                   :desc "Evaluate & replace region"             "E"   #'+eval:replace-region
                   :desc "Format buffer/region"                  "f"   #'+format/region-or-buffer
                   :desc "Find implementations"                  "i"   #'+lookup/implementations
                   :desc "Jump to documentation"                 "k"   #'+lookup/documentation
                   :desc "Send to repl"                          "s"   #'+eval/send-region-to-repl
                   ;; :desc "Find type definition"                  "t"   #'+lookup/type-definition
                   (:prefix ("t" . "text")
                            "f" #'copy-as-format
                            "a" #'copy-as-format-asciidoc
                            "b" #'copy-as-format-bitbucket
                            "d" #'copy-as-format-disqus
                            "g" #'copy-as-format-github
                            "l" #'copy-as-format-gitlab
                            "c" #'copy-as-format-hipchat
                            "h" #'copy-as-format-html
                            "j" #'copy-as-format-jira
                            "m" #'copy-as-format-markdown
                            "w" #'copy-as-format-mediawiki
                            "o" #'copy-as-format-org-mode
                            "p" #'copy-as-format-pod
                            "r" #'copy-as-format-rst
                            "s" #'copy-as-format-slack)
                   :desc "Delete trailing whitespace"            "w"   #'delete-trailing-whitespace
                   :desc "Delete trailing newlines"              "W"   #'zenit/delete-trailing-newlines
                   :desc "List errors"                           "x"   #'+default/diagnostics)

      ;;; <leader> f --- file
      (:prefix-map ("f" . "file")
       :desc "Open project editorconfig"   "c"   #'editorconfig-find-current-editorconfig
       :desc "Copy this file"              "C"   #'zenit/copy-this-file
       :desc "Find directory"              "d"   #'+default/dired
       :desc "Delete this file"            "D"   #'zenit/delete-this-file
       (:prefix ("e" . "emacs config")
        :desc "Find file in emacs.d"        "e"   #'zenit/find-in-emacsd
        :desc "Browse emacs.d"              "E"   #'zenit/browse-emacsd
        :desc "Open .emacs.d git repo"      "g"   #'zenit/open-in-magit
        :desc "Find file in local config"   "l"   #'zenit/find-file-in-local-config
        :desc "Browse local config"         "L"   #'zenit/open-local-config
        :desc "Open local config file"      "c"   #'zenit/goto-local-config-file
        :desc "Open local init file"        "i"   #'zenit/goto-local-init-file
        :desc "Open local package file"     "p"   #'zenit/goto-local-packages-file
        :desc "Search in .emacs.d"          "s"   #'zenit/search-in-emacsd
        (:prefix ("m" . "maintanance")
         :desc "Search versions"       "."   #'zenit/search-versions
         :desc "Module info"           "l"   #'zenit/help-module-locate
         :desc "Module info"           "i"   #'zenit/help-module-info
         :desc "Update package"        "p"   #'zenit/update-package
         :desc "Update all packages"   "P"   #'zenit/update-all-packages))
       :desc "Find file"                   "f"   #'find-file
       :desc "Find file from here"         "F"   #'+default/find-file-under-here
       :desc "Locate file"                 "l"   #'locate
       :desc "Find file in private config" "p"   #'zenit/find-file-in-private-config
       :desc "Browse private config"       "P"   #'zenit/open-private-config
       :desc "Recent files"                "r"   #'recentf-open-files
       :desc "Rename/move file"            "R"   #'zenit/move-this-file
       :desc "Save file"                   "s"   #'save-buffer
       :desc "Save file as..."             "S"   #'write-file
       :desc "Sudo find file"              "u"   #'zenit/sudo-find-file
       :desc "Sudo this file"              "U"   #'zenit/sudo-this-file
       :desc "Yank file path"              "y"   #'+default/yank-buffer-path
       :desc "Yank file path from project" "Y"   #'+default/yank-buffer-path-relative-to-project)

      ;;; <leader> g --- git/version control
      (:prefix-map ("g" . "git")
       :desc "Revert file"                 "R"   #'vc-revert
       :desc "Copy link to remote"         "y"   #'+vc/browse-at-remote-kill
       :desc "Copy link to homepage"       "Y"   #'+vc/browse-at-remote-kill-homepage
       (:when (modulep! :ui vc-gutter)
         (:when (modulep! :ui hydra)
           :desc "VCGutter"                "."   #'+vc/gutter-hydra/body)
         :desc "Revert hunk at point"      "r"   #'+vc-gutter/save-and-revert-hunk
         :desc "stage hunk at point"       "s"   #'+vc-gutter/stage-hunk
         :desc "Git time machine"          "t"   #'git-timemachine-toggle
         :desc "Jump to next hunk"         "]"   #'+vc-gutter/next-hunk
         :desc "Jump to previous hunk"     "["   #'+vc-gutter/previous-hunk)
       (:when (modulep! :tools magit)
         :desc "Magit dispatch"            "/"   #'magit-dispatch
         :desc "Magit file dispatch"       "."   #'magit-file-dispatch
         :desc "Forge dispatch"            "'"   #'forge-dispatch
         :desc "Magit switch branch"       "b"   #'magit-branch-checkout
         :desc "Magit status"              "g"   #'magit-status
         :desc "Magit status here"         "G"   #'magit-status-here
         :desc "Magit file delete"         "D"   #'magit-file-delete
         :desc "Magit blame"               "B"   #'magit-blame-addition
         :desc "Magit clone"               "C"   #'magit-clone
         :desc "Magit fetch"               "F"   #'magit-fetch
         :desc "Magit buffer log"          "L"   #'magit-log-buffer-file
         :desc "Git stage this file"       "S"   #'magit-file-stage
         :desc "Git unstage this file"     "U"   #'magit-file-unstage
         (:prefix ("f" . "find")
          :desc "Find file"                 "f"   #'magit-find-file
          :desc "Find gitconfig file"       "g"   #'magit-find-git-config-file
          :desc "Find commit"               "c"   #'magit-show-commit
          :desc "Find issue"                "i"   #'forge-visit-issue
          :desc "Find pull request"         "p"   #'forge-visit-pullreq)
         (:prefix ("o" . "open in browser")
          :desc "Browse file or region"     "o"   #'+vc/browse-at-remote
          :desc "Browse homepage"           "h"   #'+vc/browse-at-remote-homepage
          :desc "Browse remote"             "r"   #'forge-browse-remote
          :desc "Browse commit"             "c"   #'forge-browse-commit
          :desc "Browse an issue"           "i"   #'forge-browse-issue
          :desc "Browse a pull request"     "p"   #'forge-browse-pullreq
          :desc "Browse issues"             "I"   #'forge-browse-issues
          :desc "Browse pull requests"      "P"   #'forge-browse-pullreqs)
         (:prefix ("l" . "list")
          :desc "List repositories"         "r"   #'magit-list-repositories
          :desc "List submodules"           "s"   #'magit-list-submodules
          :desc "List issues"               "i"   #'forge-list-issues
          :desc "List pull requests"        "p"   #'forge-list-pullreqs
          :desc "List notifications"        "n"   #'forge-list-notifications)
         (:prefix ("c" . "create")
          :desc "Initialize repo"           "r"   #'magit-init
          :desc "Clone repo"                "R"   #'magit-clone
          :desc "Commit"                    "c"   #'magit-commit-create
          :desc "Fixup"                     "f"   #'magit-commit-fixup
          :desc "Branch"                    "b"   #'magit-branch-and-checkout
          :desc "Issue"                     "i"   #'forge-create-issue
          :desc "Pull request"              "p"   #'forge-create-pullreq)))

      ;;; <leader> i --- insert
      (:prefix-map ("i" . "insert")
       :desc "Emoji"                         "e"   #'emojify-insert-emoji
       :desc "Current file name"             "f"   #'+default/insert-file-path
       :desc "Current file path"             "F"   (cmd!! #'+default/insert-file-path t)
       :desc "Evil ex path"                  "p"   (cmd! (evil-ex "R!echo "))
       :desc "From evil register"            "r"   #'evil-show-registers
       :desc "Snippet"                       "s"   #'tempel-insert
       :desc "Unicode"                       "u"   #'insert-char
       :desc "From clipboard"                "y"   #'+default/yank-pop)

      ;;; <leader> n --- notes
      (:prefix-map ("n" . "notes")
       :desc "Search notes for symbol"      "*" #'+default/search-notes-for-symbol-at-point
       :desc "Org agenda"                   "a" #'org-agenda
       (:when (modulep! :tools biblio)
         :desc "Bibliographic entries"        "b"
         (cond ((modulep! :completion vertico)  #'citar-open-entry)
               ((modulep! :completion ivy)      #'ivy-bibtex)
               ((modulep! :completion helm)     #'helm-bibtex)))

       :desc "Toggle last org-clock"        "c" #'+org/toggle-last-clock
       :desc "Cancel current org-clock"     "C" #'org-clock-cancel

       :desc "Find file in notes"           "f" #'+default/find-in-notes
       :desc "Browse notes"                 "F" #'+default/browse-notes
       :desc "Org store link"               "l" #'org-store-link
       :desc "Tags search"                  "m" #'org-tags-view
       :desc "Org capture"                  "n" #'org-capture
       :desc "Goto capture"                 "N" #'org-capture-goto-target
       :desc "Active org-clock"             "o" #'org-clock-goto
       :desc "Todo list"                    "t" #'org-todo-list
       :desc "Search notes"                 "s" #'+default/org-notes-search
       :desc "Search org agenda headlines"  "S" #'+default/org-notes-headlines
       :desc "View search"                  "v" #'org-search-view
       :desc "Org export to clipboard"        "y" #'+org/export-to-clipboard
       :desc "Org export to clipboard as RTF" "Y" #'+org/export-to-clipboard-as-rich-text

       (:when (modulep! :emacs org +roam)
         (:prefix ("r" . "roam")
          :desc "Switch to buffer"              "b" #'org-roam-switch-to-buffer
          :desc "Org Roam Capture"              "c" #'org-roam-capture
          :desc "Find file"                     "f" #'org-roam-find-file
          :desc "Show graph"                    "g" #'org-roam-graph
          :desc "Insert"                        "i" #'org-roam-insert
          :desc "Insert (skipping org-capture)" "I" #'org-roam-insert-immediate
          :desc "Org Roam"                      "r" #'org-roam
          (:prefix ("d" . "by date")
           :desc "Arbitrary date" "d" #'org-roam-dailies-find-date
           :desc "Today"          "t" #'org-roam-dailies-find-today
           :desc "Tomorrow"       "m" #'org-roam-dailies-find-tomorrow
           :desc "Yesterday"      "y" #'org-roam-dailies-find-yesterday)))

       (:when (modulep! :emacs org +roam2)
         (:prefix ("r" . "roam")
          :desc "Open random node"           "a" #'org-roam-node-random
          :desc "Find node"                  "f" #'org-roam-node-find
          :desc "Find ref"                   "F" #'org-roam-ref-find
          :desc "Show graph"                 "g" #'org-roam-graph
          :desc "Insert node"                "i" #'org-roam-node-insert
          :desc "Capture to node"            "n" #'org-roam-capture
          :desc "Toggle roam buffer"         "r" #'org-roam-buffer-toggle
          :desc "Launch roam buffer"         "R" #'org-roam-buffer-display-dedicated
          :desc "Sync database"              "s" #'org-roam-db-sync
          (:prefix ("d" . "by date")
           :desc "Goto previous note"        "b" #'org-roam-dailies-goto-previous-note
           :desc "Goto date"                 "d" #'org-roam-dailies-goto-date
           :desc "Capture date"              "D" #'org-roam-dailies-capture-date
           :desc "Goto next note"            "f" #'org-roam-dailies-goto-next-note
           :desc "Goto tomorrow"             "m" #'org-roam-dailies-goto-tomorrow
           :desc "Capture tomorrow"          "M" #'org-roam-dailies-capture-tomorrow
           :desc "Capture today"             "n" #'org-roam-dailies-capture-today
           :desc "Goto today"                "t" #'org-roam-dailies-goto-today
           :desc "Capture today"             "T" #'org-roam-dailies-capture-today
           :desc "Goto yesterday"            "y" #'org-roam-dailies-goto-yesterday
           :desc "Capture yesterday"         "Y" #'org-roam-dailies-capture-yesterday
           :desc "Find directory"            "-" #'org-roam-dailies-find-directory))))

      ;;; <leader> o --- open
      (:prefix-map ("o" . "open")
       :desc "Org agenda"       "A"  #'org-agenda
       (:prefix ("a" . "org agenda")
        :desc "Agenda"         "a"  #'org-agenda
        :desc "Todo list"      "t"  #'org-todo-list
        :desc "Tags search"    "m"  #'org-tags-view
        :desc "View search"    "v"  #'org-search-view)
       :desc "Default browser"    "b"  #'browse-url-of-file
       :desc "Start debugger"     "d"  #'+debugger/start
       :desc "New frame"          "f"  #'make-frame
       :desc "Select frame"       "F"  #'select-frame-by-name
       :desc "REPL"               "r"  #'+eval/open-repl-other-window
       :desc "REPL (same window)" "R"  #'+eval/open-repl-same-window
       :desc "Dired"              "-"  #'dired-jump
       ;; REVIEW 2023-02-02: Kepp it?
       (:when (modulep! :ui treemacs)
         :desc "Project sidebar" "p" #'+treemacs/toggle
         :desc "Find file in project sidebar" "P" #'treemacs-find-file)
       (:when (modulep! :term shell)
         :desc "Toggle shell popup"    "t" #'+shell/toggle
         :desc "Open shell here"       "T" #'+shell/here)
       (:when (modulep! :term term)
         :desc "Toggle terminal popup" "t" #'+term/toggle
         :desc "Open terminal here"    "T" #'+term/here)
       (:when (modulep! :term vterm)
         :desc "Toggle vterm popup"    "t" #'+vterm/toggle
         :desc "Open vterm here"       "T" #'+vterm/here)
       (:when (modulep! :term eshell)
         :desc "Toggle eshell popup"   "e" #'+eshell/toggle
         :desc "Open eshell here"      "E" #'+eshell/here)
       ;; REVIEW 2023-02-02: Kepp it?
       (:when (modulep! :tools docker)
         :desc "Docker" "D" #'docker)
       (:when (modulep! :tools llm)
        (:prefix ("l" . "llm")
         :desc "Add text to context"        "a" #'gptel-add
         :desc "Explain"                    "e" #'gptel-quick
         :desc "Add file to context"        "f" #'gptel-add-file
         :desc "Open gptel"                 "l" #'gptel
         :desc "Send to gptel"              "s" #'gptel-send
         :desc "Open gptel menu"            "m" #'gptel-menu
         :desc "Rewrite"                    "r" #'gptel-rewrite
         :desc "Org: set topic"             "o" #'gptel-org-set-topic
         :desc "Org: set properties"        "O" #'gptel-org-set-properties)))

     ;;; <leader> p --- project
      (:prefix-map ("p" . "project")
       :desc "Browse project"               "." #'+default/browse-project
       :desc "Browse other project"         ">" #'zenit/browse-in-other-project
       :desc "Run cmd in project root"      "!" #'projectile-run-shell-command-in-root
       :desc "Async cmd in project root"    "&" #'projectile-run-async-shell-command-in-root
       :desc "Add new project"              "a" #'projectile-add-known-project
       :desc "Switch to project buffer"     "b" #'projectile-switch-to-buffer
       :desc "Compile in project"           "c" #'projectile-compile-project
       :desc "Repeat last command"          "C" #'projectile-repeat-last-command
       :desc "Remove known project"         "d" #'projectile-remove-known-project
       :desc "Discover projects in folder"  "D" #'+default/discover-projects
       :desc "Edit project .dir-locals"     "e" #'projectile-edit-dir-locals
       :desc "Find file in project"         "f" #'projectile-find-file
       :desc "Find file in other project"   "F" #'zenit/find-file-in-other-project
       :desc "Configure project"            "g" #'projectile-configure-project
       :desc "Invalidate project cache"     "i" #'projectile-invalidate-cache
       :desc "Kill project buffers"         "k" #'projectile-kill-buffers
       :desc "Find sibling file"            "o" #'find-sibling-file
       :desc "Switch project"               "p" #'projectile-switch-project
       :desc "Find recent project files"    "r" #'projectile-recentf
       :desc "Run project"                  "R" #'projectile-run-project
       :desc "Save project files"           "s" #'projectile-save-project-buffers
       :desc "List project todos"           "t" #'magit-todos-list
       :desc "Test project"                 "T" #'projectile-test-project
       :desc "Pop up scratch buffer"        "x" #'zenit/open-project-scratch-buffer
       :desc "Switch to scratch buffer"     "X" #'zenit/switch-to-project-scratch-buffer)

      ;;; <leader> q --- quit/session
      (:prefix-map ("q" . "quit/session")
       :desc "Restart emacs server"         "d" #'+default/restart-server
       :desc "Delete frame"                 "f" #'delete-frame
       :desc "Clear current frame"          "F" #'zenit/kill-all-buffers
       :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
       :desc "Quit Emacs"                   "q" #'save-buffers-kill-terminal
       :desc "Quit Emacs without saving"    "Q" #'evil-quit-all-with-error-code
       :desc "Quick save current session"   "s" #'zenit/quicksave-session
       :desc "Restore last session"         "l" #'zenit/quickload-session
       :desc "Save session to file"         "S" #'zenit/save-session
       :desc "Restore session from file"    "L" #'zenit/load-session
       :desc "Restart & restore Emacs"      "R" #'zenit/restart-and-restore
       :desc "Restart Emacs"                "r" #'zenit/restart)

      ;;; <leader> r --- remote
      (:when (modulep! :tools upload)
        (:prefix-map ("r" . "remote")
         :desc "Browse remote"              "b" #'ssh-deploy-browse-remote-base-handler
         :desc "Browse relative"            "B" #'ssh-deploy-browse-remote-handler
         :desc "Download remote"            "d" #'ssh-deploy-download-handler
         :desc "Delete local & remote"      "D" #'ssh-deploy-delete-handler
         :desc "Eshell base terminal"       "e" #'ssh-deploy-remote-terminal-eshell-base-handler
         :desc "Eshell relative terminal"   "E" #'ssh-deploy-remote-terminal-eshell-handler
         :desc "Move/rename local & remote" "m" #'ssh-deploy-rename-handler
         :desc "Open this file on remote"   "o" #'ssh-deploy-open-remote-file-handler
         :desc "Run deploy script"          "s" #'ssh-deploy-run-deploy-script-handler
         :desc "Upload local"               "u" #'ssh-deploy-upload-handler
         :desc "Upload local (force)"       "U" #'ssh-deploy-upload-handler-forced
         :desc "Diff local & remote"        "x" #'ssh-deploy-diff-handler
         :desc "Browse remote files"        "." #'ssh-deploy-browse-remote-handler
         :desc "Detect remote changes"      ">" #'ssh-deploy-remote-changes-handler))

      ;;; <leader> s --- search
      (:prefix-map ("s" . "search")
       :desc "Search buffer"                "b"
       (cond ((modulep! :completion vertico)   #'+default/search-buffer))
       :desc "Search all open buffers"      "B"
       (cond ((modulep! :completion vertico)   (cmd!! #'consult-line-multi 'all-buffers)))
       :desc "Search current directory"     "d" #'+default/search-cwd
       :desc "Search other directory"       "D" #'+default/search-other-cwd
       :desc "Search .emacs.d"              "e" #'+default/search-emacsd
       :desc "Locate file"                  "f" #'locate
       :desc "Jump to symbol"               "i" #'imenu
       :desc "Jump to symbol in open buffers" "I"
       (cond ((modulep! :completion vertico)   #'consult-imenu-multi))
       :desc "Jump to visible link"         "l" #'link-hint-open-link
       :desc "Jump to link"                 "L" #'ffap-menu
       :desc "Jump list"                    "j" #'evil-show-jumps
       :desc "Jump to bookmark"             "m" #'bookmark-jump
       :desc "Look up online"               "o" #'+lookup/online
       :desc "Look up online (w/ prompt)"   "O" #'+lookup/online-select
       :desc "Look up in local docsets"     "k" #'+lookup/in-docsets
       :desc "Look up in all docsets"       "K" #'+lookup/in-all-docsets
       :desc "Search project"               "p" #'+default/search-project
       :desc "Search other project"         "P" #'+default/search-other-project
       :desc "Jump to mark"                 "r" #'evil-show-marks
       :desc "Search buffer"                "s" #'+default/search-buffer
       :desc "Search buffer for thing at point" "S"
       (cond ((modulep! :completion vertico)   #'+vertico/search-symbol-at-point))
       :desc "Dictionary"                   "t" #'+lookup/dictionary-definition
       :desc "Thesaurus"                    "T" #'+lookup/synonyms
       :desc "Undo history"                 "u"
       (cond ((modulep! :emacs undo)           #'undo-tree-visualize)))

      ;;; <leader> t --- toggle
      (:prefix-map ("t" . "toggle")
       :desc "Big mode"                     "b" #'zenit-big-font-mode
       :desc "Fill Column Indicator"        "c" #'global-display-fill-column-indicator-mode
       :desc "Diff Highlights (Git Gutter)" "d" #'diff-hl-mode
       :desc "Flymake"                      "f" #'flymake-mode
       (:when (modulep! :checkers syntax -flymake)
         :desc "Flycheck"                   "f" #'flycheck-mode)
       :desc "Frame fullscreen"             "F" #'toggle-frame-fullscreen
       :desc "Evil goggles"                 "g" #'evil-goggles-mode
       (:when (modulep! :ui indent-guides)
         :desc "Indent guides"              "i" #'indent-bars-mode)
       :desc "Indent style"                 "I" #'zenit/toggle-indent-style
       :desc "Line numbers"                 "l" #'zenit/toggle-line-numbers
       (:when (modulep! :ui minimap)
         :desc "Minimap"                      "m" #'minimap-mode)
       (:when (modulep! :emacs org +present)
         :desc "org-tree-slide mode"        "p" #'org-tree-slide-mode)
       :desc "Read-only mode"               "r" #'read-only-mode
       (:when (and (modulep! :checkers spell) (executable-find "enchant-2"))
         :desc "Spell checker"              "s" #'jinx-mode)
       (:when (and (modulep! :checkers spell) (not (executable-find "enchant-2")))
         :desc "Spell checker"              "s" #'flyspell-mode)
       (:when (modulep! :ui ligatures)
         :desc "Code ligatures"             "t" #'+ligatures/toggle)
       :desc "Soft line wrapping"           "w" #'visual-line-mode
       (:when (modulep! :editor word-wrap)
         :desc "Soft line wrapping"         "w" #'+word-wrap-mode)
       (:when (modulep! :ui zen)
         :desc "Zen mode"                   "z" #'+zen/toggle
         :desc "Zen mode (fullscreen)"      "Z" #'+zen/toggle-fullscreen)))

(after! which-key
  (let ((prefix-re (regexp-opt (list zenit-leader-key zenit-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:C-w\\|%s w\\) m\\'" prefix-re))
                  nil . "maximize")
                which-key-replacement-alist)))

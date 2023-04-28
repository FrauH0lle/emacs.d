;; editor/evil/config.el -*- lexical-binding: t; -*-

(defvar +evil-want-o/O-to-continue-comments t
  "If non-nil, the o/O keys will continue comment lines if the
point is on a line with a linewise comment.")

(defvar evil-want-C-g-bindings t)
(defvar evil-want-C-i-jump nil)
(defvar evil-want-C-u-scroll t)  ; moved the universal arg to <leader> u
(defvar evil-want-C-u-delete t)
(defvar evil-want-C-w-delete t)
(defvar evil-want-Y-yank-to-eol t)
(defvar evil-want-abbrev-expand-on-insert-exit nil)
(defvar evil-respect-visual-line-mode nil)

(use-package! evil
  :preface
  (setq evil-disable-insert-state-bindings t
        evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-mode-line-format 'nil
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; enable emacs keybinds in insert
        evil-disable-insert-state-bindings t
        ;; more emacs like behavior
        evil-move-beyond-eol t
        evil-move-cursor-back nil
        ;; if the current state is obvious from the cursor's color/shape, then
        ;; we won't need superfluous indicators to do it instead.
        evil-default-cursor '+evil-default-cursor-fn
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        ;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        evil-ex-interactive-search-highlight 'selected-window
        ;; It's infuriating that innocuous "beginning of line" or "end of line"
        ;; errors will abort macros, so suppress them:
        evil-kbd-macro-suppress-motion-error t
        ;; Change defaults
        evil-cross-lines t
        evil-move-beyond-eol t
        evil-move-cursor-back nil
        evil-want-fine-undo t
        evil-undo-system 'undo-tree)

  (defadvice! +evil--persist-state-a (fn &rest args)
    "When changing major modes, Evil's state is lost. This advice
preserves it."
    :around #'set-auto-mode
    (if evil-state
        (evil-save-state (apply fn args))
      (apply fn args)))

  ;; Slow this down from 0.02 to prevent blocking in large or folded buffers
  ;; like magit while incrementally highlighting matches.
  (setq-hook! '(magit-mode-hook so-long-minor-mode-hook)
    evil-ex-hl-update-delay 0.25)

  ;; Layz-load evil
  :defer-incrementally t
  :init
  (defhook! +evil-init-evil-h ()
    "Initialize `evil' on `zenit-first-input-hook'.
The package should be loaded as early as possible."
    'zenit-first-input-hook :depth -105
    (require 'evil)
    (delq! 'evil-ex features)
    (add-transient-hook! 'evil-ex (provide 'evil-ex)))
  :config
  (evil-mode +1)

  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Stop copying the selection to the clipboard each time the cursor moves in
  ;; visual mode. Why? Because on most non-X systems (and in terminals with
  ;; clipboard plugins like xclip.el active), Emacs will spin up a new process
  ;; to communicate with the clipboard for each movement. On Windows, older
  ;; versions of macOS (pre-vfork), and Waylang (without pgtk), this is super
  ;; expensive and can lead to freezing and/or zombie processes.
  ;;
  ;; It also clobbers clipboard managers (see emacs-evil/evil#336).
  (setq evil-visual-update-x-selection-p nil)

  ;; Start help-with-tutorial in emacs state
  (advice-add #'help-with-tutorial :after (lambda (&rest _) (evil-emacs-state +1)))

  ;; Load popup rules as late as possible
  (eval-when! (modulep! :ui popup)
    (defhook! +evil--init-popup-rules-h ()
      "Initialize popop rules."
      'zenit-init-modules-hook
      (set-popup-rules!
       '(("^\\*evil-registers" :size 0.3)
         ("^\\*Command Line"   :size 8)))))

  ;; Change the cursor color in emacs mode
  (defhook! +evil-update-cursor-color-h ()
    "Update cursor color."
    'zenit-load-theme-hook
    (put 'cursor 'evil-normal-color (face-background 'cursor))
    (put 'cursor 'evil-emacs-color (face-foreground 'warning)))

  (defun +evil-default-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-normal-color)))
  (defun +evil-emacs-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-emacs-color)))

  ;; Ensure `evil-shift-width' always matches `tab-width'; evil does not police
  ;; this itself, so we must.
  (setq-hook! 'after-change-major-mode-hook evil-shift-width tab-width)

  (after! wgrep
    ;; A wrapper that invokes `wgrep-mark-deletion' across lines you use
    ;; `evil-delete' in wgrep buffers.
    (define-key wgrep-mode-map [remap evil-delete] #'+evil-delete))

  (defhook! +evil-disable-ex-highlights-h ()
    "Disable ex search buffer highlights."
    'zenit-escape-hook
    (when (evil-ex-hl-active-p 'evil-ex-search)
      (evil-ex-nohighlight)
      t))

  (after! eldoc
    ;; Allow eldoc to trigger directly after changing modes
    (eldoc-add-command 'evil-normal-state
                       'evil-insert
                       'evil-change
                       'evil-delete
                       'evil-replace))

  (unless noninteractive
    (setq save-silently t)
    (defhook! +evil-display-vimlike-save-message-h ()
      "Shorter, vim-esque save messages."
      'after-save-hook
      (message "\"%s\" %dL, %dC written"
               (if buffer-file-name
                   (file-relative-name (file-truename buffer-file-name) (zenit-project-root))
                 (buffer-name))
               (count-lines (point-min) (point-max))
               (buffer-size))))


  ;; HACK '=' moves the cursor to the beginning of selection. Disable this,
  ;;      since it's more disruptive than helpful.
  (defadvice! +evil--dont-move-cursor-a (orig-fn &rest args)
    "Don't move cursor on indent."
    :around #'evil-indent
    (save-excursion (apply orig-fn args)))

  (defadvice! +evil--make-numbered-markers-global-a (char)
    "In evil, registers 2-9 are buffer-local. In vim, they're
global, so..."
    :after-until #'evil-global-marker-p
    (and (>= char ?2) (<= char ?9)))

  (defadvice! +evil--fix-local-vars-a (&rest _)
    "REVIEW Fix #2493: dir-locals cannot target
fundamental-mode when evil-mode is active. See URL
`https://github.com/hlissner/doom-emacs/issues/2493'. Revert this
if this is ever fixed upstream."
    :before #'turn-on-evil-mode
    (when (eq major-mode 'fundamental-mode)
      (hack-local-variables)))

  (defadvice! +evil--fix-helpful-key-in-evil-ex-a (key-sequence)
    "HACK Invoking helpful from evil-ex throws a \"No recursive
edit is in progress\" error because, between evil-ex and
helpful,`abort-recursive-edit' gets called one time too many."
    :before #'helpful-key
    (when (evil-ex-p)
      (run-at-time 0.1 nil #'helpful-key key-sequence)
      (abort-recursive-edit)))

  ;; Make J (evil-join) remove comment delimiters when joining lines.
  (advice-add #'evil-join :around #'+evil-join-a)

  ;; Prevent gw (`evil-fill') and gq (`evil-fill-and-move') from squeezing
  ;; spaces. It doesn't in vim, so it shouldn't in evil.
  (defadvice! +evil--no-squeeze-on-fill-a (orig-fn &rest args)
    :around '(evil-fill evil-fill-and-move)
    (letf! (defun fill-region (from to &optional justify nosqueeze to-eop)
             (funcall fill-region from to justify t to-eop))
      (apply orig-fn args)))

  ;; Make ESC (from normal mode) the universal escaper. See `doom-escape-hook'.
  (advice-add #'evil-force-normal-state :after #'+evil-escape-a)

  ;; monkey patch `evil-ex-replace-special-filenames' to improve support for
  ;; file modifiers like %:p:h. This adds support for most of vim's modifiers,
  ;; and one custom one: %:P (expand to the project root).
  (advice-add #'evil-ex-replace-special-filenames :override #'+evil-replace-filename-modifiers-a)

  ;; make `try-expand-dabbrev' (from `hippie-expand') work in minibuffer
  (add-hook 'minibuffer-inactive-mode-hook #'+evil--fix-dabbrev-in-minibuffer-h)

  ;; Focus and recenter new splits
  (advice-add #'evil-window-split  :override #'+evil-window-split-a)
  (advice-add #'evil-window-vsplit :override #'+evil-window-vsplit-a)

  ;; Make o/O continue comments (see `+evil-want-o/O-to-continue-comments' to
  ;; disable)
  (advice-add #'evil-open-above :around #'+evil--insert-newline-above-and-respect-comments-a)
  (advice-add #'evil-open-below :around #'+evil--insert-newline-below-and-respect-comments-a)

  ;; These arg types will highlight matches in the current buffer
  (evil-ex-define-argument-type regexp-match
    :runner (lambda (flag &optional arg) (+evil-ex-regexp-match flag arg 'inverted)))
  (evil-ex-define-argument-type regexp-global-match
    :runner +evil-ex-regexp-match)

  (defun +evil--regexp-match-args (arg)
    (when (evil-ex-p)
      (cl-destructuring-bind (&optional arg flags)
          (evil-delimited-arguments arg 2)
        (list arg (string-to-list flags)))))

  ;; Other commands can make use of this
  (evil-define-interactive-code "<//>"
    :ex-arg regexp-match
    (+evil--regexp-match-args evil-ex-argument))

  (evil-define-interactive-code "<//!>"
    :ex-arg regexp-global-match
    (+evil--regexp-match-args evil-ex-argument))

  ;; Forward declare these so that ex completion works, even if the autoloaded
  ;; functions aren't loaded yet.
  (evil-add-command-properties '+evil:align :ex-arg 'regexp-match)
  (evil-add-command-properties '+evil:align-right :ex-arg 'regexp-match)
  (evil-add-command-properties '+multiple-cursors:evil-mc :ex-arg 'regexp-global-match)

  ;; Lazy load evil ex commands
  (after! evil-ex (load! "+commands")))

(use-package! evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :general ([remap comment-line] #'evilnc-comment-or-uncomment-lines))

(use-package! evil-snipe
  :commands evil-snipe-local-mode evil-snipe-override-local-mode
  :hook (zenit-first-input . evil-snipe-override-mode)
  :hook (zenit-first-input . evil-snipe-mode)
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t))

(use-package! evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))

(use-package! evil-textobj-anyblock
  :defer t
  :config
  (setq evil-textobj-anyblock-blocks
        '(("(" . ")")
          ("{" . "}")
          ("\\[" . "\\]")
          ("<" . ">"))))


(use-package! evil-traces
  :after evil-ex
  :config
  (pushnew! evil-traces-argument-type-alist
            '(+evil:align . evil-traces-global)
            '(+evil:align-right . evil-traces-global))
  (evil-traces-mode))


;; Allows you to use the selection for * and #
(use-package! evil-visualstar
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (after! evil
    (evil-define-key* 'visual 'global
      "*" #'evil-visualstar/begin-search-forward
      "#" #'evil-visualstar/begin-search-backward)))


;;
;;; Keybinds

(after! evil
  (load! "+keybinds.el"))

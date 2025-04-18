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
    (if (bound-and-true-p evil-state)
        (evil-save-state (apply fn args))
      (apply fn args)))

  ;; Slow this down from 0.02 to prevent blocking in large or folded buffers
  ;; like magit while incrementally highlighting matches.
  (setq-hook! '(magit-mode-hook so-long-minor-mode-hook)
    evil-ex-hl-update-delay 0.25)

  ;; Layz-load evil
  :defer-incrementally t
  :init
  (add-hook! 'zenit-first-input-hook :depth -105
    (defun +evil-init-evil-h ()
      "Initialize `evil' on `zenit-first-input-hook'.
The package should be loaded as early as possible."
      (require 'evil)
      ;; Lazy load evil ex commands, part I
      (delq! 'evil-ex features)
      (add-transient-hook! 'evil-ex (provide 'evil-ex))))
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
    (add-hook! 'zenit-after-modules-config-hook
      (defun +evil--init-popup-rules-h ()
        "Initialize popop rules."
        (set-popup-rules!
          '(("^\\*evil-registers" :size 0.3)
            ("^\\*Command Line"   :size 8))))))

  ;; Change the cursor color in emacs mode
  (add-hook! 'zenit-load-theme-hook
    (defun +evil-update-cursor-color-h ()
      "Update cursor color."
      (put 'cursor 'evil-normal-color (face-background 'cursor))
      (put 'cursor 'evil-emacs-color (face-foreground 'warning))))

  (defun +evil-default-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-normal-color)))
  (defun +evil-emacs-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-emacs-color)))

  ;; Ensure `evil-shift-width' always matches `tab-width'; evil does not police
  ;; this itself, so we must. Except in org-mode, where `tab-width' *must*
  ;; default to 8, which isn't a sensible default for `evil-shift-width'.
  (add-hook! 'after-change-major-mode-hook
    (defun +evil-adjust-shift-width-h ()
      (unless (derived-mode-p 'org-mode)
        (setq-local evil-shift-width tab-width))))

  (after! wgrep
    ;; A wrapper that invokes `wgrep-mark-deletion' across lines you use
    ;; `evil-delete' in wgrep buffers.
    (define-key wgrep-mode-map [remap evil-delete] #'+evil-delete))

  (add-hook! 'zenit-escape-hook
    (defun +evil-disable-ex-highlights-h ()
      "Disable ex search buffer highlights."
      (when (or (evil-ex-hl-active-p 'evil-ex-search)
                (bound-and-true-p anzu--state))
        (evil-ex-nohighlight)
        t)))

  (after! eldoc
    ;; Allow eldoc to trigger directly after changing modes
    (eldoc-add-command 'evil-normal-state
                       'evil-insert
                       'evil-change
                       'evil-delete
                       'evil-replace))

  (unless noninteractive
    (setq save-silently t)
    (add-hook! 'after-save-hook
      (defun +evil-display-vimlike-save-message-h ()
        "Shorter, vim-esque save messages."
        (message "\"%s\" %dL, %dC written"
                 (if buffer-file-name
                     (file-relative-name (file-truename buffer-file-name) (zenit-project-root))
                   (buffer-name))
                 (count-lines (point-min) (point-max))
                 (buffer-size)))))


  (defadvice! +evil--dont-move-cursor-a (fn &rest args)
    "HACK 2025-04-02: '=' moves the cursor to the beginning of
selection. Disable this, since it's more disruptive than helpful." 
    :around #'evil-indent
    (save-excursion (apply fn args)))

  (defadvice! +evil--make-numbered-markers-global-a (char)
    "In evil, registers 2-9 are buffer-local. In vim, they're
global, so..."
    :after-until #'evil-global-marker-p
    (and (>= char ?2) (<= char ?9)))

  ;; Make J (evil-join) remove comment delimiters when joining lines.
  (advice-add #'evil-join :around #'+evil-join-a)

  ;; Prevent gw (`evil-fill') and gq (`evil-fill-and-move') from squeezing
  ;; spaces. It doesn't in vim, so it shouldn't in evil.
  (defadvice! +evil--no-squeeze-on-fill-a (orig-fn &rest args)
    :around '(evil-fill evil-fill-and-move)
    (letf! (defun fill-region (from to &optional justify nosqueeze to-eop)
             (funcall fill-region from to justify t to-eop))
      (apply orig-fn args)))

  ;; Make ESC (from normal mode) the universal escaper. See `zenit-escape-hook'.
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

  ;; Jump tracking
  (advice-add #'evil-goto-first-line :around #'zenit-set-jump-a)
  (advice-add #'evil-goto-line       :around #'zenit-set-jump-a)

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

  ;; Lazy load evil ex commands, part II
  (compile-along! "+commands")
  (after! evil-ex
    (load! "+commands")))


;;
;;; Packages

(use-package! evil-easymotion
  :after-call zenit-first-input-hook
  :commands evilem-create evilem-default-keybindings evilem-motion-backward-word-end
  :config
  ;; Use evil-search backend, instead of isearch
  (protect-macros!
    (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
                        :bind ((evil-ex-search-highlight-all nil)))
    (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
                        :bind ((evil-ex-search-highlight-all nil)))
    (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
                        :bind ((evil-ex-search-highlight-all nil)))
    (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
                        :bind ((evil-ex-search-highlight-all nil))))

  ;; Rebind scope of w/W/e/E/ge/gE evil-easymotion motions to the visible
  ;; buffer, rather than just the current line.
  (put 'visible 'bounds-of-thing-at-point (lambda () (cons (window-start) (window-end))))
  (protect-macros!
    (evilem-make-motion evilem-motion-forward-word-begin #'evil-forward-word-begin :scope 'visible)
    (evilem-make-motion evilem-motion-forward-WORD-begin #'evil-forward-WORD-begin :scope 'visible)
    (evilem-make-motion evilem-motion-forward-word-end #'evil-forward-word-end :scope 'visible)
    (evilem-make-motion evilem-motion-forward-WORD-end #'evil-forward-WORD-end :scope 'visible)
    (evilem-make-motion evilem-motion-backward-word-begin #'evil-backward-word-begin :scope 'visible)
    (evilem-make-motion evilem-motion-backward-WORD-begin #'evil-backward-WORD-begin :scope 'visible)
    (evilem-make-motion evilem-motion-backward-word-end #'evil-backward-word-end :scope 'visible)
    (evilem-make-motion evilem-motion-backward-WORD-end #'evil-backward-WORD-end :scope 'visible)))


(use-package! evil-embrace
  :commands embrace-add-pair embrace-add-pair-regexp
  :hook (LaTeX-mode . embrace-LaTeX-mode-hook)
  :hook (LaTeX-mode . +evil-embrace-latex-mode-hook-h)
  :hook (org-mode . embrace-org-mode-hook)
  :hook (ruby-mode . embrace-ruby-mode-hook)
  :hook (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)
  :hook ((c++-mode c++-ts-mode rustic-mode csharp-mode java-mode swift-mode typescript-mode)
         . +evil-embrace-angle-bracket-modes-hook-h)
  :hook (scala-mode . +evil-embrace-scala-mode-hook-h)
  :init
  (after! evil-surround
    (evil-embrace-enable-evil-surround-integration))

  ;; HACK: This must be done ASAP, before embrace has a chance to
  ;;   buffer-localize `embrace--pairs-list' (which happens right after it calls
  ;;   `embrace--setup-defaults'), otherwise any new, global default pairs we
  ;;   define won't be in scope.
  (defadvice! +evil--embrace-init-escaped-pairs-a (&rest args)
    "Add escaped-sequence support to embrace."
    :after #'embrace--setup-defaults
    (embrace-add-pair-regexp ?\\ "\\[[{(]" "\\[]})]" #'+evil--embrace-escaped
                             (embrace-build-help "\\?" "\\?")))
  :config
  (setq evil-embrace-show-help-p nil)

  (defun +evil-embrace-scala-mode-hook-h ()
    (embrace-add-pair ?$ "${" "}"))

  (defun +evil-embrace-latex-mode-hook-h ()
    (dolist (pair '((?\' . ("`" . "\'"))
                    (?\" . ("``" . "\'\'"))))
      (delete (car pair) evil-embrace-evil-surround-keys)
      ;; Avoid `embrace-add-pair' because it would overwrite the default
      ;; rules, which we want for other modes
      (push (cons (car pair) (make-embrace-pair-struct
                              :key (car pair)
                              :left (cadr pair)
                              :right (cddr pair)
                              :left-regexp (regexp-quote (cadr pair))
                              :right-regexp (regexp-quote (cddr pair))))
            embrace--pairs-list))
    (embrace-add-pair-regexp ?l "\\[a-z]+{" "}" #'+evil--embrace-latex))

  (defun +evil-embrace-angle-bracket-modes-hook-h ()
    (let ((var (make-local-variable 'evil-embrace-evil-surround-keys)))
      (set var (delq ?< evil-embrace-evil-surround-keys))
      (set var (delq ?> evil-embrace-evil-surround-keys)))
    (embrace-add-pair-regexp ?< "\\_<[a-z0-9-_]+<" ">" #'+evil--embrace-angle-brackets)
    (embrace-add-pair ?> "<" ">")))


(use-package! evil-escape
  :commands evil-escape
  :hook (zenit-first-input . evil-escape-mode)
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
        evil-escape-key-sequence nil
        evil-escape-delay 0.15)
  (after! evil
    (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape))
  :config
  ;; `evil-escape' in the minibuffer is more disruptive than helpful. That is,
  ;; unless we have `evil-collection-setup-minibuffer' enabled, in which case we
  ;; want the same behavior in insert mode as we do in normal buffers.
  (add-hook! 'evil-escape-inhibit-functions
    (defun +evil-inhibit-escape-in-minibuffer-fn ()
      (and (minibufferp)
           (or (not (bound-and-true-p evil-collection-setup-minibuffer))
               (evil-normal-state-p))))))


(use-package! evil-exchange
  :commands evil-exchange
  :config
  (add-hook! 'zenit-escape-hook
    (defun +evil--escape-exchange-h ()
      (when evil-exchange--overlays
        (evil-exchange-cancel)
        t))))


(use-package! evil-quick-diff
  :commands (evil-quick-diff evil-quick-diff-cancel))


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


(use-package! exato
  :commands evil-outer-xml-attr evil-inner-xml-attr)


;;
;;; Keybinds

(compile-along! "+keybinds")
(after! evil
  (load! "+keybinds"))

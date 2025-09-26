;; config/default/config.el -*- lexical-binding: t; -*-

(defvar +default-want-RET-continue-comments t
  "If non-nil, RET will continue commented lines.")

(defvar +default-minibuffer-maps
  (append '(minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map
            read-expression-map))
  "A list of all the keymaps used for the minibuffer.")

;;
;;; Reasonable defaults

;;;###package avy
(setq avy-all-windows nil
      avy-all-windows-alt t
      avy-background t
      ;; the unpredictability of this (when enabled) makes it a poor default
      avy-single-candidate-jump nil)


(after! epa
  ;; With GPG 2.1+, this forces gpg-agent to use the Emacs minibuffer to prompt
  ;; for the key passphrase.
  (set 'epg-pinentry-mode 'loopback)
  ;; Default to the first enabled and non-expired key in your keyring.
  (setq-default
   epa-file-encrypt-to
   (or (default-value 'epa-file-encrypt-to)
       (unless (string-empty-p user-full-name)
         (when-let* ((context (ignore-errors (epg-make-context))))
           (cl-loop for key in (epg-list-keys context user-full-name 'public)
                    for subkey = (car (epg-key-sub-key-list key))
                    if (not (memq 'disabled (epg-sub-key-capability subkey)))
                    if (< (or (epg-sub-key-expiration-time subkey) 0)
                          (time-to-seconds))
                    collect (epg-sub-key-fingerprint subkey))))
       user-mail-address))
  ;; And suppress prompts if epa-file-encrypt-to has a default value (without
  ;; overwriting file-local values).
  (defadvice! +default--dont-prompt-for-keys-a (&rest _)
    :before #'epa-file-write-region
    (unless (local-variable-p 'epa-file-encrypt-to)
      (setq-local epa-file-encrypt-to (default-value 'epa-file-encrypt-to)))))


(after! woman
  ;; The woman-manpath default value does not necessarily match man. If we have
  ;; man available but aren't using it for performance reasons, we can extract
  ;; it's manpath.
  (when-let*
      ((path (cond
              ((executable-find "manpath")
               (split-string (cdr (zenit-call-process "manpath" "-q"))
                             path-separator t))
              ((executable-find "man")
               (split-string (cdr (zenit-call-process "man" "--path"))
                             path-separator t)))))
    (setq woman-manpath path)))


;;
;;; Smartparens config

(static-when (modulep! +smartparens)
  ;; You can disable :unless predicates with (sp-pair "'" nil :unless nil)
  ;; And disable :post-handlers with (sp-pair "{" nil :post-handlers nil)
  ;; or specific :post-handlers with:
  ;;   (sp-pair "{" nil :post-handlers '(:rem ("| " "SPC")))
  (after! smartparens
    ;; Smartparens' navigation feature is neat, but does not justify how
    ;; expensive it is. It's also less useful for evil users. This may need to
    ;; be reactivated for non-evil users though. Needs more testing!
    (add-hook! 'after-change-major-mode-hook
      (defun zenit--hool-disable-smartparens-navigate-skip-match-h ()
        "Disable `smartparens' navigation features."
        (setq sp-navigate-skip-match nil
              sp-navigate-consider-sgml-tags nil)))

    ;; Autopair quotes more conservatively; if I'm next to a word/before another
    ;; quote, I don't want to open a new pair or it would unbalance them.
    (let ((unless-list '(sp-point-before-word-p
                         sp-point-after-word-p
                         sp-point-before-same-p)))
      (sp-pair "'"  nil :unless unless-list)
      (sp-pair "\"" nil :unless unless-list))

    ;; Expand {|} => { | }
    ;; Expand {|} => {
    ;;   |
    ;; }
    (dolist (brace '("(" "{" "["))
      (sp-pair brace nil
               :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
               ;; Don't autopair opening braces if before a word character or
               ;; other opening brace. The rationale: it interferes with manual
               ;; balancing of braces, and is odd form to have s-exps with no
               ;; whitespace in between, e.g. ()()(). Insert whitespace if
               ;; genuinely want to start a new form in the middle of a word.
               :unless '(sp-point-before-word-p sp-point-before-same-p)))

    ;; In lisps ( should open a new form if before another parenthesis
    (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))

    ;; Major-mode specific fixes
    (sp-local-pair '(ruby-mode ruby-ts-mode) "{" "}"
                   :pre-handlers '(:rem sp-ruby-pre-handler)
                   :post-handlers '(:rem sp-ruby-post-handler))

    ;; Don't eagerly escape Swift style string interpolation
    (sp-local-pair '(swift-mode swift-ts-mode) "\\(" ")" :when '(sp-in-string-p))

    ;; Don't do square-bracket space-expansion where it doesn't make sense to
    (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode markdown-ts-mode gfm-mode)
                   "[" nil :post-handlers '(:rem ("| " "SPC")))

    ;; Reasonable default pairs for HTML-style comments
    (sp-local-pair (append sp--html-modes '(markdown-mode markdown-ts-mode gfm-mode))
                   "<!--" "-->"
                   :unless '(sp-point-before-word-p sp-point-before-same-p)
                   :actions '(insert) :post-handlers '(("| " "SPC")))

    ;; Disable electric keys in C modes because it interferes with smartparens
    ;; and custom bindings. We'll do it ourselves (mostly).
    (after! cc-mode
      (setq-default c-electric-flag nil)
      (dolist (key '("#" "{" "}" "/" "*" ";" "," ":" "(" ")" "\177"))
        (define-key c-mode-base-map key nil))

      ;; Smartparens and cc-mode both try to autoclose angle-brackets
      ;; intelligently. The result isn't very intelligent (causes redundant
      ;; characters), so just do it ourselves.
      (define-key! c++-mode-map "<" nil ">" nil)

      (defun +default-cc-sp-point-is-template-p (id action context)
        "Return t if point is in the right place for C++ angle-brackets."
        (and (sp-in-code-p id action context)
             (cond ((eq action 'insert)
                    (sp-point-after-word-p id action context))
                   ((eq action 'autoskip)
                    (/= (char-before) 32)))))

      (defun +default-cc-sp-point-after-include-p (id action context)
        "Return t if point is in an #include."
        (and (sp-in-code-p id action context)
             (save-excursion
               (goto-char (line-beginning-position))
               (looking-at-p "[ 	]*#include[^<]+"))))

      ;; ...and leave it to smartparens
      (sp-local-pair '(c++-mode c++-ts-mode objc-mode)
                     "<" ">"
                     :when '(+default-cc-sp-point-is-template-p
                             +default-cc-sp-point-after-include-p)
                     :post-handlers '(("| " "SPC")))

      (sp-local-pair '(c-mode c++-mode objc-mode java-mode
                       c-ts-mode c++-ts-mode java-ts-mode)
                     "/*!" "*/"
                     :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))

    ;; Expand C-style comment blocks.
    (defun +default-open-doc-comments-block (&rest _ignored)
      (save-excursion
        (newline)
        (indent-according-to-mode)))
    (sp-local-pair
     '(js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode
       rust-mode rust-ts-mode rustic-mode
       c-mode c++-mode objc-mode c-ts-mode c++-ts-mode
       csharp-mode csharp-ts-mode
       java-mode java-ts-mode
       php-mode php-ts-mode
       css-mode css-ts-mode
       scss-mode less-css-mode
       stylus-mode scala-mode)
     "/*" "*/"
     :actions '(insert)
     :post-handlers '(("| " "SPC")
                      (" | " "*")
                      ("|[i]\n[i]" "RET")))

    (after! smartparens-ml
      (sp-with-modes '(tuareg-mode fsharp-mode)
        (sp-local-pair "(*" "*)" :actions nil)
        (sp-local-pair "(*" "*"
                       :actions '(insert)
                       :post-handlers '(("| " "SPC") ("|[i]*)[d-2]" "RET")))))

    (after! smartparens-markdown
      (sp-with-modes '(markdown-mode markdown-ts-mode gfm-mode)
        (sp-local-pair "```" "```" :post-handlers '(:add ("||\n[i]" "RET")))

        ;; The original rules for smartparens had an odd quirk: inserting two
        ;; asterisks would replace nearby quotes with asterisks. These two rules
        ;; set out to fix this.
        (sp-local-pair "**" nil :actions :rem)
        (sp-local-pair "*" "*"
                       :actions '(insert skip)
                       :unless '(:rem sp-point-at-bol-p)
                       ;; * then SPC will delete the second asterix and assume
                       ;; you wanted a bullet point. * followed by another *
                       ;; will produce an extra, assuming you wanted **|**.
                       :post-handlers '(("[d1]" "SPC") ("|*" "*"))))

      ;; This keybind allows * to skip over **.
      (let ((fn (general-predicate-dispatch nil
                  (looking-at-p "\\*\\* *")
                  (cmd! (forward-char 2)))))
        (map! :map markdown-mode-map :ig "*" fn)
        (map! :after markdown-ts-mode :map markdown-ts-mode-map :ig "*" fn)))

    ;; Removes haskell-mode trailing braces
    (after! smartparens-haskell
      (sp-with-modes '(haskell-mode haskell-ts-mode haskell-interactive-mode)
        (sp-local-pair "{-" "-}" :actions nil)
        (sp-local-pair "{-#" "#-}" :actions nil)
        (sp-local-pair "{-@" "@-}" :actions nil)
        (sp-local-pair "{-" "-")
        (sp-local-pair "{-#" "#-")
        (sp-local-pair "{-@" "@-")))

    (after! (smartparens-python python)
      (sp-with-modes '(python-mode python-ts-mode)
        ;; Automatically close f-strings
        (sp-local-pair "f\"" "\"")
        (sp-local-pair "f\"\"\"" "\"\"\"")
        (sp-local-pair "f'''" "'''")
        (sp-local-pair "f'" "'"))
      ;; Original keybind interferes with smartparens rules
      (after! python
        (define-key (or (bound-and-true-p python-base-mode-map)
                        python-mode-map)
                    (kbd "DEL") nil))
      ;; Interferes with the def snippet in zenit-snippets
      (setq sp-python-insert-colon-in-function-definitions nil))))


;;
;;; Keybinding fixes

;; Highjacks backspace to delete up to nearest column multiple of `tab-width' at
;; a time. If you have smartparens enabled, it will also:
;;  a) balance spaces inside brackets/parentheses ( | ) -> (|)
;;  b) close empty multiline brace blocks in one step:
;;     {
;;     |
;;     }
;;     becomes {|}
;;  c) refresh smartparens' :post-handlers, so SPC and RET expansions work even
;;     after a backspace.
;;  d) properly delete smartparen pairs when they are encountered, without the
;;     need for strict mode.
;;  e) do none of this when inside a string
(advice-add #'delete-backward-char :override #'+default--delete-backward-char-a)

;; HACK Makes `newline-and-indent' continue comments (and more reliably).
;;      Consults `zenit-point-in-comment-functions' to detect a commented region
;;      and uses that mode's `comment-line-break-function' to continue comments.
;;      If neither exists, it will fall back to the normal behavior of
;;      `newline-and-indent'.
;;
;;      We use an advice here instead of a remapping because many modes define
;;      and remap to their own newline-and-indent commands, and tackling all
;;      those cases was judged to be more work than dealing with the edge cases
;;      on a case by case basis.
(defadvice! +default--newline-indent-and-continue-comments-a (&rest _)
  "A replacement for `newline-and-indent'.

Continues comments if executed from a commented line. Consults
`zenit-point-in-comment-functions' to determine if in a comment."
  :before-until #'newline-and-indent
  (interactive "*")
  (when (and +default-want-RET-continue-comments
             (zenit-point-in-comment-p)
             (functionp comment-line-break-function))
    (funcall comment-line-break-function nil)
    t))

;; This section is dedicated to "fixing" certain keys so that they behave
;; sensibly (and consistently with similar contexts).

;; Consistently use q to quit windows
(after! tabulated-list
  (define-key tabulated-list-mode-map "q" #'quit-window))


;;
;;; Keybind schemes

;; Custom help keys -- these aren't under `+bindings' because they ought to be
;; universal.
(define-key! help-map
  ;; new keybinds
  "'"    #'describe-char
  "M"    #'zenit/describe-active-minor-mode
  ;; REVIEW 2023-02-01: Remove if lookup does not exist
  "O"    #'+lookup/online
  "T"    #'zenit/toggle-profiler
  "W"    #'+default/man-or-woman
  "C-k"  #'describe-key-briefly
  "C-l"  #'describe-language-environment
  "C-m"  #'info-emacs-manual

  ;; Unbind `help-for-help'. Conflicts with which-key's help command for the
  ;; <leader> h prefix. It's already on ? and F1 anyway.
  "C-h"  nil

  ;; replacement keybinds
  ;; replaces `info-emacs-manual' b/c it's on C-m now
  "r"    nil
  "rt"   #'zenit/reload-theme
  "rf"   #'zenit/reload-font

  ;; make `describe-bindings' available under the b prefix which it previously
  ;; occupied. Add more binding related commands under that prefix as well
  "b"    nil
  "bb"   #'describe-bindings
  "bi"   #'which-key-show-minor-mode-keymap
  "bm"   #'which-key-show-major-mode
  "bt"   #'which-key-show-top-level
  "bf"   #'which-key-show-full-keymap
  "bk"   #'which-key-show-keymap

  ;; replaces `apropos-documentation' b/c `apropos' covers this
  "d"    nil
  "dc"   #'zenit/goto-local-config-file
  "dC"   #'zenit/goto-local-init-file
  "dd"   #'zenit-debug-mode
  "dpc"  #'zenit/help-package-config
  "dpd"  #'zenit/goto-local-packages-file
  "dph"  #'zenit/help-package-homepage
  "dt"   #'zenit/toggle-profiler

  ;; replaces `apropos-command'
  "a"    #'apropos
  "A"    #'apropos-documentation
  ;; replaces `describe-copying' b/c not useful
  "C-c"  #'describe-coding-system
  ;; replaces `Info-got-emacs-command-node' b/c redundant w/ `Info-goto-node'
  "F"    #'describe-face
  ;; replaces `view-hello-file' b/c annoying
  "h"    nil
  ;; replaces `help-with-tutorial', b/c it's less useful than `load-theme'
  "t"    #'load-theme
  ;; replaces `finder-by-keyword' b/c not useful
  "p"    #'zenit/help-packages
  ;; replaces `describe-package' b/c redundant w/ `zenit/help-packages'
  "P"    #'find-library)

(after! which-key
  (let ((prefix-re (regexp-opt (list zenit-leader-key zenit-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:<\\(?:\\(?:f1\\|help\\)>\\)\\|C-h\\|%s h\\) d\\'" prefix-re))
                  nil . "documentation")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`\\(?:<\\(?:\\(?:f1\\|help\\)>\\)\\|C-h\\|%s h\\) d p\\'" prefix-re))
                  nil . "packages")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`\\(?:<\\(?:\\(?:f1\\|help\\)>\\)\\|C-h\\|%s h\\) r\\'" prefix-re))
                  nil . "reload")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`\\(?:<\\(?:\\(?:f1\\|help\\)>\\)\\|C-h\\|%s h\\) b\\'" prefix-re))
                  nil . "bindings")
                which-key-replacement-alist)))


(static-when (modulep! +bindings)

  ;; Make M-x harder to miss
  (define-key! 'override
    "M-x" #'execute-extended-command
    "A-x" #'execute-extended-command)

  ;; A convention where C-s on popups and interactive searches will invoke
  ;; ivy/helm/vertico for their superior filtering.
  (when-let* ((command (cond ((modulep! :completion vertico)
                              #'consult-history))))
    (define-key!
      :keymaps (append +default-minibuffer-maps
                       (static-when (modulep! :editor evil)
                         '(evil-ex-completion-map)))
      "C-s" command))

  (map! :when (modulep! :completion corfu)
        :after corfu
        (:map corfu-map
              [remap corfu-insert-separator] #'+corfu/smart-sep-toggle-escape
              "C-S-s" #'+corfu/move-to-minibuffer
              "C-p" #'corfu-previous
              "C-n" #'corfu-next))
  (let ((cmds-del
         `(menu-item "Reset completion" corfu-reset
           :filter ,(lambda (cmd)
                      (when (and (>= corfu--index 0)
                                 (eq corfu-preview-current 'insert))
                        cmd))))
        (cmds-ret
         `(menu-item "Insert completion DWIM" corfu-insert
           :filter ,(lambda (cmd)
                      (pcase +corfu-want-ret-to-confirm
                        ('nil (corfu-quit) nil)
                        ('t (if (or (>= corfu--index 0)
                                    (and prefix-arg (bound-and-true-p corfu-indexed-mode)))
                                cmd))
                        ('both (funcall-interactively cmd) nil)
                        ('minibuffer
                         (if (minibufferp nil t)
                             (ignore (funcall-interactively cmd))  ; 'both' behavior
                           (if (>= corfu--index 0) cmd)))  ; 't' behavior
                        (_ cmd)))))
        (cmds-tab
         `(menu-item "Select next candidate or navigate org table" corfu-next
           :filter (lambda (cmd)
                     (cond
                      ,@(static-when (modulep! :emacs org)
                          '(((and +corfu-want-tab-prefer-navigating-org-tables
                                  (featurep 'org)
                                  (org-at-table-p))
                             #'org-table-next-field)))
                      (t cmd)))) )
        (cmds-s-tab
         `(menu-item "Select previous candidate or navigate org table"
           corfu-previous
           :filter (lambda (cmd)
                     (cond
                      ,@(static-when (modulep! :emacs org)
                          '(((and +corfu-want-tab-prefer-navigating-org-tables
                                  (featurep 'org)
                                  (org-at-table-p))
                             #'org-table-previous-field)))
                      (t cmd))))))
    (map! :when (modulep! :completion corfu)
          :map corfu-map
          [backspace] cmds-del
          "DEL" cmds-del
          :gi [return] cmds-ret
          :gi "RET" cmds-ret
          "S-TAB" cmds-s-tab
          [backtab] cmds-s-tab
          :gi "TAB" cmds-tab
          :gi [tab] cmds-tab))

  ;; Smarter C-a/C-e for both Emacs and Evil. C-a will jump to indentation.
  ;; Pressing it again will send you to the true bol. Same goes for C-e, except
  ;; it will ignore comments+trailing whitespace before jumping to eol.
  ;; (after! evil
  (map! :gnie "C-a" #'zenit/backward-to-bol-or-indent
        :gnie "C-e" #'zenit/forward-to-last-non-comment-or-eol
        ;; Standardizes the behavior of modified RET to match the behavior of
        ;; other editors, particularly Atom, textedit, textmate, and vscode, in
        ;; which ctrl+RET will add a new "item" below the current one and
        ;; cmd+RET (Mac) / meta+RET (elsewhere) will add a new, blank line below
        ;; the current one.

        ;; C-<mouse-scroll-up>   = text scale increase
        ;; C-<mouse-scroll-down> = text scale decrease
        [C-down-mouse-2] (cmd! (text-scale-set 0))

        ;; auto-indent on newline by default
        :gi [remap newline] #'newline-and-indent
        ;; insert literal newline
        :i  "S-RET"         #'+default/newline
        :i  [S-return]      #'+default/newline
        :i  "C-j"           #'+default/newline

        ;; Add new item below current (without splitting current line).
        :gi "C-RET"         #'+default/newline-below
        :gn [C-return]      #'+default/newline-below
        ;; Add new item above current (without splitting current line)
        :gi "C-S-RET"       #'+default/newline-above
        :gn [C-S-return]    #'+default/newline-above

        (:when zenit--system-macos-p
          :gn "s-RET"        #'+default/newline-below
          :gn [s-return]     #'+default/newline-below
          :gn "S-s-RET"      #'+default/newline-above
          :gn [S-s-return]   #'+default/newline-above)))


;;
;;; Bootstrap configs

(static-when (modulep! :editor evil)
  (defun +default-disable-delete-selection-mode-h ()
    (delete-selection-mode -1))
  (add-hook 'evil-insert-state-entry-hook #'delete-selection-mode)
  (add-hook 'evil-insert-state-exit-hook  #'+default-disable-delete-selection-mode-h)

  ;; Make SPC u SPC u [...] possible (#747)
  (map! :map universal-argument-map
        :prefix zenit-leader-key     "u" #'universal-argument-more
        :prefix zenit-leader-alt-key "u" #'universal-argument-more)

  (static-when (modulep! +bindings)
    (compile-along! "+evil-bindings")
    (load! "+evil-bindings")))

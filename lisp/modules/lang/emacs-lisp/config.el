;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-

(defvar +emacs-lisp-enable-extra-fontification t
  "If non-nil, highlight special forms, and defined functions and
variables.")

(defvar +emacs-lisp-outline-regexp "[ \t]*;;;;* [^ \t\n]"
  "Regexp to use for `outline-regexp' in `emacs-lisp-mode'.
This marks a foldable marker for `outline-minor-mode' in elisp
buffers.")

(defvar +emacs-lisp-disable-flycheck-in-dirs
  (list zenit-emacs-dir zenit-local-conf-dir)
  "List of directories to disable `emacs-lisp-checkdoc' in.

This checker tends to produce a lot of false positives in your
.emacs.d and private config, so it is mostly useless there.")


;; `elisp-mode' is loaded at startup. In order to lazy load its config we need
;; to pretend it isn't loaded
(defer-feature! elisp-mode emacs-lisp-mode)


;;
;;; Config

(use-package! elisp-mode
  :mode ("\\.Cask\\'" . emacs-lisp-mode)
  :config
  (eval-when! (modulep! :tools eval)
    (set-repl-handler! '(emacs-lisp-mode lisp-interaction-mode)
                       #'+emacs-lisp/open-repl)
    (set-eval-handler! '(emacs-lisp-mode lisp-interaction-mode)
                       #'+emacs-lisp-eval))
  (eval-when! (modulep! :tools lookup)
    (set-lookup-handlers! '(emacs-lisp-mode lisp-interaction-mode helpful-mode)
                          :definition    #'+emacs-lisp-lookup-definition
                          :documentation #'+emacs-lisp-lookup-documentation)
    (set-docsets! '(emacs-lisp-mode lisp-interaction-mode) "Emacs Lisp"))

  (eval-when! (modulep! :ui pretty-code)
    (set-pretty-symbols! 'emacs-lisp-mode :lambda "lambda"))
  (eval-when! (modulep! :editor rotate-text)
    (set-rotate-patterns! 'emacs-lisp-mode
      :symbols '(("t" "nil")
                 ("let" "let*")
                 ("when" "unless")
                 ("append" "prepend")
                 ("advice-add" "advice-remove")
                 ("defadvice!" "undefadvice!")
                 ("add-hook" "remove-hook")
                 ("add-hook!" "remove-hook!"))))

  (setq-hook! 'emacs-lisp-mode-hook
    ;; Emacs' built-in elisp files use a hybrid tab->space indentation scheme
    ;; with a tab width of 8. Any smaller and the indentation will be
    ;; unreadable. Since Emacs' lisp indenter doesn't respect this variable it's
    ;; safe to ignore this setting otherwise.
    tab-width 8
    ;; Don't treat autoloads or sexp openers as outline headers, we have
    ;; hideshow for that.
    outline-regexp +emacs-lisp-outline-regexp)

  ;; Fixed indenter that intends plists sensibly.
  (advice-add #'calculate-lisp-indent :override #'+emacs-lisp--calculate-lisp-indent-a)

  ;; variable-width indentation is superior in elisp. Otherwise, `dtrt-indent'
  ;; and `editorconfig' would force fixed indentation on elisp.
  (add-to-list 'zenit-detect-indentation-excluded-modes 'emacs-lisp-mode)

  (add-hook! 'emacs-lisp-mode-hook
             ;; Allow folding of outlines in comments
             #'outline-minor-mode
             ;; Make parenthesis depth easier to distinguish at a glance
             #'rainbow-delimiters-mode
             ;; Make quoted symbols easier to distinguish from free variables
             #'highlight-quoted-mode
             ;; Extend imenu support to Doom constructs
             #'+emacs-lisp-extend-imenu-h
             ;; Ensure straight sees modifications to installed packages
             #'+emacs-lisp-init-straight-maybe-h)

  ;; Flycheck produces a *lot* of false positives in emacs configs, so disable
  ;; it when you're editing them
  (add-hook 'flycheck-mode-hook #'+emacs-lisp-disable-flycheck-maybe-h)

  ;; Special syntax highlighting for elisp
  (font-lock-add-keywords
   'emacs-lisp-mode
   (append `(;; custom cookies
             ("^;;;###\\(autodef\\|if\\|package\\)[ \n]" (1 font-lock-warning-face t)))
           ;; highlight defined, special variables & functions
           (when +emacs-lisp-enable-extra-fontification
             `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face)))))

  ;; Recenter window after following definition
  (advice-add #'elisp-def :after #'zenit-recenter-a)

  (defadvice! +emacs-lisp-append-value-to-eldoc-a (fn sym)
    "Display variable value next to documentation in eldoc."
    :around #'elisp-get-var-docstring
    (when-let (ret (funcall fn sym))
      (if (boundp sym)
          (concat ret " "
                  (let* ((truncated " [...]")
                         (print-escape-newlines t)
                         (str (symbol-value sym))
                         (str (prin1-to-string str))
                         (limit (- (frame-width) (length ret) (length truncated) 1)))
                    (format (format "%%0.%ds%%s" (max limit 0))
                            (propertize str 'face 'warning)
                            (if (< (length str) limit) "" truncated))))
        ret)))

  ;; Keybindings
  (map! :localleader
        :map (emacs-lisp-mode-map lisp-interaction-mode-map)
        (:prefix ("c" . "compile")
                 "c" #'emacs-lisp-byte-compile)
        (:prefix ("d" . "debug")
                 "f" #'+emacs-lisp/edebug-instrument-defun-on
                 "F" #'+emacs-lisp/edebug-instrument-defun-off)
        (:prefix ("e" . "eval")
                 "b" #'eval-buffer
                 "d" #'eval-defun
                 "e" #'eval-last-sexp
                 "r" #'eval-region
                 "l" #'load-library)
        (:prefix ("g" . "goto")
                 "f" #'find-function
                 "v" #'find-variable
                 "l" #'find-library)
        (:prefix ("m" . "macros")
                 "e" #'macrostep-expand
                 "c" #'macrostep-collapse
                 "n" #'macrostep-next-macro
                 "N" #'macrostep-prev-macro
                 "q" #'macrostep-collapse-all)))

(use-package! ielm
  :defer t
  :config
  (eval-when! (modulep! :tools lookup)
    (set-lookup-handlers! 'inferior-emacs-lisp-mode
                          :definition    #'+emacs-lisp-lookup-definition
                          :documentation #'+emacs-lisp-lookup-documentation))

  ;; Adapted from http://www.modernemacs.com/post/comint-highlighting/ to add
  ;; syntax highlighting to ielm REPLs.
  (setq ielm-font-lock-keywords
        (append '(("\\(^\\*\\*\\*[^*]+\\*\\*\\*\\)\\(.*$\\)"
                   (1 font-lock-comment-face)
                   (2 font-lock-constant-face)))
                (when (require 'highlight-numbers nil t)
                  (highlight-numbers--get-regexp-for-mode 'emacs-lisp-mode))
                (cl-loop for (matcher . match-highlights)
                         in (append lisp-el-font-lock-keywords-2
                                    lisp-cl-font-lock-keywords-2)
                         collect
                         `((lambda (limit)
                             (when ,(if (symbolp matcher)
                                        `(,matcher limit)
                                      `(re-search-forward ,matcher limit t))
                               ;; Only highlight matches after the prompt
                               (> (match-beginning 0) (car comint-last-prompt))
                               ;; Make sure we're not in a comment or string
                               (let ((state (syntax-ppss)))
                                 (not (or (nth 3 state)
                                          (nth 4 state))))))
                           ,@match-highlights)))))


;;
;;; Packages

(use-package! elisp-demos
  :defer t
  :init
  (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package! macrostep
  :defer t
  :config
  (after! evil-collection-macrostep
    (map! :map macrostep-keymap
          :n "c" #'macrostep-collapse)))

(use-package! buttercup
  :defer t
  :minor ("/test[/-].+\\.el$" . buttercup-minor-mode)
  :preface
  ;; buttercup.el doesn't define a keymap for `buttercup-minor-mode', as we have
  ;; to fool its internal `define-minor-mode' call into thinking one exists, so
  ;; it will associate it with the mode.
  (defvar buttercup-minor-mode-map (make-sparse-keymap))
  :config
  (eval-when! (modulep! :ui popup)
    (set-popup-rule! "^\\*Buttercup\\*$" :size 0.45 :select nil :ttl 0))
  (eval-when! (modulep! :editor snippets)
    (set-yas-minor-mode! 'buttercup-minor-mode))
  (eval-when! (modulep! :editor evil)
    (add-hook 'buttercup-minor-mode-hook #'evil-normalize-keymaps))
  (map! :localleader
        :map buttercup-minor-mode-map
        :prefix "t"
        "t" #'+emacs-lisp/buttercup-run-file
        "a" #'+emacs-lisp/buttercup-run-project
        "s" #'buttercup-run-at-point))

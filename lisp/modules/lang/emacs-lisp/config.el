;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-

(defvar +emacs-lisp-enable-extra-fontification t
  "If non-nil, highlight special forms, and defined functions and
variables.")

(defvar +emacs-lisp-outline-regexp "[ \t]*;;;;* [^ \t\n]"
  "Regexp to use for `outline-regexp' in `emacs-lisp-mode'.
This marks a foldable marker for `outline-minor-mode' in elisp
buffers.")

(defvar +emacs-lisp-linter-warnings
  '(not free-vars    ; don't complain about unknown variables
    noruntime    ; don't complain about unknown function calls
    unresolved)  ; don't complain about undefined functions
  "The value for `byte-compile-warnings' in non-packages.

This reduces the verbosity of flycheck in Emacs configs and
scripts, which are so stateful that the deluge of false
positives (from the byte-compiler, package-lint, and checkdoc)
can be more overwhelming than helpful.

See `+emacs-lisp-non-package-mode' for details.")

(defvar +emacs-lisp-clone-emacs-C-src nil
  "If non-nil, prompt user to clone the Emacs source repository when
looking up a C function.")


;;
;;; Config

;; `elisp-mode' is loaded at startup. In order to lazy load its config we need
;; to pretend it isn't loaded
(defer-feature! elisp-mode emacs-lisp-mode)

(use-package! elisp-mode
  :config
  (let ((modes '(emacs-lisp-mode lisp-interaction-mode lisp-data-mode)))
    (set-repl-handler! modes #'+emacs-lisp/open-repl)
    (set-eval-handler! modes #'+emacs-lisp-eval)
    (set-lookup-handlers! `(,@modes helpful-mode)
      :definition    #'+emacs-lisp-lookup-definition
      :documentation #'+emacs-lisp-lookup-documentation)
    (set-docsets! modes "Emacs Lisp")

    (set-formatter! 'lisp-indent #'apheleia-indent-lisp-buffer :modes modes)
    (set-ligatures! modes :lambda "lambda")
    (set-rotate-patterns! modes
      :symbols '(("t" "nil")
                 ("let" "let*")
                 ("when" "unless")
                 ("append" "prepend")
                 ("advice-add" "advice-remove")
                 ("defadvice!" "undefadvice!")
                 ("add-hook" "remove-hook")
                 ("add-hook!" "remove-hook!")
                 ("it" "xit")
                 ("describe" "xdescribe"))))

  (setq-hook! 'emacs-lisp-mode-hook
    ;; Emacs' built-in elisp files use a hybrid tab->space indentation scheme
    ;; with a tab width of 8. Any smaller and the indentation will be
    ;; unreadable. Since Emacs' lisp indenter doesn't respect this variable it's
    ;; safe to ignore this setting otherwise.
    tab-width 8
    ;; Don't treat autoloads or sexp openers as outline headers, we have
    ;; hideshow for that.
    outline-regexp +emacs-lisp-outline-regexp
    outline-level #'+emacs-lisp-outline-level)

  ;; Fixed indenter that intends plists sensibly.
  (advice-add #'calculate-lisp-indent :override #'+emacs-lisp--calculate-lisp-indent-a)

  ;; variable-width indentation is superior in elisp. Otherwise, `dtrt-indent'
  ;; and `editorconfig' would force fixed indentation on elisp.
  (add-to-list 'zenit-detect-indentation-excluded-modes 'emacs-lisp-mode)

  (add-hook! '(emacs-lisp-mode-hook lisp-data-mode-local-vars-hook)
             ;; Allow folding of outlines in comments
             #'outline-minor-mode
             ;; Make parenthesis depth easier to distinguish at a glance
             #'rainbow-delimiters-mode
             ;; Make quoted symbols easier to distinguish from free variables
             #'highlight-quoted-mode
             ;; Extend imenu support to custom constructs
             #'+emacs-lisp-extend-imenu-h
             (+imenu-add-items
              '((+imenu-create-nested-index "^[ \t]*\\(?1:;;;+\\)\\**[ \t]+\\(?2:[^\n]+\\)" "Section")))
             ;; Ensure straight sees modifications to installed packages
             #'+emacs-lisp-init-straight-maybe-h)

  ;; Flycheck produces a *lot* of false positives in emacs configs, so disable
  ;; it when you're editing them
  (add-hook! '(flycheck-mode-hook flymake-mode-hook) #'+emacs-lisp-non-package-mode)

  (defadvice! +syntax--fix-elisp-flymake-load-path (orig-fn &rest args)
    "Set load path for elisp byte compilation Flymake backend"
    :around #'elisp-flymake-byte-compile
    (let ((elisp-flymake-byte-compile-load-path
           (append elisp-flymake-byte-compile-load-path load-path)))
      (apply orig-fn args)))

  ;; Special syntax highlighting for elisp
  (dolist (mode '(emacs-lisp-mode lisp-data-mode lisp-interaction-mode))
    (font-lock-add-keywords
     mode (append `(;; custom cookies
                    ("^;;;###\\(autodef\\|if\\|package\\)[ \n]" (1 font-lock-warning-face t)))
                  ;; highlight defined, special variables & functions
                  (when +emacs-lisp-enable-extra-fontification
                    `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face))))))

  (defadvice! +emacs-lisp-append-value-to-eldoc-a (fn sym)
    "Display variable value next to documentation in eldoc."
    :around #'elisp-get-var-docstring
    (when-let* ((ret (funcall fn sym)))
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
  (set-lookup-handlers! 'inferior-emacs-lisp-mode
    :definition    #'+emacs-lisp-lookup-definition
    :documentation #'+emacs-lisp-lookup-documentation)

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
  (set-tempel-minor-mode! 'buttercup-minor-mode)
  (eval-when! (modulep! :editor evil)
    (add-hook 'buttercup-minor-mode-hook #'evil-normalize-keymaps))
  (map! :localleader
        :map buttercup-minor-mode-map
        :prefix "t"
        "t" #'+emacs-lisp/buttercup-run-file
        "a" #'+emacs-lisp/buttercup-run-project
        "s" #'buttercup-run-at-point))


(use-package! helpful
  ;; a better *help* buffer
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)
  :init
  ;; Make `apropos' et co search more extensively. They're more useful this way.
  (setq apropos-do-all t)

  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)

  (defun zenit-use-helpful-a (fn &rest args)
    "Force FN to use helpful instead of the old describe-* commands."
    (letf! ((#'describe-function #'helpful-function)
            (#'describe-variable #'helpful-variable))
      (apply fn args)))

  (after! apropos
    ;; patch apropos buttons to call helpful instead of help
    (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
         (helpful-callable (button-get button 'apropos-symbol)))))
    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
         (helpful-variable (button-get button 'apropos-symbol))))))

  ;; DEPRECATED: Remove when support for 29 is dropped.
  (eval-when! (= emacs-major-version 29)
    (defadvice! zenit--find-function-search-for-symbol-save-excursion-a (fn &rest args)
      "Suppress cursor movement by `find-function-search-for-symbol'.

Addresses an unwanted side-effect in
`find-function-search-for-symbol' on Emacs 29 where the cursor is
moved to a variable's definition if it's defined in the current
buffer."
      :around #'find-function-search-for-symbol
      (let (buf pos)
        (letf! (defun find-library-name (library)
                 (let ((filename (funcall find-library-name library)))
                   (with-current-buffer (find-file-noselect filename)
                     (setq buf (current-buffer)
                           pos (point)))
                   filename))
          (prog1 (apply fn args)
            (when (buffer-live-p buf)
              (with-current-buffer buf (goto-char pos))))))))
  :config
  ;; REVIEW 2024-08-13: No idea why, but when you use `helpful' to find the
  ;;   definition of something, the major mode of the file is not triggered ...
  (defadvice! +helpful--run-auto-mode-a (&rest _)
    "Run `set-auto-mode' maunally after following a link from
`helpful'."
    :after #'helpful--navigate
    (set-auto-mode))

  (setq helpful-set-variable-function #'setq!)

  (when +emacs-lisp-clone-emacs-C-src
    ;; Standard location for the Emacs source code
    (setq source-directory (file-name-concat zenit-data-dir "src/"))

    ;; This is initialized to nil by `find-func' if the source is not cloned
    ;; when the library is loaded
    (setq find-function-C-source-directory
          (expand-file-name "src" source-directory))

    (defun zenit--clone-emacs-source-maybe ()
      "Prompt user to clone Emacs source repository if needed."
      (when (and (not (file-directory-p source-directory))
                 (not (get-buffer "*zenit clone emacs src*"))
                 (yes-or-no-p "Clone Emacs source repository? "))
        (make-directory (file-name-directory source-directory) 'parents)
        (let ((branch (concat "emacs-" (prin1-to-string emacs-major-version)))
              (compilation-buffer-name-function
               (lambda (&rest _)
                 "*zenit clone emacs src*")))
          (save-current-buffer
            (compile
             (format
              "git clone -b %s --depth 1 https://github.com/emacs-mirror/emacs.git %s"
              (shell-quote-argument branch)
              (shell-quote-argument source-directory)))))))

    (after! find-func
      (defadvice! +find-func--clone-emacs-source-a (&rest _)
        "Clone Emacs source if needed to view definition."
        :before #'find-function-C-source
        (zenit--clone-emacs-source-maybe)))

    (defadvice! +helpful--clone-emacs-source-a (library-name)
      "Prompt user to clone Emacs source code when looking up functions.
Otherwise, it only happens when looking up variables, for some
bizarre reason."
      :before #'helpful--library-path
      (when (member (file-name-extension library-name) '("c" "rs"))
        (zenit--clone-emacs-source-maybe))))

  ;; Open help:* links with helpful-* instead of describe-*
  (advice-add #'org-link--open-help :around #'zenit-use-helpful-a)

  (map! :map helpful-mode-map
        :ng "o"  #'link-hint-open-link
        :n  "gr" #'helpful-update))

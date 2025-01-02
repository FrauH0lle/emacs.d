;; lang/sh/config.el -*- lexical-binding: t; -*-


(defvar +sh-builtin-keywords
  '("cat" "cd" "chmod" "chown" "cp" "curl" "date" "echo" "find" "git" "grep"
    "kill" "less" "ln" "ls" "make" "mkdir" "mv" "pgrep" "pkill" "pwd" "rm"
    "sleep" "sudo" "touch")
  "A list of common shell commands to be fontified especially in `sh-mode'.")


;;
;;; Packages

(use-package! sh-script ; built-in
  :defer t
  :mode ("\\.bats\\'" . sh-mode)
  :mode ("\\.\\(?:zunit\\|env\\)\\'" . sh-mode)
  :mode ("/bspwmrc\\'" . sh-mode)
  :config
  ;; (set-docsets! 'sh-mode "Bash")
  (set-electric! 'sh-mode :words '("else" "elif" "fi" "done" "then" "do" "esac" ";;"))
  (set-repl-handler! 'sh-mode #'+sh/open-repl)
  ;; (set-lookup-handlers! 'sh-mode :documentation #'+sh-lookup-documentation-handler)
  (set-ligatures! 'sh-mode
    ;; Functional
    :def "function"
    ;; Types
    :true "true" :false "false"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    :in "in"
    :for "for"
    :return "return"
    ;; Other
    :dot "." :dot "source")

  (eval-when! (modulep! +lsp)
    (add-hook 'sh-mode-local-vars-hook #'lsp! 'append))

  (eval-when! (modulep! +tree-sitter)
    (add-hook 'sh-mode-local-vars-hook #'tree-sitter! 'append))

  (setq-hook! 'sh-mode-hook
    sh-indentation 2
    sh-basic-offset 2)
  (setq sh-indent-after-continuation 'always)

  ;; recognize function names with dashes in them
  (add-to-list 'sh-imenu-generic-expression
               '(sh (nil "^\\s-*function\\s-+\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?" 1)
                    (nil "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()" 1)))

  ;; `sh-set-shell' is chatty about setting up indentation rules
  (advice-add #'sh-set-shell :around #'zenit-shut-up-a)

  ;; 1. Fontifies variables in double quotes
  ;; 2. Fontify command substitution in double quotes
  ;; 3. Fontify built-in/common commands (see `+sh-builtin-keywords')
  (add-hook! 'sh-mode-hook
    (defun +sh-init-extra-fontification-h ()
      (font-lock-add-keywords
       nil `((+sh--match-variables-in-quotes
              (1 'font-lock-constant-face prepend)
              (2 'font-lock-variable-name-face prepend))
             (+sh--match-command-subst-in-quotes
              (1 'sh-quoted-exec prepend))
             (,(regexp-opt +sh-builtin-keywords 'symbols)
              (0 'font-lock-type-face append))))))
  ;; 4. Fontify delimiters by depth
  (add-hook 'sh-mode-hook #'rainbow-delimiters-mode)

  ;; autoclose backticks
  (sp-local-pair 'sh-mode "`" "`" :unless '(sp-point-before-word-p sp-point-before-same-p))

  (map! :localleader
          :map sh-mode-map
          "\\" 'sh-backslash-region
          (:prefix ("i" . "insert")
           "c" 'sh-case
           "i" 'sh-if
           "f" 'sh-function
           "o" 'sh-for
           "e" 'sh-indexed-loop
           "w" 'sh-while
           "r" 'sh-repeat
           "s" 'sh-select
           "u" 'sh-until
           "g" 'sh-while-getopts)))


(use-package! powershell
  :when (modulep! +powershell)
  :defer t
  :config
  (when (modulep! +lsp)
    (add-hook 'powershell-mode-local-vars-hook #'lsp! 'append)))

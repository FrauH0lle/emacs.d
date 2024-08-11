;; lang/ess/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "DESCRIPTION"))

(defvar +ess-R-remote-host nil
  "Host to connect to, given as a string.
Should be set in a local variable.")

(defvar +ess-R-remote-session nil
  "Name of the R session to be created, given as a string.
Should be set in a local variable.")

(defvar +ess-R-remote-cmds nil
  "Commands to be executed on the host before R is launched,
given as a list of strings. Should be set in a local
variable.")


;;
;;; Packages


;; PATCH 2024-08-02: `xterm-color'
(el-patch-feature xterm-color)
(compile-along! "patches/xterm-color")

(use-package! xterm-color
  :defer t
  :init
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  ;; Fix wrong broken color ouput in comint buffer. See
  ;; https://github.com/emacs-ess/ESS/issues/1193
  (add-hook 'inferior-ess-mode-hook
            (lambda () (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter nil t)))
  :config
  (load! "patches/xterm-color")
  (setq! xterm-color-use-bold t))

(use-package! ess
  :mode (("\\.sp\\'"           . S-mode)
         ("/R/.*\\.q\\'"       . R-mode)
         ("\\.[qsS]\\'"        . S-mode)
         ("\\.[Ss][s][c]\\'"   . S-mode)
         ("\\.SSC\\'"          . S-mode)
         ("\\.[rR]\\'"         . R-mode)
         ("\\.[rR]nw\\'"       . poly-noweb+r-mode)
         ("\\.[rR]md\\'"       . poly-markdown+r-mode)
         ("\\.[sS]nw\\'"       . poly-noweb+r-mode)
         ("\\.[rR]profile\\'"  . R-mode)
         ("NAMESPACE\\'"       . R-mode)
         ("CITATION\\'"        . R-mode)
         ("\\.omg\\'"          . omegahat-mode)
         ("\\.hat\\'"          . omegahat-mode)
         ("\\.lsp\\'"          . XLS-mode)
         ("\\.do\\'"           . STA-mode)
         ("\\.ado\\'"          . STA-mode)
         ("\\.[Ss][Aa][Ss]\\'" . SAS-mode)
         ("\\.[Ss]t\\'"        . S-transcript-mode)
         ("\\.Sout"            . S-transcript-mode)
         ("\\.[Rr]out"         . R-transcript-mode)
         ("\\.Rd\\'"           . Rd-mode)
         ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
         ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
  :commands (R stata julia SAS ess-julia-mode)
  :init
  ;; Support Juila only if no dedicated module is used.
  (eval-unless! (modulep! :lang julia)
    (add-to-list 'auto-mode-alist '("\\.[Jj][Ll]\\'" . ess-julia-mode)))
  ;; Tree-sitter support
  (eval-when! (modulep! +tree-sitter)
    (add-hook 'ess-r-mode-local-vars-hook #'tree-sitter! 'append))
  :config
  (setq ess-offset-continued 'straight
        ess-use-flymake (not (modulep! :checkers syntax))
        ess-nuke-trailing-whitespace-p t
        ess-style 'DEFAULT
        ess-history-directory (expand-file-name "ess-history/" zenit-cache-dir))

  ;; Set fontification
  ;; ESS buffer
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:keywords   . t)
          (ess-R-fl-keyword:constants  . t)
          (ess-R-fl-keyword:modifiers  . t)
          (ess-R-fl-keyword:fun-defs   . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:%op%       . t)
          (ess-fl-keyword:fun-calls    . t)
          (ess-fl-keyword:numbers      . t)
          (ess-fl-keyword:operators    . t)
          (ess-fl-keyword:delimiters   . t)
          (ess-fl-keyword:=            . t)
          (ess-R-fl-keyword:F&T        . t)))

  ;; iESS buffer
  (setq inferior-R-font-lock-keywords
        '((ess-S-fl-keyword:prompt      . t)
          (ess-R-fl-keyword:keywords    . t)
          (ess-R-fl-keyword:constants   . t)
          (ess-R-fl-keyword:modifiers   . t)
          (ess-R-fl-keyword:messages    . t)
          (ess-R-fl-keyword:fun-defs    . t)
          (ess-R-fl-keyword:assign-ops  . t)
          (ess-fl-keyword:matrix-labels . t)
          (ess-fl-keyword:fun-calls     . t)
          (ess-fl-keyword:numbers       . t)
          (ess-fl-keyword:operators     . t)
          (ess-fl-keyword:delimiters    . t)
          (ess-fl-keyword:=             . t)
          (ess-R-fl-keyword:F&T         . t)))

  (eval-when! (modulep! :tools lookup)
    (set-docsets! 'ess-r-mode :docsets "R")
    (set-lookup-handlers! '(ess-r-mode ess-julia-mode)
                          :documentation #'ess-display-help-on-object))
  (eval-when! (modulep! :tools eval)
    (set-repl-handler! 'ess-r-mode #'+ess/open-r-repl)
    (set-repl-handler! 'ess-julia-mode #'+ess/open-julia-repl)
    (set-eval-handler! 'ess-help-mode #'ess-eval-region-and-go)
    (set-eval-handler! 'ess-r-help-mode #'ess-eval-region-and-go))

  (eval-when! (modulep! :editor evil)
    (set-evil-initial-state! 'ess-r-help-mode 'normal))

  ;; HACK If `+default-want-RET-continue-comments' is true, comments are
  ;;      continued on RET. But ess-r-mode doesn't have a sane
  ;;      `comment-line-break-function', so...
  (setq-hook! 'ess-r-mode-hook
    comment-line-break-function nil)

  ;; LSP
  (eval-when! (and (modulep! :tools lsp)
                   (modulep! :lang ess +lsp))
    (add-hook! 'ess-r-mode-local-vars-hook
      (defun +ess-lsp-init-maybe-h ()
        "Use LSP mode if the buffer is not a remote."
        (unless (file-remote-p default-directory)
          (lsp!)))))

  ;; Popup rules
  (eval-when! (modulep! :ui popup)
    (after! ess-r-mode
      (set-popup-rule! "^\\*R" :side 'bottom :height 0.33 :width 0.5 :quit nil)
      (set-popup-rule! "^\\*R dired*" :side 'right :size 0.25 :height 0.5 :vslot 99 :slot 1
        :select nil :quit nil))
    (after! ess-help
      (set-popup-rule! "^\\*help.R.*" :side 'right :size 0.25 :height 0.5 :vslot 100 :slot 1
        :select t :quit t :transient t)))

  ;; Workspaces integration
  (eval-when! (modulep! :ui workspaces)
    (after! ess-r-mode
      (defun +ess-r-bookmark-make-record ()
        "Create a bookmark for the current iESS buffer."
        `(,(format "ess-r-%s"
                   (file-name-nondirectory
                    (directory-file-name
                     (file-name-directory default-directory))))
          (location . ,default-directory)
          (ess-r-wd . ,(ess-get-working-directory t))
          (handler . +ess-r-bookmark-jump)))

      (defun +ess-r-bookmark-jump (bookmark)
        "Default bookmark handler for iESS buffers."
        (let ((ess-ask-for-ess-directory nil)
              (default-directory (bookmark-prop-get bookmark 'location))
              (wd (bookmark-prop-get bookmark 'ess-r-wd)))
          (R)
          (ess-set-working-directory wd t)))

      (add-hook! 'inferior-ess-r-mode-hook
        (setq-local bookmark-make-record-function #'+ess-r-bookmark-make-record))))

  ;; REPL
  ;; Use smartparens in iESS
  (add-hook! 'inferior-ess-mode-hook #'smartparens-mode)

  ;; Use evil insert state in iEES
  (eval-when! (modulep! :editor evil)
    (after! ess-r-mode
      (set-evil-initial-state! 'inferior-ess-mode 'insert)))

  (add-hook! 'inferior-ess-mode-hook
    (defun +ess-fix-read-only-inferior-ess-mode-h ()
      "Fixes a bug when `comint-prompt-read-only' in non-nil.
See URL `https://github.com/emacs-ess/ESS/issues/300'."
      (setq-local comint-use-prompt-regexp nil)
      (setq-local inhibit-field-text-motion nil)))

  ;; Make the REPL buffer more responsive.
  (setq-hook! 'inferior-ess-mode-hook
    comint-scroll-to-bottom-on-input t
    comint-scroll-to-bottom-on-output t
    comint-move-point-for-output t)

  (add-hook! 'zenit-real-buffer-functions
    (defun +ess-inferior-buffer-p (buf)
      "Returns non-nil if BUF is a `inferior-ess' buffer."
      (with-current-buffer buf (derived-mode-p 'inferior-ess-mode))))

  ;; Save history when killing the ess inferior buffer. See
  ;; https://github.com/emacs-ess/ESS/issues/970
  (defun +ess-kill-proc-before-buffer-h ()
    ;; The .Rhistory file needs to exist beforehand
    (unless (file-exists-p comint-input-ring-file-name)
      (make-empty-file comint-input-ring-file-name))
    (let ((proc (get-buffer-process (current-buffer))))
      (when (processp proc)
        (and (derived-mode-p 'comint-mode)
             (comint-write-input-ring))
        (delete-process proc))))

  (defun +ess-run-kill-proc-maybe-h ()
    (ignore-errors
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (and (derived-mode-p 'inferior-ess-mode)
               (+ess-kill-proc-before-buffer-h))))))

  (defun +ess-comint-h ()
    (add-hook 'kill-buffer-hook #'+ess-kill-proc-before-buffer-h nil t)
    (add-hook 'kill-emacs-hook #'+ess-run-kill-proc-maybe-h nil))

  (add-hook 'comint-mode-hook #'+ess-comint-h)

  ;; PATCH 2024-08-02: `ess-rdired'
  (compile-along! "patches/ess-rdired")
  (el-patch-feature ess-rdired)

  (after! ess-rdired
    (load! "patches/ess-rdired"))

  ;; Keybinds
  (map!
   ;; REPL
   (:map inferior-ess-mode-map
    :i "C->" (cmd! (insert " %>% "))
    :i "M--" #'ess-cycle-assign
    (:when (modulep! :completion vertico)
      :i "C-r" #'consult-history)
    :n "RET" #'+ess/goto-end-of-prompt)
   (:map ess-mode-map
    :n [C-return] #'ess-eval-line-and-step
    :i "M--" #'ess-cycle-assign
    :i "C->" (cmd! (insert " %>% ")))
   (:after ess-help
           (:map ess-help-mode-map
            :n "q"  #'kill-current-buffer
            :n "Q"  #'ess-kill-buffer-and-go
            :n "K"  #'ess-display-help-on-object
            :n "go" #'ess-display-help-in-browser
            :n "gO" #'ess-display-help-apropos
            :n "gv" #'ess-display-vignettes
            :m "]]" #'ess-skip-to-next-section
            :m "[[" #'ess-skip-to-previous-section)
           (:map ess-doc-map
                 "h"    #'ess-display-help-on-object
                 "p"    #'ess-R-dv-pprint
                 "t"    #'ess-R-dv-ctable
                 [up]   #'comint-next-input
                 [down] #'comint-previous-input
                 [C-return] #'ess-eval-line))
   (:localleader
    :map ess-mode-map
    "," #'ess-eval-region-or-function-or-paragraph-and-step
    "'" #'R
    "s" #'ess-switch-to-inferior-or-script-buffer
    "S" #'ess-switch-process
    "B" #'ess-eval-buffer-and-go
    "b" #'ess-eval-buffer
    "d" #'ess-eval-region-or-line-and-step
    "D" #'ess-eval-function-or-paragraph-and-step
    "L" #'ess-eval-line-and-go
    "l" #'ess-eval-line
    "R" #'ess-eval-region-and-go
    "r" #'ess-eval-region
    "F" #'ess-eval-function-and-go
    "f" #'ess-eval-function
    ;; predefined keymaps
    "h" #'ess-doc-map
    "x" #'ess-extra-map
    "p" #'ess-r-package-dev-map
    "v" #'ess-dev-map
    ;; noweb
    (:prefix ("c" . "noweb")
             "C" #'ess-eval-chunk-and-go
             "c" #'ess-eval-chunk
             "d" #'ess-eval-chunk-and-step
             "m" #'ess-noweb-mark-chunk
             "N" #'ess-noweb-previous-chunk
             "n" #'ess-noweb-next-chunk))))


(use-package! ess-R-data-view
  :commands
  ess-R-dv-ctable
  ess-R-dv-pprint
  :config
  (map!
   :map ess-doc-map
   "h" #'ess-display-help-on-object
   "p" #'ess-R-dv-pprint
   "t" #'ess-R-dv-ctable))


(use-package! poly-R
  :mode ("\\.[rR]md$" . poly-markdown+r-mode)
  :config
  (eval-when! (modulep! :editor snippets)
    (set-tempel-minor-mode! 'poly-markdown+r-mode)))

;; editor/fold/config.el -*- lexical-binding: t; -*-

(defcustom +fold-ellipsis " [...] "
  "The ellipsis to show for ellided regions (folds).

`org-ellipsis', `truncate-string-ellipsis', and
`ts-fold-replacement' are set to this."
  :type 'string
  :group '+fold)

(defface +fold-hideshow-folded-face
  `((t (:inherit font-lock-comment-face :weight light)))
  "Face to hightlight `hideshow' overlays."
  :group 'zenit-themes)


;;
;;; Global config

(eval-when! (modulep! :editor evil)
  (after! evil
    ;; Add vimish-fold, outline-mode & hideshow support to folding commands
    (define-key! 'global
      [remap evil-toggle-fold]   #'+fold/toggle
      [remap evil-close-fold]    #'+fold/close
      [remap evil-open-fold]     #'+fold/open
      [remap evil-open-fold-rec] #'+fold/open-rec
      [remap evil-close-folds]   #'+fold/close-all
      [remap evil-open-folds]    #'+fold/open-all)
    (evil-define-key* 'motion 'global
      "zj" #'+fold/next
      "zk" #'+fold/previous
      "zf" #'evil-vimish-fold/create
      "zF" #'evil-vimish-fold/create-line
      "zd" #'vimish-fold-delete
      "zE" #'vimish-fold-delete-all)))

(after! org
  (setq org-ellipsis +fold-ellipsis))

(after! mule-util
  (setq truncate-string-ellipsis +fold-ellipsis))


;;
;;; Packages

(use-package! hideshow ; built-in
  :commands (hs-toggle-hiding
             hs-hide-block
             hs-hide-level
             hs-show-all
             hs-hide-all)
  :config
  (setq hs-hide-comments-when-hiding-all nil
        ;; Nicer code-folding overlays (with fringe indicators)
        hs-set-up-overlay #'+fold-hideshow-set-up-overlay-fn)

  (defadvice! +fold--hideshow-ensure-mode-a (&rest _)
    "Ensure `hs-minor-mode' is enabled when we need it, no sooner or
later."
    :before '(hs-toggle-hiding hs-hide-block hs-hide-level hs-show-all hs-hide-all)
    (+fold--ensure-hideshow-mode))

  ;; extra folding support for more languages
  (unless (assq 't hs-special-modes-alist)
    (setq hs-special-modes-alist
          (append
           '((vimrc-mode "{{{" "}}}" "\"")
             (yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>"
                        ""
                        "#"
                        +fold-hideshow-forward-block-by-indent-fn nil)
             (haml-mode "[#.%]" "\n" "/" +fold-hideshow-haml-forward-sexp-fn nil)
             (ruby-mode "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]"
                        "end\\|[]}]"
                        "#\\|=begin"
                        ruby-forward-sexp)
             (matlab-mode "if\\|switch\\|case\\|otherwise\\|while\\|for\\|try\\|catch"
                          "end"
                          nil (lambda (_arg) (matlab-forward-sexp)))
             (nxml-mode "<!--\\|<[^/>]*[^/]>"
                        "-->\\|</[^/>]*[^/]>"
                        "<!--" sgml-skip-tag-forward nil)
             (latex-mode
              ;; LaTeX-find-matching-end needs to be inside the env
              ("\\\\begin{[a-zA-Z*]+}\\(\\)" 1)
              "\\\\end{[a-zA-Z*]+}"
              "%"
              (lambda (_arg)
                ;; Don't fold whole document, that's useless
                (unless (save-excursion
                          (search-backward "\\begin{document}"
                                           (line-beginning-position) t))
                  (LaTeX-find-matching-end)))
              nil))
           hs-special-modes-alist
           '((t))))))


(use-package! evil-vimish-fold
  :when (modulep! :editor evil)
  :commands (evil-vimish-fold/next-fold evil-vimish-fold/previous-fold
                                        evil-vimish-fold/delete evil-vimish-fold/delete-all
                                        evil-vimish-fold/create evil-vimish-fold/create-line)
  :init
  (setq vimish-fold-dir (concat zenit-cache-dir "vimish-fold/")
        vimish-fold-indication-mode 'right-fringe)
  :config
  (vimish-fold-global-mode +1))


(use-package! ts-fold
  :when (modulep! :tools tree-sitter)
  :after tree-sitter
  :config
  (setq ts-fold-replacement +fold-ellipsis)
  (global-ts-fold-mode +1))


;; PATCH 2025-02-10: `outline-minor-faces'
(el-patch-feature outline-minor-faces)
(compile-along! "patches")
(use-package! outline-minor-faces
  :hook (outline-minor-mode . outline-minor-faces-mode)
  :config
  (load! "patches"))

(use-package! backline
  :after outline
  :config
  (advice-add #'outline-flag-region :after #'backline-update))

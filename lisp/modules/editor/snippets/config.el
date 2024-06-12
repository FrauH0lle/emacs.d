;; editor/snippets/config.el -*- lexical-binding: t; -*-

(defvar +snippets-dir
  (file-name-concat zenit-emacs-dir "templates" "snippets")
  "Directory where `tempel' will search for built-in snippets.")

(defvar +snippets-local-dir
  (file-name-concat zenit-local-conf-dir "templates" "snippets")
  "Directory where `tempel' will search for local snippets.")

(defvar +snippets-dirs
  (list +snippets-dir +snippets-local-dir)
  "Directories where `tempel' will search for snippets.
Defaults to the folders templates/snippets/ located in
`zenit-emacs-dir' and `zenit-local-conf-dir'. Templates defined
in `zenit-local-conf-dir' take precedence.")

(defvar-local +snippets--extra-modes nil
  "Stores minor modes which are eligible for templates.")

;;
;;; Packages

(use-package! tempel
  :defer t
  :config/el-patch
  ;; PATCH Allow specified minor modes to match the buffer local variable
  ;;   `+snippets--extra-modes'
  (defun tempel--condition-p (modes plist)
  "Return non-nil if one of MODES matches and the PLIST condition is satisfied."
  (and
   (cl-loop
    for m in modes thereis
    (or (eq m #'fundamental-mode)
        (el-patch-add
          (and (bound-and-true-p +snippets--extra-modes)
               (memq m +snippets--extra-modes)))
        (derived-mode-p m)
        (when-let ((remap (alist-get m (bound-and-true-p major-mode-remap-alist))))
          (derived-mode-p remap))))
   (or (not (plist-member plist :when))
       (save-excursion
         (save-restriction
           (save-match-data
             (eval (plist-get plist :when) 'lexical)))))))

  ;; PATCH Ask user to choose if a LSP snippet offers choices. This is pretty
  ;;   hacky and there should be a better way. See
  ;;   https://github.com/minad/tempel/issues/105
  (defun tempel--element (st region elt)
    "Add template ELT to ST given the REGION."
    (pcase elt
      ('nil)
      ('n (insert "\n"))
      ;; `indent-according-to-mode' fails sometimes in Org. Ignore errors.
      ('n> (insert "\n") (tempel--protect (indent-according-to-mode)))
      ('> (tempel--protect (indent-according-to-mode)))
      ((pred stringp) (insert elt))
      ('& (unless (or (bolp) (save-excursion (re-search-backward "^\\s-*\\=" nil t)))
            (insert "\n")))
      ('% (unless (or (eolp) (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
            (insert "\n")))
      ('o (unless (or (eolp) (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
            (open-line 1)))
      (`(s ,name) (tempel--field st name))
      (`(l . ,lst) (dolist (e lst) (tempel--element st region e)))
      ((or 'p `(,(or 'p 'P) . ,rest)) (apply #'tempel--placeholder st rest))
      ((or 'r 'r> `(,(or 'r 'r>) . ,rest))
       (if (not region)
           (when-let ((ov (apply #'tempel--placeholder st rest))
                      ((not rest)))
             (overlay-put ov 'tempel--enter #'tempel--done))
         (goto-char (cdr region))
         (when (eq (or (car-safe elt) elt) 'r>)
           (indent-region (car region) (cdr region) nil))))
      ;; TEMPEL EXTENSION: Quit template immediately
      ('q (overlay-put (tempel--field st) 'tempel--enter #'tempel--done))
      (el-patch-add
        (`(lsp-choice ,choices ,name)
         (overlay-put (tempel--placeholder st "CHOICES") 'tempel--enter
                      (lambda (&optional ov)
                        (tempel--element st region (let ((tempel--active nil))
                                                     (completing-read "Choose: " choices)))))))
      (_ (if-let ((ret (run-hook-with-args-until-success 'tempel-user-elements elt)))
             (tempel--element st region ret)
           ;; TEMPEL EXTENSION: Evaluate forms
           (tempel--form st elt)))))
  :config
  ;; Add snippet libraries
  (setq tempel-path (zenit-files-in +snippets-dirs :match "\\.eld$"))

  (defadvice! +tempel-update-tempel-path (&rest _)
    "Updates `tempel-path'."
    :before #'tempel-insert
    :before #'tempel-complete
    :before #'tempel-expand
    (unless (bound-and-true-p +file-templates--expanding-p)
      (setq tempel-path (zenit-files-in +snippets-dirs :match "\\.eld$"))))

  ;; Register `def-project-mode!' modes with yasnippet. This enables project
  ;; specific snippet libraries (e.g. for Laravel, React or Jekyll projects).
  (add-hook 'zenit-project-hook #'+snippets-enable-project-modes-h)

  ;; Exit snippets on ESC from normal mode
  (add-hook 'zenit-escape-hook #'tempel-abort)

  ;; REVIEW Maybe this is not needed
  (after! smartparens
    ;; Tell smartparens overlays not to interfere
    (advice-add #'tempel-expand :before #'sp-remove-active-pair-overlay))

  ;; Keybinds
  (map! :map tempel-map
        "<tab>"     #'tempel-next
        "TAB"       #'tempel-next
        "<backtab>" #'tempel-previous
        "C-g"       #'tempel-abort
        "C-<home>"  #'tempel-beginning
        "C-<end>"   #'tempel-end))

;; HACK This is rather hacky ...
(use-package! lsp-snippet
  :defer t
  :config/el-patch
  ;; PATCH Clumsy approach to fix
  ;;   https://github.com/svaante/lsp-snippet/issues/5
  (defun lsp-snippet--escaped-string-to (stop-at)
    (let ((start (lsp-snippet--index))
          (continue t)
          (el-patch-add
            (result "")))
      (while continue
        ;; Skip next token if `backslash'
        (when (lsp-snippet--take '(backslash))
          ;; Skip next token
          (lsp-snippet--ignore '(eof))
          (el-patch-add
            (setq result (concat result (substring lsp-snippet--str (1+ start) (lsp-snippet--index))))
            (setq start (lsp-snippet--index))))
        (setq continue (lsp-snippet--ignore
                        `(eof . ,stop-at)))
        (el-patch-add
          (setq result (concat result (substring lsp-snippet--str start (lsp-snippet--index))))
          (setq start (lsp-snippet--index))))
      (el-patch-swap
        (unless (eq start (lsp-snippet--index))
          (substring lsp-snippet--str start (lsp-snippet--index)))
        (when (length> result 0)
          result)))))

(use-package! lsp-snippet-tempel
  :hook (lsp-mode . lsp-snippet-tempel-lsp-mode-init)
  :init
  (with-eval-after-load 'lsp-mode
    ;; Fool `lsp-mode'
    (defadvice! +snippets-fake-yas-minor-mode (fn &rest args)
      :around #'lsp--client-capabilities
      :around #'lsp-register-client
      (if (fboundp 'yas-minor-mode)
          (apply fn args)
        (letf! ((#'yas-minor-mode #'ignore))
          (apply fn args)))))
  :config/el-patch
  ;; PATCH `lsp-mode' now checks if `yas-minor-mode' is bound
  (defun lsp-snippet-tempel-lsp-mode-init ()
    (lsp-snippet-tempel--init)
    (advice-add 'lsp--expand-snippet :override #'lsp-snippet-tempel--lsp-mode-expand-snippet)
    ;; HACK `lsp-mode' enables snippet based on `(feature 'yasnippet)'
    (el-patch-remove
      (provide 'yasnippet)))

  ;; PATCH Return custom \\='lsp-choice element for `tempel--element'
  (defun lsp-snippet-tempel--choice-fn (number choices)
    (el-patch-swap
      (lsp-snippet-tempel--placeholder-fn number (string-join choices ","))
      (let ((sym (intern (format "tabstop-%d" number))))
        `((lsp-choice ,choices ,sym))))))

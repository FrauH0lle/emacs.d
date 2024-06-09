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
  ;; `+snippets--extra-modes'
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

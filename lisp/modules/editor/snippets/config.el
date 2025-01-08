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

;; PATCH 2024-08-02: `tempel'
(compile-along! "patches/tempel")
(el-patch-feature tempel)

(use-package! tempel
  :defer t
  :config
  (load! "patches/tempel")

  ;; Add snippet libraries
  (setq tempel-path (zenit-files-in +snippets-dirs :match "\\.eld$"))

  ;; Custom elements
  ;; Include templates in templates by name
  (defun +tempel-include (elt)
    (when (eq (car-safe elt) 'i)
      (if-let* ((template (alist-get (cadr elt) (tempel--templates))))
          (cons 'l template)
        (message "Template %s not found" (cadr elt))
        nil)))
  (add-to-list 'tempel-user-elements #'+tempel-include)

  ;; REVIEW 2024-06-13: Maybe there is a better way to do this less frequently.
  ;;   However, I am not sure if this is really expensive.
  (defadvice! +tempel-update-tempel-path-a (&rest _)
    "Update `tempel-path'.

As the load order of multiple template files matters (first takes
precedence), this function ensures the correct order of all
template files."
    :before #'tempel-insert
    :before #'tempel-complete
    :before #'tempel-expand
    (unless (bound-and-true-p +file-templates--expanding-p)
      (let* ((modes (mapcar #'symbol-name
                            (append
                             ;; `fundamental-mode' should always be included
                             (ensure-list 'fundamental-mode)
                             ;; Get `major-mode' and parents
                             (parent-mode-list major-mode)
                             ;; Add `+snippets--extra-modes' if present
                             (ensure-list +snippets--extra-modes))))
             (files (zenit-files-in +snippets-dirs :match "\\.eld$"))
             (files-base (mapcar #'file-name-base files))
             snippet-files)
        (dolist (mode modes snippet-files)
          (when-let* ((member-p (member mode files-base))
                      (idx (seq-position files-base mode)))
            (push (nth idx files) snippet-files)))
        (setq tempel-path snippet-files))))

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
;; PATCH 2024-08-02: `lsp-snippet'
(compile-along! "patches/lsp-snippet")
(el-patch-feature lsp-snippet)

(after! lsp-snippet
  (load! "patches/lsp-snippet"))


;; PATCH 2024-08-02: `lsp-snippet-tempel'
(compile-along! "patches/lsp-snippet-tempel")
(el-patch-feature lsp-snippet-tempel)

(use-package! lsp-snippet-tempel
  :hook (lsp-mode . lsp-snippet-tempel-lsp-mode-init)
  :init
  (after! lsp-mode
    ;; Fool `lsp-mode'
    (defadvice! +snippets-fake-yas-minor-mode (fn &rest args)
      :around #'lsp--client-capabilities
      :around #'lsp-register-client
      (if (fboundp 'yas-minor-mode)
          (apply fn args)
        (letf! ((#'yas-minor-mode #'ignore))
          (apply fn args)))))
  :config
  (load! "patches/lsp-snippet-tempel"))

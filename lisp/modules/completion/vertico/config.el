;; completion/vertico/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(use-package! vertico
  :hook (zenit-first-input . vertico-mode)
  :init
  (defadvice! +vertico-crm-indicatora-a (args)
    :filter-args #'completing-read-multiple
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  :config
  (setq vertico-count 17
        vertico-cycle t)
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))

  (map! :when (modulep! :editor evil)
        :map vertico-map
        "M-RET" #'vertico-exit-input
        "C-SPC" #'+vertico/embark-preview
        "C-j"   #'vertico-next
        "C-M-j" #'vertico-next-group
        "C-k"   #'vertico-previous
        "C-M-k" #'vertico-previous-group
        "C-h" (general-predicate-dispatch nil (eq 'file (vertico--metadata-get 'category)) #'vertico-directory-up)
        "C-l" (general-predicate-dispatch nil (eq 'file (vertico--metadata-get 'category)) #'+vertico/enter-or-preview))

  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (map! :map vertico-map "DEL" #'vertico-directory-delete-char)

  ;; These commands are problematic and automatically show the *Completions* buffer
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  (defadvice! +vertico--suppress-completion-help-a (fn &rest args)
    :around #'ffap-menu-ask
    (letf! ((#'minibuffer-completion-help #'ignore))
      (apply fn args))))


(use-package! orderless
  :after-call zenit-first-input-hook
  :config
  (setq orderless-affix-dispatch-alist
        '((?! . orderless-without-literal)
          (?& . orderless-annotation)
          (?% . char-fold-to-regexp)
          (?` . orderless-initialism)
          (?= . orderless-literal)
          (?^ . orderless-literal-prefix)
          (?~ . orderless-flex))
        orderless-style-dispatchers
        '(+vertico-orderless-dispatch
          +vertico-orderless-disambiguation-dispatch))

  (add-to-list
   'completion-styles-alist
   '(+vertico-basic-remote-try-completion
     +vertico-basic-remote-all-completions
     "Use basic completion on remote files only"))
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; note that despite override in the name orderless can still be used in
        ;; find-file etc.
        completion-category-overrides '((file (styles orderless partial-completion)))
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-component-separator #'orderless-escapable-split-on-space)
  ;; ...otherwise find-file gets different highlighting than other commands
  (set-face-attribute 'completions-first-difference nil :inherit nil))


;; PATCH 2024-09-04: `consult'
(compile-along! "patches/workspaces.el")
(el-patch-feature consult)

(use-package! consult
  :defer t
  :preface
  (define-key!
    [remap bookmark-jump]                 #'consult-bookmark
    [remap evil-show-marks]               #'consult-mark
    [remap evil-show-jumps]               #'+vertico/jump-list
    [remap evil-show-registers]           #'consult-register
    [remap goto-line]                     #'consult-goto-line
    [remap imenu]                         #'consult-imenu
    [remap Info-search]                   #'consult-info
    [remap locate]                        #'consult-locate
    [remap load-theme]                    #'consult-theme
    [remap recentf-open-files]            #'consult-recent-file
    [remap switch-to-buffer]              #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
    [remap yank-pop]                      #'consult-yank-pop)
  :config
  (load! "patches/workspaces.el")

  (defadvice! +vertico--consult-recentf-a (&rest _args)
    "`consult-recent-file' needs to have `recentf-mode' on to
 work correctly.

`consult-buffer' needs `recentf-mode' to show file candidates."
    :before (list #'consult-recent-file #'consult-buffer)
    (recentf-mode +1))

  (defadvice! +vertico--use-evil-registers-a (fn &rest args)
    "Use `evil-register-list' if `evil-mode' is active."
    :around #'consult-register--alist
    (let ((register-alist
           (if (bound-and-true-p evil-local-mode)
               (evil-register-list)
             register-alist)))
      (apply fn args)))

  ;; Track jump
  (advice-add #'consult--read :around #'zenit-set-jump-a)

  (setq consult-project-root-function #'zenit-project-root
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        consult-fd-args
        '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
          "--color=never"
          ;; https://github.com/sharkdp/fd/issues/839
          "--full-path --absolute-path"
          "--hidden --exclude .git"
          (if (featurep :system 'windows) "--path-separator=/")))


  (protect-macros!
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file
     consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
     :preview-key "C-SPC"))
  (eval-when! (modulep! :config default)
    (protect-macros!
      (consult-customize
       +default/search-project +default/search-other-project
       +default/search-project-for-symbol-at-point
       +default/search-cwd +default/search-other-cwd
       +default/search-notes-for-symbol-at-point
       +default/search-emacsd
       :preview-key "C-SPC")))
  (consult-customize
   consult-theme
   :preview-key (list "C-SPC" :debounce 0.5 'any))
  (eval-when! (modulep! :emacs org)
    (defvar +vertico--consult-org-source
      (list :name     "Org Buffer"
            :category 'buffer
            :narrow   ?o
            :hidden   t
            :face     'consult-buffer
            :history  'buffer-name-history
            :state    #'consult--buffer-state
            :new
            (lambda (name)
              (with-current-buffer (get-buffer-create name)
                (insert "#+title: " name "\n\n")
                (org-mode)
                (consult--buffer-action (current-buffer))))
            :items
            (lambda ()
              (mapcar #'buffer-name
                      (if (featurep 'org)
                          (org-buffer-list)
                        (seq-filter
                         (lambda (x)
                           (eq (buffer-local-value 'major-mode x) 'org-mode))
                         (buffer-list)))))))
    (add-to-list 'consult-buffer-sources '+vertico--consult-org-source 'append)))


(use-package! consult-dir
  :general
  ([remap list-directory] #'consult-dir)
  (:keymaps 'vertico-map
            "C-x C-d" #'consult-dir
            "C-x C-j" #'consult-dir-jump-file)
  :config
  (eval-when! (modulep! :tools docker)
    (defun +vertico--consult-dir-podman-hosts ()
      (tramp-container--completion-function "podman"))

    (defun +vertico--consult-dir-docker-hosts ()
      (tramp-container--completion-function "docker"))

    (defvar +vertico--consult-dir-source-tramp-podman
      `(:name     "Podman"
        :narrow   ?p
        :category file
        :face     consult-file
        :history  file-name-history
        :items    ,#'+vertico--consult-dir-podman-hosts)
      "Podman candidate source for `consult-dir'.")

    (defvar +vertico--consult-dir-source-tramp-docker
      `(:name     "Docker"
        :narrow   ?d
        :category file
        :face     consult-file
        :history  file-name-history
        :items    ,#'+vertico--consult-dir-docker-hosts)
      "Docker candidate source for `consult-dir'.")

    (add-to-list 'consult-dir-sources '+vertico--consult-dir-source-tramp-podman t)
    (add-to-list 'consult-dir-sources '+vertico--consult-dir-source-tramp-docker t))

  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t))


(use-package! consult-flycheck
  :when (modulep! :checkers syntax -flymake)
  :after (consult flycheck))


(use-package! embark
  :defer t
  :init
  ;; Allow C-h to open Consult when calling which-key without a prefix.
  (setq which-key-use-C-h-commands t
        prefix-help-command #'embark-prefix-help-command)
  (defvar +vertico-which-key-current-keymap nil
    "The current keymap being displayed by which-key.")
  (defadvice! +vertico-which-key-update-current-keymap-a (_keymap-name keymap &rest args)
    :before #'which-key--show-keymap
    (setq +vertico-which-key-current-keymap keymap))
  (defadvice! +vertico-which-key-consult-C-h-dispatch (oldfun)
    :around #'which-key-C-h-dispatch
    (setq this-command 'embark-prefix-help-command)
    (cond ((not (which-key--popup-showing-p))
           (call-interactively #'embark-prefix-help-command))
          ((string-empty-p (which-key--current-key-string))
           (embark-bindings-in-keymap +vertico-which-key-current-keymap))
          (t (call-interactively #'embark-prefix-help-command))))

  (map! [remap describe-bindings] #'embark-bindings
        "C-;"               #'embark-act  ; to be moved to :config default if accepted
        (:map minibuffer-local-map
         "C-;"               #'embark-act
         "C-c C-;"           #'embark-export
         "C-c C-l"           #'embark-collect
         :desc "Export to writable buffer" "C-c C-e" #'+vertico/embark-export-write)
        (:leader
         :desc "Actions" "a" #'embark-act)) ; to be moved to :config default if accepted
  :config
  (require 'consult)

  (set-popup-rule! "^\\*Embark Export:" :size 0.35 :ttl 0 :quit nil)

  (after! which-key
    (defadvice! +vertico--embark-which-key-prompt-a (fn &rest args)
      "Hide the which-key indicator immediately when using the completing-read prompter."
      :around #'embark-completing-read-prompter
      (which-key--hide-popup-ignore-command)
      (let ((embark-indicators
             (remq #'embark-which-key-indicator embark-indicators)))
        (apply fn args)))
    (cl-nsubstitute #'+vertico-embark-which-key-indicator #'embark-mixed-indicator embark-indicators))

  ;; add the package! target finder before the file target finder,
  ;; so we don't get a false positive match.
  (let ((pos (or (cl-position
                  'embark-target-file-at-point
                  embark-target-finders)
                 (length embark-target-finders))))
    (cl-callf2
        cons
        '+vertico-embark-target-package-fn
        (nthcdr pos embark-target-finders)))
  (defvar-keymap +vertico/embark-zenit-package-map
    :doc "Keymap for Embark package actions for packages installed."
    "h" #'zenit/help-packages
    "b" #'zenit/bump-package
    "c" #'zenit/help-package-config
    "u" #'zenit/help-package-homepage)
  (setf (alist-get 'package embark-keymap-alist) #'+vertico/embark-zenit-package-map)
  (map! (:map embark-file-map
         :desc "Open target with sudo"        "s"   #'zenit/sudo-find-file
         (:when (modulep! :tools magit)
           :desc "Open magit-status of target" "g"   #'+vertico/embark-magit-status)
         (:when (modulep! :ui workspaces)
           :desc "Open in new workspace"       "TAB" #'+vertico/embark-open-in-new-workspace
           :desc "Open in new workspace"       "<tab>" #'+vertico/embark-open-in-new-workspace))))


(use-package! marginalia
  :hook (zenit-first-input . marginalia-mode)
  :init
  (map! :map minibuffer-local-map
        :desc "Cycle marginalia views" "M-A" #'marginalia-cycle)
  :config
  (advice-add #'marginalia--project-root :override #'zenit-project-root)
  (pushnew! marginalia-command-categories
            '(+default/find-file-under-here . file)
            '(zenit/find-file-in-emacsd . project-file)
            '(zenit/find-file-in-other-project . project-file)
            '(zenit/find-file-in-private-config . file)
            '(zenit/describe-active-minor-mode . minor-mode)
            '(flycheck-error-list-set-filter . builtin)
            '(projectile-find-file . project-file)
            '(projectile-recentf . project-file)
            '(projectile-switch-to-buffer . buffer)
            '(projectile-switch-project . project-file)))


(use-package! wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

;; From https://github.com/minad/vertico/wiki#candidate-display-transformations-custom-candidate-highlighting
;;
;; Uses `add-face-text-property' instead of `propertize' unlike the above
;; snippet because `'append' is necessary to not override the match font lock
;; See: https://github.com/minad/vertico/issues/389
(use-package! vertico-multiform
  :hook (vertico-mode . vertico-multiform-mode)
  :config
  (defvar +vertico-transform-functions nil)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (defun +vertico-sort-directories-first (files)
    "Sort FILES by alpha and put elements ending with a slash first."
    (setq files (vertico-sort-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (defun +vertico-highlight-directory (file)
    "If FILE ends with a slash, highlight it as a directory."
    (when (string-suffix-p "/" file)
      (add-face-text-property 0 (length file) 'marginalia-file-priv-dir 'append file))
    file)

  (defun +vertico-highlight-enabled-mode (cmd)
    "If MODE is enabled, highlight it as font-lock-constant-face."
    (let ((sym (intern cmd)))
      (with-current-buffer (nth 1 (buffer-list))
        (if (or (eq sym major-mode)
                (and
                 (memq sym minor-mode-list)
                 (boundp sym)
                 (symbol-value sym)))
            (add-face-text-property 0 (length cmd) 'font-lock-constant-face 'append cmd)))
      cmd))

  (pushnew! (alist-get 'file vertico-multiform-categories)
            '(+vertico-transform-functions . +vertico-highlight-directory)
            ;; Sort directories before files
            '(vertico-sort-function . +vertico-sort-directories-first))
  (pushnew! (alist-get 'execute-extended-command vertico-multiform-commands)
            '(+vertico-transform-functions . +vertico-highlight-enabled-mode)))

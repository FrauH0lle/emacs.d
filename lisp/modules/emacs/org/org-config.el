;; emacs/org/org-config.el -*- lexical-binding: t; -*-

(defun +zenit-org--init-config ()
(zenit--log "Loading org config :)")
(use-package! toc-org ; auto-table of contents
  :hook (org-mode . toc-org-enable)
  :config
  (setq toc-org-hrefify-default "gh")

  (defadvice! +org-inhibit-scrolling-a (fn &rest args)
    "Prevent the jarring scrolling that occurs when the-ToC is
regenerated."
    :around #'toc-org-insert-toc
    (let ((p (set-marker (make-marker) (point)))
          (s (window-start)))
      (prog1 (apply fn args)
        (goto-char p)
        (set-window-start nil s t)
        (set-marker p nil)))))


(use-package! org-crypt ; built-in
  :when (modulep! +crypt)
  :commands org-encrypt-entries org-encrypt-entry org-decrypt-entries org-decrypt-entry
  :hook (org-load . org-crypt-use-before-save-magic)
  :preface
  ;; org-crypt falls back to CRYPTKEY property then `epa-file-encrypt-to', which
  ;; is a better default than the empty string `org-crypt-key' defaults to.
  (defvar org-crypt-key nil)
  (after! org (add-to-list 'org-tags-exclude-from-inheritance "crypt")))


(use-package! org-clock ; built-in
  :commands org-clock-save
  :init
  (setq org-clock-persist-file (concat zenit-data-dir "org-clock-save.el"))
  (defadvice! +org--clock-load-a (&rest _)
    "Lazy load org-clock until its commands are used."
    :before '(org-clock-in
              org-clock-out
              org-clock-in-last
              org-clock-goto
              org-clock-cancel)
    (org-clock-load))
  :config
  (setq org-clock-persist 'history
        ;; Resume when clocking into task with open clock
        org-clock-in-resume t
        ;; Remove log if task was clocked for 0:00 (accidental clocking)
        org-clock-out-remove-zero-time-clocks t
        ;; The default value (5) is too conservative.
        org-clock-history-length 20)
  (add-hook 'kill-emacs-hook #'org-clock-save))


(use-package! org-eldoc
  :hook (org-mode . org-eldoc-load)
  :init (setq org-eldoc-breadcrumb-separator " → ")
  :config
  ;; HACK Infinite recursion when eldoc kicks in 'org' or 'python' src blocks.
  (puthash "org" #'ignore org-eldoc-local-functions-cache)
  (puthash "plantuml" #'ignore org-eldoc-local-functions-cache)
  (puthash "python" #'python-eldoc-function org-eldoc-local-functions-cache))


(use-package! org-pdftools
  :when (modulep! :tools pdf)
  :commands org-pdftools-export
  :init
  (after! org
    ;; HACK Fixes an issue where org-pdftools link handlers will throw a
    ;;   'pdf-info-epdfinfo-program is not executable' error whenever any link
    ;;   is stored or exported (whether or not they're a pdf link). This error
    ;;   gimps org until `pdf-tools-install' is run, but this is poor UX, so we
    ;;   suppress it.
    (defun +org--pdftools-link-handler (fn &rest args)
      "Produces a link handler for org-pdftools that suppresses
missing-epdfinfo errors whenever storing or exporting links."
      (lambda (&rest args)
        (and (ignore-errors (require 'org-pdftools nil t))
             (file-executable-p pdf-info-epdfinfo-program)
             (apply fn args))))
    (org-link-set-parameters (or (bound-and-true-p org-pdftools-link-prefix) "pdf")
                             :follow   (+org--pdftools-link-handler #'org-pdftools-open)
                             :complete (+org--pdftools-link-handler #'org-pdftools-complete-link)
                             :store    (+org--pdftools-link-handler #'org-pdftools-store-link)
                             :export   (+org--pdftools-link-handler #'org-pdftools-export))
    (add-hook! 'org-open-link-functions
      (defun +org-open-legacy-pdf-links-fn (link)
        "Open pdftools:* and pdfviews:* links as if they were
 pdf:* links."
        (let ((regexp "^pdf\\(?:tools\\|view\\):"))
          (when (string-match-p regexp link)
            (org-pdftools-open (replace-regexp-in-string regexp "" link))
            t))))))


(use-package! evil-org
  :when (modulep! :editor evil)
  :hook (org-mode . evil-org-mode)
  :hook (org-capture-mode . evil-insert-state)
  :init
  (defvar evil-org-retain-visual-state-on-shift t)
  (defvar evil-org-special-o/O '(table-row))
  (defvar evil-org-use-additional-insert t)
  :config
  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  (evil-org-set-key-theme)
  (add-hook! 'org-tab-first-hook :append
             ;; Only fold the current tree, rather than recursively
             #'+org-cycle-only-current-subtree-h
             ;; Clear babel results if point is inside a src block
             #'+org-clear-babel-results-h)
  (let-alist evil-org-movement-bindings
    (let ((Cright  (concat "C-" .right))
          (Cleft   (concat "C-" .left))
          (Cup     (concat "C-" .up))
          (Cdown   (concat "C-" .down))
          (CSright (concat "C-S-" .right))
          (CSleft  (concat "C-S-" .left))
          (CSup    (concat "C-S-" .up))
          (CSdown  (concat "C-S-" .down)))
      (map! :map evil-org-mode-map
            :ni [C-return]   #'+org/insert-item-below
            :ni [C-S-return] #'+org/insert-item-above
            ;; navigate table cells (from insert-mode)
            :i Cright (general-predicate-dispatch nil
                        (org-at-table-p) #'org-table-next-field
                        #'org-end-of-line)
            :i Cleft  (general-predicate-dispatch nil
                        (org-at-table-p) #'org-table-previous-field
                        #'org-beginning-of-line)
            :i Cup    (general-predicate-dispatch nil
                        (org-at-table-p) #'+org/table-previous-row
                        #'org-up-element)
            :i Cdown  (general-predicate-dispatch nil
                        (org-at-table-p) #'org-table-next-row
                        #'org-down-element)
            :ni CSright   #'org-shiftright
            :ni CSleft    #'org-shiftleft
            :ni CSup      #'org-shiftup
            :ni CSdown    #'org-shiftdown
            ;; more intuitive RET keybinds
            :n [return]   #'+org/dwim-at-point
            :n "RET"      #'+org/dwim-at-point
            :i [return]   #'+org/return
            :i "RET"      #'+org/return
            :i [S-return] #'+org/shift-return
            :i "S-RET"    #'+org/shift-return
            ;; more vim-esque org motion keys (not covered by evil-org-mode)
            :m "]h"  #'org-forward-heading-same-level
            :m "[h"  #'org-backward-heading-same-level
            :m "]l"  #'org-next-link
            :m "[l"  #'org-previous-link
            :m "]c"  #'org-babel-next-src-block
            :m "[c"  #'org-babel-previous-src-block
            :n "gQ"  #'+org/reformat-at-point
            ;; sensible vim-esque folding keybinds
            :n "za"  #'+org/toggle-fold
            :n "zA"  #'org-shifttab
            :n "zc"  #'+org/close-fold
            :n "zC"  #'outline-hide-subtree
            :n "zm"  #'+org/hide-next-fold-level
            :n "zM"  #'+org/close-all-folds
            :n "zn"  #'org-tree-to-indirect-buffer
            :n "zo"  #'+org/open-fold
            :n "zO"  #'outline-show-subtree
            :n "zr"  #'+org/show-next-fold-level
            :n "zR"  #'+org/open-all-folds
            :n "zi"  #'org-toggle-inline-images

            :map org-read-date-minibuffer-local-map
            Cleft    (cmd! (org-eval-in-calendar '(calendar-backward-day 1)))
            Cright   (cmd! (org-eval-in-calendar '(calendar-forward-day 1)))
            Cup      (cmd! (org-eval-in-calendar '(calendar-backward-week 1)))
            Cdown    (cmd! (org-eval-in-calendar '(calendar-forward-week 1)))
            CSleft   (cmd! (org-eval-in-calendar '(calendar-backward-month 1)))
            CSright  (cmd! (org-eval-in-calendar '(calendar-forward-month 1)))
            CSup     (cmd! (org-eval-in-calendar '(calendar-backward-year 1)))
            CSdown   (cmd! (org-eval-in-calendar '(calendar-forward-year 1)))))))


(use-package! evil-org-agenda
  :when (modulep! :editor evil)
  :hook (org-agenda-mode . evil-org-agenda-mode)
  :config
  (evil-org-agenda-set-keys)
  (evil-define-key* 'motion evil-org-agenda-mode-map
    (kbd zenit-leader-key) nil))


;;
;;; Bootstrap

(use-package! org
  ;; :defer-incrementally
  ;; calendar find-func format-spec org-macs org-compat org-faces org-entities
  ;; org-list org-pcomplete org-src org-footnote org-macro ob org org-agenda
  ;; org-capture
  :defer t
  :preface
  ;; Set to nil so we can detect user changes to them later (and fall back on
  ;; defaults otherwise).
  (defvar org-directory nil)
  (defvar org-id-locations-file nil)
  (defvar org-attach-id-dir nil)
  (defvar org-babel-python-command nil)

  (setq org-persist-directory (concat zenit-cache-dir "org/persist/")
        org-publish-timestamp-directory (concat zenit-cache-dir "org/timestamps/")
        org-preview-latex-image-directory (concat zenit-cache-dir "org/latex/")
        ;; Recognize a), A), a., A., etc -- must be set before org is loaded.
        org-list-allow-alphabetical t)

  ;; Make most of the default modules opt-in to lighten its first-time load
  ;; delay. I sincerely doubt most users use them all.
  (defvar org-modules
    '(;; ol-doi
      ;; ol-w3m
      ;; ol-bbdb
      ol-bibtex
      ;; ol-docview
      ;; ol-gnus
      ;; ol-info
      ;; ol-irc
      ;; ol-mhe
      ;; ol-rmail
      ;; ol-eww
      ))

  ;; Custom org modules
  (eval-when! (modulep! +dragndrop)
    (eval-when-compile
      (message "(modulep! +dragndrop) is: %s" (modulep! +dragndrop)))
    ;; (compile-along! "addons/dragndrop")
    (load! "addons/dragndrop"))
  (eval-when! (modulep! +pomodoro)
    ;; (compile-along! "addons/pomodoro")
    (load! "addons/pomodoro"))
  (eval-when! (modulep! +present)
    ;; (compile-along! "addons/present")
    (load! "addons/present"))
  (eval-when! (modulep! +pretty)
    ;; (compile-along! "addons/pretty")
    (load! "addons/pretty"))
  (eval-when! (modulep! +jupyter)
    ;; (compile-along! "addons/jupyter")
    (load! "addons/jupyter"))

  ;; Add our general hooks after the submodules, so that any hooks the
  ;; submodules add run after them, and can overwrite any defaults if necessary.
  (add-hook! 'org-mode-hook
             ;; `show-paren-mode' causes flickering with indent overlays made by
             ;; `org-indent-mode', so we turn off show-paren-mode altogether
             #'zenit-disable-show-paren-mode-h
             ;; disable `show-trailing-whitespace'; shows a lot of false positives
             #'zenit-disable-show-trailing-whitespace-h)

  ;; (add-hook! 'org-load-hook
  ;;            #'+org-init-org-directory-h
  ;;            #'+org-init-appearance-h
  ;;            ;; #'+org-init-agenda-h
  ;;            #'+org-init-attachments-h
  ;;            #'+org-init-babel-h
  ;;            #'+org-init-babel-lazy-loader-h
  ;;            #'+org-init-capture-defaults-h
  ;;            #'+org-init-capture-frame-h
  ;;            #'+org-init-custom-links-h
  ;;            #'+org-init-export-h
  ;;            ;; #'+org-init-habit-h
  ;;            #'+org-init-hacks-h
  ;;            #'+org-init-keybinds-h
  ;;            #'+org-init-popup-rules-h
  ;;            #'+org-init-smartparens-h)

  ;; Wait until an org-protocol link is opened via emacsclient to load
  ;; `org-protocol'. Normally you'd simply require `org-protocol' and use it,
  ;; but the package loads all of org for no compelling reason, so...
  (defadvice! +org--server-visit-files-a (fn files &rest args)
    "Advise `server-visit-files' to load `org-protocol' lazily."
    :around #'server-visit-files
    (if (not (cl-loop with protocol =
                      (if (featurep :system 'windows)
                          ;; On Windows, the file arguments for `emacsclient'
                          ;; get funnelled through `expand-file-path' by
                          ;; `server-process-filter'. This substitutes
                          ;; backslashes with forward slashes and converts each
                          ;; path to an absolute one. However, *all* absolute
                          ;; paths on Windows will match the regexp ":/+", so we
                          ;; need a more discerning regexp.
                          (regexp-quote
                           (or (bound-and-true-p org-protocol-the-protocol)
                               "org-protocol"))
                        ;; ...but since there is a miniscule possibility users
                        ;; have changed `org-protocol-the-protocol' I don't want
                        ;; this behavior for macOS/Linux users.
                        "")
                      for var in files
                      if (string-match-p (format "%s:/+" protocol) (car var))
                      return t))
        (apply fn files args)
      (require 'org-protocol)
      (apply #'org--protocol-detect-protocol-server fn files args)))
  (after! org-protocol
    (advice-remove 'server-visit-files #'org--protocol-detect-protocol-server))

  ;; In case the user has eagerly loaded org from their configs
  (when (and (featurep 'org)
             (not byte-compile-current-file))
    (unless (zenit-context-p 'reload)
      (message "`org' was already loaded by the time emacs/org loaded, this may cause issues"))
    (run-hooks 'org-load-hook))

  :config

  (add-to-list 'zenit-debug-variables 'org-export-async-debug)

  (set-eval-handler! 'org-mode #'+org-eval-handler)

  (add-hook! 'org-mode-hook
             ;; HACK: Somehow, users/packages still find a way to modify tab-width in
             ;;   org-mode. Since org-mode treats a non-standerd tab-width as an error
             ;;   state, I use this hook to makes it much harder to change by accident.
             (add-hook! 'after-change-major-mode-hook :local
                        ;; The second check is necessary, in case of `org-edit-src-code' which
                        ;; clones a buffer and changes its major-mode.
                        (when (derived-mode-p 'org-mode)
                          (setq tab-width 8)))

             ;; HACK: `save-place' can position the cursor in an invisible region. This
             ;;   makes it visible unless `org-inhibit-startup' or
             ;;   `org-inhibit-startup-visibility-stuff' is non-nil.
             (add-hook 'save-place-after-find-file-hook #'+org-make-last-point-visible-h nil t))

  ;; Save target buffer after archiving a node.
  (setq org-archive-subtree-save-file-p t)

  ;; Don't number headings with these tags
  (setq org-num-face '(:inherit org-special-keyword :underline nil :weight bold)
        org-num-skip-tags '("noexport" "nonum"))

  ;; Other org properties are all-caps. Be consistent.
  (setq org-effort-property "EFFORT")

  ;; Global ID state means we can have ID links anywhere. This is required for
  ;; `org-brain', however.
  (setq org-id-locations-file-relative t)

  ;; HACK `org-id' doesn't check if `org-id-locations-file' exists or is
  ;;   writeable before trying to read/write to it.
  (defadvice! +org--fail-gracefully-a (&rest _)
    :before-while '(org-id-locations-save org-id-locations-load)
    (file-writable-p org-id-locations-file))

  (add-hook 'org-open-at-point-functions #'zenit-set-jump-h)
  ;; HACK For functions that dodge `org-open-at-point-functions', like
  ;;   `org-id-open', `org-goto', or roam: links.
  (advice-add #'org-mark-ring-push :around #'zenit-set-jump-a)

  ;; Allow shift selection
  (setq org-support-shift-select t)

  ;; Add the ability to play gifs, at point or throughout the buffer. However,
  ;; 'playgifs' is stupid slow and there's not much I can do to fix it; use at
  ;; your own risk.
  (add-to-list 'org-startup-options '("inlinegifs" +org-startup-with-animated-gifs at-point))
  (add-to-list 'org-startup-options '("playgifs"   +org-startup-with-animated-gifs t))
  (add-hook! 'org-mode-local-vars-hook
    (defun +org-init-gifs-h ()
      (remove-hook 'post-command-hook #'+org-play-gif-at-point-h t)
      (remove-hook 'post-command-hook #'+org-play-all-gifs-h t)
      (pcase +org-startup-with-animated-gifs
        (`at-point (add-hook 'post-command-hook #'+org-play-gif-at-point-h nil t))
        (`t (add-hook 'post-command-hook #'+org-play-all-gifs-h nil t))))))
)

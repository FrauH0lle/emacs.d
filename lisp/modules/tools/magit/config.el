;; tools/magit/config.el -*- lexical-binding: t; -*-

(defvar +magit-fringe-size '(13 . 1)
  "Size of the fringe in magit-mode buffers.

Can be an integer or a cons cell whose CAR and CDR are integer
widths for the left and right fringe.

Only has an effect in GUI Emacs.")


;;
;;; Packages

(use-package! magit
  :commands magit-file-delete
  :defer-incrementally dash f s with-editor git-commit package eieio transient
  :init
  (setq magit-auto-revert-mode nil)  ; we do this ourselves further down
  ;; Must be set early to prevent ~/.emacs.d/transient from being created
  (setq transient-levels-file  (concat zenit-data-dir "transient/levels")
        transient-values-file  (concat zenit-data-dir "transient/values")
        transient-history-file (concat zenit-data-dir "transient/history"))
  :config
  (add-to-list 'zenit-debug-variables 'magit-refresh-verbose)

  (setq transient-default-level 5
        magit-diff-refine-hunk t ; show granular diffs in selected hunk
        ;; Don't autosave repo buffers. This is too magical, and saving can
        ;; trigger a bunch of unwanted side-effects, like save hooks and
        ;; formatters. Trust the user to know what they're doing.
        magit-save-repository-buffers nil
        ;; Don't display parent/related refs in commit buffers; they are rarely
        ;; helpful and only add to runtime costs.
        magit-revision-insert-related-refs nil)
  (add-hook 'magit-process-mode-hook #'goto-address-mode)

  ;; Center the target file, because it's poor UX to have it at the bottom of
  ;; the window after invoking `magit-status-here'.
  (advice-add #'magit-status-here :after #'zenit-recenter-a)

  ;; The default location for git-credential-cache is in
  ;; ~/.cache/git/credential. However, if ~/.git-credential-cache/ exists, then
  ;; it is used instead. Magit seems to be hardcoded to use the latter, so here
  ;; we override it to have more correct behavior.
  (unless (file-exists-p "~/.git-credential-cache/")
    (setq magit-credential-cache-daemon-socket
          (zenit-glob (or (getenv "XDG_CACHE_HOME")
                          "~/.cache/")
                      "git/credential/socket")))

  ;; Prevent sudden window position resets when staging/unstaging/discarding/etc
  ;; hunks in `magit-status-mode' buffers. It's disorienting, especially on
  ;; larger projects.
  (defvar +magit--pos nil)
  (defhook! +magit--set-window-state-h ()
    "Prevent scrolling when manipulating magit-status hunks."
    'magit-pre-refresh-hook
    (setq-local +magit--pos (list (current-buffer) (point) (window-start))))

  (defhook! +magit--restore-window-state-h ()
    "Prevent scrolling when manipulating magit-status hunks."
    'magit-post-refresh-hook
    (when (and +magit--pos (eq (current-buffer) (car +magit--pos)))
      (goto-char (cadr +magit--pos))
      (set-window-start nil (caddr +magit--pos) t)
      (kill-local-variable '+magit--pos)))

  (setq transient-display-buffer-action '(display-buffer-below-selected)
        magit-display-buffer-function #'+magit-display-buffer
        magit-bury-buffer-function #'magit-mode-quit-window)

  (eval-when! (modulep! :ui popup)
    (set-popup-rule! "^\\(?:\\*magit\\|magit:\\| \\*transient\\*\\)" :ignore t))
  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)

  ;; Add additional switches that seem common enough
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  ;; so magit buffers can be switched to (except for process buffers)
  (defun +magit-buffer-p (buf)
    (with-current-buffer buf
      (and (derived-mode-p 'magit-mode)
           (not (eq major-mode 'magit-process-mode)))))
  (add-to-list 'zenit-real-buffer-functions #'+magit-buffer-p nil #'eq)

  ;; Clean up after magit by killing leftover magit buffers and reverting
  ;; affected buffers (or at least marking them as need-to-be-reverted).
  (define-key magit-mode-map "q" #'+magit/quit)
  (define-key magit-mode-map "Q" #'+magit/quit-all)

  ;; Close transient with ESC
  (define-key transient-map [escape] #'transient-quit-one)

  (defhook! +magit-enlargen-fringe-h ()
    "Make fringe larger in magit."
    'magit-mode-hook
    (add-hook! 'window-configuration-change-hook :local
      (and (display-graphic-p)
           (derived-mode-p 'magit-mode)
           +magit-fringe-size
           (let ((left  (or (car-safe +magit-fringe-size) +magit-fringe-size))
                 (right (or (cdr-safe +magit-fringe-size) +magit-fringe-size)))
             (set-window-fringes nil left right)))))

  ;; An optimization that particularly affects macOS and Windows users: by
  ;; resolving `magit-git-executable' Emacs does less work to find the
  ;; executable in your PATH, which is great because it is called so frequently.
  ;; However, absolute paths will break magit in TRAMP/remote projects if the
  ;; git executable isn't in the exact same location.
  (defhook! +magit-optimize-process-calls-h ()
    'magit-status-mode-hook
    (when-let (path (executable-find magit-git-executable t))
      (setq-local magit-git-executable path)))

  (defhook! +magit-reveal-point-if-invisible-h ()
    "Reveal the point if in an invisible region."
    'magit-diff-visit-file-hook
    (if (derived-mode-p 'org-mode)
        (org-reveal '(4))
      (require 'reveal)
      (reveal-post-command))))

;; (after! magit
;;   (set-evil-initial-state! 'magit-mode 'emacs))

(use-package! magit-todos
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?") ; make colon optional
  (define-key magit-todos-section-map "j" nil))

(use-package! evil-collection-magit
  :when (modulep! :editor evil)
  :defer t
  :init (defvar evil-collection-magit-use-z-for-folds t)
  :config
  ;; q is enough; ESC is too easy for to accidentally press, especially when
  ;; traversing modes in magit buffers.
  (evil-define-key* 'normal magit-status-mode-map [escape] nil)

  ;; Fix these keybinds because they are blacklisted
  (map! (:map magit-mode-map
         :nv "q" #'+magit/quit
         :nv "Q" #'+magit/quit-all
         :nv "]" #'magit-section-forward-sibling
         :nv "[" #'magit-section-backward-sibling
         :nv "gr" #'magit-refresh
         :nv "gR" #'magit-refresh-all)
        (:map magit-status-mode-map
         :nv "gz" #'magit-refresh)
        (:map magit-diff-mode-map
         :nv "gd" #'magit-jump-to-diffstat-or-diff))

  ;; A more intuitive behavior for TAB in magit buffers:
  (define-key! 'normal
    (magit-status-mode-map
     magit-stash-mode-map
     magit-revision-mode-map
     magit-process-mode-map
     magit-diff-mode-map)
    [tab] #'magit-section-toggle)

  (after! git-rebase
    (dolist (key '(("M-k" . "gk") ("M-j" . "gj")))
      (when-let (desc (assoc (car key) evil-collection-magit-rebase-commands-w-descriptions))
        (setcar desc (cdr key))))
    (evil-define-key* evil-collection-magit-state git-rebase-mode-map
      "gj" #'git-rebase-move-line-down
      "gk" #'git-rebase-move-line-up))

  (after! magit-gitflow
    (evil-define-key* '(normal visual) magit-mode-map
      "%" #'magit-gitflow-popup)
    (transient-append-suffix 'magit-dispatch 'magit-worktree
      '("%" "Gitflow" magit-gitflow-popup))))


(use-package! evil-collection-magit-section
  :when (modulep! :editor evil)
  :defer t
  :init
  (defvar evil-collection-magit-section-use-z-for-folds evil-collection-magit-use-z-for-folds)
  (after! magit-section
    ;; These numbered keys mask the numerical prefix keys. Since they've already
    ;; been replaced with z1, z2, z3, etc (and 0 with g=), there's no need to
    ;; keep them around:
    (undefine-key! magit-section-mode-map "M-1" "M-2" "M-3" "M-4" "1" "2" "3" "4" "0")
    ;; `evil-collection-magit-section' binds these redundant keys.
    (map! :map magit-section-mode-map :n "1" nil :n "2" nil :n "3" nil :n "4" nil)))

;; tools/magit/config.el -*- lexical-binding: t; -*-

(defvar +magit-open-windows-in-direction 'right
  "What direction to open new windows from the status buffer.
For example, diffs and log buffers. Accepts `left', `right',
`up', and `down'.")

(defvar +magit-fringe-size '(13 . 1)
  "Size of the fringe in magit-mode buffers.

Can be an integer or a cons cell whose CAR and CDR are integer
widths for the left and right fringe.

Only has an effect in GUI Emacs.")


(defvar +magit-auto-revert 'local
  "If non-nil, revert associated buffers after Git operations.

These buffers are auto-reverted via side-effects immediately if
they're visible or reverted next time they're switched to. This
is intended to be a much more efficient replacement for
`magit-auto-revert-mode' and `global-auto-revert-mode', and
should not be used together with them! Set this to `nil' if you
plan to use the above.

Accepts one of three values OR a predicate function:

t
  Revert any associated buffers.
local
  Same as `t', except remote (TRAMP) buffers are ignored.
nil
  Don't do any auto-reverting at all.
FUNCTION
  If given a function, it will be passed a buffer associated with
  the current Magit session and must return non-nil to signal
  this is buffer is safe to revert (now or later, when switched
  to).")

;;
;;; Packages

(use-package! magit
  :commands magit-file-delete
  :defer-incrementally (dash f s with-editor git-commit package eieio transient)
  :init
  (setq magit-auto-revert-mode nil)  ; we do this ourselves further down
  ;; Must be set early to prevent ~/.emacs.d/transient from being created
  (setq transient-levels-file  (concat zenit-data-dir "transient/levels")
        transient-values-file  (concat zenit-data-dir "transient/values")
        transient-history-file (concat zenit-data-dir "transient/history"))
  :config
  (set-debug-variable! 'magit-refresh-verbose)

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

  ;; Since the project likely now contains new files, purge the projectile cache
  ;; so `projectile-find-file' et all don't produce an stale file list
  (add-hook! 'magit-refresh-buffer-hook
    (defun +magit-invalidate-projectile-cache-h ()
      ;; Only invalidate the hot cache and nothing else (everything else is
      ;; expensive busy work, and we don't want to slow down magit's
      ;; refreshing).
      (let (projectile-require-project-root
            projectile-enable-caching
            projectile-verbose)
        (letf! ((#'recentf-cleanup #'ignore))
          (projectile-invalidate-cache nil)))))
  ;; Use a more efficient strategy to auto-revert buffers whose git state has
  ;; changed: refresh the visible buffers immediately...
  (add-hook 'magit-post-refresh-hook #'+magit-mark-stale-buffers-h)
  ;; ...then refresh the rest only when we switch to them or refocus the active
  ;; frame, not all at once.
  (add-hook 'zenit-switch-buffer-hook #'+magit-revert-buffer-maybe-h)
  (add-hook 'zenit-switch-frame-hook #'+magit-mark-stale-buffers-h)

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
  (defvar +magit--refreshed-buffer nil)
  (add-hook! 'magit-pre-refresh-hook
    (defun +magit--set-window-state-h ()
      (setq-local +magit--refreshed-buffer
                  (list (current-buffer) (point) (window-start)))))
  (add-hook! 'magit-post-refresh-hook
    (defun +magit--restore-window-state-h ()
      (cl-destructuring-bind (&optional buf pt beg) +magit--refreshed-buffer
        (when (and buf (eq (current-buffer) buf))
          (goto-char pt)
          (set-window-start nil beg t)
          (kill-local-variable '+magit--refreshed-buffer)))))

  (setq magit-display-buffer-function #'+magit-display-buffer-fn
        magit-bury-buffer-function #'magit-mode-quit-window)

  ;; Pop up transient windows at the bottom of the window where it was invoked.
  ;; This is more ergonomic for users with large displays or many splits.
  (setq transient-display-buffer-action
        '(display-buffer-below-selected
          (dedicated . t)
          (inhibit-same-window . t))
        transient-show-during-minibuffer-read t)

  (set-popup-rule! "^\\(?:\\*magit\\|magit:\\| \\*transient\\*\\)" :ignore t)

  ;; The mode-line isn't useful in these popups and take up valuable screen
  ;; estate, so free it up.
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

  (add-hook! 'magit-section-mode-hook
    (add-hook! 'window-configuration-change-hook :local
      (defun +magit-enlargen-fringe-h ()
        "Make fringe larger in magit."
        (and (display-graphic-p)
             (derived-mode-p 'magit-section-mode)
             +magit-fringe-size
             (let ((left  (or (car-safe +magit-fringe-size) +magit-fringe-size))
                   (right (or (cdr-safe +magit-fringe-size) +magit-fringe-size)))
               (set-window-fringes nil left right))))))

  (add-hook! 'magit-diff-visit-file-hook
    (defun +magit-reveal-point-if-invisible-h ()
      "Reveal the point if in an invisible region."
      (if (derived-mode-p 'org-mode)
          (org-reveal '(4))
        (require 'reveal)
        (reveal-post-command)))))


(use-package! forge
  :when (modulep! +forge)
  ;; We defer loading even further because forge's dependencies will try to
  ;; compile emacsql, which is a slow and blocking operation.
  :after-call magit-status
  :commands forge-create-pullreq forge-create-issue
  :preface
  (setq forge-database-file (concat zenit-data-dir "forge/forge-database.sqlite"))
  (setq forge-add-default-bindings (not (modulep! :editor evil)))
  :init
  (after! ghub-graphql
    ;; Killing recreating the status buffer prevents progress updates from being
    ;; relayed through the modeline. Use `message' instead.
    (setq ghub-graphql-message-progress t))
  :config
  ;; All forge list modes are derived from `forge-topic-list-mode'
  (map! :map forge-topic-list-mode-map :n "q" #'kill-current-buffer)
  (when (not forge-add-default-bindings)
    (map! :map magit-mode-map [remap magit-browse-thing] #'forge-browse
          :map magit-remote-section-map [remap magit-browse-thing] #'forge-browse-remote
          :map magit-branch-section-map [remap magit-browse-thing] #'forge-browse-branch))
  (set-popup-rule! "^\\*?[0-9]+:\\(?:new-\\|[0-9]+$\\)" :size 0.45 :modeline t :ttl 0 :quit nil)
  (set-popup-rule! "^\\*\\(?:[^/]+/[^ ]+ #[0-9]+\\*$\\|Issues\\|Pull-Requests\\|forge\\)" :ignore t))


(use-package! code-review
  :when (modulep! +forge)
  :after magit
  :init
  ;; TODO This needs to either a) be cleaned up or better b) better map things
  ;; to fit
  (after! evil-collection-magit
    (dolist (binding evil-collection-magit-mode-map-bindings)
      (pcase-let* ((`(,states _ ,evil-binding ,fn) binding))
        (dolist (state states)
          (evil-collection-define-key state 'code-review-mode-map evil-binding fn))))
    (evil-set-initial-state 'code-review-mode evil-default-state))
  (setq code-review-db-database-file (file-name-concat zenit-data-dir "code-review/code-review-db-file.sqlite")
        code-review-log-file (file-name-concat zenit-data-dir "code-review/code-review-error.log")
        code-review-download-dir (file-name-concat zenit-data-dir "code-review/"))
  :config
  (transient-append-suffix 'magit-merge "d"
    '("y" "Review pull request" +magit/start-code-review))
  (after! forge
    (transient-append-suffix 'forge-dispatch "c u"
      '("c r" "Review pull request" +magit/start-code-review))))


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
         :nv "gd" #'magit-jump-to-diffstat-or-diff)
        ;; Don't open recursive process buffers
        (:map magit-process-mode-map
         :nv "`" #'ignore))

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
      (when-let* ((desc (assoc (car key) evil-collection-magit-rebase-commands-w-descriptions)))
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
  :config
  (defadvice! +magit--override-evil-collection-defaults-a (&rest _)
    :after #'evil-collection-magit-section-setup
    ;; These numbered keys mask the numerical prefix keys. Since they've already
    ;; been replaced with z1, z2, z3, etc (and 0 with g=), there's no need to
    ;; keep them around:
    (undefine-key! magit-section-mode-map "M-1" "M-2" "M-3" "M-4" "1" "2" "3" "4" "0")
    ;; `evil-collection-magit-section' binds these redundant keys.
    (map! :map magit-section-mode-map :n "1" nil :n "2" nil :n "3" nil :n "4" nil)))


(use-package! git-commit
  :hook (zenit-first-file . global-git-commit-mode)
  :config
  (set-tempel-minor-mode! 'git-commit-mode)

  ;; Enforce git commit conventions.
  ;; See https://chris.beams.io/posts/git-commit/
  (setq git-commit-summary-max-length 50
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  (setq-hook! 'git-commit-mode-hook fill-column 72)

  (add-hook! 'git-commit-setup-hook
    (defun +vc-start-in-insert-state-maybe-h ()
      "Start git-commit-mode in insert state if in a blank commit message,
otherwise in default state."
      (when (and (bound-and-true-p evil-mode)
                 (not (evil-emacs-state-p))
                 (bobp) (eolp))
        (evil-insert-state)))))

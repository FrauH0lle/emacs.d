;; tools/dired/config.el -*- lexical-binding: t; -*-

(defvar +dired-dirvish-icon-provider 'nerd-icons
  "Icon provider to use for dirvish when the module is enabled.")

(use-package! dired
  :commands dired-jump
  :init
  (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
        dired-hide-details-hide-symlink-targets nil
        ;; don't prompt to revert, just do it
        dired-auto-revert-buffer #'dired-buffer-stale-p
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Ask whether destination dirs should get created when copying/removing
        ;; files.
        dired-create-destination-dirs 'ask
        ;; Where to store image caches
        image-dired-dir (concat zenit-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        ;; Screens are larger nowadays, we can afford slightly larger thumbnails
        image-dired-thumb-size 150)
  :config
  ;; (set-popup-rule! "^\\*image-dired"
  ;;   :slot 20 :size 0.8 :select t :quit nil :ttl 0)
  (eval-when! (modulep! :editor evil)
    (after! evil
      (set-evil-initial-state! 'image-dired-display-image-mode 'emacs)))

  (let ((args (list "-ahl" "-v" "--group-directories-first")))
    (eval-when! zenit--system-bsd-p
      ;; Use GNU ls as `gls' from `coreutils' if available. Add `(setq
      ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning
      ;; when not using GNU ls.
      (if-let* ((gls (executable-find "gls")))
          (setq insert-directory-program gls)
        ;; BSD ls doesn't support -v or --group-directories-first
        (setq args (list (car args)))))
    (setq dired-listing-switches (string-join args " "))

    (add-hook! 'dired-mode-hook
      (defun +dired-disable-gnu-ls-flags-maybe-h ()
        "Remove extraneous switches from `dired-actual-switches' when
it's uncertain that they are supported (e.g. over TRAMP or on
Windows).

Fixes #1703: dired over TRAMP displays a blank screen.
Fixes #3939: unsortable dired entries on Windows."
        (when (or (file-remote-p default-directory)
                  (and (boundp 'ls-lisp-use-insert-directory-program)
                       (not ls-lisp-use-insert-directory-program)))
          (setq-local dired-actual-switches (car args))))))

  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil)

  (defadvice! +dired--no-revert-in-virtual-buffers-a (&rest args)
    "Don't auto-revert in dired-virtual buffers (see `dired-virtual-revert')."
    :before-while #'dired-buffer-stale-p
    (not (eq revert-buffer-function #'dired-virtual-revert)))

  (map! :map dired-mode-map
        ;; Kill all dired buffers on q
        :ng "q" #'+dired/quit-all
        ;; To be consistent with ivy/helm+wgrep integration
        "C-c C-e" #'wdired-change-to-wdired-mode))


(use-package! dired-rsync
  :general (dired-mode-map "C-c C-r" #'dired-rsync))


(use-package! diredfl
  :hook (dired-mode . diredfl-mode))


(use-package! dirvish
  :when (modulep! +dirvish)
  :defer t
  :init (after! dired (dirvish-override-dired-mode))
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dirvish-cache-dir (concat zenit-cache-dir "dirvish/")
        dirvish-hide-details nil
        dirvish-attributes '(git-msg))
  (defer-until! (bound-and-true-p dired-omit-mode)
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..*$")))
  (when (modulep! +icons)
    (push +dired-dirvish-icon-provider dirvish-attributes))
  (map! :map dirvish-mode-map
        :n "b" #'dirvish-goto-bookmark
        :n "z" #'dirvish-show-history
        :n "f" #'dirvish-file-info-menu
        :n "F" #'dirvish-toggle-fullscreen
        :n "l" #'dired-find-file
        :n "h" #'dired-up-directory
        :localleader
        "h" #'dired-omit-mode))


(use-package! fd-dired
  :when zenit-fd-executable
  :defer t
  :init
  (global-set-key [remap find-dired] #'fd-dired)
  ;; (set-popup-rule! "^\\*F\\(?:d\\|ind\\)\\*$" :ignore t)
  )

(use-package! dired-aux
  :defer t
  :config
  (setq dired-create-destination-dirs 'ask
        dired-vc-rename-file t))

;;;###package dired-git-info
(map! :after dired
      :map (dired-mode-map ranger-mode-map)
      :ng ")" #'dired-git-info-mode)
(setq dgi-commit-message-format "%h %cs %s"
      dgi-auto-hide-details-p nil)
(after! wdired
  ;; Temporarily disable `dired-git-info-mode' when entering wdired, due to
  ;; reported incompatibilities.
  (defvar +dired--git-info-p nil)
  (defadvice! +dired--disable-git-info-a (&rest _)
    :before #'wdired-change-to-wdired-mode
    (setq +dired--git-info-p (bound-and-true-p dired-git-info-mode))
    (when +dired--git-info-p
      (dired-git-info-mode -1)))
  (defadvice! +dired--reactivate-git-info-a (&rest _)
    :after '(wdired-exit
             wdired-abort-changes
             wdired-finish-edit)
    (when +dired--git-info-p
      (dired-git-info-mode +1))))

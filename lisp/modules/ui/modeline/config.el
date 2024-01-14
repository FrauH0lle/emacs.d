;; ui/modeline/config.el -*- lexical-binding: t; -*-

(use-package! doom-modeline
  :hook (zenit-after-init . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode) ; filesize in modeline
  :hook (doom-modeline-mode . column-number-mode)   ; cursor column in modeline
  :init
  ;; We display project info in the modeline ourselves
  (setq projectile-dynamic-mode-line nil)
  ;; Set these early so they don't trigger variable watchers
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        ;; Only show file encoding if it's non-UTF-8 and different line endings
        ;; than the current OSes preference
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-default-eol-type
        (cond (IS-WINDOWS 1)
              (0)))

  :config
  ;; Fix an issue where these two variables aren't defined in TTY Emacs on MacOS
  (defvar mouse-wheel-down-event nil)
  (defvar mouse-wheel-up-event nil)

  (add-hook 'after-setting-font-hook #'+modeline-resize-for-font-h)
  (add-hook 'zenit-load-theme-hook #'doom-modeline-refresh-bars)

  (add-to-list 'doom-modeline-mode-alist '(+doom-dashboard-mode . dashboard))
  (defhook! +modeline-hide-in-non-status-buffer-h ()
    "Show minimal modeline in magit-status buffer, no modeline
elsewhere."
    'magit-mode-hook
    (if (eq major-mode 'magit-status-mode)
        (doom-modeline-set-modeline 'magit)
      (hide-mode-line-mode)))

  ;; Some functions modify the buffer, causing the modeline to show a false
  ;; modified state, so force them to behave.
  (defadvice! +modeline--inhibit-modification-hooks-a (orig-fn &rest args)
    :around #'ws-butler-after-save
    (with-silent-modifications (apply orig-fn args)))

  ;; Prevent flashing modeline
  (advice-add #'doom-modeline-redisplay :override #'ignore)

  ;; Extensions
  (use-package! anzu
    :after-call isearch-mode)

  (use-package! evil-anzu
    :when (modulep! :editor evil)
    :after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
    :config (global-anzu-mode +1))

  ;; Add symlink status/toggle icon
  (defcustom +doom-modeline-buffer-symlink-icon t
    "Whether to display an icon if the buffer's visited file is
a symlink. It respects `doom-modeline-icon'."
    :type 'boolean
    :group 'doom-modeline)

  (after! doom-modeline
    (defvar-local +doom-modeline--buffer-file-symlink-icon nil)
    (defun +doom-modeline-update-buffer-file-symlink-icon (&rest _)
      "Update the buffer's symlink status in mode-line."
      (setq +doom-modeline--buffer-file-symlink-icon
            (when +doom-modeline-buffer-symlink-icon
              (ignore-errors
                (concat
                 (cond ((or (file-symlink-p (buffer-file-name))
                            zenit--symlink-origin)
                        (let ((icon (doom-modeline-buffer-file-state-icon
                                     "nf-md-file_link" "󱅷" "%1*"
                                     `(:inherit doom-modeline-warning
                                       :weight ,(if doom-modeline-icon
                                                    'normal
                                                  'bold)))))
                          (propertize icon
                                      'help-echo "mouse-1: Toggle symlink"
                                      'mouse-face 'mode-line-highlight
                                      'local-map (let ((map (make-sparse-keymap)))
                                                   (define-key map [mode-line mouse-1]
                                                               #'zenit/toggle-symlink)
                                                   map))))
                       (t "")))))))

    (eval-and-compile
      (defsubst +doom-modeline--buffer-symlink-icon ()
        "The icon of the current buffer symlink."
        (when +doom-modeline-buffer-symlink-icon
          (when-let ((icon (+doom-modeline-update-buffer-file-symlink-icon)))
            (unless (string-empty-p icon)
              (concat
               (doom-modeline-display-icon icon)
               (doom-modeline-vspc)))))))

    (doom-modeline-def-segment buffer-info
      "Combined information about the current buffer, including
the current working directory, the file name, and its
state (modified, read-only or non-existent)."
      (concat
       (doom-modeline-spc)
       (doom-modeline--buffer-mode-icon)
       (doom-modeline--buffer-state-icon)
       (doom-modeline--buffer-name)
       (doom-modeline-spc)
       (+doom-modeline--buffer-symlink-icon)))))

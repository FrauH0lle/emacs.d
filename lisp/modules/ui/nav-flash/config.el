;; ui/nav-flash/config.el -*- lexical-binding: t; -*-

(defvar +nav-flash-exclude-commands
  '(mouse-set-point mouse-drag-region evil-mouse-drag-region +org/dwim-at-point
    org-find-file org-find-file-at-mouse corfu--goto)
  "A list of commands that should not trigger nav-flash.")

(defvar +nav-flash-exclude-modes
  '(so-long-mode special-mode comint-mode term-mode vterm-mode)
  "List of major modes where nav-flash won't automatically trigger.")


;;
;;; Packages

(use-package! pulsar
  :defer t
  :init
  (add-hook! '(imenu-after-jump-hook
               better-jumper-post-jump-hook
               counsel-grep-post-action-hook
               consult-after-jump-hook
               dumb-jump-after-jump-hook)
             #'+nav-flash-blink-cursor-maybe-h)

  (add-hook 'zenit-switch-window-hook #'+nav-flash-blink-cursor-maybe-h)

  ;; `org'
  (add-hook 'org-follow-link-hook #'+nav-flash-delayed-blink-cursor-h)

  ;; `saveplace'
  (advice-add #'save-place-find-file-hook :after #'+nav-flash-blink-cursor-a)

  ;; `evil'
  (advice-add #'evil-window-top    :after #'+nav-flash-blink-cursor-a)
  (advice-add #'evil-window-middle :after #'+nav-flash-blink-cursor-a)
  (advice-add #'evil-window-bottom :after #'+nav-flash-blink-cursor-a)

  ;; Bound to `ga' for evil users
  (advice-add #'what-cursor-position :after #'+nav-flash-blink-cursor-a)

  ;; `recenter'
  (defadvice! +nav-flash-pulse-after-recenter (&rest _)
    "Pulse after recenter."
    :after #'recenter
    (pulsar-pulse-line)))

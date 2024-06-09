;; ui/nav-flash/config.el -*- lexical-binding: t; -*-

(defvar +nav-flash-exclude-commands
  '(mouse-set-point mouse-drag-region evil-mouse-drag-region +org/dwim-at-point
    org-find-file org-find-file-at-mouse)
  "A list of commands that should not trigger nav-flash.")

(defvar +nav-flash-exclude-modes
  '(so-long-mode special-mode comint-mode term-mode vterm-mode)
  "List of major modes where nav-flash won't automatically trigger.")

(defvar +nav-flash--last-point nil
  "Stores buffer, window and point of last pulse.")


;;
;;; Packages

(use-package! pulsar
  :hook (zenit-first-input . pulsar-global-mode)
  :config
  (pushnew! pulsar-pulse-functions
            ;; `saveplace'
            'save-place-find-file-hook
            ;; `evil'
            'evil-window-top
            'evil-window-middle
            'evil-window-bottom
            ;; Bound to `ga' for evil users
            'what-cursor-position)

  ;; Respect `+nav-flash-exclude-commands' and `+nav-flash-exclude-modes'
  (defadvice! +nav-flash-blink-cursor-maybe-a (fn &rest args)
    "No-ops if in `+nav-flash-exclude-modes', or triggered from one of
`+nav-flash-exclude-commands'."
    :around #'pulsar-pulse-line
    (unless (or (memq this-command +nav-flash-exclude-commands)
                (memq real-this-command +nav-flash-exclude-commands)
                (bound-and-true-p so-long-minor-mode)
                (minibufferp)
                (apply #'derived-mode-p +nav-flash-exclude-modes)
                (equal +nav-flash--last-point
                       (list (selected-window)
                             (current-buffer)
                             (point))))
      (prog1
          (apply fn args)
        (setq +nav-flash--last-point (list (selected-window) (current-buffer) (point))))))

  (add-hook! '(zenit-switch-window-hook
               imenu-after-jump-hook
               better-jumper-post-jump-hook
               consult-after-jump-hook
               dumb-jump-after-jump-hook)
             #'pulsar-pulse-line)

  ;; `org'
  (defhook! +nav-flash-delayed-blink-cursor-h (&rest _)
    "Like `+nav-flash-blink-cursor', but links after a tiny pause, in
case it isn't clear at run-time if the point will be in the
correct window/buffer (like for `org-follow-link-hook')."
    'org-follow-link-hook
    (run-at-time 0.1 nil #'pulsar-pulse-line)))

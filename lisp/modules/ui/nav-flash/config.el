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

(defvar +nav-flash--focus-target nil
  "Frame and window awaiting a focus pulse, or nil.")

(defvar +nav-flash--focus-timer nil
  "One-shot idle timer for the pending focus pulse, or nil.")

(defvar +nav-flash--command-active nil
  "Non-nil between pre-command and post-command hooks.")

(defun +nav-flash--focus-command-p ()
  "Return non-nil if the current command handles focus."
  (or (memq this-command '(handle-switch-frame handle-select-window))
      (memq real-this-command '(handle-switch-frame handle-select-window))))

(defun +nav-flash--cancel-focus ()
  "Cancel the pending focus pulse and its timer."
  (when (timerp +nav-flash--focus-timer)
    (cancel-timer +nav-flash--focus-timer))
  (setq +nav-flash--focus-timer nil
        +nav-flash--focus-target nil))

(defun +nav-flash--defer-focus ()
  "Coalesce focus pulses for the currently selected frame and window."
  (let ((target (list (selected-frame) (selected-window))))
    (unless (equal target +nav-flash--focus-target)
      (+nav-flash--cancel-focus)
      (setq +nav-flash--focus-target target)))
  ;; Window-selection hooks can request this again on redisplay, after the
  ;; post-command hook already armed it. Preserve an armed timer
  ;; for the same target, or arrange one if this is the first focus request.
  ;; During a command/tracker, only the final post-command hook may arm it.
  (unless (or +nav-flash--command-active track-mouse
              (timerp +nav-flash--focus-timer))
    (setq +nav-flash--focus-timer
          (run-with-idle-timer 0.05 nil #'+nav-flash--finish-focus))))

(defun +nav-flash--finish-focus ()
  "Pulse at the final focus destination once command input settles."
  (setq +nav-flash--focus-timer nil)
  ;; Idle timers also run inside `read-key', including a long mouse press.
  ;; Do not rearm here: the next completed command will arrange another idle
  ;; check, after tracking and any normalized mouse-up replay have finished.
  (unless (or +nav-flash--command-active track-mouse (input-pending-p))
    (let ((target +nav-flash--focus-target))
      (+nav-flash--cancel-focus)
      (when (and target
                 (frame-live-p (car target))
                 (window-live-p (cadr target))
                 (eq (car target) (selected-frame))
                 (eq (cadr target) (selected-window))
                 (not (eq (frame-focus-state (car target)) nil)))
        (with-current-buffer (window-buffer (cadr target))
          (when (bound-and-true-p pulsar-mode)
            ;; This pulse belongs to focus, not to the excluded mouse command
            ;; which placed point. All other eligibility gates still apply.
            (let (this-command real-this-command)
              (pulsar-pulse-line))))))))

(defun +nav-flash--pre-command-h ()
  "Keep focus timers out of commands and mouse tracking."
  (setq +nav-flash--command-active t)
  (when (timerp +nav-flash--focus-timer)
    (cancel-timer +nav-flash--focus-timer)
    (setq +nav-flash--focus-timer nil)))

(defun +nav-flash--post-command-h ()
  "Arm one focus pulse after command hooks and queued input settle."
  (setq +nav-flash--command-active nil)
  (when +nav-flash--focus-target
    (if (or (not (frame-live-p (car +nav-flash--focus-target)))
            (not (window-live-p (cadr +nav-flash--focus-target)))
            (not (eq (car +nav-flash--focus-target) (selected-frame))))
        (+nav-flash--cancel-focus)
      ;; A focus click may select another window in the same frame. Follow
      ;; that click, but discard focus superseded by keyboard navigation.
      (if (and (not (eq (cadr +nav-flash--focus-target) (selected-window)))
               (not (mouse-event-p last-input-event)))
          (+nav-flash--cancel-focus)
        (setcar (cdr +nav-flash--focus-target) (selected-window))
        (unless (timerp +nav-flash--focus-timer)
          (setq +nav-flash--focus-timer
                (run-with-idle-timer 0.05 nil #'+nav-flash--finish-focus)))))))

(defun +nav-flash--post-command-pulse-a (fn &rest args)
  "Defer only focus-owned automatic Pulsar pulses from FN.
Pass ARGS and all other commands through unchanged, including edit and
region pulses that do not go through `pulsar-pulse-line'."
  (if (+nav-flash--focus-command-p)
      (+nav-flash--defer-focus)
    ;; A normal automatic line pulse supersedes pending focus. Do not apply
    ;; this to the independent edit/region branches of Pulsar's dispatcher.
    (when (or (memq this-command pulsar-pulse-functions)
              (memq real-this-command pulsar-pulse-functions))
      (+nav-flash--cancel-focus))
    (apply fn args)))

(defun +nav-flash-blink-cursor-maybe-a (fn &rest args)
  "Call FN with ARGS if this navigation pulse is eligible.
Defer focus pulses until mouse input settles. Otherwise respect command,
mode, minibuffer and repeated-position exclusions."
  (if (+nav-flash--focus-command-p)
      (+nav-flash--defer-focus)
    (unless (or (memq this-command +nav-flash-exclude-commands)
                (memq real-this-command +nav-flash-exclude-commands)
                (bound-and-true-p so-long-minor-mode)
                (minibufferp)
                (apply #'derived-mode-p +nav-flash-exclude-modes)
                (equal +nav-flash--last-point
                       (list (selected-window) (current-buffer) (point))))
      (prog1 (apply fn args)
        (setq +nav-flash--last-point
              (list (selected-window) (current-buffer) (point)))))))

(defun +nav-flash--switch-window-h ()
  "Coalesce automatic window flashes with any pending focus pulse."
  ;; Redisplay normally clears both command variables before running window
  ;; selection hooks. A pending target still owns this automatic flash, but
  ;; must not turn unrelated explicit line/region pulse requests into focus.
  (cond ((+nav-flash--focus-command-p)
         (+nav-flash--defer-focus))
        (+nav-flash--focus-target
         (if (and (eq (car +nav-flash--focus-target) (selected-frame))
                  (window-live-p (cadr +nav-flash--focus-target)))
             (+nav-flash--defer-focus)
           (+nav-flash--cancel-focus)))
        (t (pulsar-pulse-line))))


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

  (advice-add #'pulsar-pulse-line :around #'+nav-flash-blink-cursor-maybe-a)
  (advice-add #'pulsar-post-command-pulse :around #'+nav-flash--post-command-pulse-a)
  (add-hook 'pre-command-hook #'+nav-flash--pre-command-h)
  (add-hook 'post-command-hook #'+nav-flash--post-command-h 90)

  (remove-hook 'zenit-switch-window-hook #'pulsar-pulse-line)
  (add-hook 'zenit-switch-window-hook #'+nav-flash--switch-window-h)
  (add-hook! '(imenu-after-jump-hook
               better-jumper-post-jump-hook
               consult-after-jump-hook
               dumb-jump-after-jump-hook)
             #'pulsar-pulse-line)

  ;; `org'
  (add-hook! 'org-follow-link-hook
    (defun +nav-flash-delayed-blink-cursor-h (&rest _)
      "Like `+nav-flash-blink-cursor', but links after a tiny pause in
case it isn't clear at run-time if the point will be in the
correct window/buffer (like for `org-follow-link-hook')."
      (run-at-time 0.1 nil #'pulsar-pulse-line))))

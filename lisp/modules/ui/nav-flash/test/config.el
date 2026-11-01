;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; ui/nav-flash/test/config.el

(require 'cl-lib)
(require 'pulsar)
(require 'evil)
(require 'so-long)
(require 'zenit-test)
(require 'zenit-use-package)
(zenit-require 'zenit-lib 'modules)
(zenit-modules-initialize)
(zenit-load (zenit-module-locate-path :ui 'nav-flash "config.el"))

(defmacro +nav-flash-test-with-buffer (&rest body)
  "Run BODY with isolated focus state and recorded Pulsar calls."
  (declare (indent 0) (debug t))
  `(let ((+nav-flash--focus-target nil)
         (+nav-flash--focus-timer nil)
         (+nav-flash--command-active nil)
         (+nav-flash--last-point nil)
         (pre-command-hook '(+nav-flash--pre-command-h))
         (post-command-hook '(+nav-flash--post-command-h))
         (unread-command-events nil)
         (this-command nil)
         (real-this-command nil)
         (last-input-event nil)
         (track-mouse nil)
         (pending-input nil)
         (native-input-pending-p (symbol-function 'input-pending-p))
         (focused t)
         pulses)
     (save-window-excursion
       (with-temp-buffer
         (insert "alpha beta\nsecond line\nlast line\n")
         (set-window-buffer (selected-window) (current-buffer))
         (goto-char 1)
         (pulsar-mode 1)
         (cl-letf (((symbol-function 'input-pending-p)
                    (lambda (&rest _)
                      (or pending-input (funcall native-input-pending-p))))
                   ((symbol-function 'frame-focus-state) (lambda (_) focused))
                   ((symbol-function 'pulsar--create-pulse)
                    (lambda (locus face)
                      (push (list (selected-window) (current-buffer) (point)
                                  locus face)
                            pulses))))
           (unwind-protect (progn ,@body)
             (+nav-flash--cancel-focus)
             (pulsar-mode -1)))))))

(defmacro +nav-flash-test-command (command &rest body)
  "Run BODY and command hooks as COMMAND."
  (declare (indent 1) (debug t))
  `(let ((this-command ,command)
         (real-this-command ,command))
     (run-hooks 'pre-command-hook)
     ,@body
     (run-hooks 'post-command-hook)))

(defun +nav-flash-test-idle ()
  "Deliver the scheduled idle callback deterministically in batch."
  (should (timerp +nav-flash--focus-timer))
  (cancel-timer +nav-flash--focus-timer)
  (+nav-flash--finish-focus))

(zenit-deftest +nav-flash--focus-target
  ()
  (should (boundp '+nav-flash--focus-target)))

(zenit-deftest +nav-flash--focus-timer
  ()
  (should (boundp '+nav-flash--focus-timer)))

(zenit-deftest +nav-flash--command-active
  ()
  (should (boundp '+nav-flash--command-active)))

(zenit-deftest +nav-flash--focus-command-p
  ()
  (let ((this-command ,command)
        (real-this-command ,real))
    (should (eq ,expected (not (null (+nav-flash--focus-command-p))))))
  (command real expected)
  'handle-switch-frame nil t
  nil 'handle-select-window t
  'mouse-set-point 'mouse-set-point nil
  'next-line 'next-line nil)

(zenit-deftest +nav-flash--cancel-focus
  ()
  (+nav-flash-test-with-buffer
    (+nav-flash--defer-focus)
    (+nav-flash--post-command-h)
    (let ((timer +nav-flash--focus-timer))
      (should (memq timer timer-idle-list))
      (+nav-flash--cancel-focus)
      (should-not (memq timer timer-idle-list))
      (should-not +nav-flash--focus-timer)
      (should-not +nav-flash--focus-target)
      (+nav-flash--cancel-focus))))

(zenit-deftest +nav-flash--defer-focus
  ()
  (+nav-flash-test-with-buffer
    (+nav-flash-test-command 'handle-switch-frame)
    (let ((timer +nav-flash--focus-timer))
      (+nav-flash-test-command 'handle-select-window)
      (should-not (memq timer timer-idle-list))
      (should (equal (list (selected-frame) (selected-window))
                     +nav-flash--focus-target))
      (should-not pulses)
      (+nav-flash-test-idle)
      (should (= 1 (length pulses)))
      (should-not +nav-flash--focus-target))))

(zenit-deftest +nav-flash--pre-command-h
  ()
  (progn
    (should (memq #'+nav-flash--pre-command-h pre-command-hook))
    (+nav-flash-test-with-buffer
      (+nav-flash-test-command 'handle-switch-frame)
      (let ((timer +nav-flash--focus-timer))
        (+nav-flash--pre-command-h)
        (should +nav-flash--command-active)
        (should +nav-flash--focus-target)
        (should-not +nav-flash--focus-timer)
        (should-not (memq timer timer-idle-list))))))

(zenit-deftest +nav-flash--post-command-h
  ()
  (progn
    (should (memq #'+nav-flash--post-command-h post-command-hook))
    (+nav-flash-test-with-buffer
      (+nav-flash--defer-focus)
      (+nav-flash--post-command-h)
      (let ((timer +nav-flash--focus-timer))
        (+nav-flash--post-command-h)
        (should (eq timer +nav-flash--focus-timer))
        (should-not +nav-flash--command-active))
      ,test))
  (test)
  :doc "A mouse click may finish in another window of the focused frame"
  (let ((destination (split-window-right))
        (last-input-event (list 'mouse-1 (list (selected-window) 1 '(1 . 1) 0))))
    (select-window destination)
    (+nav-flash--post-command-h)
    (+nav-flash-test-idle)
    (should (eq destination (caar pulses))))

  :doc "A keyboard window change supersedes pending focus"
  (progn
    (select-window (split-window-right))
    (+nav-flash--post-command-h)
    (should-not +nav-flash--focus-target)
    (should-not +nav-flash--focus-timer))

  :doc "A dead window cancels pending focus"
  (progn
    (split-window-right)
    (delete-window (selected-window))
    (+nav-flash--post-command-h)
    (should-not +nav-flash--focus-target)
    (should-not +nav-flash--focus-timer))

  :doc "A dead or superseded frame cancels pending focus"
  (progn
    (setcar +nav-flash--focus-target 'dead-frame)
    (+nav-flash--post-command-h)
    (should-not +nav-flash--focus-target))

  :doc "Switching to another live frame supersedes pending focus"
  (cl-letf (((symbol-function 'selected-frame) (lambda () 'another-frame)))
    (+nav-flash--post-command-h)
    (should-not +nav-flash--focus-target)
    (should-not +nav-flash--focus-timer)))

(zenit-deftest +nav-flash--finish-focus
  ()
  (+nav-flash-test-with-buffer
    (+nav-flash-test-command 'handle-switch-frame)
    ,test)
  (test)
  :doc "Focus without a click still pulses once at the settled point"
  (progn
    (+nav-flash-test-idle)
    (should (= 1 (length pulses)))
    (should (= 1 (nth 2 (car pulses))))
    (should-not +nav-flash--focus-timer)
    (should-not +nav-flash--focus-target)
    (+nav-flash--finish-focus)
    (should (= 1 (length pulses))))

  :doc "Focus pulses after an excluded mouse command at its final point"
  (progn
    (+nav-flash-test-command 'mouse-set-point
      (goto-char 14)
      (pulsar-pulse-line))
    (should-not pulses)
    (+nav-flash-test-idle)
    (should (= 1 (length pulses)))
    (should (= 14 (nth 2 (car pulses)))))

  :doc "Input in the native queue postpones focus until another command completes"
  (progn
    (setq pending-input t)
    (+nav-flash-test-idle)
    (should-not pulses)
    (should +nav-flash--focus-target)
    (should-not +nav-flash--focus-timer)
    (setq pending-input nil)
    (+nav-flash-test-command 'mouse-set-point (goto-char 15))
    (+nav-flash-test-idle)
    (should (= 15 (nth 2 (car pulses)))))

  :doc "An idle callback during tracking waits for post-command completion"
  (progn
    (let ((track-mouse t)) (+nav-flash-test-idle))
    (should-not pulses)
    (should-not +nav-flash--focus-timer)
    (+nav-flash-test-command 'mouse-drag-region (goto-char 16))
    (+nav-flash-test-idle)
    (should (= 16 (nth 2 (car pulses)))))

  :doc "An idle callback inside a command cannot pulse or recursively rearm"
  (progn
    (let ((+nav-flash--command-active t)) (+nav-flash-test-idle))
    (should-not pulses)
    (should-not +nav-flash--focus-timer)
    (+nav-flash--post-command-h)
    (+nav-flash-test-idle)
    (should (= 1 (length pulses))))

  :doc "Losing focus before idle cancels the pulse"
  (progn
    (setq focused nil)
    (+nav-flash-test-idle)
    (should-not pulses)
    (should-not +nav-flash--focus-target))

  :doc "Switching windows before idle cancels the stale destination"
  (progn
    (select-window (split-window-right))
    (+nav-flash-test-idle)
    (should-not pulses)
    (should-not +nav-flash--focus-target))

  :doc "Disabling Pulsar before idle cancels the pulse"
  (progn
    (pulsar-mode -1)
    (+nav-flash-test-idle)
    (should-not pulses))

  :doc "An excluded major mode still rejects a deferred focus pulse"
  (progn
    (setq major-mode 'special-mode)
    (+nav-flash-test-idle)
    (should-not pulses))

  :doc "So-long minor mode still rejects a deferred focus pulse"
  (let ((so-long-minor-mode t))
    (+nav-flash-test-idle)
    (should-not pulses))

  :doc "A minibuffer still rejects a deferred focus pulse"
  (cl-letf (((symbol-function 'minibufferp) (lambda (&rest _) t)))
    (+nav-flash-test-idle)
    (should-not pulses))

  :doc "A duplicate final destination still rejects a deferred focus pulse"
  (progn
    (setq +nav-flash--last-point (list (selected-window) (current-buffer) (point)))
    (+nav-flash-test-idle)
    (should-not pulses))

  :doc "The callback resolves the selected window's final buffer and point"
  (let ((destination (generate-new-buffer "nav-flash-destination")))
    (unwind-protect
        (progn
          (with-current-buffer destination
            (insert "final buffer\n")
            (goto-char 7)
            (pulsar-mode 1))
          (set-window-buffer (selected-window) destination)
          (+nav-flash-test-idle)
          (should (= 1 (length pulses)))
          (should (eq destination (nth 1 (car pulses))))
          (should (= 7 (nth 2 (car pulses)))))
      (kill-buffer destination))))

(zenit-deftest +nav-flash--post-command-pulse-a
  ()
  (progn
    (should (advice-member-p #'+nav-flash--post-command-pulse-a
                            #'pulsar-post-command-pulse))
    (+nav-flash-test-with-buffer ,test))
  (test)
  :doc "Keyboard navigation retains Pulsar's automatic line pulse"
  (progn
    (+nav-flash-test-command 'goto-line (goto-char 14))
    (should (= 1 (length pulses)))
    (should (= 14 (nth 2 (car pulses))))
    (should-not +nav-flash--focus-target))

  :doc "Automatic keyboard navigation supersedes a pending focus pulse"
  (progn
    (+nav-flash-test-command 'handle-switch-frame)
    (+nav-flash-test-command 'goto-line (goto-char 14))
    (should (= 1 (length pulses)))
    (should (= 14 (nth 2 (car pulses))))
    (should-not +nav-flash--focus-target)
    (should-not +nav-flash--focus-timer)
    (+nav-flash--finish-focus)
    (should (= 1 (length pulses))))

  :doc "An ordinary excluded mouse command does not acquire a focus pulse"
  (progn
    (+nav-flash-test-command 'mouse-set-point (goto-char 14))
    (should-not pulses)
    (should-not +nav-flash--focus-target)
    (should-not +nav-flash--focus-timer))

  :doc "Edit-region pulses retain their original boundaries and face"
  (let ((pulsar--pulse-region-changes (list (cons 2 5))))
    (+nav-flash-test-command 'yank)
    (should (equal '(2 . 5) (nth 3 (car pulses))))
    (should (eq pulsar-region-change-face (nth 4 (car pulses)))))

  :doc "An edit-region pulse does not supersede the independent focus pulse"
  (progn
    (+nav-flash-test-command 'handle-switch-frame)
    (let ((pulsar--pulse-region-changes (list (cons 2 5))))
      (+nav-flash-test-command 'yank))
    (should (= 1 (length pulses)))
    (should (eq pulsar-region-change-face (nth 4 (car pulses))))
    (should +nav-flash--focus-target)
    (+nav-flash-test-idle)
    (should (= 2 (length pulses)))
    (should-not +nav-flash--focus-target))

  :doc "Selected-region pulses still bypass the navigation wrapper"
  (let ((pulsar-pulse-region-functions '(copy-region-as-kill))
        (transient-mark-mode t))
    (goto-char 2)
    (set-mark 5)
    (setq mark-active t)
    (+nav-flash-test-command 'copy-region-as-kill)
    (should (equal '(2 . 5) (nth 3 (car pulses))))
    (should (eq pulsar-region-face (nth 4 (car pulses))))))

(zenit-deftest +nav-flash--switch-window-h
  ()
  (progn
    (should (memq #'+nav-flash--switch-window-h zenit-switch-window-hook))
    (should-not (memq #'pulsar-pulse-line zenit-switch-window-hook))
    (+nav-flash-test-with-buffer ,test))
  (test)
  :doc "Zenit window-hook focus pulses coalesce with automatic focus pulses"
  (progn
    (+nav-flash-test-command 'handle-select-window
      (run-hooks 'zenit-switch-window-hook))
    (should-not pulses)
    (+nav-flash-test-idle)
    (should (= 1 (length pulses))))

  :doc "A redisplay window hook after post-command preserves the pending timer"
  (let ((this-command 'handle-select-window)
        (real-this-command 'handle-select-window))
    (+nav-flash-test-command 'handle-select-window)
    (let ((timer +nav-flash--focus-timer))
      (should (memq timer timer-idle-list))
      ;; Explicit redisplay in a later post-command hook can run window hooks
      ;; with the completed focus command still current.
      (run-hooks 'zenit-switch-window-hook)
      (run-hooks 'zenit-switch-window-hook)
      (should (eq timer +nav-flash--focus-timer))
      (should (memq timer timer-idle-list))
      (should-not pulses)
      (+nav-flash-test-idle)
      (should (= 1 (length pulses)))
      (should-not +nav-flash--focus-target)
      (should-not +nav-flash--focus-timer)))

  :doc "A first focus request from redisplay schedules without another command"
  (let ((this-command 'handle-select-window)
        (real-this-command 'handle-select-window))
    (+nav-flash--pre-command-h)
    (+nav-flash--post-command-h)
    (should-not +nav-flash--focus-target)
    (run-hooks 'zenit-switch-window-hook)
    (should-not pulses)
    (+nav-flash-test-idle)
    (should (= 1 (length pulses))))

  :doc "Native redisplay with cleared commands preserves focus through queued input"
  (progn
    (+nav-flash-test-command 'handle-select-window)
    (should-not this-command)
    (should-not real-this-command)
    (let ((timer +nav-flash--focus-timer))
      (setq unread-command-events
            (list (list 'mouse-1 (list (selected-window) 14 '(30 . 10) 120))))
      (run-hooks 'zenit-switch-window-hook)
      (should-not pulses)
      (should (eq timer +nav-flash--focus-timer))
      (+nav-flash-test-idle)
      (should-not pulses)
      (let ((event (pop unread-command-events)))
        (+nav-flash-test-command 'mouse-set-point
          (goto-char (posn-point (event-start event)))))
      (run-hooks 'zenit-switch-window-hook)
      (should-not pulses)
      (+nav-flash-test-idle)
      (should (= 1 (length pulses)))
      (should (= 14 (nth 2 (car pulses))))))

  :doc "A nil-command window hook during tracking cannot arm a focus timer"
  (progn
    (+nav-flash-test-command 'handle-select-window)
    (+nav-flash--pre-command-h)
    (let ((track-mouse t))
      (run-hooks 'zenit-switch-window-hook)
      (should-not pulses)
      (should-not +nav-flash--focus-timer))
    (goto-char 14)
    (+nav-flash--post-command-h)
    (run-hooks 'zenit-switch-window-hook)
    (should-not pulses)
    (+nav-flash-test-idle)
    (should (= 1 (length pulses)))
    (should (= 14 (nth 2 (car pulses)))))

  :doc "A focus hook during a long press waits for the final post-command"
  (let ((this-command 'handle-select-window)
        (real-this-command 'handle-select-window))
    (+nav-flash--pre-command-h)
    (let ((track-mouse t))
      (run-hooks 'zenit-switch-window-hook)
      (run-hooks 'zenit-switch-window-hook)
      (should +nav-flash--focus-target)
      (should-not +nav-flash--focus-timer)
      (+nav-flash--finish-focus)
      (should-not pulses)
      (should-not +nav-flash--focus-timer))
    (should-not +nav-flash--focus-timer)
    (goto-char 14)
    (+nav-flash--post-command-h)
    (+nav-flash-test-idle)
    (should (= 1 (length pulses)))
    (should (= 14 (nth 2 (car pulses)))))

  :doc "Ordinary excluded mouse window-hook pulses stay excluded"
  (progn
    (+nav-flash-test-command 'mouse-drag-region
      (run-hooks 'zenit-switch-window-hook))
    (should-not pulses)
    (should-not +nav-flash--focus-target))

  :doc "Keyboard window-hook pulses keep duplicate-position suppression"
  (progn
    (+nav-flash-test-command 'switch-to-buffer
      (run-hooks 'zenit-switch-window-hook)
      (run-hooks 'zenit-switch-window-hook))
    (should (= 1 (length pulses)))))

(zenit-deftest +nav-flash-blink-cursor-maybe-a
  ()
  (progn
    (should (advice-member-p #'+nav-flash-blink-cursor-maybe-a #'pulsar-pulse-line))
    (+nav-flash-test-with-buffer
      ;; Explicit pulses remain independent of pending automatic window focus.
      (+nav-flash-test-command 'handle-switch-frame)
      (+nav-flash-test-command 'mouse-set-point (pulsar-pulse-line))
      (should-not pulses)
      (+nav-flash-test-command 'switch-to-buffer
        (pulsar-pulse-line)
        (pulsar-pulse-line))
      (should (= 1 (length pulses)))
      (should +nav-flash--focus-target)
      (+nav-flash-test-idle)
      (should (= 1 (length pulses))))))

(zenit-deftest +nav-flash-test-command
  (:vars ((transient-before (default-value 'transient-mark-mode))
          (saved-transient evil-transient-vals)))
  (+nav-flash-test-with-buffer
    (evil-local-mode 1)
    (evil-normal-state)
    (let* ((window (selected-window))
           (start (list 'down-mouse-1 (list window 4 '(30 . 10) 100) 1))
           ;; A stationary release remapped to different text is normalized
           ;; and queued by the installed Zenit Evil workaround.
           (release (list 'drag-mouse-1 (event-start start)
                          (list window 14 '(30 . 10) 120)))
           (events (list release))
           (mouse-drag-copy-region nil)
           (was-advised (advice-member-p #'+evil--mouse-drag-region-a
                                        #'evil-mouse-drag-region)))
      (zenit-load (zenit-module-locate-path :editor 'evil "autoload/advice.el"))
      (unwind-protect
          (progn
            (advice-add #'evil-mouse-drag-region :around #'+evil--mouse-drag-region-a)
            (+nav-flash-test-command 'handle-switch-frame)
            (+nav-flash-test-command 'evil-mouse-drag-region
              (cl-letf (((symbol-function 'read-key)
                         (lambda (&rest _)
                           ;; Simulate arbitrary idle time while the button is
                           ;; held: no pulse, even though a timer was delivered.
                           (+nav-flash--finish-focus)
                           (should-not pulses)
                           (should-not +nav-flash--focus-timer)
                           (pop events)))
                        ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                        ((symbol-function 'mouse-set-point)
                         (lambda (event &rest _)
                           (goto-char (posn-point (event-start event))))))
                (evil-mouse-drag-region start)))
            (should (eq 'mouse-1 (caar unread-command-events)))
            (should-not mark-active)
            ;; The tracker has returned and post-command ran, but its queued
            ;; normalized up-event must still place point before the pulse.
            (+nav-flash-test-idle)
            (should-not pulses)
            (while unread-command-events
              (let* ((event (pop unread-command-events))
                     (last-input-event event)
                     (binding (key-binding (vector (car event)))))
                (+nav-flash-test-command binding
                  (funcall binding event))))
            (+nav-flash-test-idle)
            (should (= 1 (length pulses)))
            (should (= 14 (nth 2 (car pulses))))
            (should (eq 'normal evil-state))
            (should-not mark-active))
        (unless was-advised
          (advice-remove #'evil-mouse-drag-region #'+evil--mouse-drag-region-a))
        (evil-normal-state)
        (evil-local-mode -1)
        (should (eq transient-before (default-value 'transient-mark-mode)))
        (should (equal saved-transient evil-transient-vals))))))

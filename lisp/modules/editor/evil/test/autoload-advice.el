;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; editor/evil/test/autoload-advice.el

(require 'cl-lib)
(require 'evil)
(require 'zenit-test)
(zenit-require 'zenit-lib 'modules)
(zenit-modules-initialize)
(zenit-load (zenit-module-locate-path :editor 'evil "autoload/advice.el"))

(zenit-deftest +evil--mouse-drag-region-a
  (:vars ((transient-before (default-value 'transient-mark-mode))
          (saved-transient evil-transient-vals)
          (graphic t)))
  (save-window-excursion
    (with-temp-buffer
      (insert "alpha beta\n\nomega\n")
      (set-window-buffer (selected-window) (current-buffer))
      (evil-local-mode 1)
      (evil-normal-state)
      ;; A triple click follows the second click's Visual word selection.
      (when (= ,click-count 3)
        (evil-visual-char 1 5))
      (let* ((window (selected-window))
             (position (list window ,click '(30 . 10) 100))
             (start (list ,down-event position ,click-count))
             (end-position (list window ,end ,release-xy 120))
             (release (if (memq 'drag (event-modifiers ,up-event))
                          (list ,up-event position end-position)
                        (list ,up-event end-position ,click-count)))
             (events (append ,events (list release)))
             (this-command 'evil-mouse-drag-region)
             (deactivate-mark nil)
             (double-click-fuzz 3)
             (mouse-drag-copy-region nil)
             (unread-command-events nil))
        (unwind-protect
            (progn
              (advice-add #'evil-mouse-drag-region :around #'+evil--mouse-drag-region-a)
              (should (advice-member-p #'+evil--mouse-drag-region-a
                                      #'evil-mouse-drag-region))
              (cl-letf (((symbol-function 'read-key)
                         (lambda (&rest _)
                           (or (pop events) (error "Mouse event queue exhausted"))))
                        ((symbol-function 'display-graphic-p) (lambda (&rest _) graphic))
                        ((symbol-function 'mouse-set-point)
                         (lambda (event &rest _)
                           (goto-char (posn-point (event-start event))))))
                (evil-mouse-drag-region start)
                (ert-info ("Immediately after the mouse tracker")
                  (should (eq ,state evil-state)))
                ,assert
                ;; Evil's mark-activation hook is delayed, and it replays
                ;; up-events. Both must stay harmless for a plain click.
                (run-hooks 'post-command-hook)
                (while unread-command-events
                  (let* ((event (pop unread-command-events))
                         (last-input-event event)
                         (this-command (key-binding (vector (car event)))))
                    (should (functionp this-command))
                    (funcall this-command event)
                    (run-hooks 'post-command-hook)))
                (ert-info ("After post-command hooks and queued mouse events")
                  (should (eq ,final-state evil-state)))
                (when (eq ,state 'normal) (should-not mark-active))))
          (advice-remove #'evil-mouse-drag-region #'+evil--mouse-drag-region-a)
          ;; Visual state changes default transient-mark-mode; restore it
          ;; before killing this buffer so later cases stay independent.
          (evil-normal-state)
          (evil-local-mode -1)
          (should (eq transient-before (default-value 'transient-mark-mode)))
          (should (equal saved-transient evil-transient-vals))))))
  (click end down-event up-event click-count release-xy events state final-state assert)

  :doc "A plain single click stays Normal at the clicked position"
  4 4 'down-mouse-1 'mouse-1 1 '(30 . 10) nil 'normal 'normal
  (should (= 4 (point)))

  :doc "A frame focus event during a click does not enter Visual"
  4 4 'down-mouse-1 'mouse-1 1 '(30 . 10)
  (list (list 'switch-frame (selected-frame))) 'normal 'normal
  (should (= 4 (point)))

  :doc "A window selection event during a click does not enter Visual"
  4 4 'down-mouse-1 'mouse-1 1 '(30 . 10)
  (list (list 'select-window window)) 'normal 'normal
  (should (= 4 (point)))

  :doc "Motion inside the same character does not enter Visual"
  4 4 'down-mouse-1 'mouse-1 1 '(30 . 10)
  (list (list 'mouse-movement (list window 4 '(30 . 11) 110))) 'normal 'normal
  (should (= 4 (point)))

  :doc "A same-position click on an empty line stays Normal"
  12 12 'down-mouse-1 'mouse-1 1 '(30 . 10)
  (list (list 'mouse-movement (list window 12 '(30 . 11) 110))) 'normal 'normal
  (should (= 12 (point)))

  :doc "A same-position click at end of buffer stays Normal"
  19 19 'down-mouse-1 'mouse-1 1 '(30 . 10)
  (list (list 'mouse-movement (list window 19 '(30 . 11) 110))) 'normal 'normal
  (should (= 19 (point)))

  :doc "Moving to the next character still creates a Visual selection"
  4 5 'down-mouse-1 'mouse-1 1 '(40 . 10)
  (list (list 'mouse-movement (list window 5 '(40 . 10) 110))) 'visual 'visual
  (should-not (= (point) (mark)))

  :doc "A backward drag still creates a Visual selection"
  4 3 'down-mouse-1 'mouse-1 1 '(20 . 10)
  (list (list 'mouse-movement (list window 3 '(20 . 10) 110))) 'visual 'visual
  (should-not (= (point) (mark)))

  :doc "A focus event followed by a real drag still selects text"
  4 9 'down-mouse-1 'mouse-1 1 '(80 . 10)
  (list (list 'switch-frame (selected-frame))
        (list 'mouse-movement (list window 9 '(80 . 10) 110))) 'visual 'visual
  (should-not (= (point) (mark)))

  ;; Native Evil briefly enters Visual for a collapsed drag, then exits it
  ;; when post-command hooks see the inactive region. Preserve both stages.
  :doc "Dragging out and back reaches Evil's tracker unchanged"
  4 4 'down-mouse-1 'mouse-1 1 '(30 . 10)
  (list (list 'mouse-movement (list window 5 '(40 . 10) 110))
        (list 'mouse-movement (list window 4 '(30 . 10) 115))) 'visual 'normal
  (should (= (point) (mark)))

  :doc "Double-click word selection is preserved"
  4 4 'double-down-mouse-1 'double-mouse-1 2 '(30 . 10) nil 'visual 'visual
  (should (eq 'char evil-visual-selection))

  :doc "Triple-click line selection is preserved"
  4 4 'triple-down-mouse-1 'triple-mouse-1 3 '(30 . 10) nil 'visual 'visual
  (should (eq 'line evil-visual-selection))

  :doc "An active zero-width mark cannot reactivate Visual after redisplay"
  4 4 'down-mouse-1 'mouse-1 1 '(30 . 10) nil 'normal 'normal
  (progn
    (should-not mark-active)
    (goto-char 5))

  :doc "A stationary release remapped to different text is still a click"
  4 9 'down-mouse-1 'mouse-1 1 '(30 . 10)
  (list (list 'mouse-movement (list window 4 '(31 . 10) 110))) 'normal 'normal
  (should (memq release unread-command-events))

  :doc "Redisplay changing the motion event's text position is not a drag"
  4 9 'down-mouse-1 'mouse-1 1 '(30 . 10)
  (list (list 'mouse-movement (list window 9 '(30 . 10) 110))) 'normal 'normal
  (should (memq release unread-command-events))

  :doc "A native drag release caused by text remapping replays as a click"
  4 9 'down-mouse-1 'drag-mouse-1 1 '(30 . 10) nil 'normal 'normal
  (should (eq 'mouse-1 (caar unread-command-events)))

  :doc "Physical movement below the drag threshold is a click across glyphs"
  4 5 'down-mouse-1 'mouse-1 1 '(32 . 10)
  (list (list 'mouse-movement (list window 5 '(32 . 10) 110))) 'normal 'normal
  (should-not mark-active)

  :doc "Movement at exactly the drag threshold still starts a drag"
  4 5 'down-mouse-1 'mouse-1 1 '(33 . 10)
  (list (list 'mouse-movement (list window 5 '(33 . 10) 110))) 'visual 'visual
  (should-not (= (point) (mark)))

  :doc "A physical drag within one glyph reaches Evil's tracker unchanged"
  4 4 'down-mouse-1 'drag-mouse-1 1 '(34 . 10)
  (list (list 'mouse-movement (list window 4 '(34 . 10) 110))) 'visual 'visual
  (should (= (point) (mark)))

  :doc "Terminal cell movement uses the drag threshold's eighth-character units"
  4 5 'down-mouse-1 'drag-mouse-1 1 '(31 . 10)
  (progn
    (setq graphic nil)
    (list (list 'mouse-movement (list window 5 '(31 . 10) 110)))) 'visual 'visual
  (should-not (= (point) (mark))))

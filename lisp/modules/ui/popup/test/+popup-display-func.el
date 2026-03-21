;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; ui/popup/test/+popup-display-func.el
;;
;; Tests for popup window placement behavior.
;; Verifies the vslot/slot stacking logic of
;; `+popup-display-buffer-stacked-side-window-fn'.

(require 'zenit-test)
(zenit-require 'zenit-lib 'modules)
(zenit-modules-initialize)
(require 'el-patch)
(zenit-load (zenit-module-locate-path :ui 'popup "+popup-display-func.el"))


;;
;;; Helpers

(defvar +popup-test--buffers nil
  "Buffers created during tests, cleaned up in :after-each.")

(defun +popup-test--make-buffer (name)
  "Create a test buffer NAME, tracked for cleanup."
  (let ((buf (get-buffer-create name)))
    (push buf +popup-test--buffers)
    buf))

(defun +popup-test--display (buffer side &optional slot vslot extra-alist)
  "Display BUFFER as side window on SIDE with SLOT and VSLOT.
EXTRA-ALIST is an optional alist of additional display parameters
such as ((window-width . 20)).  Returns the window on success."
  (+popup-display-buffer-stacked-side-window-fn
   buffer
   `((side . ,side)
     (slot . ,(or slot 0))
     (vslot . ,(or vslot 0))
     ,@extra-alist)))

(defun +popup-test--windows-on-side (side)
  "Return all live windows whose `window-side' parameter is SIDE."
  (seq-filter
   (lambda (w)
     (eq (window-parameter w 'window-side) side))
   (window-list nil 'no-mini)))

(defun +popup-test--left-edge (win)
  "Left (column) edge of WIN."
  (nth 0 (window-edges win)))

(defun +popup-test--top-edge (win)
  "Top (row) edge of WIN."
  (nth 1 (window-edges win)))

(defun +popup-test--right-edge (win)
  "Right (column) edge of WIN."
  (nth 2 (window-edges win)))

(defun +popup-test--bottom-edge (win)
  "Bottom (row) edge of WIN."
  (nth 3 (window-edges win)))

(defun +popup-test--cleanup ()
  "Remove all side windows and kill test buffers."
  (ignore-errors (delete-other-windows))
  (dolist (buf +popup-test--buffers)
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (setq +popup-test--buffers nil))


;;
;;; Tests: Function definition

(zenit-deftest +popup-display-buffer-stacked-side-window-fn
  (:doc "`+popup-display-buffer-stacked-side-window-fn' is defined")
  (should (fboundp '+popup-display-buffer-stacked-side-window-fn)))


;;
;;; Tests: Basic placement — a single popup on each side

(zenit-deftest +popup-display-func/basic-bottom
  (:doc "A single popup opens on the bottom side"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf (+popup-test--make-buffer "*test-bottom*"))
         (win (+popup-test--display buf 'bottom)))
    (should win)
    (should (eq 'bottom (window-parameter win 'window-side)))
    (should (= 0 (window-parameter win 'window-vslot)))))

(zenit-deftest +popup-display-func/basic-top
  (:doc "A single popup opens on the top side"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf (+popup-test--make-buffer "*test-top*"))
         (win (+popup-test--display buf 'top)))
    (should win)
    (should (eq 'top (window-parameter win 'window-side)))
    (should (= 0 (window-parameter win 'window-vslot)))))

(zenit-deftest +popup-display-func/basic-right
  (:doc "A single popup opens on the right side"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf (+popup-test--make-buffer "*test-right*"))
         (win (+popup-test--display buf 'right)))
    (should win)
    (should (eq 'right (window-parameter win 'window-side)))
    (should (= 0 (window-parameter win 'window-vslot)))))

(zenit-deftest +popup-display-func/basic-left
  (:doc "A single popup opens on the left side"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf (+popup-test--make-buffer "*test-left*"))
         (win (+popup-test--display buf 'left)))
    (should win)
    (should (eq 'left (window-parameter win 'window-side)))
    (should (= 0 (window-parameter win 'window-vslot)))))


;;
;;; Tests: vslot parameter is recorded

(zenit-deftest +popup-display-func/vslot-parameter-set
  (:doc "The window-vslot parameter is set on created windows"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf (+popup-test--make-buffer "*test-vslot*"))
         (win (+popup-test--display buf 'bottom 0 -5)))
    (should win)
    (should (= -5 (window-parameter win 'window-vslot)))))


;;
;;; Tests: Same vslot replaces existing window

(zenit-deftest +popup-display-func/same-vslot-reuses-window
  (:doc "A popup with the same side/slot/vslot reuses the existing window"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf1 (+popup-test--make-buffer "*test-reuse1*"))
         (buf2 (+popup-test--make-buffer "*test-reuse2*"))
         (win1 (+popup-test--display buf1 'bottom 0 0))
         (win2 (+popup-test--display buf2 'bottom 0 0)))
    (should win1)
    (should win2)
    ;; Second display should reuse the same window
    (should (eq win1 win2))
    ;; Window should now show buf2
    (should (eq buf2 (window-buffer win2)))))


;;
;;; Tests: Bottom side — vslot stacking
;;
;; For :side 'bottom, vslot controls stacking from the bottom
;; edge toward the center (upward):
;;   - Lower vslot → closer to bottom edge (higher TOP value)
;;   - Higher vslot → closer to center (lower TOP value)

(zenit-deftest +popup-display-func/bottom-vslot-stacking-order
  (:doc "Bottom popups: higher vslot is above (closer to center)"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-low  (+popup-test--make-buffer "*test-bottom-low*"))
         (buf-high (+popup-test--make-buffer "*test-bottom-high*"))
         ;; Open low vslot first, then high
         (win-low  (+popup-test--display buf-low  'bottom 0 -10))
         (win-high (+popup-test--display buf-high 'bottom 0 10)))
    (should win-low)
    (should win-high)
    (should-not (eq win-low win-high))
    ;; Higher vslot should be above (smaller TOP edge = closer to center)
    (should (< (+popup-test--top-edge win-high)
               (+popup-test--top-edge win-low)))))

(zenit-deftest +popup-display-func/bottom-vslot-stacking-reverse-order
  (:doc "Bottom popups: vslot stacking is independent of opening order"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-low  (+popup-test--make-buffer "*test-bottom-low*"))
         (buf-high (+popup-test--make-buffer "*test-bottom-high*"))
         ;; Open high vslot first, then low
         (win-high (+popup-test--display buf-high 'bottom 0 10))
         (win-low  (+popup-test--display buf-low  'bottom 0 -10)))
    (should win-low)
    (should win-high)
    (should-not (eq win-low win-high))
    ;; Higher vslot should still be above regardless of opening order
    (should (< (+popup-test--top-edge win-high)
               (+popup-test--top-edge win-low)))))

(zenit-deftest +popup-display-func/top-vslot-stacking-reverse-order
  (:doc "Top popups: vslot stacking is independent of opening order"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-low  (+popup-test--make-buffer "*test-top-low*"))
         (buf-high (+popup-test--make-buffer "*test-top-high*"))
         ;; Open high vslot first, then low
         (win-high (+popup-test--display buf-high 'top 0 10))
         (win-low  (+popup-test--display buf-low  'top 0 -10)))
    (should win-low)
    (should win-high)
    (should-not (eq win-low win-high))
    ;; Higher vslot should still be below (closer to center) regardless of opening order
    (should (> (+popup-test--top-edge win-high)
               (+popup-test--top-edge win-low)))))


;;
;;; Tests: Top side — vslot stacking
;;
;; For :side 'top, vslot controls stacking from the top edge
;; toward the center (downward):
;;   - Lower vslot → closer to top edge (lower TOP value)
;;   - Higher vslot → closer to center (higher TOP value)

(zenit-deftest +popup-display-func/top-vslot-stacking-order
  (:doc "Top popups: higher vslot is below (closer to center)"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-low  (+popup-test--make-buffer "*test-top-low*"))
         (buf-high (+popup-test--make-buffer "*test-top-high*"))
         (win-low  (+popup-test--display buf-low  'top 0 -10))
         (win-high (+popup-test--display buf-high 'top 0 10)))
    (should win-low)
    (should win-high)
    (should-not (eq win-low win-high))
    ;; Higher vslot should be below (larger TOP edge = closer to center)
    (should (> (+popup-test--top-edge win-high)
               (+popup-test--top-edge win-low)))))


;;
;;; Tests: Right side — vslot stacking
;;
;; For :side 'right, vslot controls stacking from the right
;; edge toward the center (leftward):
;;   - Lower vslot → closer to right edge (larger LEFT value)
;;   - Higher vslot → closer to center (smaller LEFT value)

(zenit-deftest +popup-display-func/right-vslot-stacking-order
  (:doc "Right popups: higher vslot is more to the left (closer to center)"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-low  (+popup-test--make-buffer "*test-right-low*"))
         (buf-high (+popup-test--make-buffer "*test-right-high*"))
         (win-high (+popup-test--display buf-high 'right 0 10))
         (win-low  (+popup-test--display buf-low  'right 0 -10)))
    (should win-low)
    (should win-high)
    (should-not (eq win-low win-high))
    ;; Higher vslot should be more to the left (smaller LEFT edge)
    (should (< (+popup-test--left-edge win-high)
               (+popup-test--left-edge win-low)))))

(zenit-deftest +popup-display-func/right-vslot-stacking-reverse-order
  (:doc "Right popups: vslot stacking is independent of opening order"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-low  (+popup-test--make-buffer "*test-right-low*"))
         (buf-high (+popup-test--make-buffer "*test-right-high*"))
         ;; Open low vslot first, then high
         (win-low  (+popup-test--display buf-low  'right 0 -10))
         (win-high (+popup-test--display buf-high 'right 0 10)))
    (should win-low)
    (should win-high)
    (should-not (eq win-low win-high))
    ;; Higher vslot should still be more to the left
    (should (< (+popup-test--left-edge win-high)
               (+popup-test--left-edge win-low)))))

(zenit-deftest +popup-display-func/left-vslot-stacking-reverse-order
  (:doc "Left popups: vslot stacking is independent of opening order"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-low  (+popup-test--make-buffer "*test-left-low*"))
         (buf-high (+popup-test--make-buffer "*test-left-high*"))
         ;; Open high vslot first, then low
         (win-high (+popup-test--display buf-high 'left 0 10))
         (win-low  (+popup-test--display buf-low  'left 0 -10)))
    (should win-low)
    (should win-high)
    (should-not (eq win-low win-high))
    ;; Higher vslot should still be more to the right (closer to center)
    (should (> (+popup-test--left-edge win-high)
               (+popup-test--left-edge win-low)))))


;;
;;; Tests: Left side — vslot stacking
;;
;; For :side 'left, vslot controls stacking from the left edge
;; toward the center (rightward):
;;   - Lower vslot → closer to left edge (smaller LEFT value)
;;   - Higher vslot → closer to center (larger LEFT value)

(zenit-deftest +popup-display-func/left-vslot-stacking-order
  (:doc "Left popups: higher vslot is more to the right (closer to center)"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-low  (+popup-test--make-buffer "*test-left-low*"))
         (buf-high (+popup-test--make-buffer "*test-left-high*"))
         (win-low  (+popup-test--display buf-low  'left 0 -10))
         (win-high (+popup-test--display buf-high 'left 0 10)))
    (should win-low)
    (should win-high)
    (should-not (eq win-low win-high))
    ;; Higher vslot should be more to the right (larger LEFT edge)
    (should (> (+popup-test--left-edge win-high)
               (+popup-test--left-edge win-low)))))


;;
;;; Tests: Reopening preserves vslot ordering
;;
;; This is the core bug from the issue: after closing and
;; reopening a popup, it should return to its correct vslot
;; position, not just stack at the end.

(zenit-deftest +popup-display-func/right-reopen-preserves-vslot
  (:doc "Right side: reopening popup1 restores its vslot position relative to popup2"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  ;; Reproduce the exact issue from the bug report:
  ;; popup1 (vslot 10) and popup2 (vslot -10) on the right side.
  ;; popup1 should be to the LEFT of popup2 (higher vslot = closer to center).
  ;; After closing popup1 and reopening, it should still be to the left.
  (let* ((buf1 (+popup-test--make-buffer "*popup1*"))
         (buf2 (+popup-test--make-buffer "*popup2*"))
         ;; Step 1: Open popup1
         (win1 (+popup-test--display buf1 'right 0 10))
         ;; Step 2: Open popup2
         (win2 (+popup-test--display buf2 'right 0 -10)))
    (should win1)
    (should win2)
    ;; Verify initial ordering: popup1 (vslot 10) LEFT of popup2 (vslot -10)
    (should (< (+popup-test--left-edge win1)
               (+popup-test--left-edge win2)))
    ;; Step 3: Close popup1
    (delete-window win1)
    ;; Step 4: Reopen popup1
    (let ((win1-new (+popup-test--display buf1 'right 0 10)))
      (should win1-new)
      ;; popup1 should still be to the LEFT of popup2
      (let ((win2-current (get-buffer-window buf2)))
        (should win2-current)
        (should (< (+popup-test--left-edge win1-new)
                   (+popup-test--left-edge win2-current)))))))

(zenit-deftest +popup-display-func/bottom-reopen-preserves-vslot
  (:doc "Bottom side: reopening a popup restores its vslot position"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-high (+popup-test--make-buffer "*popup-high*"))
         (buf-low  (+popup-test--make-buffer "*popup-low*"))
         ;; Open both
         (win-high (+popup-test--display buf-high 'bottom 0 10))
         (win-low  (+popup-test--display buf-low  'bottom 0 -10)))
    (should win-high)
    (should win-low)
    ;; Verify initial ordering: high vslot above low vslot
    (should (< (+popup-test--top-edge win-high)
               (+popup-test--top-edge win-low)))
    ;; Close high vslot popup
    (delete-window win-high)
    ;; Reopen it
    (let ((win-high-new (+popup-test--display buf-high 'bottom 0 10)))
      (should win-high-new)
      ;; Should still be above
      (let ((win-low-current (get-buffer-window buf-low)))
        (should win-low-current)
        (should (< (+popup-test--top-edge win-high-new)
                   (+popup-test--top-edge win-low-current)))))))

(zenit-deftest +popup-display-func/top-reopen-preserves-vslot
  (:doc "Top side: reopening a popup restores its vslot position"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-high (+popup-test--make-buffer "*popup-high*"))
         (buf-low  (+popup-test--make-buffer "*popup-low*"))
         ;; Open both
         (win-low  (+popup-test--display buf-low  'top 0 -10))
         (win-high (+popup-test--display buf-high 'top 0 10)))
    (should win-low)
    (should win-high)
    ;; Verify initial ordering: high vslot below low vslot (closer to center)
    (should (> (+popup-test--top-edge win-high)
               (+popup-test--top-edge win-low)))
    ;; Close high vslot popup
    (delete-window win-high)
    ;; Reopen it
    (let ((win-high-new (+popup-test--display buf-high 'top 0 10)))
      (should win-high-new)
      ;; Should still be below (closer to center)
      (let ((win-low-current (get-buffer-window buf-low)))
        (should win-low-current)
        (should (> (+popup-test--top-edge win-high-new)
                   (+popup-test--top-edge win-low-current)))))))

(zenit-deftest +popup-display-func/left-reopen-preserves-vslot
  (:doc "Left side: reopening a popup restores its vslot position"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-high (+popup-test--make-buffer "*popup-high*"))
         (buf-low  (+popup-test--make-buffer "*popup-low*"))
         (win-low  (+popup-test--display buf-low  'left 0 -10))
         (win-high (+popup-test--display buf-high 'left 0 10)))
    (should win-low)
    (should win-high)
    ;; High vslot should be to the right (closer to center)
    (should (> (+popup-test--left-edge win-high)
               (+popup-test--left-edge win-low)))
    ;; Close low vslot popup
    (delete-window win-low)
    ;; Reopen it
    (let ((win-low-new (+popup-test--display buf-low 'left 0 -10)))
      (should win-low-new)
      ;; Should still be to the left (closer to edge)
      (let ((win-high-current (get-buffer-window buf-high)))
        (should win-high-current)
        (should (< (+popup-test--left-edge win-low-new)
                   (+popup-test--left-edge win-high-current)))))))


;;
;;; Tests: Window size preservation across vslots
;;
;; Opening a second vslot must not resize the first popup window.

(zenit-deftest +popup-display-func/right-vslot-preserves-width
  (:doc "Right popups: second vslot does not resize the first"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf1 (+popup-test--make-buffer "*test-size1*"))
         (buf2 (+popup-test--make-buffer "*test-size2*"))
         (width 15)
         (alist `((window-width . ,width)))
         (win1 (+popup-test--display buf1 'right 0 10 alist))
         (win1-width (window-total-width win1))
         (win2 (+popup-test--display buf2 'right 0 -10 alist)))
    (should win1)
    (should win2)
    ;; New window should have the requested width
    (should (= width (window-total-width win2)))
    ;; First window must not have been resized
    (should (= win1-width (window-total-width win1)))))

(zenit-deftest +popup-display-func/right-vslot-preserves-width-reverse
  (:doc "Right popups: size preserved regardless of opening order"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf1 (+popup-test--make-buffer "*test-size1*"))
         (buf2 (+popup-test--make-buffer "*test-size2*"))
         (width 15)
         (alist `((window-width . ,width)))
         ;; Open edge vslot first, then center
         (win1 (+popup-test--display buf1 'right 0 -10 alist))
         (win1-width (window-total-width win1))
         (win2 (+popup-test--display buf2 'right 0 10 alist)))
    (should win1)
    (should win2)
    (should (= width (window-total-width win2)))
    (should (= win1-width (window-total-width win1)))))

(zenit-deftest +popup-display-func/bottom-vslot-preserves-height
  (:doc "Bottom popups: second vslot does not resize the first"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf1 (+popup-test--make-buffer "*test-size1*"))
         (buf2 (+popup-test--make-buffer "*test-size2*"))
         (height 5)
         (alist `((window-height . ,height)))
         (win1 (+popup-test--display buf1 'bottom 0 10 alist))
         (win1-height (window-total-height win1))
         (win2 (+popup-test--display buf2 'bottom 0 -10 alist)))
    (should win1)
    (should win2)
    (should (= height (window-total-height win2)))
    (should (= win1-height (window-total-height win1)))))

(zenit-deftest +popup-display-func/top-vslot-preserves-height
  (:doc "Top popups: second vslot does not resize the first"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf1 (+popup-test--make-buffer "*test-size1*"))
         (buf2 (+popup-test--make-buffer "*test-size2*"))
         (height 5)
         (alist `((window-height . ,height)))
         (win1 (+popup-test--display buf1 'top 0 -10 alist))
         (win1-height (window-total-height win1))
         (win2 (+popup-test--display buf2 'top 0 10 alist)))
    (should win1)
    (should win2)
    (should (= height (window-total-height win2)))
    (should (= win1-height (window-total-height win1)))))

(zenit-deftest +popup-display-func/left-vslot-preserves-width
  (:doc "Left popups: second vslot does not resize the first"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf1 (+popup-test--make-buffer "*test-size1*"))
         (buf2 (+popup-test--make-buffer "*test-size2*"))
         (width 15)
         (alist `((window-width . ,width)))
         (win1 (+popup-test--display buf1 'left 0 -10 alist))
         (win1-width (window-total-width win1))
         (win2 (+popup-test--display buf2 'left 0 10 alist)))
    (should win1)
    (should win2)
    (should (= width (window-total-width win2)))
    (should (= win1-width (window-total-width win1)))))

(zenit-deftest +popup-display-func/right-reopen-preserves-width
  (:doc "Right popups: reopening a closed vslot does not resize the other"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf1 (+popup-test--make-buffer "*test-size1*"))
         (buf2 (+popup-test--make-buffer "*test-size2*"))
         (width 15)
         (alist `((window-width . ,width)))
         (win1 (+popup-test--display buf1 'right 0 10 alist))
         (win2 (+popup-test--display buf2 'right 0 -10 alist))
         (win2-width (window-total-width win2)))
    (should win1)
    (should win2)
    ;; Close win1, then reopen
    (delete-window win1)
    (let ((win1-new (+popup-test--display buf1 'right 0 10 alist)))
      (should win1-new)
      (should (= width (window-total-width win1-new)))
      ;; win2 must not have been resized
      (should (= win2-width (window-total-width (get-buffer-window buf2)))))))


;;
;;; Tests: Slot (lateral) positioning within the same vslot

(zenit-deftest +popup-display-func/bottom-slot-lateral-order
  (:doc "Bottom popups: positive slot is to the right, negative to the left"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-left  (+popup-test--make-buffer "*test-slot-left*"))
         (buf-right (+popup-test--make-buffer "*test-slot-right*"))
         (win-left  (+popup-test--display buf-left  'bottom -1 0))
         (win-right (+popup-test--display buf-right 'bottom  1 0)))
    (should win-left)
    (should win-right)
    (should-not (eq win-left win-right))
    ;; Negative slot should be to the left
    (should (< (+popup-test--left-edge win-left)
               (+popup-test--left-edge win-right)))))

(zenit-deftest +popup-display-func/right-slot-vertical-order
  (:doc "Right popups: positive slot is below, negative is above"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-above (+popup-test--make-buffer "*test-slot-above*"))
         (buf-below (+popup-test--make-buffer "*test-slot-below*"))
         (win-above (+popup-test--display buf-above 'right -1 0))
         (win-below (+popup-test--display buf-below 'right  1 0)))
    (should win-above)
    (should win-below)
    (should-not (eq win-above win-below))
    ;; Negative slot should be above (smaller TOP edge)
    (should (< (+popup-test--top-edge win-above)
               (+popup-test--top-edge win-below)))))

(zenit-deftest +popup-display-func/top-slot-lateral-order
  (:doc "Top popups: positive slot is to the right, negative to the left"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-left  (+popup-test--make-buffer "*test-slot-left*"))
         (buf-right (+popup-test--make-buffer "*test-slot-right*"))
         (win-left  (+popup-test--display buf-left  'top -1 0))
         (win-right (+popup-test--display buf-right 'top  1 0)))
    (should win-left)
    (should win-right)
    (should-not (eq win-left win-right))
    ;; Negative slot should be to the left
    (should (< (+popup-test--left-edge win-left)
               (+popup-test--left-edge win-right)))))

(zenit-deftest +popup-display-func/left-slot-vertical-order
  (:doc "Left popups: positive slot is below, negative is above"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-above (+popup-test--make-buffer "*test-slot-above*"))
         (buf-below (+popup-test--make-buffer "*test-slot-below*"))
         (win-above (+popup-test--display buf-above 'left -1 0))
         (win-below (+popup-test--display buf-below 'left  1 0)))
    (should win-above)
    (should win-below)
    (should-not (eq win-above win-below))
    ;; Negative slot should be above (smaller TOP edge)
    (should (< (+popup-test--top-edge win-above)
               (+popup-test--top-edge win-below)))))


;;
;;; Tests: Three vslot levels

(zenit-deftest +popup-display-func/bottom-three-vslots
  (:doc "Bottom side: three vslots stack in correct order"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-a (+popup-test--make-buffer "*test-vs-a*"))
         (buf-b (+popup-test--make-buffer "*test-vs-b*"))
         (buf-c (+popup-test--make-buffer "*test-vs-c*"))
         ;; Open in scrambled order: middle, low, high
         (win-b (+popup-test--display buf-b 'bottom 0  0))
         (win-c (+popup-test--display buf-c 'bottom 0 -5))
         (win-a (+popup-test--display buf-a 'bottom 0  5)))
    (should win-a)
    (should win-b)
    (should win-c)
    ;; Expected order from top to bottom: a (5), b (0), c (-5)
    (should (< (+popup-test--top-edge win-a)
               (+popup-test--top-edge win-b)))
    (should (< (+popup-test--top-edge win-b)
               (+popup-test--top-edge win-c)))))

(zenit-deftest +popup-display-func/right-three-vslots
  (:doc "Right side: three vslots stack in correct order"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-a (+popup-test--make-buffer "*test-vs-a*"))
         (buf-b (+popup-test--make-buffer "*test-vs-b*"))
         (buf-c (+popup-test--make-buffer "*test-vs-c*"))
         ;; Open in scrambled order
         (win-b (+popup-test--display buf-b 'right 0  0))
         (win-c (+popup-test--display buf-c 'right 0 -5))
         (win-a (+popup-test--display buf-a 'right 0  5)))
    (should win-a)
    (should win-b)
    (should win-c)
    ;; Expected order from left to right: a (5), b (0), c (-5)
    (should (< (+popup-test--left-edge win-a)
               (+popup-test--left-edge win-b)))
    (should (< (+popup-test--left-edge win-b)
               (+popup-test--left-edge win-c)))))


;;
;;; Tests: Mixed slot and vslot

(zenit-deftest +popup-display-func/bottom-mixed-slot-vslot
  (:doc "Bottom side: slots subdivide within a vslot level"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf-a (+popup-test--make-buffer "*test-mix-a*"))
         (buf-b (+popup-test--make-buffer "*test-mix-b*"))
         (buf-c (+popup-test--make-buffer "*test-mix-c*"))
         ;; Two popups in vslot 0 with different slots, one in vslot -5
         (win-a (+popup-test--display buf-a 'bottom -1 0))
         (win-b (+popup-test--display buf-b 'bottom  1 0))
         (win-c (+popup-test--display buf-c 'bottom  0 -5)))
    (should win-a)
    (should win-b)
    (should win-c)
    ;; win-a and win-b share a vslot level (0) — both should be above win-c (-5)
    (should (< (+popup-test--top-edge win-a)
               (+popup-test--top-edge win-c)))
    (should (< (+popup-test--top-edge win-b)
               (+popup-test--top-edge win-c)))
    ;; Within vslot 0: win-a (slot -1) left of win-b (slot 1)
    (should (< (+popup-test--left-edge win-a)
               (+popup-test--left-edge win-b)))))


;;
;;; Tests: Error cases

(zenit-deftest +popup-display-func/invalid-side-signals-error
  (:doc "Invalid side value signals an error"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let ((buf (+popup-test--make-buffer "*test-invalid*")))
    (should-error
     (+popup-display-buffer-stacked-side-window-fn
      buf '((side . invalid) (slot . 0) (vslot . 0))))))

(zenit-deftest +popup-display-func/invalid-slot-signals-error
  (:doc "Non-numeric slot signals an error"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let ((buf (+popup-test--make-buffer "*test-invalid*")))
    (should-error
     (+popup-display-buffer-stacked-side-window-fn
      buf '((side . bottom) (slot . "bad") (vslot . 0))))))

(zenit-deftest +popup-display-func/invalid-vslot-signals-error
  (:doc "Non-numeric vslot signals an error"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let ((buf (+popup-test--make-buffer "*test-invalid*")))
    (should-error
     (+popup-display-buffer-stacked-side-window-fn
      buf '((side . bottom) (slot . 0) (vslot . "bad"))))))


;;
;;; Tests: vslot persistent parameter

(zenit-deftest +popup-display-func/vslot-persistent-parameter
  (:doc "window-vslot is added to `window-persistent-parameters'"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf (+popup-test--make-buffer "*test-persist*"))
         (_win (+popup-test--display buf 'bottom 0 -3)))
    (should (assq 'window-vslot window-persistent-parameters))))


;;
;;; Tests: Default values

(zenit-deftest +popup-display-func/defaults-side-bottom
  (:doc "Side defaults to bottom when omitted from alist"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf (+popup-test--make-buffer "*test-defaults*"))
         (win (+popup-display-buffer-stacked-side-window-fn
               buf '((slot . 0) (vslot . 0)))))
    (should win)
    (should (eq 'bottom (window-parameter win 'window-side)))))

(zenit-deftest +popup-display-func/defaults-slot-zero
  (:doc "Slot defaults to 0 when omitted from alist"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf (+popup-test--make-buffer "*test-defaults*"))
         (win (+popup-display-buffer-stacked-side-window-fn
               buf '((side . bottom) (vslot . 0)))))
    (should win)
    (should (= 0 (window-parameter win 'window-slot)))))

(zenit-deftest +popup-display-func/defaults-vslot-zero
  (:doc "Vslot defaults to 0 when omitted from alist"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf (+popup-test--make-buffer "*test-defaults*"))
         (win (+popup-display-buffer-stacked-side-window-fn
               buf '((side . bottom) (slot . 0)))))
    (should win)
    (should (= 0 (window-parameter win 'window-vslot)))))


;;
;;; Tests: Window dedication

(zenit-deftest +popup-display-func/window-dedicated-popup
  (:doc "Created window is dedicated with the `popup' flag"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf (+popup-test--make-buffer "*test-dedicated*"))
         (win (+popup-test--display buf 'bottom)))
    (should win)
    (should (eq 'popup (window-dedicated-p win)))))


;;
;;; Tests: Correct buffer displayed

(zenit-deftest +popup-display-func/buffer-displayed-in-window
  (:doc "The requested buffer is displayed in the created window"
   :before-each
   (setq window--sides-inhibit-check t)
   :after-each
   (+popup-test--cleanup))
  (let* ((buf (+popup-test--make-buffer "*test-buf-display*"))
         (win (+popup-test--display buf 'right 0 0)))
    (should win)
    (should (eq buf (window-buffer win)))))

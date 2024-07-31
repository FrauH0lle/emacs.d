;; ui/popup/+popup-display-func.el -*- lexical-binding: t; -*-

;; PATCH We create `+popup-display-buffer-stacked-side-window-fn' by creating a
;;   fork of `display-buffer-in-side-window'
(cl-eval-when (compile)
  (require 'el-patch)
  (require 'window))

(el-patch-feature window)

(after! window
  (el-patch-defun (el-patch-swap display-buffer-in-side-window +popup-display-buffer-stacked-side-window-fn) (buffer alist)
    (el-patch-concat
      "Display BUFFER in a side window of the selected frame.\n"
      (el-patch-add "Fork of `display-buffer-in-side-window'.\n\n")

      "ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.  The following two symbols, when used in ALIST, have
a special meaning:

 `side' denotes the side of the frame where the new window shall
   be located.  Valid values are `bottom', `right', `top' and
   `left'.  The default is `bottom'.

 `slot' if non-nil, specifies the window slot where to display
   BUFFER.  A value of zero or nil means use the middle slot on
   the specified side.  A negative value means use a slot
   preceding (that is, above or on the left of) the middle slot.
   A positive value means use a slot following (that is, below or
   on the right of) the middle slot.  The default is zero.\n\n"

      (el-patch-add " `vslot' like `slot' but controls popup stacking (from the edge
   of the frame toward the center).\n\n")

      "If the current frame size or the settings of `window-sides-slots'
do not permit making a new window, a suitable existing window may
be reused and have its `window-slot' parameter value accordingly
modified.

Unless `display-buffer-mark-dedicated' is non-nil, dedicate the
side window used to BUFFER so that it does not get reused by
other `display-buffer' action functions.  Return the window used
for displaying BUFFER, nil if no suitable window can be found.

This function installs the `window-side' and `window-slot'
parameters and makes them persistent.  It neither modifies ALIST
nor installs any other window parameters unless they have been
explicitly provided via a `window-parameters' entry in ALIST.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter.")
    (let* ((side (or (cdr (assq 'side alist)) 'bottom))
           (slot (or (cdr (assq 'slot alist)) 0))
           (el-patch-add (vslot (or (cdr (assq 'vslot alist)) 0)))
           (left-or-right (memq side '(left right)))
           (el-patch-add (display-buffer-mark-dedicated (or display-buffer-mark-dedicated 'popup))))
      (cond
       ((not (memq side '(top bottom left right)))
        (error "Invalid side %s specified" side))
       ((not (numberp slot))
        (error "Invalid slot %s specified" slot))
       (el-patch-add
         ((not (numberp vslot))
          (error "Invalid vslot %s specified" vslot))))

      (let* ((el-patch-add
               (live (get-window-with-predicate
                      (lambda (window)
                        (and (eq (window-parameter window 'window-side) side)
                             (eq (window-parameter window 'window-vslot) vslot)))
                      nil)))
             ;; As opposed to the `window-side' property, our `window-vslot'
             ;; parameter is set only on a single live window and never on
             ;; internal windows. Moreover, as opposed to
             ;; `window-with-parameter' (as used by the original
             ;; `display-buffer-in-side-window'), `get-window-with-predicate'
             ;; only returns live windows anyway. In any case, we will have
             ;; missed the major side window and got a child instead if the
             ;; major side window happens to be an internal window with multiple
             ;; children. In that case, all childen should have the same
             ;; `window-vslot' parameter, and the major side window is the
             ;; parent of the live window.
             (el-patch-add
               (prev (and live (window-prev-sibling live)))
               (next (and live (window-next-sibling live)))
               (prev-vslot (and prev (window-parameter prev 'window-vslot)))
               (next-vslot (and next (window-parameter next 'window-vslot))))
             (major (el-patch-swap
                      (window-with-parameter 'window-side side nil t)
                      (and live
                           (if (or (eq prev-vslot vslot) (eq next-vslot vslot))
                               (window-parent live)
                             live))))
             ;; `major' is the major window on SIDE, `windows' the list of
             ;; life windows on SIDE.
             (reversed (window--sides-reverse-on-frame-p (selected-frame)))
             (windows
              (cond
               ((window-live-p major)
                (list major))
               ((window-valid-p major)
                (let* ((first (window-child major))
                       (next (window-next-sibling first))
                       (windows (list next first)))
                  (setq reversed (> (window-parameter first 'window-slot)
                                    (window-parameter next 'window-slot)))
                  (while (setq next (window-next-sibling next))
                    (setq windows (cons next windows)))
                  (if reversed windows (nreverse windows))))))
             (slots (when major (max 1 (window-child-count major))))
             (max-slots
              (nth (cond
                    ((eq side 'left) 0)
                    ((eq side 'top) 1)
                    ((eq side 'right) 2)
                    ((eq side 'bottom) 3))
                   window-sides-slots))
             (window--sides-inhibit-check t)
             (el-patch-remove
               (alist (if (assq 'dedicated alist)
                          alist
                        (cons `(dedicated . ,(or display-buffer-mark-dedicated 'side))
                              alist))))
             window this-window this-slot prev-window next-window
             best-window best-slot abs-slot)

        (cond
         ((and (numberp max-slots) (<= max-slots 0))
          ;; No side-slots available on this side.  Don't raise an error,
          ;; just return nil.
          nil)
         ((not windows)
          ;; No major side window exists on this side, make one.
          (el-patch-swap
            (window--make-major-side-window buffer side slot alist)
            (cl-letf (((symbol-function 'window--make-major-side-window-next-to)
                       (lambda (_side) (frame-root-window (selected-frame)))))
              (when-let (window (window--make-major-side-window buffer side slot alist))
                (set-window-parameter window 'window-vslot vslot)
                (add-to-list 'window-persistent-parameters '(window-vslot . writable))
                window))))
         (t
          ;; Scan windows on SIDE.
          (catch 'found
            (dolist (window windows)
              (setq this-slot (window-parameter window 'window-slot))
              (cond
               ;; The following should not happen and probably be checked
               ;; by window--sides-check.
               ((not (numberp this-slot)))
               ((= this-slot slot)
                ;; A window with a matching slot has been found.
                (setq this-window window)
                (throw 'found t))
               (t
                ;; Check if this window has a better slot value wrt the
                ;; slot of the window we want.
                (setq abs-slot
                      (if (or (and (> this-slot 0) (> slot 0))
                              (and (< this-slot 0) (< slot 0)))
                          (abs (- slot this-slot))
                        (+ (abs slot) (abs this-slot))))
                (unless (and best-slot (<= best-slot abs-slot))
                  (setq best-window window)
                  (setq best-slot abs-slot))
                (if reversed
                    (cond
                     ((<= this-slot slot)
                      (setq next-window window))
                     ((not prev-window)
                      (setq prev-window window)))
                  (cond
                   ((<= this-slot slot)
                    (setq prev-window window))
                   ((not next-window)
                    (setq next-window window))))))))

          ;; `this-window' is the first window with the same SLOT.
          ;; `prev-window' is the window with the largest slot < SLOT.  A new
          ;; window will be created after it.
          ;; `next-window' is the window with the smallest slot > SLOT.  A new
          ;; window will be created before it.
          ;; `best-window' is the window with the smallest absolute difference
          ;; of its slot and SLOT.
          (or (and this-window
                   ;; Reuse `this-window'.
                   (with-current-buffer buffer
                     (setq window--sides-shown t))
                   (window--display-buffer buffer this-window 'reuse alist))
              (and (or (not max-slots) (< slots max-slots))
                   (or (and next-window
                            ;; Make new window before `next-window'.
                            (let ((next-side (if left-or-right 'above 'left))
                                  (el-patch-add (+popup--internal t))
                                  (window-combination-resize 'side))
                              (setq window (split-window-no-error
                                            next-window nil next-side))))
                       (and prev-window
                            ;; Make new window after `prev-window'.
                            (let ((prev-side (if left-or-right 'below 'right))
                                  (el-patch-add (+popup--internal t))
                                  (window-combination-resize 'side))
                              (setq window (split-window-no-error
                                            prev-window nil prev-side)))))
                   (set-window-parameter window 'window-slot slot)
                   (el-patch-add
                     (set-window-parameter window 'window-vslot vslot))
                   (with-current-buffer buffer
                     (setq window--sides-shown t))
                   (window--display-buffer buffer window 'window alist))
              (and best-window
                   ;; Reuse `best-window'.
                   (progn
                     ;; Give best-window the new slot value.
                     (set-window-parameter best-window 'window-slot slot)
                     (with-current-buffer buffer
                       (setq window--sides-shown t))
                     (window--display-buffer
                      buffer best-window 'reuse alist))))))))))

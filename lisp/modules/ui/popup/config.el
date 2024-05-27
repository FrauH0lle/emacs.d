;; ui/popup/config.el -*- lexical-binding: t; -*-

;; PATCH We create `+popup-display-buffer-stacked-side-window-fn' by creating a
;;   fork of `display-buffer-in-side-window'
(eval-when-compile
  (require 'el-patch))

(el-patch-feature window)
(with-eval-after-load 'window
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
           (left-or-right (memq side '(left right))))
      (cond
       ((not (memq side '(top bottom left right)))
        (error "Invalid side %s specified" side))
       ((not (numberp slot))
        (error "Invalid slot %s specified" slot))
       (el-patch-add
         ((not (numberp vslot))
          (error "Invalid vslot %s specified" vslot))))

      (let* ((major (el-patch-swap
                      (window-with-parameter 'window-side side nil t)
                      (get-window-with-predicate
                       (lambda (window)
                         (and (eq (window-parameter window 'window-side) side)
                              (eq (window-parameter window 'window-vslot) vslot)))
                       nil)))
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
             (alist (if (assq 'dedicated alist)
                        alist
                      (cons `(dedicated . ,(or display-buffer-mark-dedicated 'side))
                            alist)))
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


(defconst +popup-window-parameters '(ttl quit select modeline popup)
  "A list of custom parameters to be added to
`window-persistent-parameters'. Modifying this has no effect,
unless done before ui/popup loads.")

(defvar +popup-default-display-buffer-actions
  '(+popup-display-buffer-stacked-side-window-fn)
  "The functions to use to display the popup buffer.")

(defvar +popup-default-alist
  '((window-height . 0.16) ; remove later
    (reusable-frames . visible))
  "The default alist for `display-buffer-alist' rules.")

(defvar +popup-default-parameters
  '((transient . t)   ; remove later
    (quit . t)        ; remove later
    (select . ignore) ; remove later
    (no-other-window . t))
  "The default window parameters.")


(defvar +popup-margin-width 1
  "Size of the margins to give popup windows. Set this to nil to
disable margin adjustment.")

(defvar +popup-open-popup-alist nil
  "Alist of currently live (window . buffer)s that are treated as
popups.")

(defvar +popup-buried-popup-alist nil
  "Alist of currently buried (window . buffer)s that are treated as
popups.")

(defvar +popup--inhibit-transient nil
  "If non-nil, do not kill popup buffer (window parameter ttl).")
(defvar +popup--inhibit-select nil
  "If non-nil, do not select popup buffer (window parameter select).")
(defvar +popup--old-display-buffer-alist nil
  "Alist storing old `display-buffer-alist'.")
(defvar +popup--remember-last t
  "If non-nil, store last popup.")
(defvar +popup--last nil
  "Stores the last popup configuration.")
(defvar-local +popup--timer nil
  "Stores current timer for killing the buffer.")
(defvar-local +popup--popup-status nil
  "Identifies a buffer as a popup by its buffer-local value.
Valid values are \\='popup, \\='raised or nil.")


;;
;;; Global modes

(defvar +popup-mode-map (make-sparse-keymap)
  "Active keymap in a session with the popup system enabled. See
`+popup-mode'.")

(defvar +popup-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (when (modulep! :editor evil)
      ;; For maximum escape coverage in emacs state buffers; this only works in
      ;; GUI Emacs, in tty Emacs use C-g instead
      (define-key map [escape] #'zenit/escape))
    map)
  "Active keymap in popup windows. See `+popup-buffer-mode'.")

(define-minor-mode +popup-mode
  "Global minor mode representing the popup management system."
  :init-value nil
  :global t
  :keymap +popup-mode-map
  (cond (+popup-mode
         (add-hook 'zenit-escape-hook #'+popup-close-on-escape-h 'append)
         (setq +popup--old-display-buffer-alist display-buffer-alist
               display-buffer-alist +popup--display-buffer-alist
               window--sides-inhibit-check t)
         (dolist (prop +popup-window-parameters)
           (push (cons prop 'writable) window-persistent-parameters))
         (add-hook 'window-configuration-change-hook #'+popup-update-popups-h)
         (add-hook 'zenit-switch-frame-hook #'+popup-update-popups-h))
        (t
         (remove-hook 'zenit-escape-hook #'+popup-close-on-escape-h)
         (remove-hook 'window-configuration-change-hook #'+popup-update-popups-h)
         (remove-hook 'zenit-switch-frame-hook #'+popup-update-popups-h)
         (setq display-buffer-alist +popup--old-display-buffer-alist
               window--sides-inhibit-check nil
               +popup-buried-popup-alist nil
               +popup-open-popup-alist nil)
         (+popup-cleanup-rules-h)
         (dolist (prop +popup-window-parameters)
           (delq (assq prop window-persistent-parameters)
                 window-persistent-parameters)))))


(define-minor-mode +popup-buffer-mode
  "Minor mode for individual popup windows.

It is enabled when a buffer is displayed in a popup window and
disabled when that window has been changed or closed."
  :init-value nil
  :keymap +popup-buffer-mode-map
  (if (not +popup-buffer-mode)
      (remove-hook 'after-change-major-mode-hook #'+popup-set-modeline-on-enable-h t)
    (add-hook 'after-change-major-mode-hook #'+popup-set-modeline-on-enable-h
              nil 'local)
    (when (timerp +popup--timer)
      (remove-hook 'kill-buffer-hook #'+popup-kill-buffer-hook-h t)
      (cancel-timer +popup--timer)
      (setq +popup--timer nil))))

(put '+popup-buffer-mode 'permanent-local t)
(put '+popup-buffer-mode 'permanent-local-hook t)
(put '+popup-set-modeline-on-enable-h 'permanent-local-hook t)


;;
;;; Macros

(defmacro with-popup-rules! (rules &rest body)
  "Evaluate BODY with popup RULES. RULES is a list of popup rules. Each rule
should match the arguments of `+popup-define' or the :popup setting."
  (declare (indent defun))
  `(let ((+popup--display-buffer-alist +popup--old-display-buffer-alist)
         display-buffer-alist)
     (set-popup-rules! ,rules)
     (when (bound-and-true-p +popup-mode)
       (setq display-buffer-alist +popup--display-buffer-alist))
     ,@body))

(defmacro save-popups! (&rest body)
  "Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa)."
  `(let* ((in-popup-p (+popup-buffer-p))
          (popups (+popup-windows))
          (+popup--inhibit-transient t)
          buffer-list-update-hook
          +popup--last)
     (dolist (p popups)
       (+popup/close p 'force))
     (unwind-protect
         (progn ,@body)
       (when popups
         (let ((origin (selected-window)))
           (+popup/restore)
           (unless in-popup-p
             (select-window origin)))))))


;;
;;; Default popup rules & bootstrap

(set-popup-rules!
  '(("^\\*Completions" :ignore t)
    ("^\\*Local variables\\*$"
     :vslot -1 :slot 1 :size +popup-shrink-to-fit)
    ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)"
     :vslot -2 :size 0.3  :autosave t :quit t :ttl nil)
    ("^\\*\\(?:zenit \\|Pp E\\)"  ; transient buffers (no interaction required)
     :vslot -3 :size +popup-shrink-to-fit :autosave t :select ignore :quit t :ttl 0)
    ("^\\*zenit:"  ; editing buffers (interaction required)
     :vslot -4 :size 0.35 :autosave t :select t :modeline t :quit nil :ttl t)
    ("^\\*zenit:\\(?:v?term\\|e?shell\\)-popup"  ; editing buffers (interaction required)
     :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil)
    ("^\\*zenit:scratch"
     :ignore t)
    ("^\\*\\(?:Wo\\)?Man "
     :vslot -6 :size 0.45 :select t :quit t :ttl 0)
    ("^\\*Calc"
     :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0)
    ("^\\*Customize"
     :slot 2 :side right :size 0.5 :select t :quit nil)
    ("^ \\*undo-tree\\*"
     :slot 2 :side left :size 20 :select t :quit t)
    ;; `help-mode', `helpful-mode'
    ("^\\*\\([Hh]elp\\|Apropos\\)"
     :slot 2 :vslot -8 :size 0.42 :select t)
    ("^\\*eww\\*"  ; `eww' (and used by dash docsets)
     :vslot -11 :size 0.35 :select t)
    ("^\\*xwidget"
     :vslot -11 :size 0.35 :select nil)
    ("^\\*info\\*$"  ; `Info-mode'
     :slot 2 :vslot 2 :size 0.45 :select t))
  '(("^\\*Warnings" :vslot 99 :size 0.25)
    ("^\\*Backtrace" :vslot 99 :size 0.4 :quit nil)
    ("^\\*CPU-Profiler-Report "    :side bottom :vslot 100 :slot 1 :height 0.4 :width 0.5 :quit nil)
    ("^\\*Memory-Profiler-Report " :side bottom :vslot 100 :slot 2 :height 0.4 :width 0.5 :quit nil)
    ("^\\*Process List\\*" :side bottom :vslot 101 :size 0.25 :select t :quit t)
    ("^\\*\\(?:Proced\\|timer-list\\|Abbrevs\\|Output\\|Occur\\|unsent mail.*?\\|message\\)\\*" :ignore t)))

(add-hook 'zenit-init-ui-hook #'+popup-mode 'append)

(add-hook! '+popup-buffer-mode-hook
           #'+popup-adjust-fringes-h
           #'+popup-adjust-margins-h
           #'+popup-set-modeline-on-enable-h
           #'+popup-unset-modeline-on-disable-h)


;;
;;; Hacks

(load! "+hacks")

;; ui/popup/config.el -*- lexical-binding: t; -*-

(defvar +popup--internal nil)

(defconst +popup-window-parameters '(ttl quit select modeline popup tabbed)
  "A list of custom parameters to be added to
`window-persistent-parameters'. Modifying this has no
effect, unless done before ui/popup loads.")

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

(defvar +popup-reference-buffers nil
  "List of buffer designators that should be treated as popups.

Each entry can be a

  - regexp (string) matching buffer name
  - major mode (symbol)
  - predicate function taking a buffer
  - (cons (regexp/mode/fn) \\='hide) to suppress the popup")

(defvar-local +popup-buffer-status nil
  "Property list describing a popup buffer.
This plist can have the following properties:

  :status Identifies a buffer as a popup
    Valid values are \\='popup, \\='raised, \\='user-popup or nil.

    \\='popup This is a popup buffer specified in
    `+popup-reference-buffers'.

    \\='raised This is a popup buffer raised to regular status by
    the user.

    \\='user-popup This is a regular buffer lowered to popup
    status by the user.

    \\='suppressed This popup buffer is suppressed.

  :tabbed This popup buffer is part of a tabbed window.

    Valid values are \\='top, \\='bottom, \\='left, \\='right and
    nil.

For :ttl, :quit, :select, :modeline and :autosave see
`set-popup-rule!'.")

(defvar +popup-group-function nil
  "Function that returns a popup context.

When set to nil popups are not grouped by context.

This function is called with no arguments and should return a
string or symbol identifying a popup buffer's group. This
identifier is used to associate popups with regular buffers (such
as by project, directory, or `major-mode') so that popup-cycling
from a regular buffer is restricted to its associated group.")

(defvar +popup-open-buffers-alist nil
  "Alist of currently live (window . buffer)s that are treated as popups.")

(defvar +popup-buried-buffers-alist nil
  "Alist of currently buried (window . buffer)s that are treated as popups.

If `+popup-group-function' is non-nil, these are
grouped by the predicate `+popup-group-function'.")


(defvar +popup--inhibit-transient nil
  "If non-nil, do not kill popup buffer (window parameter ttl).")
(defvar +popup--inhibit-select nil
  "If non-nil, do not select popup buffer (window parameter
 select).")
(defvar +popup--old-display-buffer-alist nil
  "Alist storing old `display-buffer-alist'.")
(defvar +popup--old-reference-buffers nil
  "Alist storing old `+popup-reference-buffers'.")
(defvar +popup--remember-last t
  "If non-nil, store last popup.")
(defvar +popup--last nil
  "Stores the last popup configuration.")
(defvar-local +popup--parents nil
  "Stores the popup's parent buffers.")
(defvar +popup--ignore-parent nil
  "If non-nil, do not record parent buffer.")
(defvar-local +popup--timer nil
  "Stores current timer for killing the buffer.")


;;
;;; Global modes

(defvar +popup-mode-map (make-sparse-keymap)
  "Active keymap in a session with the popup system enabled.
See `+popup-mode'.")

(defvar +popup-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (when (modulep! :editor evil)
      ;; For maximum escape coverage in emacs state buffers; this only works in
      ;; GUI Emacs, in tty Emacs use C-g instead
      (define-key map [escape] #'zenit/escape))
    map)
  "Active keymap in popup windows. See `+popup-buffer-mode'.")

(define-minor-mode +popup-mode
  "Global minor mode representing Doom's popup management system."
  :init-value nil
  :global t
  :group 'zenit
  :keymap +popup-mode-map
  (cond (;; Turning ON
         +popup-mode
         (+popup-update-reference-vars)
         (+popup-update-popup-alists-h)

         (add-hook 'window-configuration-change-hook #'+popup-suppress-popups-h)
         (add-hook 'window-configuration-change-hook #'+popup-update-popup-alists-h)
         (add-hook 'zenit-switch-buffer-hook #'+popup-update-popup-alists-h)
         (add-hook 'select-frame-hook #'+popup-update-popup-alists-h)

         (add-hook 'zenit-escape-hook #'+popup-close-on-escape-h 'append)
         (setq +popup--old-display-buffer-alist display-buffer-alist
               display-buffer-alist +popup--display-buffer-alist
               +popup--old-reference-buffers +popup-reference-buffers
               +popup-reference-buffers +popup--reference-buffers
               window--sides-inhibit-check t)
         (dolist (prop +popup-window-parameters)
           (push (cons prop 'writable) window-persistent-parameters)))
        (;; Turning OFF
         t
         (remove-hook 'zenit-switch-buffer-hook #'+popup-update-popup-alists-h)
         (remove-hook 'window-configuration-change-hook #'+popup-update-popup-alists-h)
         (remove-hook 'window-configuration-change-hook #'+popup-suppress-popups-h)
         (remove-hook 'select-frame-hook #'+popup-update-popup-alists-h)

         (remove-hook 'zenit-escape-hook #'+popup-close-on-escape-h)
         (setq display-buffer-alist +popup--old-display-buffer-alist
               +popup-reference-buffers +popup--old-reference-buffers
               window--sides-inhibit-check nil)
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
  (cond (;; Turning ON
         +popup-buffer-mode

         (when (and (+popup-parameter 'tabbed)
                    (+popup-parameter 'tabbed (current-buffer)))
           (set-window-dedicated-p (selected-window) nil)
           (set-window-parameter (selected-window) 'quit nil)

           (setq-local tab-line-tabs-function #'+popup-tabs-fn)
           (tab-line-mode +1))

         (add-hook 'after-change-major-mode-hook #'+popup-set-modeline-on-enable-h
                   nil 'local)
         (when (timerp +popup--timer)
           (remove-hook 'kill-buffer-hook #'+popup-kill-buffer-hook-h t)
           (cancel-timer +popup--timer)
           (setq +popup--timer nil)))
        (;; Turning OFF
         t
         (setq +popup-buffer-status (plist-put +popup-buffer-status :tabbed nil))
         (when (bound-and-true-p tab-line-mode)
           (tab-line-mode -1))

         (remove-hook 'after-change-major-mode-hook #'+popup-set-modeline-on-enable-h t))))

(put '+popup-buffer-mode 'permanent-local t)
(put '+popup-buffer-mode 'permanent-local-hook t)
(put '+popup-set-modeline-on-enable-h 'permanent-local-hook t)


;;
;;; Default popup rules & bootstrap

(set-popup-rules!
  '(("^\\*Completions" :ignore t)
    ("^\\*Local variables\\*$"
     :vslot -1 :slot 1 :size +popup-shrink-to-fit)
    ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)"
     :vslot -2 :size 0.33  :autosave t :quit t :ttl nil)
    ("^\\*\\(?:zenit \\|Pp E\\)"  ; transient buffers (no interaction required)
     :vslot -3 :size +popup-shrink-to-fit :autosave t :select ignore :quit t :ttl 0)
    ("^\\*zenit:"  ; editing buffers (interaction required)
     :vslot -4 :size 0.35 :autosave t :select t :modeline t :quit nil :ttl t)
    ("^\\*zenit:\\(?:v?term\\|e?shell\\)-popup"  ; editing buffers (interaction required)
     :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil)
    ("^\\*zenit:scratch"
     :slot 2 :vslot -4 :side right :size 0.5 :autosave t :select t :modeline t :quit nil :ttl t)
    ("^\\*\\(?:Wo\\)?Man "
     :vslot -6 :size 0.45 :select t :quit t :ttl 0)
    ("^\\*Calc"
     :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0)
    ("^\\*Customize"
     :slot 2 :side right :size 0.5 :select t :quit nil)
    ("^ \\*undo-tree\\*"
     :slot 2 :side left :size 20 :select t :quit t)
    ("^ \\*undo-tree Diff\\*"
     :slot 2 :side bottom :size +popup-shrink-to-fit :select ignore :quit t :ttl 0)
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

;; Customized display buffer function
;; PATCH 2024-08-02: `window'
(el-patch-feature window)
(compile-along! "+popup-display-func")
(autoload! "+popup-display-func" #'+popup-display-buffer-stacked-side-window-fn)

(compile-along! "+hacks")

(add-hook! '+popup-mode-hook :append
  (load! "+hacks"))

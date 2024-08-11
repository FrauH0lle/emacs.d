;; ui/transient-state/config.el -*- lexical-binding: t; -*-

;; :)

(defvar-keymap
  :doc "Vertico minibuffer keymap derived from `minibuffer-local-map'."
  "<remap> <beginning-of-buffer>" #'vertico-first
  "<remap> <minibuffer-beginning-of-buffer>" #'vertico-first
  "<remap> <end-of-buffer>" #'vertico-last
  "<remap> <scroll-down-command>" #'vertico-scroll-down
  "<remap> <scroll-up-command>" #'vertico-scroll-up
  "<remap> <next-line>" #'vertico-next
  "<remap> <previous-line>" #'vertico-previous
  "<remap> <next-line-or-history-element>" #'vertico-next
  "<remap> <previous-line-or-history-element>" #'vertico-previous
  "<remap> <backward-paragraph>" #'vertico-previous-group
  "<remap> <forward-paragraph>" #'vertico-next-group
  "<remap> <exit-minibuffer>" #'vertico-exit
  "<remap> <kill-ring-save>" #'vertico-save
  "M-RET" #'vertico-exit-input
  "TAB" #'vertico-insert)

(defvar +transient-states-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") #'+emacs-buffers-transient-state/body)
    (define-key map (kbd "w") #'+emacs-windows-transient-state/body)
    (define-key map (kbd "x") #'+emacs-text-zoom-transient-state/body)
    (define-key map (kbd "g") #'+vc-gutter-hydra/body)
    (define-key map (kbd "G") #'+vc-smerge-hydra/body)
    map)
  "Keymap for misc transient-states.")

(defun tsc--self-modifying-add-command (command-symbol sequence)
  (interactive "CSelect a command: \nMkey sequence: ")

  ;; Generate an infix that will call the command and add it to the
  ;; second group (index 1 at the 0th position)
  (transient-insert-suffix
    'tsc-self-modifying
    '(0 1 0) ; set the child in `tsc-inception' for help with this argument
    (list sequence (format "Call %s" command-symbol) command-symbol :transient t))

  ;; we must re-enter the transient to force the layout update
  (transient-setup 'tsc-self-modifying))

(transient-define-prefix tsc-self-modifying ()
  "Prefix that uses `transient-insert-suffix' to add commands to itself."

  [["Add New Commands"
    ("a" "add command" tsc--self-modifying-add-command)]
   ["User Defined"
    ""]]) ; blank line suffix creates an insertion point

;; (tsc-self-modifying)
;;

(transient-define-prefix +fold-transient ()
  "Fold navigation transient"
  :transient-suffix 'transient--do-stay
  [:description
   (lambda ()
     (concat "WEEEEEEEEEE\n"))
   ["Movement"
    :pad-keys t
    ("n" "next" +fold/next)
    ("j" "next" +fold/next)
    ("p" "previous" +fold/previous)
    ("k" "previous" +fold/previous)]
   ["Actions"
  :pad-keys t
    ("TAB" "toggle" +fold/toggle)
    ;; ("<tab>" +fold/toggle)
    ("o" "open" +fold/open)
    ("c" "close" +fold/close)
    ("O" "open all" +fold/open-all)
    ("C" "close all" +fold/close-all)]]
   [:class transient-row
           ("q" "quit" transient-quit-all)
           ("C-g" "quit" transient-quit-all)])

(transient-append-suffix '+fold-transient '(0)
  ["Options for pytest-django"
   ("--rd" "reuse DB" "--reuse-db")])

;; Transients
(transient-define-suffix casual-lib-quit-all ()
  "Casual suffix to call `transient-quit-all'."
  :transient nil
  :if-not #'casual-lib-quit-all-hide-navigation-p
  :key "C-q"
  :description "Dismiss"
  (interactive)
  (transient-quit-all))

(transient-define-suffix casual-lib-quit-one ()
  "Casual suffix to call `transient-quit-one'."
  :transient nil
  :key "C-g"
  :if-not #'casual-lib-hide-navigation-p
  :description (casual-lib--quit-one-suffix-label)
  (interactive)
  (transient-quit-one))

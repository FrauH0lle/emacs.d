;; ui/popup/autoload/settings.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +popup--display-buffer-alist nil
  "Equivalent of `display-buffer-alist'.
Created by `set-popup-rule!' and `set-popup-rules!'.")

;;;###autoload
(defvar +popup--reference-buffers nil
  "Equivalent of `+popup-reference-buffers'.
Created by `set-popup-rule!' and `set-popup-rules!'.")

;;;###autoload
(defvar +popup-defaults
  (list :side   'bottom
        :height 0.16
        :width  40
        :quit   t
        :select #'ignore
        :ttl    5)
  "Default properties for popup rules for `set-popup-rule!'.")

;;;###autoload
(defun +popup-make-rule (predicate plist)
  "Create a popup display rule from PREDICATE and PLIST.
PREDICATE is a condition to match buffers, which can be:
- A symbol (treated as major-mode if name ends with '-mode')
- A cons cell (treated as (MAJOR-MODE . PREDICATE))
- Any valid condition for `buffer-match-p'

PLIST is a property list containing popup configuration options.
See `set-popup-rule!' for details on available properties.

Returns a display-buffer action list suitable for
`display-buffer-alist'."
  (let* ((predicate (if (consp predicate) (car predicate) predicate))
         (predicate (if (and (symbolp predicate)
                             (string-suffix-p "-mode" (symbol-name predicate)))
                        `(major-mode . ,predicate)
                      predicate)))
    (if (plist-get plist :ignore)
        (list predicate nil)
      (let* ((plist (append plist +popup-defaults))
             (alist
              `((actions       . ,(plist-get plist :actions))
                (side          . ,(plist-get plist :side))
                (size          . ,(plist-get plist :size))
                (window-width  . ,(plist-get plist :width))
                (window-height . ,(plist-get plist :height))
                (slot          . ,(plist-get plist :slot))
                (vslot         . ,(plist-get plist :vslot))))
             (params
              `((ttl      . ,(plist-get plist :ttl))
                (quit     . ,(plist-get plist :quit))
                (select   . ,(plist-get plist :select))
                (modeline . ,(plist-get plist :modeline))
                (autosave . ,(plist-get plist :autosave))
                (tabbed   . ,(plist-get plist :tabbed))
                ,@(plist-get plist :parameters))))
        `(,predicate (+popup-buffer)
          ,@alist
          (window-parameters ,@params))))))

;;;###autoload
(defun +popup-make-plist (rule)
  "Convert RULE (a `display-buffer-alist' entry) back to a plist.
This is the reverse of `+popup-make-rule'."
  (let ((predicate (pcase (car rule)
                     (`(major-mode . ,pred) (car pred))
                     (pred pred)))
        (action-fn (cadr rule))
        (rule-tail (cddr rule))
        plist)
    (cond ((null rule) nil)
          ;; Handle ignored rules (:ignore t)
          ((null action-fn)
           (list predicate :ignore t))
          (t
           (cl-loop for (key . val) in rule-tail
                    do (pcase key
                         ('actions (setq plist (plist-put plist :actions val)))
                         ('side (setq plist (plist-put plist :side val)))
                         ('size (setq plist (plist-put plist :size val)))
                         ('window-width (setq plist (plist-put plist :width val)))
                         ('window-height (setq plist (plist-put plist :height val)))
                         ('slot (setq plist (plist-put plist :slot val)))
                         ('vslot (setq plist (plist-put plist :vslot val)))
                         ('window-parameters
                          (cl-loop for (wkey . wval) in val
                                   do (pcase wkey
                                        ('ttl (setq plist (plist-put plist :ttl wval)))
                                        ('quit (setq plist (plist-put plist :quit wval)))
                                        ('select (setq plist (plist-put plist :select wval)))
                                        ('modeline (setq plist (plist-put plist :modeline wval)))
                                        ('autosave (setq plist (plist-put plist :autosave wval)))
                                        ('tabbed (setq plist (plist-put plist :tabbed wval)))
                                        ('((foo t)) '(bla . t))
                                        (x (let ((alist (plist-get plist :parameters)))
                                             (setf (alist-get x alist) wval)
                                             (setq plist (plist-put plist :parameters alist)))))))))
           `(,predicate ,@plist)))))

;;;###autodef
(defun set-popup-rule! (predicate &rest plist)
  "Define a popup rule.

These rules affect buffers displayed with `pop-to-buffer' and
`display-buffer'(or their siblings). Buffers displayed with
`switch-to-buffer' (and its variants) will not be affected by
these rules (as they are unaffected by `display-buffer-alist',
which powers the popup management system).

PREDICATE accepts anything that the CONDITION argument in
`buffer-match-p' takes (Emacs 29 or newer).

Some buffers might require a regexp and major mode rule due to
the way they are created.

PLIST can be made up of any of the following properties:

:ignore BOOL
  If BOOL is non-nil, popups matching PREDICATE will not be
  handled by the popup system. Use this for buffers that have
  their own window management system like magit or helm.

:actions ACTIONS
  ACTIONS is a list of functions or an alist containing (FUNCTION
  . ALIST). See `display-buffer'\\='s second argument for more
  information on its format and what it accepts. If omitted,
  `+popup-default-display-buffer-actions' is used.

:side \\='bottom|\\='top|\\='left|\\='right
  Which side of the frame to open the popup on. This is only
  respected if `+popup-display-buffer-stacked-side-window-fn' or
  `display-buffer-in-side-window' is in :actions or
  `+popup-default-display-buffer-actions'.

:tabbed BOOL
  Can be t or nil. If t, the popup will displayed in a tabbed
  window.

:size/:width/:height FLOAT|INT|FN
  Determines the size of the popup. If more than one of these
  size properties are given :size always takes precedence, and is
  mapped with window-width or window-height depending on what
  :side the popup is opened. Setting a height for a popup that
  opens on the left or right is harmless, but comes into play if
  two popups occupy the same :vslot.

  If a FLOAT (0 < x < 1), the number represents how much of the
    window will be consumed by the popup (a percentage).
  If an INT, the number determines the size in lines (height) or
    units of character width (width).
  If a function, it takes one argument: the popup window, and can
    do whatever it wants with it, typically resize it, like
    `+popup-shrink-to-fit'.

:slot/:vslot INT
  (This only applies to popups with a :side and only if :actions
  is blank or contains the
  `+popup-display-buffer-stacked-side-window-fn' action) These
  control how multiple popups are laid out. INT can be any
  integer, positive and negative.

  :slot controls lateral positioning (e.g. the horizontal
    positioning for top/bottom popups, or vertical positioning
    for left/right popups).
  :vslot controls popup stacking (from the edge of the frame
    toward the center).

  Let's assume popup A and B are opened with :side \\='bottom, in
  that order.

    If they possess the same :slot and :vslot, popup B will
      replace popup A.
    If popup B has a higher :slot, it will open to the right of
      popup A.
    If popup B has a lower :slot, it will open to the left of
      popup A.
    If popup B has a higher :vslot, it will open above popup A.
    If popup B has a lower :vslot, it will open below popup A.

:ttl INT|BOOL|FN
  Stands for time-to-live. It can be t, an integer, nil or a
  function. This controls how (and if) the popup system will
  clean up after the popup.

  If any non-zero integer, wait that many seconds before killing
    the buffer (and any associated processes).
  If 0, the buffer is immediately killed.
  If nil, the buffer won't be killed and is left to its own
    devices.
  If t, resort to the default :ttl in `+popup-defaults'. If none
    exists, this is the same as nil.
  If a function, it takes one argument: the target popup buffer.
    The popup system does nothing else and ignores the function's
    return value.

:quit FN|BOOL|\\='other|\\='current
  Can be t, \\='other, \\='current, nil, or a function. This
  determines the behavior of the ESC/C-g keys in or outside of
  popup windows.

  If t, close the popup if ESC/C-g is pressed anywhere.
  If \\='other, close this popup if ESC/C-g is pressed outside of
    any popup. This is great for popups you may press ESC/C-g a
    lot in.
  If \\='current, close the current popup if ESC/C-g is pressed
    from inside of the popup. This makes it harder to
    accidentally close a popup until you really want to.
  If nil, pressing ESC/C-g will never close this popup.
  If a function, it takes one argument: the to-be-closed popup
    window, and is run when ESC/C-g is pressed while that popup
    is open. It must return one of the other values to determine
    the fate of the popup.

:select BOOL|FN
  Can be a boolean or function. The boolean determines whether to
  focus the popup window after it opens (non-nil) or focus the
  origin window (nil).

  If a function, it takes two arguments: the popup window and
    originating window (where you were before the popup opened).
    The popup system does nothing else and ignores the function's
    return value.

:modeline BOOL|FN|LIST
  Can be t (show the default modeline), nil (show no modeline), a
  function that returns a modeline format or a valid value for
  `mode-line-format' to be used verbatim. The function takes no
  arguments and is run in the context of the popup buffer.

:autosave BOOL|FN
  This parameter determines what to do with modified buffers when
  closing popup windows. It accepts t, \\='ignore, a function or
  nil.

  If t, no prompts. Just save them automatically (if they're
    file-visiting buffers). Same as \\='ignore for
    non-file-visiting buffers.
  If nil (the default), prompt the user what to do if the buffer
    is file-visiting and modified.
  If \\='ignore, no prompts, no saving. Just silently kill it.
  If a function, it is run with one argument: the popup buffer,
    and must return non-nil to save or nil to do nothing (but no
    prompts).

:parameters ALIST
  An alist of custom window parameters. See `(elisp)Window
  Parameters'.

If any of these are omitted, defaults derived from
`+popup-defaults' will be used.

\(fn PREDICATE &key IGNORE ACTIONS SIDE SIZE WIDTH HEIGHT SLOT
VSLOT TTL QUIT SELECT MODELINE AUTOSAVE PARAMETERS)"
  (declare (indent defun))
  (push (+popup-make-rule predicate plist) +popup--display-buffer-alist)
  (if (plist-get plist :ignore)
      (setq +popup--reference-buffers (delete predicate +popup--reference-buffers))
    (push predicate +popup--reference-buffers))
  (when (bound-and-true-p +popup-mode)
    (+popup-cleanup-rules-h)
    (+popup-update-reference-vars))
  +popup--display-buffer-alist)

;;;###autodef
(defun set-popup-rules! (&rest rulesets)
  "Defines multiple popup rules.

Every entry in RULESETS should be a list of alists where the CAR
is the predicate and CDR is a plist. See `set-popup-rule!' for
details on the predicate and plist.

Example:

  (set-popup-rules!
    \\='((\"^ \\*\" :slot 1 :vslot -1 :size #'+popup-shrink-to-fit)
      (\"^\\*\"  :slot 1 :vslot -1 :select t))
    \\='((\"^\\*Completions\" :slot -1 :vslot -2 :ttl 0)
      (\"^\\*Compil\\(?:ation\\|e-Log\\)\" :size 0.3 :ttl 0 :quit t)))"
  (declare (indent 0))
  (dolist (rules rulesets)
    (dolist (rule rules)
      (push (+popup-make-rule (car rule) (cdr rule))
            +popup--display-buffer-alist)
      (if (plist-get (cdr rule) :ignore)
          (setq +popup--reference-buffers (delete (car rule) +popup--reference-buffers))
        (push (car rule) +popup--reference-buffers))))
  (when (bound-and-true-p +popup-mode)
    (+popup-cleanup-rules-h)
    (+popup-update-reference-vars))
  +popup--display-buffer-alist)

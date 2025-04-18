;; lisp/core/zenit-keybinds.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(defvar mac-command-modifier)
(defvar ns-command-modifier)
(defvar mac-option-modifier)
(defvar ns-option-modifier)
(defvar mac-right-option-modifier)
(defvar ns-right-option-modifier)
(defvar w32-lwindow-modifier)
(defvar w32-rwindow-modifier)

;; `cl-extra'
(declare-function cl-subseq "cl-extra" (seq start &optional end))

;; `cl-seq'
(declare-function cl-position "cl-seq" (cl-item cl-seq &rest cl-keys))

;; `which-key'
(declare-function which-key-add-key-based-replacements "ext:which-key" (key-sequence replacement &rest more))
(declare-function which-key-setup-side-window-bottom "ext:which-key" ())
(declare-function which-key-key-order-alpha "ext:which-key" (acons bcons))


;; A centralized keybinds system, integrated with `which-key' to preview
;; available keybindings. All built into one powerful macro: `map!'. If evil is
;; never loaded, then evil bindings set with `map!' are ignored (i.e. omitted
;; entirely for performance reasons).

(defvar zenit-leader-key "SPC"
  "The leader prefix key for Evil users.")

(defvar zenit-leader-alt-key "M-m"
  "An alternative leader prefix key.
Used for Insert and Emacs states, and for non-evil users.")

(defvar zenit-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar zenit-localleader-alt-key "M-m m"
  "The localleader prefix key, for major-mode specific commands.
Used for Insert and Emacs states, and for non-evil users.")

(defvar zenit-leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> keys.")


;;
;;; Global keybind settings

(cond
 (zenit--system-macos-p
  ;; mac-* variables are used by the special emacs-mac build of Emacs by
  ;; Yamamoto Mitsuharu, while other builds use ns-*.
  (setq mac-command-modifier      'super
        ns-command-modifier       'super
        mac-option-modifier       'meta
        ns-option-modifier        'meta
        ;; Free up the right option for character composition
        mac-right-option-modifier 'none
        ns-right-option-modifier  'none))
 (zenit--system-windows-p
  (setq w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)))

;; HACK: Emacs can't distinguish C-i from TAB, or C-m from RET, in either GUI or
;;   TTY frames. This is a byproduct of its history with the terminal, which
;;   can't distinguish them either, however, Emacs has separate input events for
;;   many contentious keys like TAB and RET (like [tab] and [return], aka
;;   "<tab>" and "<return>"), which are only triggered in GUI frames, so here, I
;;   create one for C-i. Won't work in TTY frames, though. The :os tty module
;;   has a workaround for that though.
(defun zenit-init-input-decode-map-h ()
  "Initialize `input-decode-map'.

This workaround is necessary to distinguish between C-i and TAB,
and C-m and RET because Emacs cannot distinguish these keys in
TTY frames, and this function only works in GUI frames.

The function is automatically called on frame creation in daemon
mode, or immediately in non-daemon mode."
  (pcase-dolist (`(,key ,fallback . ,events)
                 '(([C-i] [?\C-i] tab kp-tab)
                   ([C-m] [?\C-m] return kp-return)))
    (define-key
     input-decode-map fallback
     (cmd! (if (when-let* ((keys (this-single-command-raw-keys)))
                 (and (display-graphic-p)
                      (not (cl-loop for event in events
                                    if (cl-position event keys)
                                    return t))
                      ;; Use FALLBACK if nothing is bound to KEY, otherwise
                      ;; we've broken all pre-existing FALLBACK keybinds.
                      (key-binding
                       (vconcat (if (= 0 (length keys)) [] (cl-subseq keys 0 -1))
                                key) nil t)))
               key fallback)))))

;; `input-decode-map' bindings are resolved on first invokation, and are
;; frame-local, so they must be rebound on every new frame.
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'zenit-init-input-decode-map-h)
  (zenit-init-input-decode-map-h))


;;
;;; Universal, non-nuclear escape

(defvar zenit-escape-hook nil
  "A hook run when C-g (or ESC in normal mode) is pressed.

More specifically, when `zenit/escape' is pressed. If any hook
returns non-nil, all hooks after it are ignored.")

(defun zenit/escape (&optional interactive)
  "Run `zenit-escape-hook' and handle escape/quit functionality.

This function serves as a universal escape mechanism that:
1. Quits the minibuffer if active
2. Runs `zenit-escape-hook' until a hook returns non-nil
3. Preserves keyboard macros
4. Falls back to `keyboard-quit'

When called interactively (INTERACTIVE is non-nil), sets
`this-command' appropriately for minibuffer quitting and keyboard
quitting."
  (interactive (list 'interactive))
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (when interactive
           (setq this-command 'abort-recursive-edit))
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'zenit-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((unwind-protect (keyboard-quit)
           (when interactive
             (setq this-command 'keyboard-quit))))))

(global-set-key [remap keyboard-quit] #'zenit/escape)

(with-eval-after-load 'eldoc
  (eldoc-add-command 'zenit/escape))


;;
;;; General + leader/localleader keys

(require 'general)

(cl-eval-when (compile)
  (require 'el-patch))

;; PATCH `general-predicate-dispatch' Each branch binds 'it' to the return value
;;   of the predicate (anaphoric)
(el-patch-feature general)
(el-patch-cl-defmacro general-predicate-dispatch
  (fallback-def &rest defs
                &key docstring
                &allow-other-keys)
  (el-patch-let ((doc "Create a menu item that will run FALLBACK-DEF or a definition from DEFS.
DEFS consists of <predicate> <definition> pairs. Binding this menu-item to a key
will cause that key to act as the corresponding definition (a command, keymap,
etc) for the first matched predicate. If no predicate is matched FALLBACK-DEF
will be run. When FALLBACK-DEF is nil and no predicates are matched, the keymap
with the next highest precedence for the pressed key will be checked. DOCSTRING
can be specified as a description for the menu item.")
                 (dec (declare (indent 1))))
    (el-patch-remove dec doc)
    (el-patch-add doc dec))
  ;; remove keyword arguments from defs and sort defs into pairs
  (let ((defs (cl-loop for (key value) on defs by 'cddr
                       unless (keywordp key)
                       collect (list key value))))
    `'(menu-item
       ,(or docstring "") nil
       :filter (lambda (&optional _)
                 (el-patch-swap
                   (cond ,@(mapcar (lambda (pred-def)
                                     `(,(car pred-def) ,(cadr pred-def)))
                                   defs)
                         (t ,fallback-def))
                   (let (it)
                     (cond ,@(mapcar (lambda (pred-def)
                                       `((setq it ,(car pred-def))
                                         ,(cadr pred-def)))
                                     defs)
                           (t ,fallback-def))))))))

;; Convenience aliases
(defalias 'define-key! #'general-def)
(defalias 'undefine-key! #'general-unbind)
;; Prevent "X starts with non-prefix key Y" errors except at startup.
(add-hook 'zenit-after-modules-init-hook #'general-auto-unbind-keys)

;; HACK: `map!' uses this instead of `define-leader-key!' because it consumes
;;   20-30% more startup time, so we reimplement it ourselves.
(defmacro zenit--define-leader-key (&rest keys)
  "Macro to define keybindings under the leader key.

The macro processes KEY-DEF pairs in KEYS, where each pair consists of:
- A key sequence (string or vector)
- A definition (command or `general'\\='s extended definition syntax)

Special keyword arguments:
:prefix/:infix  - Set a prefix key for subsequent bindings
:which-key      - Add descriptions for which-key (through
                  extended def syntax)
:ignore         - Ingore this keybinding

Examples:
  (zenit--define-leader-key
   :prefix \"SPC\"
   \"a\" #\\='test-command-1)

  (zenit--define-leader-key
   \"a\" #\\='test-command-1
   \"b\" \\='(:def #\\='test-command-2 :which-key \"Test command 2\")
   \"c\" \\='(:def :ignore :which-key \"Test command 3\"))"
  (let (prefix forms wkforms)
    (while keys
      (let ((key (pop keys))
            (def (pop keys)))
        (if (keywordp key)
            (when (memq key '(:prefix :infix))
              (setq prefix def))
          (when prefix
            (setq key `(general--concat t ,prefix ,key)))
          (let* ((udef (cdr-safe (zenit-unquote def)))
                 (bdef (if (general--extended-def-p udef)
                           (general--extract-def (general--normalize-extended-def udef))
                         def)))
            (unless (eq bdef :ignore)
              (push `(define-key zenit-leader-map (general--kbd ,key)
                      ,bdef)
                    forms))
            (when-let* ((desc (cadr (memq :which-key udef))))
              (prependq!
               wkforms `((which-key-add-key-based-replacements
                           (general--concat t zenit-leader-alt-key ,key)
                           ,desc)
                         (which-key-add-key-based-replacements
                           (general--concat t zenit-leader-key ,key)
                           ,desc))))))))
    (macroexp-progn
     (append (and wkforms `((after! which-key ,@(nreverse wkforms))))
             (nreverse forms)))))

(defmacro define-leader-key! (&rest args)
  "Define <leader> keys.

ARGS as in `general-define-key', but does not support :states,
:wk-full-keys or :keymaps. Use `map!' for a more convenient
interface.

See `zenit-leader-key' and `zenit-leader-alt-key' to change the
leader prefix."
  `(general-define-key
    :states nil
    :wk-full-keys nil
    :keymaps 'zenit-leader-map
    ,@args))

(defmacro define-localleader-key! (&rest args)
  "Define <localleader> key.

ARGS as in `general-define-key', but does not support
:major-modes, :states, :prefix or :non-normal-prefix. Use `map!'
for a more convenient interface.

See `zenit-localleader-key' and `zenit-localleader-alt-key' to
change the localleader prefix."
  (eval-if! (zenit-module-p :editor 'evil)
      ;; :non-normal-prefix doesn't apply to non-evil sessions (only evil's
      ;; emacs state)
      `(general-define-key
        :states '(normal visual motion emacs insert)
        :major-modes t
        :prefix zenit-localleader-key
        :non-normal-prefix zenit-localleader-alt-key
        ,@args)
    `(general-define-key
      :major-modes t
      :prefix zenit-localleader-alt-key
      ,@args)))

;; We use a prefix commands instead of general's :prefix/:non-normal-prefix
;; properties because general is incredibly slow binding keys en mass with them
;; in conjunction with :states -- an effective doubling of Emacs's startup time!
(define-prefix-command 'zenit/leader 'zenit-leader-map)
(define-key zenit-leader-map [override-state] 'all)

;; Bind `zenit-leader-key' and `zenit-leader-alt-key' as late as possible to give
;; the user a chance to modify them.
(add-hook! 'zenit-after-init-hook
  (defun zenit-init-leader-keys-h ()
    "Bind `zenit-leader-key' and `zenit-leader-alt-key'."
    (let ((map general-override-mode-map))
      (if (not (zenit-module-p :editor 'evil))
          (progn
            (cond ((equal zenit-leader-alt-key "C-c")
                   (set-keymap-parent zenit-leader-map mode-specific-map))
                  ((equal zenit-leader-alt-key "C-x")
                   (set-keymap-parent zenit-leader-map ctl-x-map)))
            (define-key map (kbd zenit-leader-alt-key) 'zenit/leader))
        (after! evil
          (evil-define-key* '(normal visual motion) map (kbd zenit-leader-key) 'zenit/leader)
          (evil-define-key* '(emacs insert) map (kbd zenit-leader-alt-key) 'zenit/leader)))
      (general-override-mode +1))))


;;
;;; Packages

(use-package! which-key
  :hook (zenit-first-input . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-idle-delay 0)
  :config
  ;; general improvements to which-key readability
  (which-key-setup-side-window-bottom)
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3)

  (which-key-add-key-based-replacements zenit-leader-key "<leader>")
  (which-key-add-key-based-replacements zenit-localleader-key "<localleader>")
  (which-key-add-key-based-replacements zenit-leader-alt-key "<leader>")
  (which-key-add-key-based-replacements zenit-localleader-alt-key "<localleader>"))


;;
;;; `map!' macro

(defvar zenit-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "A list of cons cells that map a letter to a evil state symbol.")

(defun zenit--map-keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.

For example, :nvi will map to (list \\='normal \\='visual \\='insert). See
`zenit-evil-state-alist' to customize this."
  (cl-loop for l across (zenit-keyword-name keyword)
           if (assq l zenit-evil-state-alist) collect (cdr it)
           else do (error "not a valid state: %s" l)))


;; specials
(defvar zenit--map-forms nil
  "List of forms generated during processing of a `map!' block.")
(defvar zenit--map-fn nil
  "Function used to define keys within a `map!' block.
Set by :leader, :localleader, etc.")
(defvar zenit--map-batch-forms nil
  "Batch of key-definition forms grouped by state.
Used in `zenit--map-commit'.")
(defvar zenit--map-state '(:dummy t)
  "Current state of the keymap being defined by `map!'.")
(defvar zenit--map-parent-state nil
  "Parent state inherited by nested `:prefix' blocks.")
(defvar zenit--map-evil-p nil
  "Non-nil if the :editor evil module is active.")
(when (zenit-module-p :editor 'evil) (setq zenit--map-evil-p t))

(defun zenit--map-process (rest)
  "Process the REST of a `map!' block.
REST is the forms to be processed."
  (let ((zenit--map-fn zenit--map-fn)
        zenit--map-state
        zenit--map-forms
        desc)
    (while rest
      (let ((key (pop rest)))
        (cond ((listp key)
               (zenit--map-nested nil key))

              ((keywordp key)
               (pcase key
                 (:leader
                  (zenit--map-commit)
                  (setq zenit--map-fn 'zenit--define-leader-key))
                 (:localleader
                  (zenit--map-commit)
                  (setq zenit--map-fn 'define-localleader-key!))
                 (:after
                  (zenit--map-nested (list 'after! (pop rest)) rest)
                  (setq rest nil))
                 (:desc
                  (setq desc (pop rest)))
                 (:map
                  (zenit--map-set :keymaps `(backquote ,(ensure-list (pop rest)))))
                 (:mode
                  (push (cl-loop for m in (ensure-list (pop rest))
                                 collect (intern (concat (symbol-name m) "-map")))
                        rest)
                  (push :map rest))
                 ;; REVIEW 2023-06-05: Maybe replace with `eval-when!'/`eval-unless!'
                 ((or :when :unless)
                  (zenit--map-nested (list (intern (concat "eval-" (zenit-keyword-name key) "!")) (pop rest)) rest)
                  (setq rest nil))
                 (:prefix-map
                  (cl-destructuring-bind (prefix . desc)
                      (let ((arg (pop rest)))
                        (if (consp arg) arg (list arg)))
                    (let ((keymap (intern (format "zenit-leader-%s-map" desc))))
                      (setq rest
                            (append (list :desc desc prefix keymap
                                          :prefix prefix)
                                    rest))
                      (push `(defvar ,keymap (make-sparse-keymap))
                            zenit--map-forms))))
                 (:prefix
                  (cl-destructuring-bind (prefix . desc)
                      (let ((arg (pop rest)))
                        (if (consp arg) arg (list arg)))
                    (zenit--map-set (if zenit--map-fn :infix :prefix)
                                    prefix)
                    (when (stringp desc)
                      (setq rest (append (list :desc desc "" nil) rest)))))
                 (:textobj
                  (let* ((key (pop rest))
                         (inner (pop rest))
                         (outer (pop rest)))
                    (push `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                 (:map evil-outer-text-objects-map ,key ,outer))
                          zenit--map-forms)))
                 (_
                  (condition-case _
                      (zenit--map-def (pop rest) (pop rest)
                                      (zenit--map-keyword-to-states key)
                                      desc)
                    (error
                     (error "Not a valid `map!' property: %s" key)))
                  (setq desc nil))))

              ((zenit--map-def key (pop rest) nil desc)
               (setq desc nil)))))

    (zenit--map-commit)
    (macroexp-progn (nreverse (delq nil zenit--map-forms)))))

(defun zenit--map-append-keys (prop)
  "Append the value of PROP from parent and current state, if they both exist."
  (let ((a (plist-get zenit--map-parent-state prop))
        (b (plist-get zenit--map-state prop)))
    (if (and a b)
        `(general--concat t ,a ,b)
      (or a b))))

(defun zenit--map-nested (wrapper rest)
  "Process a nested `map!' block.
WRAPPER is the forms to wrap around the block and REST are the
forms to be processed within the block."
  (zenit--map-commit)
  (let ((zenit--map-parent-state (zenit--map-state)))
    (push (if wrapper
              (append wrapper (list (zenit--map-process rest)))
            (zenit--map-process rest))
          zenit--map-forms)))

(defun zenit--map-set (prop &optional value)
  "Update PROP in `zenit--map-state' to VALUE.
Any pending definitions in `zenit--map-batch-forms' are
committed."
  (unless (equal (plist-get zenit--map-state prop) value)
    (zenit--map-commit))
  (setq zenit--map-state (plist-put zenit--map-state prop value)))

(defun zenit--map-def (key def &optional states desc)
  "Add a key-definition to `zenit--map-batch-forms'.

KEY is the key-chord, DEF is the command to be bound to KEY,
STATES are the evil states to bind under, and DESC is a
description for which-key."
  (when (or (memq 'global states)
            (null states))
    (setq states (cons 'nil (delq 'global states))))
  (when desc
    (let (unquoted)
      (cond ((and (listp def)
                  (keywordp (car-safe (setq unquoted (zenit-unquote def)))))
             (setq def (list 'quote (plist-put unquoted :which-key desc))))
            ((setq def (cons 'list
                             (if (and (equal key "")
                                      (null def))
                                 `(:ignore t :which-key ,desc)
                               (plist-put (general--normalize-extended-def def)
                                          :which-key desc))))))))
  (dolist (state states)
    (push (list key def)
          (alist-get state zenit--map-batch-forms)))
  t)

(defun zenit--map-commit ()
  "Commit the batch of key-defs in `zenit--map-batch-forms'.
The key-defs will be comitted to `zenit--map-forms'."
  (when zenit--map-batch-forms
    (cl-loop with attrs = (zenit--map-state)
             for (state . defs) in zenit--map-batch-forms
             if (or zenit--map-evil-p (not state))
             collect `(,(or zenit--map-fn 'general-define-key)
                       ,@(if state `(:states ',state)) ,@attrs
                       ,@(mapcan #'identity (nreverse defs)))
             into forms
             finally do (push (macroexp-progn forms) zenit--map-forms))
    (setq zenit--map-batch-forms nil)))

(defun zenit--map-state ()
  "Merge `zenit--map-parent-state' with `zenit--map-state'.
The latter has priority. Does not modify either; the merged
result is a new plist."
  (let ((plist
         (append (list :prefix (zenit--map-append-keys :prefix)
                       :infix  (zenit--map-append-keys :infix)
                       :keymaps
                       (append (plist-get zenit--map-parent-state :keymaps)
                               (plist-get zenit--map-state :keymaps)))
                 zenit--map-state
                 nil))
        newplist)
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (when (and val (not (plist-member newplist key)))
          (push val newplist)
          (push key newplist))))
    newplist))

;;
(defmacro map! (&rest rest)
  "A convenience macro for defining keybinds, powered by `general'.

The keybinds in REST can have the following properties:

  :leader [...]                   an alias for (:prefix zenit-leader-key ...)
  :localleader [...]              bind to localleader; requires a keymap
  :mode [MODE(s)] [...]           inner keybinds are applied to major MODE(s)
  :map [KEYMAP(s)] [...]          inner keybinds are applied to KEYMAP(S)
  :prefix [PREFIX] [...]          set keybind prefix for following keys. PREFIX
                                  can be a cons cell: (PREFIX . DESCRIPTION)
  :prefix-map [PREFIX] [...]      same as :prefix, but defines a prefix keymap
                                  where the following keys will be bound. DO NOT
                                  USE THIS IN YOUR PRIVATE CONFIG.
  :after [FEATURE] [...]          apply keybinds when [FEATURE] loads
  :textobj KEY INNER-FN OUTER-FN  define a text object keybind pair
  :when [CONDITION] [...]
  :unless [CONDITION] [...]

  Any of the above properties may be nested, so that they only apply to a
  certain group of keybinds.

If evil isn't loaded, evil-specific bindings are ignored.

States
  :n  normal
  :v  visual
  :i  insert
  :e  emacs
  :o  operator
  :m  motion
  :r  replace
  :g  global  (binds the key without evil `current-global-map')

  These can be combined in any order, e.g. :nvi will apply to normal, visual and
  insert mode. The state resets after the following key=>def pair. If states are
  omitted the keybind will be global (no Emacs state; this is different from
  evil's Emacs state and will work in the absence of `evil-mode').

  These must be placed right before the key string.

  Do
    (map! :leader :desc \"Description\" :n \"C-c\" #\\='dosomething)
  Don't
    (map! :n :leader :desc \"Description\" \"C-c\" #\\='dosomething)
    (map! :leader :n :desc \"Description\" \"C-c\" #\\='dosomething)"
  (eval-if! (zenit-module-p :editor 'evil)
      `(general-with-eval-after-load 'evil
         ,(when (or (bound-and-true-p byte-compile-current-file)
                    (not noninteractive))
            (zenit--map-process rest)))
    (when (or (bound-and-true-p byte-compile-current-file)
              (not noninteractive))
      (zenit--map-process rest))))

(provide 'zenit-keybinds)

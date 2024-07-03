;; lisp/core/zenit-keybinds.el -*- lexical-binding: t; -*-


;; A centralized keybinds system, integrated with `which-key' to preview
;; available keybindings. All built into one powerful macro: `map!'. If evil is
;; never loaded, then evil bindings set with `map!' are ignored (i.e. omitted
;; entirely for performance reasons).

(defvar zenit-leader-key "SPC"
  "The leader prefix key for Evil users.")

(defvar zenit-leader-alt-key "M-m"
  "An alternative leader prefix key, used for Insert and Emacs
states, and for non-evil users.")

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

;; HACK: Emacs cannot distinguish between C-i from TAB. This is largely a
;;   byproduct of its history in the terminal, which can't distinguish them
;;   either, however, when GUIs came about Emacs created separate input events
;;   for more contentious keys like TAB and RET. Therefore [return] != RET,
;;   [tab] != TAB, and [backspace] != DEL.
;;
;;   In the same vein, this keybind adds a [C-i] event, so users can bind to it.
;;   Otherwise, it falls back to regular C-i keybinds.
(define-key key-translation-map [?\C-i]
            (cmd!
             (if (let ((keys (this-single-command-raw-keys)))
                   (and keys
                        (not (cl-position 'tab    keys))
                        (not (cl-position 'kp-tab keys))
                        (display-graphic-p)
                        ;; Fall back if no <C-i> keybind can be found, otherwise
                        ;; we've broken all pre-existing C-i keybinds.
                        (let ((key
                               (zenit-lookup-key
                                (vconcat (cl-subseq keys 0 -1) [C-i]))))
                          (not (or (numberp key) (null key))))))
                 [C-i] [?\C-i])))


;;
;;; Universal, non-nuclear escape

(defvar zenit-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil
users).

More specifically, when `zenit/escape' is pressed. If any hook
returns non-nil, all hooks after it are ignored.")

(defun zenit/escape (&optional interactive)
  "Run `zenit-escape-hook'."
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

(eval-when-compile
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

(defhook! +general-predicate-dispatch-fix-load-history-h ()
  "Add `general-predicate-dispatch' explicitly to load-history so
the patch can be validated."
  'zenit-first-input-hook
  (push
   `(,(locate-library "general") (defun . general-predicate-dispatch)) load-history))

;; Convenience aliases
(defalias 'define-key! #'general-def)
(defalias 'undefine-key! #'general-unbind)
;; Prevent "X starts with non-prefix key Y" errors except at startup.
(add-hook 'zenit-after-modules-init-hook #'general-auto-unbind-keys)

;; HACK: `map!' uses this instead of `define-leader-key!' because it consumes
;;   20-30% more startup time, so we reimplement it ourselves.
(defmacro zenit--define-leader-key (&rest keys)
  "Macro to define keybindings under the leader key.

KEYS is a list of key-description-command triples. A keyword
argument :prefix can be used to specify a prefix key sequence for
a group of commands. :infix works like :prefix but doesn't
consume the following argument.

:which-key can be used to provide a description for the
`which-key' package."
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
            (when-let (desc (cadr (memq :which-key udef)))
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

Uses `general-define-key' under the hood, but does not support
:states, :wk-full-keys or :keymaps. Use `map!' for a more
convenient interface.

See `zenit-leader-key' and `zenit-leader-alt-key' to change the
leader prefix."
  `(general-define-key
    :states nil
    :wk-full-keys nil
    :keymaps 'zenit-leader-map
    ,@args))

(defmacro define-localleader-key! (&rest args)
  "Define <localleader> key.

Uses `general-define-key' under the hood, but does not support
:major-modes, :states, :prefix or :non-normal-prefix. Use `map!'
for a more convenient interface.

See `zenit-localleader-key' and `zenit-localleader-alt-key' to
change the localleader prefix."
  (eval-if! (modulep! :editor evil)
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
(defhook! zenit-init-leader-keys-h ()
  "Bind `zenit-leader-key' and `zenit-leader-alt-key'."
  'zenit-after-init-hook
  (let ((map general-override-mode-map))
    (if (not (modulep! :editor evil))
        (progn
          (cond ((equal zenit-leader-alt-key "C-c")
                 (set-keymap-parent zenit-leader-map mode-specific-map))
                ((equal zenit-leader-alt-key "C-x")
                 (set-keymap-parent zenit-leader-map ctl-x-map)))
          (define-key map (kbd zenit-leader-alt-key) 'zenit/leader))
      (after! evil
        (evil-define-key* '(normal visual motion) map (kbd zenit-leader-key) 'zenit/leader)
        (evil-define-key* '(emacs insert) map (kbd zenit-leader-alt-key) 'zenit/leader)))
    (general-override-mode +1)))


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
  "List of forms generated during the processing of a
`map!'block.")
(defvar zenit--map-fn nil
  "Function used to define keys within a `map!' block. Set by
:leader, :localleader, etc.")
(defvar zenit--map-batch-forms nil
  "Batch of key-definition forms grouped by state. Used in
`zenit--map-commit'.")
(defvar zenit--map-state '(:dummy t)
  "Current state of the keymap being defined by `map!'.")
(defvar zenit--map-parent-state nil
  "Parent state inherited by nested `:prefix' blocks within a
`map!' block.")
(defvar zenit--map-evil-p nil
  "Non-nil if the :editor evil module is active.")
(when (modulep! :editor evil) (setq zenit--map-evil-p t))

(defun zenit--map-process (rest)
  "Process the rest of a `map!' block. REST is the forms to be
processed."
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
                  (zenit--map-set :keymaps `(quote ,(ensure-list (pop rest)))))
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
  "Append the value of PROP from parent and current state, if they
both exist."
  (let ((a (plist-get zenit--map-parent-state prop))
        (b (plist-get zenit--map-state prop)))
    (if (and a b)
        `(general--concat t ,a ,b)
      (or a b))))

(defun zenit--map-nested (wrapper rest)
  "Process a nested `map!' block. WRAPPER is the forms to wrap
around the block and REST are the forms to be processed within
the block."
  (zenit--map-commit)
  (let ((zenit--map-parent-state (zenit--map-state)))
    (push (if wrapper
              (append wrapper (list (zenit--map-process rest)))
            (zenit--map-process rest))
          zenit--map-forms)))

(defun zenit--map-set (prop &optional value)
  "Update PROP in `zenit--map-state' to VALUE, and commit any
pending definitions in `zenit--map-batch-forms'."
  (unless (equal (plist-get zenit--map-state prop) value)
    (zenit--map-commit))
  (setq zenit--map-state (plist-put zenit--map-state prop value)))

(defun zenit--map-def (key def &optional states desc)
  "Add a key-definition to `zenit--map-batch-forms'. KEY is the
key-chord, DEF is the command to be bound to KEY, STATES are the
evil states to bind under, and DESC is a description for
which-key."
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
  "Commit the batch of key-defs in `zenit--map-batch-forms' to
`zenit--map-forms'."
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
  "Merge `zenit--map-parent-state' with `zenit--map-state',giving
priority to the latter. Does not modify either; the merged result
is a new plist."
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

If evil isn't loaded, evil-specific bindings are ignored.

Properties
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
  omitted the keybind will be global (no emacs state; this is different from
  evil's Emacs state and will work in the absence of `evil-mode').

  These must be placed right before the key string.

  Do
    (map! :leader :desc \"Description\" :n \"C-c\" #\\='dosomething)
  Don't
    (map! :n :leader :desc \"Description\" \"C-c\" #\\='dosomething)
    (map! :leader :n :desc \"Description\" \"C-c\" #\\='dosomething)"
  (eval-if! (modulep! :editor evil)
      `(general-with-eval-after-load 'evil
         ,(when (or (bound-and-true-p byte-compile-current-file)
                    (not noninteractive))
            (zenit--map-process rest)))
    (when (or (bound-and-true-p byte-compile-current-file)
              (not noninteractive))
      (zenit--map-process rest))))

(provide 'zenit-keybinds)

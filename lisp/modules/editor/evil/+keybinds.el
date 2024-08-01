;; editor/evil/+keybinds.el -*- lexical-binding: t; -*-


;; Keybinds that have no Emacs+evil analogues (i.e. don't exist):
;;   zu{q,w} - undo last marking

(map! :v  "@"     #'+evil:apply-macro
      :m  [C-i]   #'evil-jump-forward

      ;; implement dictionary keybinds
      ;; `evil' already defines 'z=' to `ispell-word' = correct word at point
      (:when (modulep! :checkers spell)
        :n  "zg"   #'+spell/add-word
        :n  "zw"   #'+spell/remove-word
        :m  "[s"   #'+spell/previous-error
        :m  "]s"   #'+spell/next-error)

      ;; ported from vim-unimpaired
      :n  "] SPC" #'+evil/insert-newline-below
      :n  "[ SPC" #'+evil/insert-newline-above

      :n  "]b"    #'next-buffer
      :n  "[b"    #'previous-buffer
      :n  "]f"    #'+evil/next-file
      :n  "[f"    #'+evil/previous-file
      :m  "]u"    #'+evil:url-encode
      :m  "[u"    #'+evil:url-decode
      :m  "]y"    #'+evil:c-string-encode
      :m  "[y"    #'+evil:c-string-decode
      (:when (modulep! :lang web)
        :m "]x"   #'+web:encode-html-entities
        :m "[x"   #'+web:decode-html-entities)
      (:when (modulep! :ui vc-gutter)
        :m "]d"   #'+vc-gutter/next-hunk
        :m "[d"   #'+vc-gutter/previous-hunk)
      (:when (modulep! :ui hl-todo)
        :m "]t"   #'hl-todo-next
        :m "[t"   #'hl-todo-previous)
      (:when (modulep! :ui workspaces)
        :n "gt"   #'+workspace:switch-next
        :n "gT"   #'+workspace:switch-previous
        :n "]w"   #'+workspace/switch-right
        :n "[w"   #'+workspace/switch-left)

      ;; custom vim-unmpaired-esque keys
      :m  "]#"    #'+evil/next-preproc-directive
      :m  "[#"    #'+evil/previous-preproc-directive
      :m  "]a"    #'evil-forward-arg
      :m  "[a"    #'evil-backward-arg
      :m  "]c"    #'+evil/next-comment
      :m  "[c"    #'+evil/previous-comment
      :m  "]e"    #'next-error
      :m  "[e"    #'previous-error
      :n  "]F"    #'+evil/next-frame
      :n  "[F"    #'+evil/previous-frame
      :m  "]h"    #'outline-next-visible-heading
      :m  "[h"    #'outline-previous-visible-heading
      :m  "]m"    #'+evil/next-beginning-of-method
      :m  "[m"    #'+evil/previous-beginning-of-method
      :m  "]M"    #'+evil/next-end-of-method
      :m  "[M"    #'+evil/previous-end-of-method
      :n  "[o"    #'+evil/insert-newline-above
      :n  "]o"    #'+evil/insert-newline-below
      :n  "gp"    #'+evil/reselect-paste
      :v  "gp"    #'+evil/alt-paste
      :nv "g@"    #'+evil:apply-macro
      :nv "gc"    #'evilnc-comment-operator
      :nv "gO"    #'imenu
      :nv "gx"    #'evil-exchange
      :nv "gy"    #'+evil:yank-unindented
      :n  "g="    #'evil-numbers/inc-at-pt
      :n  "g-"    #'evil-numbers/dec-at-pt
      :v  "g="    #'evil-numbers/inc-at-pt-incremental
      :v  "g-"    #'evil-numbers/dec-at-pt-incremental
      :v  "g+"    #'evil-numbers/inc-at-pt
      (:when (modulep! :tools lookup)
        :nv "K"   #'+lookup/documentation
        :nv "gd"  #'+lookup/definition
        :nv "gD"  #'+lookup/references
        :nv "gf"  #'+lookup/file
        :nv "gI"  #'+lookup/implementations)
      (:when (modulep! :tools eval)
        :nv "gr"  #'+eval:region
        :n  "gR"  #'+eval/buffer
        :v  "gR"  #'+eval:replace-region
        ;; Restore these keybinds, since the blacklisted/overwritten gr/gR will
        ;; undo them:
        (:after helpful
         :map helpful-mode-map
         :n "gr" #'helpful-update)
        (:after compile
         :map (compilation-mode-map compilation-minor-mode-map)
         :n "gr" #'recompile)
        (:after dired
         :map dired-mode-map
         :n "gr" #'revert-buffer)
        (:after notmuch
         :map notmuch-common-keymap
         :n "gr" #'notmuch-refresh-this-buffer
         :n "gR" #'notmuch-poll-and-refresh-this-buffer)
        (:after elfeed
         :map elfeed-search-mode-map
         :n "gr" #'elfeed-search-update--force
         :n "gR" #'elfeed-search-fetch)
        (:after eglot
         :map eglot-mode-map
         :nv "gd" #'+lookup/definition
         :nv "gD" #'+lookup/references))

      ;; custom evil keybinds
      :nv "zn"    #'+evil:narrow-buffer
      :n  "zN"    #'zenit/widen-indirectly-narrowed-buffer
      :n  "zx"    #'kill-current-buffer
      :n  "ZX"    #'zenit/save-and-kill-buffer
      ;; don't leave visual mode after shifting
      :v  "<"     #'+evil/shift-left  ; vnoremap < <gv
      :v  ">"     #'+evil/shift-right  ; vnoremap > >gv

      ;; window management (prefix "C-w")
      (:map evil-window-map
            ;; Navigation
            "C-h"     #'evil-window-left
            "C-j"     #'evil-window-down
            "C-k"     #'evil-window-up
            "C-l"     #'evil-window-right
            "C-w"     #'other-window
            ;; Extra split commands
            "S"       #'+evil/window-split-and-follow
            "V"       #'+evil/window-vsplit-and-follow
            ;; Swapping windows
            "H"       #'+evil/window-move-left
            "J"       #'+evil/window-move-down
            "K"       #'+evil/window-move-up
            "L"       #'+evil/window-move-right
            "C-S-w"   #'ace-swap-window
            ;; Window undo/redo
            (:prefix "m"
                     "m"       #'zenit/window-maximize-buffer
                     "v"       #'zenit/window-maximize-vertically
                     "s"       #'zenit/window-maximize-horizontally)
            "u"       #'winner-undo
            "C-u"     #'winner-undo
            "C-r"     #'winner-redo
            "o"       #'zenit/window-enlargen
            ;; Delete window
            "d"       #'evil-window-delete
            "C-C"     #'ace-delete-window
            "T"       #'tear-off-window)

      ;; text objects
      :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
      :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
      :textobj "c" #'evilnc-inner-comment              #'evilnc-outer-commenter
      :textobj "f" #'+evil:defun-txtobj                #'+evil:defun-txtobj
      :textobj "g" #'+evil:whole-buffer-txtobj         #'+evil:whole-buffer-txtobj
      :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
      :textobj "j" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down
      :textobj "k" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
      :textobj "q" #'+evil:inner-any-quote             #'+evil:outer-any-quote
      :textobj "u" #'+evil:inner-url-txtobj            #'+evil:outer-url-txtobj
      :textobj "x" #'evil-inner-xml-attr               #'evil-outer-xml-attr

      ;; evil-easymotion
      (:after evil-easymotion
       :m "gs" `("easymotion" . ,evilem-map)
       (:map evilem-map
             "a" `("evil-forward-arg" . ,(protect-macros! (evilem-create #'evil-forward-arg)))
             "A" `("evil-backward-arg" . ,(protect-macros! (evilem-create #'evil-backward-arg)))
             "s" #'evil-avy-goto-char-2
             "SPC" `("goto-char all windows" . ,(cmd! (let ((current-prefix-arg t)) (evil-avy-goto-char-timer))))
             "/" #'evil-avy-goto-char-timer))

      ;; evil-snipe
      (:after evil-snipe
       :map evil-snipe-parent-transient-map
       "C-;" (cmd! (require 'evil-easymotion)
                   (call-interactively
                    (protect-macros!
                      (evilem-create #'evil-snipe-repeat
                                     :bind ((evil-snipe-scope 'whole-buffer)
                                            (evil-snipe-enable-highlight)
                                            (evil-snipe-enable-incremental-highlight)))))))

      ;; evil-surround
      :v "S" #'evil-surround-region
      :o "s" #'evil-surround-edit
      :o "S" #'evil-Surround-edit

      ;; evil-lion
      :n "gl" #'evil-lion-left
      :n "gL" #'evil-lion-right
      :v "gl" #'evil-lion-left
      :v "gL" #'evil-lion-right

      ;; Emulation of Vim's omni-completion keybinds
      (:prefix "C-x"
               (:when (modulep! :completion corfu)
                 :i "C-l"  #'cape-line
                 :i "C-k"  #'cape-keyword
                 :i "C-f"  #'cape-file
                 :i "C-]"  #'complete-tag
                 :i "s"    #'cape-dict
                 :i "C-s"  #'tempel-complete
                 :i "C-o"  #'completion-at-point
                 :i "C-n"  #'cape-dabbrev
                 :i "C-p"  #'+corfu/dabbrev-this-buffer)))


(after! (which-key evil-easymotion)
  (cl-pushnew '(("\\`g s \\[\\'")
                nil . "backward-section")
              which-key-replacement-alist)
  (cl-pushnew '(("\\`g s \\]\\'")
                nil . "forward-section")
              which-key-replacement-alist)
  (cl-pushnew '(("\\`g s g\\'")
                nil . "line-or-word")
              which-key-replacement-alist))

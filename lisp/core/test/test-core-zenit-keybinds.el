;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-core-zenit-keybinds.el

(describe "core/zenit-keybinds"

  (require 'zenit-start)
  (require 'zenit-use-package)
  (load! "zenit-keybinds" zenit-core-dir)

  (describe "zenit/escape"
    (it "aborts recursive edit when minibuffer is active"
      (spy-on 'abort-recursive-edit)
      (spy-on 'minibuffer-window-active-p :and-return-value t)
      (zenit/escape)
      (expect 'abort-recursive-edit :to-have-been-called))

    (it "runs escape hooks and stops if any returns non-nil"
      (letf! ((defun hook1 () nil)
              (defun hook2 () t)
              (defun hook3 () nil))
        (add-hook 'zenit-escape-hook #'hook1)
        (add-hook 'zenit-escape-hook #'hook2)
        (add-hook 'zenit-escape-hook #'hook3)
        (spy-on 'hook1 :and-call-through)
        (spy-on 'hook2 :and-call-through)
        (spy-on 'hook3 :and-call-through)
        (zenit/escape)
        (expect 'hook1 :not :to-have-been-called)
        (expect 'hook2 :to-have-been-called)
        (expect 'hook3 :to-have-been-called)
        (remove-hook 'zenit-escape-hook #'hook1)
        (remove-hook 'zenit-escape-hook #'hook2)
        (remove-hook 'zenit-escape-hook #'hook3)))

    (it "does not abort when a keyboard macro is being defined or executed"
      (spy-on 'keyboard-quit)
      (let ((defining-kbd-macro t))
        (zenit/escape)
        (expect 'keyboard-quit :not :to-have-been-called))
      (let ((executing-kbd-macro t))
        (zenit/escape)
        (expect 'keyboard-quit :not :to-have-been-called)))

    (it "aborts normally when no other condition is met"
      (spy-on 'keyboard-quit)
      (zenit/escape)
      (expect 'keyboard-quit :to-have-been-called)))


  (describe "zenit--define-leader-key"
    (it "generates correct code for key bindings"
      (expect '(zenit--define-leader-key
                :prefix "p"
                "a" #'command1
                "b" #'command2
                "c" #'command3
                "d" :which-key "description")
              :to-expand-into
              '(progn
                 (define-key zenit-leader-map
                             (general--kbd (general--concat t "p" "a"))
                             (function command1))
                 (define-key zenit-leader-map
                             (general--kbd (general--concat t "p" "b"))
                             (function command2))
                 (define-key zenit-leader-map
                             (general--kbd (general--concat t "p" "c"))
                             (function command3))
                 (define-key zenit-leader-map
                             (general--kbd (general--concat t "p" "d"))
                             :which-key)
                 (define-key zenit-leader-map
                             (general--kbd (general--concat t "p" "description"))
                             nil)))))


  (describe "define-leader-key!"
    (it "generates correct code for key bindings"
      (expect '(define-leader-key!
                "a" #'command1
                "b" #'command2
                "c" #'command3)
              :to-expand-into
              '(general-define-key :states nil :wk-full-keys nil :keymaps 'zenit-leader-map
                "a" (function command1)
                "b" (function command2)
                "c" (function command3)))))


  (describe "define-localleader-key!"
    (it "generates correct code for key bindings if evil module is active"
      (zenit-module-set :editor 'evil :flags nil)
      (load (expand-file-name "zenit-keybinds.el" zenit-core-dir) nil t)
      (expect '(define-localleader-key!
                "a" #'command1
                "b" #'command2
                "c" #'command3)
              :to-expand-into
              '(general-define-key
                :states '(normal visual motion emacs insert)
                :major-modes t :prefix zenit-localleader-key :non-normal-prefix zenit-localleader-alt-key
                "a" (function command1)
                "b" (function command2)
                "c" (function command3))))

    (it "generates correct code for key bindings if evil module is not active"
      (zenit-module-set :editor 'evil nil)
      (load (expand-file-name "zenit-keybinds.el" zenit-core-dir) nil t)
      (expect '(define-localleader-key!
                "a" #'command1
                "b" #'command2
                "c" #'command3)
              :to-expand-into
              '(general-define-key
                :major-modes t :prefix zenit-localleader-alt-key
                "a" (function command1)
                "b" (function command2)
                "c" (function command3)))))


  (describe "zenit-init-leader-keys-h"
    (it "is defined"
      (expect (fboundp 'zenit-init-leader-keys-h) :to-be-truthy))

    (it "is attached to correct hook"
      (expect (member 'zenit-init-leader-keys-h zenit-after-init-hook) :to-be-truthy)))


  (describe "zenit--map-append-keys"
    (it "returns nil when both zenit--map-parent-state and zenit--map-state do not contain the property"
      (let ((zenit--map-parent-state nil)
            (zenit--map-state nil))
        (expect (zenit--map-append-keys :prop) :to-be nil)))

    (it "returns the value from zenit--map-parent-state when zenit--map-state does not contain the property"
      (let ((zenit--map-parent-state '(:prop "parent"))
            (zenit--map-state nil))
        (expect (zenit--map-append-keys :prop) :to-equal "parent")))

    (it "returns the value from zenit--map-state when zenit--map-parent-state does not contain the property"
      (let ((zenit--map-parent-state nil)
            (zenit--map-state '(:prop "child")))
        (expect (zenit--map-append-keys :prop) :to-equal "child")))

    (it "returns a general--concat form with values from both zenit--map-parent-state and zenit--map-state when both contain the property"
      (let ((zenit--map-parent-state '(:prop "parent"))
            (zenit--map-state '(:prop "child")))
        (expect (zenit--map-append-keys :prop) :to-equal '(general--concat t "parent" "child")))))


  (describe "zenit--map-nested"
    (it "processes a nested map without a wrapper"
      (let ((zenit--map-state '(:prop "state"))
            (zenit--map-forms nil))
        (spy-on 'zenit--map-commit :and-call-through)
        (spy-on 'zenit--map-process :and-return-value 'processed)
        (zenit--map-nested nil '(:nested-forms))
        (expect 'zenit--map-commit :to-have-been-called)
        (expect 'zenit--map-process :to-have-been-called-with '(:nested-forms))
        (expect zenit--map-forms :to-equal '(processed))))

    (it "processes a nested map with a wrapper"
      (let ((zenit--map-state '(:prop "state"))
            (zenit--map-forms nil))
        (spy-on 'zenit--map-commit :and-call-through)
        (spy-on 'zenit--map-process :and-return-value 'processed)
        (zenit--map-nested '(wrap) '(:nested-forms))
        (expect 'zenit--map-commit :to-have-been-called)
        (expect 'zenit--map-process :to-have-been-called-with '(:nested-forms))
        (expect zenit--map-forms :to-equal '((wrap processed))))))


  (describe "zenit--map-set"
    (it "sets a new property value in the state"
      (let ((zenit--map-state '(:prop1 "value1"))
            (zenit--map-batch-forms '((:state1 ((:key1 . :def1))))))
        (spy-on 'zenit--map-commit :and-call-through)
        (zenit--map-set :prop2 "value2")
        (expect 'zenit--map-commit :to-have-been-called)
        (expect zenit--map-state :to-equal '(:prop1 "value1" :prop2 "value2"))))

    (it "updates an existing property value in the state"
      (let ((zenit--map-state '(:prop1 "value1"))
            (zenit--map-batch-forms '((:state1 ((:key1 . :def1))))))
        (spy-on 'zenit--map-commit :and-call-through)
        (zenit--map-set :prop1 "updated")
        (expect 'zenit--map-commit :to-have-been-called)
        (expect zenit--map-state :to-equal '(:prop1 "updated"))))

    (it "does not call commit if the value is not changing"
      (let ((zenit--map-state '(:prop1 "value1"))
            (zenit--map-batch-forms '((:state1 ((:key1 . :def1))))))
        (spy-on 'zenit--map-commit :and-call-through)
        (zenit--map-set :prop1 "value1")
        (expect 'zenit--map-commit :not :to-have-been-called)
        (expect zenit--map-state :to-equal '(:prop1 "value1")))))


  (describe "zenit--map-def"
    (it "adds a key-definition to `zenit--map-batch-forms' for global state"
      (let ((zenit--map-batch-forms nil))
        (zenit--map-def "k" 'defun-command '(global) "description")
        (expect zenit--map-batch-forms
                :to-equal '((nil ("k" (list :def defun-command :which-key "description")))))))

    (it "adds a key-definition to `zenit--map-batch-forms' for specified states"
      (let ((zenit--map-batch-forms nil))
        (zenit--map-def "k" 'defun-command '(normal insert) "description")
        (expect zenit--map-batch-forms
                :to-equal '((insert ("k" (list :def defun-command :which-key "description")))
                            (normal ("k" (list :def defun-command :which-key "description")))))))

    (it "includes description in def when key is empty string and def is null"
      (let ((zenit--map-batch-forms nil))
        (zenit--map-def "" nil '(global) "description")
        (expect zenit--map-batch-forms
                :to-equal '((nil ("" (list :ignore t :which-key "description")))))))

    (it "includes description in def when def is a plist"
      (let ((zenit--map-batch-forms nil))
        (zenit--map-def "k" '(:key "value") '(global) "description")
        (expect zenit--map-batch-forms
                :to-equal '((nil ("k" '(:key "value" :which-key "description")))))))

    (it "adds multiple key-definitions for multiple states"
      (let ((zenit--map-batch-forms nil))
        (zenit--map-def "k1" 'defun-command1 '(global normal) "description1")
        (zenit--map-def "k2" 'defun-command2 '(global insert) "description2")
        (expect zenit--map-batch-forms
                :to-equal '((insert ("k2" (list :def defun-command2 :which-key "description2")))
                            (normal ("k1" (list :def defun-command1 :which-key "description1")))
                            (nil ("k2" (list :def defun-command2 :which-key "description2"))
                                 ("k1" (list :def defun-command1 :which-key "description1"))))))))


  (describe "zenit--map-commit"
    (it "does nothing when `zenit--map-batch-forms' is nil"
      (let ((zenit--map-forms '(initial-value))
            (zenit--map-batch-forms nil))
        (zenit--map-commit)
        (expect zenit--map-forms :to-equal '(initial-value))))

    (it "commits key-defs in `zenit--map-batch-forms' to `zenit--map-forms' for non-evil state when zenit--map-evil-p is non-nil"
      (let ((zenit--map-forms nil)
            (zenit--map-batch-forms '((normal (("k" defun-command)))))
            (zenit--map-state '(:key value))
            (zenit--map-evil-p t)
            (zenit--map-fn 'general-define-key))
        (zenit--map-commit)
        (expect zenit--map-forms
                :to-equal `((general-define-key :states 'normal :key value ("k" defun-command))))))

    (it "doesn't commit key-defs in `zenit--map-batch-forms' for non-evil state when zenit--map-evil-p is nil"
      (let ((zenit--map-forms nil)
            (zenit--map-batch-forms '((normal (("k" defun-command)))))
            (zenit--map-state '(:key value))
            (zenit--map-evil-p nil)
            (zenit--map-fn 'general-define-key))
        (zenit--map-commit)
        (expect zenit--map-forms :to-equal '(nil))))

    (it "commits key-defs in `zenit--map-batch-forms' for nil state regardless of zenit--map-evil-p"
      (let ((zenit--map-forms nil)
            (zenit--map-batch-forms '((nil (("k" defun-command)))))
            (zenit--map-state '(:key value))
            (zenit--map-evil-p nil)
            (zenit--map-fn 'general-define-key))
        (zenit--map-commit)
        (expect zenit--map-forms :to-equal `((general-define-key :key value ("k" defun-command))))))

    (it "clears `zenit--map-batch-forms' after committing"
      (let ((zenit--map-forms nil)
            (zenit--map-batch-forms '((nil (("k" defun-command)))))
            (zenit--map-state '(:key value))
            (zenit--map-evil-p nil)
            (zenit--map-fn 'general-define-key))
        (zenit--map-commit)
        (expect zenit--map-batch-forms :to-equal nil))))


  (describe "zenit--map-state"
    (it "merges `zenit--map-parent-state' and `zenit--map-state' giving priority to the latter"
      (let ((zenit--map-parent-state '(:prefix "p" :infix "i" :keymaps ("parent")))
            (zenit--map-state '(:prefix "c" :infix "j" :keymaps ("child"))))
        (expect (zenit--map-state)
                :to-equal '(:keymaps ("parent" "child")
                            :infix (general--concat t "i" "j")
                            :prefix (general--concat t "p" "c")))))

    (it "returns the current state when the parent state is nil"
      (let ((zenit--map-parent-state nil)
            (zenit--map-state '(:prefix "c" :infix "j" :keymaps ("child"))))
        (expect (zenit--map-state) :to-equal '(:keymaps ("child") :infix "j" :prefix "c"))))

    (it "returns the parent state when the current state is nil"
      (let ((zenit--map-parent-state '(:prefix "p" :infix "i" :keymaps ("parent")))
            (zenit--map-state nil))
        (expect (zenit--map-state) :to-equal '(:keymaps ("parent") :infix "i" :prefix "p"))))

    (it "returns an empty list when both states are nil"
      (let ((zenit--map-parent-state nil)
            (zenit--map-state nil))
        (expect (zenit--map-state) :to-equal nil))))


  (describe "zenit--map-process"
    (it "maps key to commad"
      (expect (zenit--map-process '("x" #'command))
              :to-equal '(general-define-key "x" #'command)))

    (it "processes :leader keyword correctly"
      (expect (zenit--map-process '(:leader "x" #'command))
              :to-equal '(zenit--define-leader-key "x" #'command)))

    (it "processes :localleader keyword correctly"
      (expect (zenit--map-process '(:localleader "x" #'command))
              :to-equal '(define-localleader-key! "x" #'command)))

    (it "processes :after keyword correctly"
      (expect (zenit--map-process '(:after foo "x" #'command))
              :to-equal '(after! foo (general-define-key "x" #'command))))

    (it "processes :desc keyword correctly"
      (expect (zenit--map-process '(:desc "foo" "x" #'command))
              :to-equal '(general-define-key "x" (list :def #'command :which-key "foo"))))

    (it "processes :map keyword correctly"
      (expect (zenit--map-process '(:map foo-map "x" #'command))
              :to-equal '(general-define-key :keymaps '(foo-map) "x" #'command)))

    (it "processes :mode keyword correctly"
      (expect (zenit--map-process '(:mode foo-mode "x" #'command))
              :to-equal '(general-define-key :keymaps '(foo-mode-map) "x" #'command)))

    (it "processes :when/:unless keyword correctly"
      (expect (zenit--map-process '(:when t "x" #'command))
              :to-equal '(when t (general-define-key "x" #'command)))
      (expect (zenit--map-process '(:unless t "x" #'command))
              :to-equal '(unless t (general-define-key "x" #'command))))

    (it "processes :prefix-map keyword correctly"
      (expect (zenit--map-process '(:prefix-map ("a" . "foo") "x" #'command))
              :to-equal '(progn
                           (defvar zenit-leader-foo-map (make-sparse-keymap))
                           (general-define-key "a" (list :def zenit-leader-foo-map :which-key "foo"))
                           (general-define-key :prefix "a" "x" #'command))))

    (it "processes :prefix keyword correctly"
      (expect (zenit--map-process '(:prefix ("a" . "foo") "x" #'command))
              :to-equal '(general-define-key :prefix "a" "" (list :ignore t :which-key "foo") "x" #'command)))

    (it "processes :textobj keyword correctly"
      (expect (zenit--map-process '(:textobj "a" #'foo-inner-arg #'foo-outer-arg))
              :to-equal '(map! (:map evil-inner-text-objects-map "a" #'foo-inner-arg)
                               (:map evil-outer-text-objects-map "a" #'foo-outer-arg))))

    (it "processes evil stats keywords correctly"
      (expect (zenit--map-process '(:nvi "a" #'foo))
              :to-equal '(progn
                           (general-define-key :states 'insert "a" #'foo)
                           (general-define-key :states 'visual "a" #'foo)
                           (general-define-key :states 'normal "a" #'foo))))

    (it "throws an error if keyword is unknown"
      (expect (zenit--map-process '(:doesnotexist "a" #'foo))
              :to-throw)))


  (describe "map!"
    (before-all
      (setq noninteractive nil))

    (after-all
      (setq noninteractive t))

    (it "generates correct code for key bindings if evil module is active"
      (zenit-module-set :editor 'evil :depth 1)
      (load (expand-file-name "zenit-keybinds.el" zenit-core-dir) nil t)
      (expect '(map!
                "a" #'command1
                "b" #'command2
                "c" #'command3)
              :to-expand-into
              '(general-with-eval-after-load 'evil
                 (general-define-key
                  "a" (function command1)
                  "b" (function command2)
                  "c" (function command3)))))

    (it "generates correct code for key bindings if evil module is not active"
      (zenit-module-set :editor 'evil nil)
      (load (expand-file-name "zenit-keybinds.el" zenit-core-dir) nil t)
      (expect '(map!
                "a" #'command1
                "b" #'command2
                "c" #'command3)
              :to-expand-into
              '(general-define-key
                "a" (function command1)
                "b" (function command2)
                "c" (function command3))))))

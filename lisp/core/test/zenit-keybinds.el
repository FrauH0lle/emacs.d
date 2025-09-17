;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/zenit-keybinds.el

(require 'zenit-test)
(require 'el-patch)
(require 'zenit-el-patch)
(require 'zenit-use-package)
(require 'zenit-keybinds)
(zenit-require 'zenit-lib 'modules)

(zenit-deftest zenit-leader-key
  (:doc "`zenit-leader-key' is defined")
  (should (boundp 'zenit-leader-key)))

(zenit-deftest zenit-leader-alt-key
  (:doc "`zenit-leader-alt-key' is defined")
  (should (boundp 'zenit-leader-alt-key)))

(zenit-deftest zenit-localleader-key
  (:doc "`zenit-localleader-key' is defined")
  (should (boundp 'zenit-localleader-key)))

(zenit-deftest zenit-localleader-alt-key
  (:doc "`zenit-localleader-alt-key' is defined")
  (should (boundp 'zenit-localleader-alt-key)))

(zenit-deftest zenit-leader-map
  (:doc "`zenit-leader-map' is defined")
  (should (boundp 'zenit-leader-map)))

(zenit-deftest zenit-escape-hook
  (:doc "`zenit-escape-hook' is defined")
  (should (boundp 'zenit-escape-hook)))

(zenit-deftest zenit/escape
  (:doc "`zenit/escape' is defined")
  (should (fboundp 'zenit/escape)))

(zenit-deftest general-auto-unbind-keys
  (:doc "`general-auto-unbind-keys' is a member of `zenit-after-modules-init-hook'")
  (should (member #'general-auto-unbind-keys zenit-after-modules-init-hook)))

(zenit-deftest zenit--define-leader-key ()
  ,test
  (test)
  :doc "`zenit--define-leader-key' correctly binds keys"
  (let ((zenit-leader-map (make-sparse-keymap)))
    (zenit--define-leader-key
     "a" #'test-command-1
     "b" '(:def #'test-command-2 :which-key "Test command")
     "c" '(:def :ignore :which-key "Test command"))
    (should
     (equal
      '(progn
         (after! which-key
           (which-key-add-key-based-replacements
             (general--concat t zenit-leader-key "b")
             "Test command")
           (which-key-add-key-based-replacements
             (general--concat t zenit-leader-alt-key "b")
             "Test command"))
         (define-key zenit-leader-map
                     (general--kbd "a")
                     (function test-command-1))
         (define-key zenit-leader-map
                     (general--kbd "b")
                     (function test-command-2)))
      (macroexpand '(zenit--define-leader-key
                     "a" #'test-command-1
                     "b" '(:def #'test-command-2 :which-key "Test command")))))
    (should (eq (keymap-lookup zenit-leader-map "a") #'test-command-1))
    (should (eq (keymap-lookup zenit-leader-map "b") #'test-command-2)))
  :doc "`zenit--define-leader-key' correctly binds keys with :prefix"
  (let ((zenit-leader-map (make-sparse-keymap)))
    (zenit--define-leader-key
     :prefix "SPC"
     "a" #'test-command-1
     "b" '(:def #'test-command-2 :which-key "Test command"))
    (should (eq (keymap-lookup zenit-leader-map (general--concat t "SPC" "a")) #'test-command-1))
    (should (eq (keymap-lookup zenit-leader-map (general--concat t "SPC" "b")) #'test-command-2))))

(zenit-deftest define-leader-key!
  (:doc "`define-leader-key!' expands correctly")
  (should
   (equal
    '(general-define-key :states nil :wk-full-keys nil :keymaps 'zenit-leader-map "a"
      (function test-command-1))
    (macroexpand '(define-leader-key! "a" #'test-command-1)))))

(zenit-deftest define-localleader-key!
  (:vars ((zenit-modules (make-hash-table :test #'equal))))
  ,test
  (test)
  :doc "`define-localleader-key!' expands correctly with evil"
  (progn
    (zenit-module-set :editor 'evil :flags nil)
    (load (expand-file-name "zenit-keybinds.el" zenit-core-dir) nil t)
    (should
     (equal
      '(general-define-key :states
        '(normal visual motion emacs insert)
        :major-modes t :prefix zenit-localleader-key :non-normal-prefix zenit-localleader-alt-key "a"
        (function test-command-1))
      (macroexpand '(define-localleader-key! "a" #'test-command-1)))))
  :doc "`define-localleader-key!' expands correctly without evil"
  (progn
    (zenit-module-set :editor 'evil nil)
    (load (expand-file-name "zenit-keybinds.el" zenit-core-dir) nil t)
    (should
     (equal
      '(general-define-key
        :major-modes t :prefix zenit-localleader-alt-key "a"
        (function test-command-1))
      (macroexpand '(define-localleader-key! "a" #'test-command-1))))))

(zenit-deftest zenit-init-leader-keys-h
  (:doc "`zenit-init-leader-keys-h' is a member of `zenit-after-init-hook'")
  (progn
    (should (fboundp 'zenit-init-leader-keys-h))
    (should (member #'zenit-init-leader-keys-h zenit-after-init-hook))))

(zenit-deftest zenit-evil-state-alist
  (:doc "`zenit-evil-state-alist' is defined")
  (should (boundp 'zenit-evil-state-alist)))

(zenit-deftest zenit--map-keyword-to-states
  (:doc "`zenit--map-keyword-to-states' converts keywords to evil state symbols")
  (should (equal ',exp (zenit--map-keyword-to-states ,in)))
  (in exp)
  :n (normal)
  :v (visual)
  :i (insert)
  :e (emacs)
  :o (operator)
  :m (motion)
  :r (replace)
  :g (global)
  :nvieomrg (normal visual insert emacs operator motion replace global))

(zenit-deftest zenit--map-forms
  (:doc "`zenit--map-forms' is defined")
  (should (boundp 'zenit--map-forms)))

(zenit-deftest zenit--map-fn
  (:doc "`zenit--map-fn' is defined")
  (should (boundp 'zenit--map-fn)))

(zenit-deftest zenit--map-batch-forms
  (:doc "`zenit--map-batch-forms' is defined")
  (should (boundp 'zenit--map-batch-forms)))

(zenit-deftest zenit--map-state
  (:doc "`zenit--map-state' is defined")
  (should (boundp 'zenit--map-state)))

(zenit-deftest zenit--map-parent-state
  (:doc "`zenit--map-parent-state' is defined")
  (should (boundp 'zenit--map-parent-state)))

(zenit-deftest zenit--map-evil-p
  (:doc "`zenit--map-evil-p' is defined")
  (should (boundp 'zenit--map-evil-p)))

(zenit-deftest zenit--map-process ()
  ,test
  (test)
  :doc "`zenit--map-process' maps key to command"
  (should (equal '(general-define-key "x" #'command)
                 (zenit--map-process '("x" #'command))))

  :doc "`zenit--map-process' processes :leader keyword"
  (should (equal '(zenit--define-leader-key "x" #'command)
                 (zenit--map-process '(:leader "x" #'command))))

  :doc "`zenit--map-process' processes :localleader keyword"
  (should (equal '(define-localleader-key! "x" #'command)
                 (zenit--map-process '(:localleader "x" #'command))))

  :doc "`zenit--map-process' processes :after keyword"
  (should (equal '(after! foo (general-define-key "x" #'command))
                 (zenit--map-process '(:after foo "x" #'command))))

  :doc "`zenit--map-process' processes :desc keyword"
  (should (equal '(general-define-key "x" (list :def #'command :which-key "foo"))
                 (zenit--map-process '(:desc "foo" "x" #'command))))

  :doc "`zenit--map-process' processes :map keyword"
  (should (equal '(general-define-key :keymaps (backquote (foo-map)) "x" #'command)
                 (zenit--map-process '(:map foo-map "x" #'command))))

  :doc "`zenit--map-process' processes :mode keyword"
  (should (equal '(general-define-key :keymaps (backquote (foo-mode-map)) "x" #'command)
                 (zenit--map-process '(:mode foo-mode "x" #'command))))

  :doc "`zenit--map-process' processes :when/:unless keywords"
  (should (equal '(eval-when! t (general-define-key "x" #'command))
                 (zenit--map-process '(:when t "x" #'command))))
  (should (equal '(eval-unless! t (general-define-key "x" #'command))
                 (zenit--map-process '(:unless t "x" #'command))))

  :doc "`zenit--map-process' processes :prefix-map keyword"
  (should (equal '(progn
                    (defvar zenit-leader-foo-map (make-sparse-keymap))
                    (general-define-key "a" (list :def zenit-leader-foo-map :which-key "foo"))
                    (general-define-key :prefix "a" "x" #'command))
                 (zenit--map-process '(:prefix-map ("a" . "foo") "x" #'command))))

  :doc "`zenit--map-process' processes :prefix keyword"
  (should (equal '(general-define-key :prefix "a" "" (list :ignore t :which-key "foo") "x" #'command)
                 (zenit--map-process '(:prefix ("a" . "foo") "x" #'command))))

  :doc "`zenit--map-process' processes :textobj keyword"
  (should (equal '(map! (:map evil-inner-text-objects-map "a" #'foo-inner-arg)
                        (:map evil-outer-text-objects-map "a" #'foo-outer-arg))
                 (zenit--map-process '(:textobj "a" #'foo-inner-arg #'foo-outer-arg))))

  :doc "`zenit--map-process' processes evil state keywords"
  (should (equal '(progn
                    (general-define-key :states 'insert "a" #'foo)
                    (general-define-key :states 'visual "a" #'foo)
                    (general-define-key :states 'normal "a" #'foo))
                 (zenit--map-process '(:nvi "a" #'foo))))

  :doc "`zenit--map-process' throws error for unknown keyword"
  (should-error (zenit--map-process '(:doesnotexist "a" #'foo))))

(zenit-deftest zenit--map-append-keys ()
  ,test
  (test)
  :doc "`zenit--map-append-keys' returns nil when both parent and state don't contain property"
  (let ((zenit--map-parent-state nil)
        (zenit--map-state nil))
    (should (null (zenit--map-append-keys :prop))))

  :doc "`zenit--map-append-keys' returns parent value when state doesn't contain property"
  (let ((zenit--map-parent-state '(:prop "parent"))
        (zenit--map-state nil))
    (should (equal "parent" (zenit--map-append-keys :prop))))

  :doc "`zenit--map-append-keys' returns state value when parent doesn't contain property"
  (let ((zenit--map-parent-state nil)
        (zenit--map-state '(:prop "child")))
    (should (equal "child" (zenit--map-append-keys :prop))))

  :doc "`zenit--map-append-keys' concatenates values when both contain property"
  (let ((zenit--map-parent-state '(:prop "parent"))
        (zenit--map-state '(:prop "child")))
    (should (equal '(general--concat t "parent" "child") (zenit--map-append-keys :prop)))))

(zenit-deftest zenit--map-nested ()
  ,test
  (test)
  :doc "`zenit--map-nested' processes nested map without wrapper"
  (letf! ((zenit--map-state '(:prop "state"))
          (zenit--map-forms nil)
          (defun zenit--map-process (rest) 'processed))
    (zenit--map-nested nil '(:nested-forms))
    (should (equal '(processed) zenit--map-forms)))

  :doc "`zenit--map-nested' processes nested map with wrapper"
  (letf! ((zenit--map-state '(:prop "state"))
          (zenit--map-forms nil)
          (defun zenit--map-process (rest) 'processed))
    (zenit--map-nested '(wrap) '(:nested-forms))
    (should (equal '((wrap processed)) zenit--map-forms))))

(zenit-deftest zenit--map-set ()
  ,test
  (test)
  :doc "`zenit--map-set' sets new property value in state"
  (let ((zenit--map-state '(:prop1 "value1"))
        (zenit--map-batch-forms '((:state1 ((:key1 . :def1))))))
    (zenit--map-set :prop2 "value2")
    (should (equal '(:prop1 "value1" :prop2 "value2") zenit--map-state)))

  :doc "`zenit--map-set' updates existing property value in state"
  (let ((zenit--map-state '(:prop1 "value1"))
        (zenit--map-batch-forms '((:state1 ((:key1 . :def1))))))
    (zenit--map-set :prop1 "updated")
    (should (equal '(:prop1 "updated") zenit--map-state)))

  :doc "`zenit--map-set' doesn't call commit if value isn't changing"
  (let ((zenit--map-state '(:prop1 "value1"))
        (zenit--map-batch-forms '((:state1 ((:key1 . :def1))))))
    (zenit--map-set :prop1 "value1")
    (should (equal '(:prop1 "value1") zenit--map-state))))

(zenit-deftest zenit--map-def ()
  ,test
  (test)
  :doc "`zenit--map-def' adds key-definition for global state"
  (let ((zenit--map-batch-forms nil))
    (zenit--map-def "k" 'defun-command '(global) "description")
    (should (equal '((nil ("k" (list :def defun-command :which-key "description"))))
                   zenit--map-batch-forms)))

  :doc "`zenit--map-def' adds key-definition for specified states"
  (let ((zenit--map-batch-forms nil))
    (zenit--map-def "k" 'defun-command '(normal insert) "description")
    (should (equal '((insert ("k" (list :def defun-command :which-key "description")))
                     (normal ("k" (list :def defun-command :which-key "description"))))
                   zenit--map-batch-forms)))

  :doc "`zenit--map-def' includes description for empty key with null def"
  (let ((zenit--map-batch-forms nil))
    (zenit--map-def "" nil '(global) "description")
    (should (equal '((nil ("" (list :ignore t :which-key "description"))))
                   zenit--map-batch-forms)))

  :doc "`zenit--map-def' includes description when def is a plist"
  (let ((zenit--map-batch-forms nil))
    (zenit--map-def "k" '(:key "value") '(global) "description")
    (should (equal '((nil ("k" '(:key "value" :which-key "description"))))
                   zenit--map-batch-forms)))

  :doc "`zenit--map-def' adds multiple key-definitions for multiple states"
  (let ((zenit--map-batch-forms nil))
    (zenit--map-def "k1" 'defun-command1 '(global normal) "description1")
    (zenit--map-def "k2" 'defun-command2 '(global insert) "description2")
    (should (equal '((insert ("k2" (list :def defun-command2 :which-key "description2")))
                     (normal ("k1" (list :def defun-command1 :which-key "description1")))
                     (nil ("k2" (list :def defun-command2 :which-key "description2"))
                          ("k1" (list :def defun-command1 :which-key "description1"))))
                   zenit--map-batch-forms))))

(zenit-deftest zenit--map-commit ()
  ,test
  (test)
  :doc "`zenit--map-commit' does nothing when batch forms is nil"
  (let ((zenit--map-forms '(initial-value))
        (zenit--map-batch-forms nil))
    (zenit--map-commit)
    (should (equal '(initial-value) zenit--map-forms)))

  :doc "`zenit--map-commit' commits key-defs for non-evil state with evil enabled"
  (let ((zenit--map-forms nil)
        (zenit--map-batch-forms '((normal (("k" defun-command)))))
        (zenit--map-state '(:key value))
        (zenit--map-evil-p t)
        (zenit--map-fn 'general-define-key))
    (zenit--map-commit)
    (should (equal `((general-define-key :states 'normal :key value ("k" defun-command)))
                   zenit--map-forms)))

  :doc "`zenit--map-commit' doesn't commit key-defs for non-evil state without evil"
  (let ((zenit--map-forms nil)
        (zenit--map-batch-forms '((normal (("k" defun-command)))))
        (zenit--map-state '(:key value))
        (zenit--map-evil-p nil)
        (zenit--map-fn 'general-define-key))
    (zenit--map-commit)
    (should (equal '(nil) zenit--map-forms)))

  :doc "`zenit--map-commit' commits key-defs for nil state regardless of evil"
  (let ((zenit--map-forms nil)
        (zenit--map-batch-forms '((nil (("k" defun-command)))))
        (zenit--map-state '(:key value))
        (zenit--map-evil-p nil)
        (zenit--map-fn 'general-define-key))
    (zenit--map-commit)
    (should (equal `((general-define-key :key value ("k" defun-command)))
                   zenit--map-forms)))

  :doc "`zenit--map-commit' clears batch forms after committing"
  (let ((zenit--map-forms nil)
        (zenit--map-batch-forms '((nil (("k" defun-command)))))
        (zenit--map-state '(:key value))
        (zenit--map-evil-p nil)
        (zenit--map-fn 'general-define-key))
    (zenit--map-commit)
    (should (null zenit--map-batch-forms))))

(zenit-deftest zenit--map-state ()
  ,test
  (test)
  :doc "`zenit--map-state' merges parent and state with priority to state"
  (let ((zenit--map-parent-state '(:prefix "p" :infix "i" :keymaps ("parent")))
        (zenit--map-state '(:prefix "c" :infix "j" :keymaps ("child"))))
    (should (equal '(:keymaps ("parent" "child")
                     :infix (general--concat t "i" "j")
                     :prefix (general--concat t "p" "c"))
                   (zenit--map-state))))

  :doc "`zenit--map-state' returns current state when parent is nil"
  (let ((zenit--map-parent-state nil)
        (zenit--map-state '(:prefix "c" :infix "j" :keymaps ("child"))))
    (should (equal '(:keymaps ("child") :infix "j" :prefix "c")
                   (zenit--map-state))))

  :doc "`zenit--map-state' returns parent state when current is nil"
  (let ((zenit--map-parent-state '(:prefix "p" :infix "i" :keymaps ("parent")))
        (zenit--map-state nil))
    (should (equal '(:keymaps ("parent") :infix "i" :prefix "p")
                   (zenit--map-state))))

  :doc "`zenit--map-state' returns nil when both states are nil"
  (let ((zenit--map-parent-state nil)
        (zenit--map-state nil))
    (should (null (zenit--map-state)))))

(zenit-deftest map!
  (:vars ((zenit-modules (make-hash-table :test #'equal))))
  ,test
  (test)
  :doc "`map!' expands correctly with evil module active"
  (progn
    (zenit-module-set :editor 'evil :depth 1)
    (load (expand-file-name "zenit-keybinds.el" zenit-core-dir) nil t)
    (let ((noninteractive nil))
      (should (equal '(general-with-eval-after-load 'evil
                        (general-define-key
                         "a" #'command1
                         "b" #'command2
                         "c" #'command3))
                     (macroexpand-1 '(map!
                                      "a" #'command1
                                      "b" #'command2
                                      "c" #'command3))))))

  :doc "`map!' expands correctly without evil module"
  (progn
    (zenit-module-set :editor 'evil nil)
    (load (expand-file-name "zenit-keybinds.el" zenit-core-dir) nil t)
    (let ((noninteractive nil))
      (should (equal '(general-define-key
                       "a" #'command1
                       "b" #'command2
                       "c" #'command3)
                     (macroexpand-1 '(map!
                                      "a" #'command1
                                      "b" #'command2
                                      "c" #'command3)))))))

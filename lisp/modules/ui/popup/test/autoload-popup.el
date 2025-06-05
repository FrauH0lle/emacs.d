;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; ui/popup/test/autoload-popup.el

(require 'zenit-test)
(require 'zenit-modules)
(zenit-require 'zenit-lib 'plist)
(zenit-load (zenit-module-locate-path :ui 'popup "autoload/popup.el"))

(zenit-deftest +popup--reference-modes
  (:doc "`+popup--reference-modes' is defined")
  (should (boundp '+popup--reference-modes)))

(zenit-deftest +popup--reference-names
  (:doc "`+popup--reference-names' is defined")
  (should (boundp '+popup--reference-names)))

(zenit-deftest +popup--reference-predicates
  (:doc "`+popup--reference-predicates' is defined")
  (should (boundp '+popup--reference-predicates)))

(zenit-deftest +popup--suppressed-names
  (:doc "`+popup--suppressed-names' is defined")
  (should (boundp '+popup--suppressed-names)))

(zenit-deftest +popup--suppressed-modes
  (:doc "`+popup--suppressed-modes' is defined")
  (should (boundp '+popup--suppressed-modes)))

(zenit-deftest +popup--suppressed-predicates
  (:doc "`+popup--suppressed-predicates' is defined")
  (should (boundp '+popup--suppressed-predicates)))

(zenit-deftest +popup--find-popup-buffers
  (:vars
   (+popup--suppressed-names
    (a (get-buffer-create "a"))
    (b (get-buffer-create "b"))
    (c (get-buffer-create "c"))
    (d (get-buffer-create "d")))
   :before-each
   (progn
     (defvar-local +popup-buffer-status nil)
     (push "^[d]$" +popup--suppressed-names)
     (with-current-buffer a
       (setq +popup-buffer-status '(:status popup)))
     (with-current-buffer b
       (setq +popup-buffer-status '(:status raised)))
     (with-current-buffer c
       (setq +popup-buffer-status '(:status user-popup)))
     (with-current-buffer d
       (setq +popup-buffer-status '(:status user-popup))))
   :after-each
   (progn
     (setq +popup--suppressed-names nil)
     (makunbound '+popup-buffer-status)
     (mapc #'kill-buffer (list a b c d))))
  ,test
  (test)
  :doc "`+popup--find-popup-buffers' returns popup buffers"
  (should (zenit-test-same-items-p
           `((nil . ,a) (nil . ,c) (nil . ,d))
           (+popup--find-popup-buffers (list a b c d))
           :test #'equal))
  :doc "`+popup--find-popup-buffers' marks buffers which should be suppressed"
  (should (eq 'suppressed
              (progn
                (+popup--find-popup-buffers (list a b c d))
                (+popup-buffer-parameter 'status d)))))

(zenit-deftest +popup--find-buried-popup-buffers
  (:vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b"))
    (c (get-buffer-create "c")))
   :before-each
   (progn
     (defvar-local +popup-buffer-status nil)
     (defvar +popup-group-function nil)
     (with-current-buffer a
       (setq +popup-buffer-status '(:status popup)))
     (with-current-buffer b
       (setq +popup-buffer-status '(:status raised)))
     (with-current-buffer c
       (setq +popup-buffer-status '(:status user-popup))))
   :after-each
   (progn
     (makunbound '+popup-buffer-status)
     (makunbound '+popup-group-function)
     (mapc #'kill-buffer (list a b c))))
  ,test
  (test)
  :doc "`+popup--find-buried-popup-buffers' returns buried popup buffers"
  (should (zenit-test-same-items-p
           `((nil (nil . ,c) (nil . ,a)))
           (+popup--find-buried-popup-buffers)
           :test #'equal))
  :doc "`+popup--find-buried-popup-buffers' returns buried popup buffers by group"
  (let ((+popup-group-function (lambda ()
                                 (when (equal (buffer-name (current-buffer)) "c")
                                   "test-group"))))
    (should (zenit-test-same-items-p
             `((nil (nil . ,a))
               ("test-group" (nil . ,c)))
             (+popup--find-buried-popup-buffers)
             :test #'equal))))

(zenit-deftest +popup--find-open-popup-buffers
  (:doc "`+popup--find-open-popup-buffers' returns popup buffers displayed in a window"
   :vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b"))
    (c (get-buffer-create "c")))
   :before-each
   (progn
     (defvar-local +popup-buffer-status nil)
     (with-current-buffer a
       (setq +popup-buffer-status '(:status popup)))
     (with-current-buffer b
       (setq +popup-buffer-status '(:status raised)))
     (with-current-buffer c
       (setq +popup-buffer-status '(:status user-popup))))
   :after-each
   (progn
     (makunbound '+popup-buffer-status)
     (mapc #'kill-buffer (list a b c))))
  ,test
  (test)
  (progn
    (set-window-buffer (selected-window) a)
    (should (zenit-test-same-items-p
             `((,(selected-window) . ,a))
             (+popup--find-open-popup-buffers)
             :test #'equal))))

(zenit-deftest +popup--update-buried-popup-list
  (:vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b"))
    (c (get-buffer-create "c")))
   :before-each
   (defvar +popup-buried-buffers-alist nil)
   :after-each
   (progn
     (makunbound '+popup-buried-buffers-alist)
     (mapc #'kill-buffer (list a b c))))
  ,test
  (test)
  :doc "`+popup--update-buried-popup-list' adds to `+popup-buried-buffers-alist'"
  (progn
    (+popup--update-buried-popup-list nil a 'add)
    (+popup--update-buried-popup-list "test-group" b 'add)
    (+popup--update-buried-popup-list nil c 'add)
    (should (zenit-test-same-items-p
             `(("test-group" (nil . ,b))
               (nil (nil . ,c) (nil . ,a)))
             +popup-buried-buffers-alist
             :test #'equal)))
  :doc "`+popup--update-buried-popup-list' removes from `+popup-buried-buffers-alist'"
  (progn
    (+popup--update-buried-popup-list nil a 'add)
    (+popup--update-buried-popup-list "test-group" b 'add)
    (+popup--update-buried-popup-list nil c 'add)
    (+popup--update-buried-popup-list nil a 'remove)
    (+popup--update-buried-popup-list nil c 'remove)
    (should (zenit-test-same-items-p
             `(("test-group" (nil . ,b)) (nil))
             +popup-buried-buffers-alist
             :test #'equal))))

(zenit-deftest +popup--record-parent
  (:vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b"))
    (c (get-buffer-create "c")))
   :before-each
   (progn
     (defvar-local +popup-buffer-status nil)
     (defvar-local +popup--parents nil)
     (defvar +popup--ignore-parent nil)
     (dolist (buf (list a b c))
       (with-current-buffer buf
         (setq +popup-buffer-status '(:status popup))))
     (with-current-buffer b
       (push `(,a . nil) +popup--parents))
     (push "^[abc]$" +popup--reference-names))
   :after-each
   (progn
     (makunbound '+popup-buffer-status)
     (makunbound '+popup--parents)
     (makunbound '+popup--ignore-parent)
     (setq +popup--reference-names nil)
     (mapc #'kill-buffer (list a b c))))
  (let ((+popup--ignore-parent ,ignore))
    (+popup--record-parent ,parent ,buffer)
    (should (zenit-test-same-items-p
             ,expected
             (buffer-local-value '+popup--parents ,buffer)
             :test #'equal)))
  (parent buffer ignore expected)
  :doc "`+popup--record-parent' records the parent buffer"
  a b nil `((,a . nil))
  :doc "`+popup--record-parent' does not record when `+popup--ignore-parent' is nil"
  a c t nil
  :doc "`+popup--record-parent' does not record when buffer and parent are equal"
  a a nil nil
  :doc "`+popup--record-parent' does not record duplicates"
  b c nil `((,b . nil) (,a . nil))
  :doc "`+popup--record-parent' does not record when buffer is not a popup buffer"
  a (get-buffer "*Messages*") nil nil
  :doc "`+popup--record-parent' adds to previous records"
  b c nil `((,a . nil) (,b . nil)))

(zenit-deftest +popup--kill-buffer
  (:vars
   ((a (get-buffer-create "a")))
   :before-each
   (progn
     (defvar +popup-defaults nil)
     (defvar-local +popup--timer nil))
   :after-each
   (progn
     (makunbound '+popup-defaults)
     (makunbound '+popup--timer)
     (mapc #'kill-buffer (list a))))
  ,test
  (test)
  :doc "`+popup--kill-buffer' kills buffer if it is not visible"
  (progn
    (+popup--kill-buffer a 1)
    (should-not (buffer-live-p a)))
  :doc "`+popup--kill-buffer' kills buffer as soon as it is not visible via a timer"
  (progn
    (set-window-buffer (selected-window) a)
    (with-current-buffer a
      (should-not (timerp +popup--timer)))
    (+popup--kill-buffer a 1)
    (with-current-buffer a
      (should (timerp +popup--timer)))
    (should (buffer-live-p a))))

(zenit-deftest +popup-buffer-parameter
  (:vars
   ((a (get-buffer-create "a")))
   :before-each
   (progn
     (defvar-local +popup-buffer-status nil)
     (with-current-buffer a
       (setq +popup-buffer-status '(:status popup :test-param "value"))))
   :after-each
   (progn
     (kill-buffer a)
     (makunbound '+popup-buffer-status)))
  ,test
  (test)
  :doc "`+popup-buffer-parameter' retrieves existing parameter"
  (should (equal "value" (+popup-buffer-parameter 'test-param a)))
  :doc "`+popup-buffer-parameter' returns nil for non-existent parameter"
  (should-not (+popup-buffer-parameter 'non-existent a))
  :doc "`+popup-buffer-parameter' works with current buffer when no buffer specified"
  (with-current-buffer a
    (should (equal "value" (+popup-buffer-parameter 'test-param)))))

(zenit-deftest +popup-buffer-set-parameter
  (:vars
   ((a (get-buffer-create "a")))
   :before-each
   (defvar-local +popup-buffer-status nil)
   :after-each
   (progn
     (kill-buffer a)
     (makunbound '+popup-buffer-status)))
  ,test
  (test)
  :doc "`+popup-buffer-set-parameter' sets single parameter"
  (progn
    (+popup-buffer-set-parameter a :test-param "value")
    (should (equal "value" (+popup-buffer-parameter 'test-param a))))
  :doc "`+popup-buffer-set-parameter' sets multiple parameters"
  (progn
    (+popup-buffer-set-parameter a :param1 "val1" :param2 "val2")
    (should (equal "val1" (+popup-buffer-parameter 'param1 a)))
    (should (equal "val2" (+popup-buffer-parameter 'param2 a))))
  :doc "`+popup-buffer-set-parameter' overwrites existing parameters"
  (progn
    (+popup-buffer-set-parameter a :param "old")
    (+popup-buffer-set-parameter a :param "new")
    (should (equal "new" (+popup-buffer-parameter 'param a)))))

(zenit-deftest +popup-buffer-p
  (:vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b"))
    (c (get-buffer-create "c")))
   :before-each
   (progn
     (push "^a$" +popup--reference-names)
     (push 'prog-mode +popup--reference-modes)
     (push (lambda (buf) (equal (buffer-name buf) "c"))
           +popup--reference-predicates)
     (defvar-local +popup-buffer-status nil)
     (with-current-buffer b
       (prog-mode)))
   :after-each
   (progn
     (setq +popup--reference-names nil)
     (setq +popup--reference-modes nil)
     (setq +popup--reference-predicates nil)
     (mapc #'kill-buffer (list a b c))))
  ,test
  (test)
  :doc "`+popup-buffer-p' returns t for popup buffers by name"
  (should (+popup-buffer-p a))
  :doc "`+popup-buffer-p' returns t for popup buffers by mode"
  (should (+popup-buffer-p b))
  :doc "`+popup-buffer-p' returns t for popup buffers by predicate"
  (should (+popup-buffer-p c))
  :doc "`+popup-buffer-p' returns nil for non-popup buffers"
  (should-not (+popup-buffer-p (get-buffer "*Messages*"))))

(zenit-deftest +popup-buffer-suppress-p
  (:vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b"))
    (c (get-buffer-create "c")))
   :before-each
   (progn
     (push "^a$" +popup--suppressed-names)
     (push 'prog-mode +popup--suppressed-modes)
     (push (lambda (buf) (equal (buffer-name buf) "c"))
           +popup--suppressed-predicates)
     (defvar-local +popup-buffer-status nil)
     (with-current-buffer b
       (prog-mode)))
   :after-each
   (progn
     (setq +popup--suppressed-names nil)
     (setq +popup--suppressed-modes nil)
     (setq +popup--suppressed-predicates nil)
     (mapc #'kill-buffer (list a b c))))
  ,test
  (test)
  :doc "`+popup-buffer-suppress-p' returns t for suppressed buffers by name"
  (should (+popup-buffer-suppress-p a))
  :doc "`+popup-buffer-suppress-p' returns t for suppressed buffers by mode"
  (should (+popup-buffer-suppress-p b))
  :doc "`+popup-buffer-suppress-p' returns t for suppressed buffers by predicate"
  (should (+popup-buffer-suppress-p c))
  :doc "`+popup-buffer-suppress-p' returns nil for non-suppressed buffers"
  (should-not (+popup-buffer-suppress-p (get-buffer "*Messages*"))))

(zenit-deftest +popup--maybe-select-window
  (:vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b")))
   :before-each
   (progn
     (defvar +popup--inhibit-select nil)
     (defvar-local +popup-buffer-status nil)
     (with-current-buffer a
       (setq +popup-buffer-status '(:status popup :select t)))
     (with-current-buffer b
       (setq +popup-buffer-status '(:status raised :select nil))))
   :after-each
   (progn
     (makunbound '+popup-buffer-status)
     (makunbound '+popup--inhibit-select)
     (mapc #'kill-buffer (list a b))
     (delete-other-windows)))
  ,test
  (test)
  :doc "`+popup--maybe-select-window' selects window based `select' parameter for popup buffers"
  (let* ((win1 (split-window))
         (win2 (split-window)))
    (set-window-buffer win1 a)
    (set-window-buffer win2 b)
    (+popup--maybe-select-window win1 (selected-window))
    (should (eq (selected-window) win1))
    (+popup--maybe-select-window win2 (selected-window))
    (should (eq (selected-window) win1)))
  :doc "`+popup--maybe-select-window' respects `+popup--inhibit-select'"
  (let* ((win1 (split-window))
         (win2 (split-window))
         (+popup--inhibit-select t))
    (set-window-buffer win1 a)
    (set-window-buffer win2 b)
    (+popup--maybe-select-window win1 (selected-window))
    (should-not (eq (selected-window) win1))))

(zenit-deftest +popup--delete-popup
  (:doc "`+popup--delete-popup' is defined")
  (should (fboundp '+popup--delete-popup)))

(zenit-deftest +popup--delete-window
  (:doc "`+popup--delete-window' is defined")
  (should (fboundp '+popup--delete-window)))

(zenit-deftest +popup--delete-other-windows
  (:doc "`+popup--delete-other-windows' is defined")
  (should (fboundp '+popup--delete-other-windows)))

(zenit-deftest +popup--split-window
  (:doc "`+popup--split-window' is defined")
  (should (fboundp '+popup--split-window)))

(zenit-deftest +popup-window-parameter
  (:doc "`+popup-window-parameter' fetches specified parameter"
   :after-each
   (delete-other-windows))
  (let* ((win1 (selected-window))
         (win2 (split-window-right))
         (win3 (split-window-right)))
    (set-window-parameter win2 'test-param t)
    (set-window-parameter win3 'test-param (lambda (&rest _) (concat "hello")))
    (should-not (+popup-window-parameter 'test-param win1))
    (should (+popup-window-parameter 'test-param win2))
    (should (equal "hello" (+popup-window-parameter 'test-param win3)))))

(zenit-deftest +popup-window-p
  (:doc "`+popup-window-p' returns non-nil if window is a popup"
   :before-each
   (progn
     (defvar +popup-mode t)
     (set-window-parameter (selected-window) 'popup t))
   :after-each
   (progn
     (makunbound '+popup-mode)
     (set-window-parameter (selected-window) 'popup nil)
     (delete-other-windows)))
  (should (+popup-window-p (selected-window))))

(zenit-deftest +popup-windows
  (:doc "`+popup-windows' returns a list of popup windows"
   :vars*
   ((win1 (selected-window))
    (win2 (split-window-right))
    (win3 (split-window-right)))
   :before-each
   (progn
     (defvar +popup-mode t)
     (set-window-parameter win2 'popup t)
     (set-window-parameter win3 'popup t))
   :after-each
   (progn
     (makunbound '+popup-mode)
     (delete-other-windows)))
  (should (zenit-test-same-items-p (list win2 win3) (+popup-windows))))

(zenit-deftest +popup-shrink-to-fit
  (:doc "`+popup-shrink-to-fit' is defined")
  (should (fboundp '+popup-shrink-to-fit)))

(zenit-deftest +popup-alist-from-window-state
  (:doc "`+popup-alist-from-window-state' convert window state to alist"
   :vars*
   ((win1 (selected-window))
    (win2 (split-window-right)))
   :before-each
   (progn
     (defvar +popup-mode t)
     (push (cons 'test-param 'writable) window-persistent-parameters)
     (set-window-parameter win2 'test-param 900))
   :after-each
   (progn
     (makunbound '+popup-mode)
     (setq foo '((a . 1) (b . 2)))
     (setf (alist-get 'test-param window-persistent-parameters nil 'remove) nil)
     (delete-other-windows)))
  (let ((params (+popup-alist-from-window-state (window-state-get win2))))
    (should (alist-get 'test-param (alist-get 'window-parameters params)))))

(zenit-deftest +popup-tab--close-tab-current-window
  (:doc "`+popup-tab--close-tab-current-window' is defined")
  (should (fboundp '+popup-tab--close-tab-current-window)))

(zenit-deftest +popup-tab--close-tabs-current-window
  (:doc "`+popup-tab--close-tabs-current-window' is defined")
  (should (fboundp '+popup-tab--close-tabs-current-window)))

(zenit-deftest +popup-tab-get-tabs-fn
  (:doc "`+popup-tab-get-tabs-fn' returns buffers of the same window side and popup group"
   :vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b"))
    (c (get-buffer-create "c"))
    (d (get-buffer-create "d")))
   :before-each
   (progn
     (defvar +popup-buried-buffers-alist
       `(("test-group" (nil . ,c))
         (nil (nil . ,a) (nil . ,b) (nil . ,d))))
     (defvar +popup-group-function
       (lambda ()
         (when (equal (buffer-name (current-buffer)) "c")
           "test-group")))
     (+popup-buffer-set-parameter a :tabbed 'right)
     (+popup-buffer-set-parameter b :tabbed 'bottom)
     (+popup-buffer-set-parameter c :tabbed 'right)
     (+popup-buffer-set-parameter d :tabbed 'right))
   :after-each
   (progn
     (makunbound '+popup-buried-buffers-alist)
     (makunbound '+popup-group-function)
     (mapc #'kill-buffer (list a b c d))))
  (with-current-buffer ,cur-buf
    (should (zenit-test-same-items-p ,expected (+popup-tab-get-tabs-fn) :test #'equal)))
  (cur-buf expected)
  a (list a d)
  b (list b)
  c (list c)
  d (list d a))

(zenit-deftest +popup-tab-single-tab-p
  (:doc "`+popup-tab-single-tab-p' returns non-nil if buffer is the only tab candidate"
   :vars
   ((a (get-buffer-create "a")))
   :before-each
   (progn
     (defvar +popup-buried-buffers-alist nil)
     (defvar +popup-group-function nil)
     (+popup-buffer-set-parameter a :tabbed 'right))
   :after-each
   (progn
     (makunbound '+popup-buried-buffers-alist)
     (makunbound '+popup-group-function)
     (mapc #'kill-buffer (list a))))
  (should (+popup-tab-single-tab-p a)))

(zenit-deftest +popup--remember
  (:doc "`+popup--remember' stores buffers in `+popup--last'"
   :vars
   ((a (get-buffer-create "a"))
    (b (get-buffer-create "b"))
    (c (get-buffer-create "c"))
    (d (get-buffer-create "d")))
   :before-each
   (defvar +popup--last nil)
   :after-each
   (progn
     (makunbound '+popup--last)
     (mapc #'kill-buffer (list a b c d))))
  (progn
    (+popup--remember (list a b c d))
    (should (zenit-test-same-items-p (list a b c d) +popup--last))))

(zenit-deftest +popup--normalize-alist
  (:doc "`+popup--normalize-alist' merges `+popup-default-alist' and `+popup-default-parameters' into alist"
        :before-each
   (progn
     (defvar +popup-default-alist '((window-height . 0.16)
                                    (reusable-frames . visible)))
     (defvar +popup-default-parameters '((transient . t)
                                         (quit . t)
                                         (select . ignore)
                                         (no-other-window . t))))
   :after-each
   (progn
     (makunbound '+popup-default-alist)
     (makunbound '+popup-default-parameters)))
  (should (equal '((+popup-buffer) (actions) (side . bottom) (window-width . 40)
                   (window-height . +popup-shrink-to-fit) (slot) (vslot)
                   (window-parameters (ttl . 5) (quit . t) (select) (modeline)
                                      (autosave) (tabbed) (transient . t)
                                      (no-other-window . t))
                   (reusable-frames . visible))
                 (+popup--normalize-alist
                  '((+popup-buffer) (actions) (side . bottom)
                    (size . +popup-shrink-to-fit) (window-width . 40)
                    (window-height . 0.16) (slot) (vslot)
                    (window-parameters (ttl . 5) (quit . t) (select) (modeline)
                                       (autosave) (tabbed)))))))

(zenit-deftest +popup--init
  (:doc "`+popup--init' is defined")
  (should (fboundp '+popup--init)))

(zenit-deftest +popup-update-reference-vars
  (:doc "`+popup-update-reference-vars' updates internal reference lists from `+popup-reference-buffers'"
   :before-each
   (progn
     (defvar +popup-reference-buffers nil)
     (defvar +popup--reference-modes nil)
     (defvar +popup--reference-names nil)
     (defvar +popup--reference-predicates nil)
     (defvar +popup--suppressed-names nil)
     (defvar +popup--suppressed-modes nil)
     (defvar +popup--suppressed-predicates nil)
     (push (cons "^[d]$" 'hide) +popup-reference-buffers)
     (push "^[abc]$" +popup-reference-buffers)
     (push 'prog-mode +popup-reference-buffers)
     (push (lambda (buf) (equal (buffer-name buf) "c"))
           +popup-reference-buffers))
   :after-each
   (progn
     (makunbound '+popup-reference-buffers)
     (makunbound '+popup--reference-modes)
     (makunbound '+popup--reference-names)
     (makunbound '+popup--reference-predicates)
     (makunbound '+popup--suppressed-names)
     (makunbound '+popup--suppressed-modes)
     (makunbound '+popup--suppressed-predicates)))
  (progn
    (+popup-update-reference-vars)
    (should (zenit-test-same-items-p '("^[d]$") +popup--suppressed-names :test #'equal))
    (should (zenit-test-same-items-p '("^[abc]$") +popup--reference-names :test #'equal))
    (should (zenit-test-same-items-p '(prog-mode) +popup--reference-modes :test #'equal))
    (should (zenit-test-same-items-p
             `(,(lambda (buf) (equal (buffer-name buf) "c")))
             +popup--reference-predicates :test #'equal))))

(zenit-deftest +popup-buffer
  (:doc "`+popup-buffer' is defined")
  (should (fboundp '+popup-buffer)))

(zenit-deftest with-popup-rules!
  (:doc "`with-popup-rules!' is defined")
  (should (fboundp 'with-popup-rules!)))

(zenit-deftest save-popups!
  (:doc "`save-popups!' is defined")
  (should (fboundp 'save-popups!)))

(zenit-deftest +popup-update-popup-alists-h
  (:doc "`+popup-update-popup-alists-h' is defined")
  (should (fboundp '+popup-update-popup-alists-h)))

(zenit-deftest +popup-suppress-popups-h
  (:doc "`+popup-suppress-popups-h' is defined")
  (should (fboundp '+popup-suppress-popups-h)))

(zenit-deftest +popup-adjust-fringes-h
  (:doc "`+popup-adjust-fringes-h' is defined")
  (should (fboundp '+popup-adjust-fringes-h)))

(zenit-deftest +popup-adjust-margins-h
  (:doc "`+popup-adjust-margins-h' is defined")
  (should (fboundp '+popup-adjust-margins-h)))

(zenit-deftest +popup-set-modeline-on-enable-h
  (:doc "`+popup-set-modeline-on-enable-h' is defined")
  (should (fboundp '+popup-set-modeline-on-enable-h)))

(zenit-deftest +popup-unset-modeline-on-disable-h
  (:doc "`+popup-unset-modeline-on-disable-h' is defined")
  (should (fboundp '+popup-unset-modeline-on-disable-h)))

(zenit-deftest +popup-close-on-escape-h
  (:doc "`+popup-close-on-escape-h' is defined")
  (should (fboundp '+popup-close-on-escape-h)))

(zenit-deftest +popup-cleanup-rules-h
  (:doc "`+popup-cleanup-rules-h' is defined")
  (should (fboundp '+popup-cleanup-rules-h)))

(zenit-deftest +popup-kill-buffer-hook-h
  (:doc "`+popup-kill-buffer-hook-h' is defined")
  (should (fboundp '+popup-kill-buffer-hook-h)))

(zenit-deftest +popup/buffer
  (:doc "`+popup/buffer' is defined")
  (should (fboundp '+popup/buffer)))

(zenit-deftest +popup/other
  (:doc "`+popup/other' is defined")
  (should (fboundp '+popup/other)))

(zenit-deftest +popup--reap-parents
  (:doc "`+popup--reap-parents' is defined")
  (should (fboundp '+popup--reap-parents)))

(zenit-deftest +popup/close
  (:doc "`+popup/close' is defined")
  (should (fboundp '+popup/close)))

(zenit-deftest +popup/close-all
  (:doc "`+popup/close-all' is defined")
  (should (fboundp '+popup/close-all)))

(zenit-deftest +popup/toggle
  (:doc "`+popup/toggle' is defined")
  (should (fboundp '+popup/toggle)))

(zenit-deftest +popup/restore
  (:doc "`+popup/restore' is defined")
  (should (fboundp '+popup/restore)))

(zenit-deftest +popup/raise
  (:doc "`+popup/raise' is defined")
  (should (fboundp '+popup/raise)))

(zenit-deftest +popup/toggle-type
  (:doc "`+popup/toggle-type' is defined")
  (should (fboundp '+popup/toggle-type)))

(zenit-deftest +popup-get-rule
  (:doc "`+popup-get-rule' is defined")
  (should (fboundp '+popup-get-rule)))

(zenit-deftest +popup/diagnose
  (:doc "`+popup/diagnose' is defined")
  (should (fboundp '+popup/diagnose)))

(zenit-deftest +popup-close-a
  (:doc "`+popup-close-a' is defined")
  (should (fboundp '+popup-close-a)))

(zenit-deftest +popup-save-a
  (:doc "`+popup-save-a' is defined")
  (should (fboundp '+popup-save-a)))

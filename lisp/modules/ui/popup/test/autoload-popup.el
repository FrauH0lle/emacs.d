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
  (should-not (cl-set-difference `((nil . ,a) (nil . ,c))
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
  (should-not (cl-set-difference `((nil (nil . ,c) (nil . ,a)))
                                 (+popup--find-buried-popup-buffers)
                                 :test #'equal))
  :doc "`+popup--find-buried-popup-buffers' returns buried popup buffers by group"
  (let ((+popup-group-function (lambda ()
                                 (when (equal (buffer-name (current-buffer)) "c")
                                   "test-group"))))
    (should-not (cl-set-difference `((nil (nil . ,a))
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
    (should-not (cl-set-difference `((,(selected-window) . ,a))
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
    (should (cl-set-difference `((nil (nil . ,a) (nil . ,c))
                                 ("test-group" (nil . ,b)))
                               +popup-buried-buffers-alist)))
  :doc "`+popup--update-buried-popup-list' removes from `+popup-buried-buffers-alist'"
  (progn
    (+popup--update-buried-popup-list nil a 'add)
    (+popup--update-buried-popup-list "test-group" b 'add)
    (+popup--update-buried-popup-list nil c 'add)
    (+popup--update-buried-popup-list nil a 'remove)
    (+popup--update-buried-popup-list nil c 'remove)
    (should (cl-set-difference `(("test-group" (nil . ,b)))
                               +popup-buried-buffers-alist))))

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
    (should-not (cl-set-difference ,expected
                                   (buffer-local-value '+popup--parents ,buffer)
                                   :test #'equal)))
  (parent buffer ignore expected)
  :doc "`+popup--record-parent' records the parent buffer"
  a b nil `((,a . nil))
  :doc "`+popup--record-parent' does not record when `+popup--ignore-parent' is nil"
  a b t nil
  :doc "`+popup--record-parent' does not record when buffer and parent are equal"
  a a nil nil
  :doc "`+popup--record-parent' does not record duplicates"
  b c nil `((,b . nil))
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
  :doc "`+popup--kill-buffer' kills buffer as soon as it is not visible"
  (progn
    (set-window-buffer (selected-window) a)
    (+popup--kill-buffer a 1)
    (should (buffer-live-p a))
    (scratch-buffer)
    (sleep-for 1.5)
    (should-not (buffer-live-p a))))

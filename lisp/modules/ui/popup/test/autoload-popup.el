;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; ui/popup/test/autoload-popup.el

(require 'zenit-test)
(require 'zenit-modules)
(zenit-require 'zenit-lib 'plist)
(zenit-load (zenit-module-locate-path :ui 'popup "autoload/popup.el"))

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
  (should-not (cl-set-difference (list (cons nil a) (cons nil c))
                                 (+popup--find-popup-buffers (list a b c d))
                                 :key #'cdr))
  :doc "`+popup--find-popup-buffers' marks buffers which should be suppressed"
  (should (eq 'suppressed
              (progn
                (+popup--find-popup-buffers (list a b c d))
                (+popup-buffer-parameter 'status d)))))

(zenit-deftest +popup--find-buried-popup-buffers
  (:doc "`+popup--find-buried-popup-buffers' returns popup buffers"
   :vars
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
  (should-not (cl-set-difference (cons nil (list (cons nil a) (cons nil c)))
                                 (+popup--find-buried-popup-buffers (list a b c))
                                 :key #'cdr)))

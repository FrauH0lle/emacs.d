;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; ui/popup/test/autoload-settings.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'modules)
(zenit-modules-initialize)
(zenit-load (zenit-module-locate-path :ui 'popup "autoload/settings.el"))

(zenit-deftest +popup--display-buffer-alist
  (:doc "`+popup--display-buffer-alist' is defined")
  (should (boundp '+popup--display-buffer-alist)))

(zenit-deftest +popup--reference-buffers
  (:doc "`+popup--reference-buffers' is defined")
  (should (boundp '+popup--reference-buffers)))

(zenit-deftest +popup-defaults
  (:doc "`+popup-defaults' is defined")
  (should (boundp '+popup-defaults)))

(zenit-deftest +popup-make-rule
  (:doc "`+popup-make-rule' creates a popup display rule")
  (should (equal '("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)"
                   (+popup-buffer) (actions) (side . bottom) (size . 0.33)
                   (window-width . 40) (window-height . 0.16) (slot) (vslot . -2)
                   (window-parameters (ttl) (quit . t) (select . ignore)
                                      (modeline) (autosave . t) (tabbed)))
                 (+popup-make-rule "^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)"
                                   '(:vslot -2 :size 0.33  :autosave t :quit t :ttl nil)))))

(zenit-deftest set-popup-rule!
  (:doc "`set-popup-rule!' defines a popup rule")
  (progn
    (set-popup-rule!
      "^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)"
      :vslot -2 :size 0.33  :autosave t :quit t :ttl nil)
    (should (zenit-test-contains-items-p
             '(("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)"
                (+popup-buffer) (actions) (side . bottom) (size . 0.33)
                (window-width . 40) (window-height . 0.16) (slot) (vslot . -2)
                (window-parameters (ttl) (quit . t) (select . ignore)
                                   (modeline) (autosave . t) (tabbed))))
             +popup--display-buffer-alist
             :test #'equal))
    (should (zenit-test-contains-items-p
             '("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)")
             +popup--reference-buffers
             :test #'equal))
    (set-popup-rule!
      "^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)"
      :ignore t)
    (should (zenit-test-contains-items-p
             '(("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)" nil))
             +popup--display-buffer-alist
             :test #'equal))
    (should-not (zenit-test-contains-items-p
                 '("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)")
                 +popup--reference-buffers
                 :test #'equal))))

(zenit-deftest set-popup-rules!
  (:doc "`set-popup-rules!' defines popup rules")
  (progn
    (set-popup-rules!
      '(("^\\*Warnings" :vslot 99 :size 0.25)
        ("^\\*Backtrace" :vslot 99 :size 0.4 :quit nil)))
    (should (zenit-test-contains-items-p
             '(("^\\*Backtrace"
                (+popup-buffer) (actions) (side . bottom) (size . 0.4)
                (window-width . 40) (window-height . 0.16) (slot) (vslot . 99)
                (window-parameters
                 (ttl . 5) (quit) (select . ignore) (modeline) (autosave) (tabbed)))
               ("^\\*Warnings"
                (+popup-buffer) (actions) (side . bottom) (size . 0.25)
                (window-width . 40) (window-height . 0.16) (slot) (vslot . 99)
                (window-parameters
                 (ttl . 5) (quit . t) (select . ignore) (modeline) (autosave) (tabbed))))
             +popup--display-buffer-alist
             :test #'equal))
    (should (zenit-test-contains-items-p
             '("^\\*Backtrace" "^\\*Warnings")
             +popup--reference-buffers
             :test #'equal))
    (set-popup-rules!
      '(("^\\*Backtrace" :ignore t)))
    (should (zenit-test-contains-items-p
             '(("^\\*Backtrace" nil))
             +popup--display-buffer-alist
             :test #'equal))
    (should-not (zenit-test-contains-items-p
                 '("^\\*Backtrace")
                 +popup--reference-buffers
                 :test #'equal))))

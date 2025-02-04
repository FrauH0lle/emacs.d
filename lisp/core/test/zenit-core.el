;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/zenit-core.el

(require 'zenit-test)
(zenit-require 'zenit-core)

(zenit-deftest zenit-os-type
  (:doc "`zenit-os-type' is defined")
  (should (boundp 'zenit-os-type)))

(zenit-deftest zenit-system
  (:doc "`zenit-system' is defined")
  (should (boundp 'zenit-system)))

(zenit-deftest zenit--system-windows-p
  (:doc "`zenit--system-windows-p' is defined")
  (should (boundp 'zenit--system-windows-p)))

(zenit-deftest zenit--system-macos-p
  (:doc "`zenit--system-macos-p' is defined")
  (should (boundp 'zenit--system-macos-p)))

(zenit-deftest zenit--system-linux-p
  (:doc "`zenit--system-linux-p' is defined")
  (should (boundp 'zenit--system-linux-p)))

(zenit-deftest zenit--system-unix-p
  (:doc "`zenit--system-unix-p' is defined")
  (should (boundp 'zenit--system-unix-p)))

(zenit-deftest zenit--system-bsd-p
  (:doc "`zenit--system-bsd-p' is defined")
  (should (boundp 'zenit--system-bsd-p)))

(zenit-deftest  zenit-init-time
  (:doc "` zenit-init-time' is defined")
  (should (boundp ' zenit-init-time)))

(zenit-deftest zenit-core-dir
  (:doc "`zenit-core-dir' is defined")
  (should (boundp 'zenit-core-dir)))

(zenit-deftest zenit-modules-dir
  (:doc "`zenit-modules-dir' is defined")
  (should (boundp 'zenit-modules-dir)))

(zenit-deftest zenit-local-conf-dir
  (:doc "`zenit-local-conf-dir' is defined")
  (should (boundp 'zenit-local-conf-dir)))

(zenit-deftest zenit-local-dir
  (:doc "`zenit-local-dir' is defined")
  (should (boundp 'zenit-local-dir)))

(zenit-deftest zenit-data-dir
  (:doc "`zenit-data-dir' is defined")
  (should (boundp 'zenit-data-dir)))

(zenit-deftest zenit-cache-dir
  (:doc "`zenit-cache-dir' is defined")
  (should (boundp 'zenit-cache-dir)))

(zenit-deftest zenit-env-file
  (:doc "`zenit-env-file' is defined")
  (should (boundp 'zenit-env-file)))

(zenit-deftest zenit-config-init-file
  (:doc "`zenit-config-init-file' is defined")
  (should (boundp 'zenit-config-init-file)))

(zenit-deftest command-line-1@respect-file-handlers
  (:doc "`command-line-1@respect-file-handlers' advises `command-line-1'")
  (progn
    (should (fboundp 'command-line-1@respect-file-handlers))
    (should (advice-member-p 'command-line-1@respect-file-handlers #'command-line-1))))

(zenit-deftest zenit--reset-file-handler-alist-h
  (:doc "`zenit--reset-file-handler-alist-h' is a member of `emacs-startup-hook'")
  (progn
    (should (fboundp 'zenit--reset-file-handler-alist-h))
    (should (member #'zenit--reset-file-handler-alist-h emacs-startup-hook))))

(zenit-deftest zenit-context
  (:doc "`zenit-context' is defined")
  (should (boundp 'zenit-context)))

(zenit-deftest zenit-context--check
  (:doc "`zenit-context--check' does not return an error if the context is valid")
  (progn
    (should-not (zenit-context--check 'init))
    (should-error (zenit-context--check 'does-not-exist))))

(zenit-deftest zenit-context-p
  (:doc "`zenit-context-p' returns non-nil if context is active"
   :vars ((zenit-context '(init eval t))))
  (progn
    (should (zenit-context-p 'init))
    (should (zenit-context-p 'eval))
    (should-not (zenit-context-p 'does-not-exist))))

(zenit-deftest zenit-context-push
  (:doc "`zenit-context-push' returns non-nil if context is added"
   :vars ((zenit-context '(t))))
  (progn
    (should (zenit-context-push 'init))
    (should (zenit-context-push 'eval))
    (should-error (zenit-context-push 'does-not-exist))))

(zenit-deftest zenit-context-pop
  (:doc "`zenit-context-pop' returns non-nil if context is removed"
   :vars ((zenit-context '(init eval t))))
  (progn
    (should (zenit-context-pop 'init))
    (should (zenit-context-pop 'eval))
    (should-not (zenit-context-pop 'does-not-exist))
    (should-error (zenit-context-pop 'does-not-exist t))))

(zenit-deftest zenit-context-with
  (:doc "`zenit-context-with' returns non-nil if context is added"
   :vars ((zenit-context '(t))))
  (zenit-context-with '(init eval t)
    (should (zenit-context-p 'init))
    (should (zenit-context-p 'eval))
    (should-not (zenit-context-p 'does-not-exist))))

(zenit-deftest zenit--write-to-etc-dir-a
  (:doc "`zenit--write-to-etc-dir-a' advises `locate-user-emacs-file'")
  (progn
    (should (fboundp 'zenit--write-to-etc-dir-a))
    (should (advice-member-p 'zenit--write-to-etc-dir-a #'locate-user-emacs-file))))

(zenit-deftest zenit--save-enabled-commands-to-custom-file-a
  (:doc "`zenit--save-enabled-commands-to-custom-file-a' advises `en/disable-command' and `locate-user-emacs-file'")
  (progn
    (should (fboundp 'zenit--save-enabled-commands-to-custom-file-a))
    (should (advice-member-p 'zenit--save-enabled-commands-to-custom-file-a #'en/disable-command))
    (should (advice-member-p 'zenit--save-enabled-commands-to-custom-file-a #'locate-user-emacs-file))))

(zenit-deftest comp-effective-async-max-jobs@set-default-cpus
  (:doc "`comp-effective-async-max-jobs@set-default-cpus' advises `comp-effective-async-max-jobs'")
  (progn
    (should (fboundp 'comp-effective-async-max-jobs@set-default-cpus))
    (should (advice-member-p 'comp-effective-async-max-jobs@set-default-cpus #'comp-effective-async-max-jobs))))

(zenit-deftest zenit-before-init-hook
  (:doc "`zenit-before-init-hook' is defined")
  (should (boundp 'zenit-before-init-hook)))

(zenit-deftest zenit-after-init-hook
  (:doc "`zenit-after-init-hook' is defined")
  (should (boundp 'zenit-after-init-hook)))

(zenit-deftest zenit--begin-init-h
  (:doc "`zenit--begin-init-h' is a member of `zenit-before-init-hook'")
  (progn
    (should (fboundp 'zenit--begin-init-h))
    (should (member #'zenit--begin-init-h zenit-before-init-hook))))

(zenit-deftest zenit--end-init-h
  (:doc "`zenit--end-init-h' is a member of `zenit-after-init-hook'")
  (progn
    (should (fboundp 'zenit--end-init-h))
    (should (member #'zenit--end-init-h zenit-after-init-hook))))

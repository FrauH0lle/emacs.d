;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/zenit-core.el

(require 'zenit-test)
(require 'zenit-core)

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

(zenit-deftest zenit-modules-load-path
  (:doc "`zenit-modules-load-path' is defined")
  (should (boundp 'zenit-modules-load-path)))

(zenit-deftest zenit-local-dir
  (:doc "`zenit-local-dir' is defined")
  (should (boundp 'zenit-local-dir)))

(zenit-deftest zenit-data-dir
  (:doc "`zenit-data-dir' is defined")
  (should (boundp 'zenit-data-dir)))

(zenit-deftest zenit-cache-dir
  (:doc "`zenit-cache-dir' is defined")
  (should (boundp 'zenit-cache-dir)))

(zenit-deftest zenit-state-dir
  (:doc "`zenit-state-dir' is defined")
  (should (boundp 'zenit-state-dir)))

(zenit-deftest zenit-env-file
  (:doc "`zenit-env-file' is defined")
  (should (boundp 'zenit-env-file)))

(zenit-deftest zenit-config-init-file
  (:doc "`zenit-config-init-file' is defined")
  (should (boundp 'zenit-config-init-file)))

(zenit-deftest zenit-module-init-file
  (:doc "`zenit-module-init-file' is defined")
  (should (boundp 'zenit-module-init-file)))

(zenit-deftest zenit-module-config-file
  (:doc "`zenit-module-config-file' is defined")
  (should (boundp 'zenit-module-config-file)))

(zenit-deftest zenit-module-packages-file
  (:doc "`zenit-module-packages-file' is defined")
  (should (boundp 'zenit-module-packages-file)))

(zenit-deftest zenit-module-control-file
  (:doc "`zenit-module-control-file' is defined")
  (should (boundp 'zenit-module-control-file)))

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
    (skip-unless (boundp 'native-comp-eln-load-path))
    (should (fboundp 'comp-effective-async-max-jobs@set-default-cpus))
    (should (advice-member-p 'comp-effective-async-max-jobs@set-default-cpus #'comp-effective-async-max-jobs))))

(zenit-deftest zenit-before-init-hook
  (:doc "`zenit-before-init-hook' is defined")
  (should (boundp 'zenit-before-init-hook)))

(zenit-deftest zenit-after-init-hook
  (:doc "`zenit-after-init-hook' is defined")
  (should (boundp 'zenit-after-init-hook)))

(zenit-deftest zenit-before-modules-init-hook
  (:doc "`zenit-before-modules-init-hook' is defined")
  (should (boundp 'zenit-before-modules-init-hook)))

(zenit-deftest zenit-after-modules-init-hook
  (:doc "`zenit-after-modules-init-hook' is defined")
  (should (boundp 'zenit-after-modules-init-hook)))

(zenit-deftest zenit-before-modules-config-hook
  (:doc "`zenit-before-modules-config-hook' is defined")
  (should (boundp 'zenit-before-modules-config-hook)))

(zenit-deftest zenit-after-modules-config-hook
  (:doc "`zenit-after-modules-config-hook' is defined")
  (should (boundp 'zenit-after-modules-config-hook)))

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

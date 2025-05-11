;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/zenit-ui.el

(require 'zenit-test)
(require 'zenit-use-package)
(require 'zenit-ui)

(zenit-deftest zenit-theme
  (:doc "`zenit-theme' is defined")
  (should (boundp 'zenit-theme)))

(zenit-deftest zenit-font
  (:doc "`zenit-font' is defined")
  (should (boundp 'zenit-font)))

(zenit-deftest zenit-variable-pitch-font
  (:doc "`zenit-variable-pitch-font' is defined")
  (should (boundp 'zenit-variable-pitch-font)))

(zenit-deftest zenit-serif-font
  (:doc "`zenit-serif-font' is defined")
  (should (boundp 'zenit-serif-font)))

(zenit-deftest zenit-symbol-font
  (:doc "`zenit-symbol-font' is defined")
  (should (boundp 'zenit-symbol-font)))

(zenit-deftest zenit-emoji-font
  (:doc "`zenit-emoji-font' is defined")
  (should (boundp 'zenit-emoji-font)))

(zenit-deftest zenit-emoji-fallback-font-families
  (:doc "`zenit-emoji-fallback-font-families' is defined")
  (should (boundp 'zenit-emoji-fallback-font-families)))

(zenit-deftest zenit-symbol-fallback-font-families
  (:doc "`zenit-symbol-fallback-font-families' is defined")
  (should (boundp 'zenit-symbol-fallback-font-families)))

(zenit-deftest zenit-init-ui-hook
  (:doc "`zenit-init-ui-hook' is defined")
  (should (boundp 'zenit-init-ui-hook)))

(zenit-deftest zenit-load-theme-hook
  (:doc "`zenit-load-theme-hook' is defined")
  (should (boundp 'zenit-load-theme-hook)))

(zenit-deftest zenit-switch-buffer-hook
  (:doc "`zenit-switch-buffer-hook' is defined")
  (should (boundp 'zenit-switch-buffer-hook)))

(zenit-deftest zenit-switch-window-hook
  (:doc "`zenit-switch-window-hook' is defined")
  (should (boundp 'zenit-switch-window-hook)))

(zenit-deftest zenit-switch-frame-hook
  (:doc "`zenit-switch-frame-hook' is defined")
  (should (boundp 'zenit-switch-frame-hook)))

(zenit-deftest zenit-run-switch-buffer-hooks-h
  (:doc "`zenit-run-switch-buffer-hooks-h' is defined")
  (should (fboundp 'zenit-run-switch-buffer-hooks-h)))

(zenit-deftest zenit-run-switch-window-hooks-h
  (:doc "`zenit-run-switch-window-hooks-h' is defined")
  (should (fboundp 'zenit-run-switch-window-hooks-h)))

(zenit-deftest zenit-switch-frame-hook-debounce-delay
  (:doc "`zenit-switch-frame-hook-debounce-delay' is defined")
  (should (boundp 'zenit-switch-frame-hook-debounce-delay)))

(zenit-deftest zenit--run-switch-frame-hooks-fn
  (:doc "`zenit--run-switch-frame-hooks-fn' is defined")
  (should (fboundp 'zenit--run-switch-frame-hooks-fn)))

(zenit-deftest zenit-run-switch-frame-hooks-fn
  (:doc "`zenit-run-switch-frame-hooks-fn' is defined")
  (should (fboundp 'zenit-run-switch-frame-hooks-fn)))

(zenit-deftest zenit-protect-fallback-buffer-h
  (:doc "`zenit-protect-fallback-buffer-h' is defined")
  (should (fboundp 'zenit-protect-fallback-buffer-h)))

(zenit-deftest zenit-highlight-non-default-indentation-h
  (:doc "`zenit-highlight-non-default-indentation-h' is defined")
  (should (fboundp 'zenit-highlight-non-default-indentation-h)))

(zenit-deftest zenit--switch-to-fallback-buffer-maybe-a
  (:doc "`zenit--switch-to-fallback-buffer-maybe-a' advises `kill-current-buffer'")
  (progn
    (should (fboundp 'zenit--switch-to-fallback-buffer-maybe-a))
    (should (advice-member-p 'zenit--switch-to-fallback-buffer-maybe-a #'kill-current-buffer))))

(zenit-deftest zenit-init-fonts-h
  (:doc "`zenit-init-fonts-h' is defined")
  (should (fboundp 'zenit-init-fonts-h)))

(zenit-deftest zenit-init-theme-h
  (:doc "`zenit-init-theme-h' is defined")
  (should (fboundp 'zenit-init-theme-h)))

(zenit-deftest zenit--detect-colorscheme-a
  (:doc "`zenit--detect-colorscheme-a' advises `provide-theme'")
  (progn
    (should (fboundp 'zenit--detect-colorscheme-a))
    (should (advice-member-p 'zenit--detect-colorscheme-a #'provide-theme))))

(zenit-deftest zenit--theme-is-colorscheme-p
  (:doc "`zenit--theme-is-colorscheme-p' is defined")
  (should (fboundp 'zenit--theme-is-colorscheme-p)))

(zenit-deftest zenit-fix-frame-color-parameters-h
  (:doc "`zenit-fix-frame-color-parameters-h' is a member of `after-make-frame-functions'")
  (progn
    (should (fboundp 'zenit-fix-frame-color-parameters-h))
    (should (member #'zenit-fix-frame-color-parameters-h after-make-frame-functions))))

(zenit-deftest zenit-init-ui-h
  (:doc "`zenit-init-ui-h' is a member of `window-setup-hook'")
  (progn
    (should (fboundp 'zenit-init-ui-h))
    (should (member #'zenit-init-ui-h window-setup-hook))))

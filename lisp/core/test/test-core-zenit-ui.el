;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-core-zenit-ui.el

(describe "core/zenit-ui"

  (require 'zenit-use-package)
  (load! "zenit-ui" zenit-core-dir)

  (describe "zenit-run-switch-buffer-hooks-h"
    (after-each
      (setq zenit-switch-buffer-hook nil))

    (it "runs the zenit-switch-buffer-hook"
      (let ((test-hook-ran nil))
        (add-hook 'zenit-switch-buffer-hook (lambda () (setq test-hook-ran t)))
        (zenit-run-switch-buffer-hooks-h)
        (expect test-hook-ran :to-be t)))

    (it "ignores optional argument"
      (let ((test-hook-ran nil))
        (add-hook 'zenit-switch-buffer-hook (lambda () (setq test-hook-ran t)))
        (zenit-run-switch-buffer-hooks-h 'ignore-this)
        (expect test-hook-ran :to-be t))))


  (describe "zenit-run-switch-window-or-frame-hooks-h"
    (after-each
      (setq zenit-switch-frame-hook nil)
      (setq zenit-switch-window-hook nil))

    (it "runs the zenit-switch-frame-hook when switching frames"
      (spy-on 'old-selected-frame :and-return-value t)
      (spy-on 'selected-frame)
      (let ((test-hook-ran nil))
        (add-hook 'zenit-switch-frame-hook (lambda () (setq test-hook-ran t)))
        (with-temp-buffer
          (zenit-run-switch-window-or-frame-hooks-h)
          (expect test-hook-ran :to-be t))))

    (it "runs the zenit-switch-window-hook when switching windows"
      (let ((test-hook-ran nil))
        (add-hook 'zenit-switch-window-hook (lambda () (setq test-hook-ran t)))
        (with-temp-buffer
          (split-window)
          (other-window 1)
          (zenit-run-switch-window-or-frame-hooks-h)
          (delete-window)
          (expect test-hook-ran :to-be t)))))


  (describe "zenit-protect-fallback-buffer-h"
    (before-each
      (setq temp-buffer (generate-new-buffer "temp"))
      (spy-on 'zenit-fallback-buffer :and-return-value temp-buffer))

    (after-each
      (kill-buffer temp-buffer))

    (it "prevents killing the fallback buffer"
      (with-current-buffer temp-buffer
        (expect (zenit-protect-fallback-buffer-h) :to-be nil)))

    (it "allows killing non-fallback buffers"
      (with-temp-buffer
        (expect (zenit-protect-fallback-buffer-h) :to-be t))))


  (describe "zenit-highlight-non-default-indentation-h"
    (before-each
      (spy-on 'require :and-return-value t)
      (spy-on 'whitespace-mode))

    (it "does nothing in fundamental-mode"
      (with-temp-buffer
        (setq major-mode 'fundamental-mode)
        (zenit-highlight-non-default-indentation-h)
        (expect 'require :not :to-have-been-called)
        (expect 'whitespace-mode :not :to-have-been-called)))

    (it "does nothing if global-whitespace-mode is active"
      (with-temp-buffer
        (setq global-whitespace-mode t)
        (zenit-highlight-non-default-indentation-h)
        (expect 'require :not :to-have-been-called)
        (expect 'whitespace-mode :not :to-have-been-called)
        (setq global-whitespace-mode nil)))

    (it "does nothing if buffer is not file-visiting"
      (with-temp-buffer
        (setq buffer-file-name nil)
        (zenit-highlight-non-default-indentation-h)
        (expect 'require :not :to-have-been-called)
        (expect 'whitespace-mode :not :to-have-been-called)))

    (it "highlights tabs when indent-tabs-mode is nil"
      (with-temp-buffer
        (setq major-mode 'emacs-lisp-mode)
        (setq buffer-file-name "temp")
        (setq indent-tabs-mode nil)
        (zenit-highlight-non-default-indentation-h)
        (expect 'require :to-have-been-called-with 'whitespace)
        (expect 'whitespace-mode :to-have-been-called-with +1)
        (expect whitespace-style :to-contain 'tabs)
        (expect whitespace-style :to-contain 'tab-mark)))

    (it "highlights spaces when indent-tabs-mode is t"
      (with-temp-buffer
        (setq major-mode 'emacs-lisp-mode)
        (setq buffer-file-name "temp")
        (setq indent-tabs-mode t)
        (zenit-highlight-non-default-indentation-h)
        (expect 'require :to-have-been-called-with 'whitespace)
        (expect 'whitespace-mode :to-have-been-called-with +1)
        (expect whitespace-style :to-contain 'indentation))))


  (describe "zenit--switch-to-fallback-buffer-maybe-a"
    (it "is defined"
      (expect (fboundp 'zenit--switch-to-fallback-buffer-maybe-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p 'zenit--switch-to-fallback-buffer-maybe-a 'kill-current-buffer) :to-be-truthy)))


  (describe "zenit-ediff-save-wconf-h"
    (before-all
      (require 'ediff))

    (it "is defined"
      (expect (fboundp 'zenit-ediff-save-wconf-h) :to-be-truthy))

    (it "is attached to correct hook"
      (expect (member 'zenit-ediff-save-wconf-h ediff-before-setup-hook) :to-be-truthy)))


  (describe "zenit-ediff-restore-wconf-h"
    (before-all
      (require 'ediff))

    (it "is defined"
      (expect (fboundp 'zenit-ediff-restore-wconf-h) :to-be-truthy))

    (it "is attached to correct hook"
      (expect (member 'zenit-ediff-restore-wconf-h ediff-quit-hook) :to-be-truthy)
      (expect (member 'zenit-ediff-restore-wconf-h ediff-suspend-hook) :to-be-truthy)))


  (describe "+hl-line--turn-on-global-hl-line-mode"
    (before-each
      (setq hl-line-mode nil)
      (setq global-hl-line-modes nil))

    (it "Does not turn on hl-line-mode if it is already on"
      (setq hl-line-mode t)
      (spy-on 'hl-line-mode)
      (+hl-line--turn-on-global-hl-line-mode)
      (expect 'hl-line-mode :not :to-have-been-called))

    (it "Does not turn on hl-line-mode if global-hl-line-modes is nil"
      (setq global-hl-line-modes nil)
      (spy-on 'hl-line-mode)
      (+hl-line--turn-on-global-hl-line-mode)
      (expect 'hl-line-mode :not :to-have-been-called))

    (it "Turns on hl-line-mode if global-hl-line-modes is t"
      (setq global-hl-line-modes t)
      (spy-on 'hl-line-mode)
      (+hl-line--turn-on-global-hl-line-mode)
      (expect 'hl-line-mode :to-have-been-called-with +1))

    (it "Does not turn on hl-line-mode if current mode is in global-hl-line-modes"
      (setq global-hl-line-modes '(not text-mode))
      (spy-on 'derived-mode-p :and-return-value t)
      (spy-on 'hl-line-mode)
      (+hl-line--turn-on-global-hl-line-mode)
      (expect 'hl-line-mode :not :to-have-been-called))

    (it "Turns on hl-line-mode if current mode is derived from a mode in global-hl-line-modes"
      (setq global-hl-line-modes '(text-mode))
      (spy-on 'derived-mode-p :and-return-value t)
      (spy-on 'hl-line-mode)
      (+hl-line--turn-on-global-hl-line-mode)
      (expect 'hl-line-mode :to-have-been-called-with +1)))


  (describe "zenit-truly-disable-hl-line-h"
    (it "is defined"
      (expect (fboundp 'zenit-truly-disable-hl-line-h) :to-be-truthy))

    (it "is attached to correct hook"
      (expect (member 'zenit-truly-disable-hl-line-h hl-line-mode-hook) :to-be-truthy)))


  (describe "zenit-disable-hl-line-h"
    (it "is defined"
      (expect (fboundp 'zenit-disable-hl-line-h) :to-be-truthy))

    (it "is attached to correct hook"
      (expect (member 'zenit-disable-hl-line-h evil-visual-state-entry-hook) :to-be-truthy)
      (expect (member 'zenit-disable-hl-line-h activate-mark-hook) :to-be-truthy)))


  (describe "zenit-enable-hl-line-maybe-h"
    (it "is defined"
      (expect (fboundp 'zenit-enable-hl-line-maybe-h) :to-be-truthy))

    (it "is attached to correct hook"
      (expect (member 'zenit-enable-hl-line-maybe-h evil-visual-state-exit-hook) :to-be-truthy)
      (expect (member 'zenit-enable-hl-line-maybe-h deactivate-mark-hook) :to-be-truthy)))


  (describe "zenit--make-font-specs"
    (it "returns '((t nil)) for a face with no specs"
      (let* ((face 'my-empty-face)
             (font "Monaco-12")
             (expected '((t (:family unspecified :foundry unspecified :slant unspecified
                             :weight unspecified :height unspecified :width unspecified)))))
        (defface my-empty-face nil "A face with no specs.")
        (expect (zenit--make-font-specs face font) :to-equal expected)))

    (it "returns altered specs for a face with some specs"
      (let* ((face 'my-face-with-specs)
             (font "Monaco-12")
             (expected `((t (:family "Arial" :foundry "outline" :slant italic
                             :weight bold :height 100 :width semi-condensed)))))
        (defface my-face-with-specs
          '((t (:family "Arial" :foundry "outline" :slant italic
                :weight bold :height 100 :width semi-condensed)))
          "A face with some specs.")
        (expect (zenit--make-font-specs face font) :to-equal expected)))

    (it "returns unaltered specs for a face with non-matching display conditions"
      (let* ((face 'my-face-with-non-matching-display)
             (font "Monaco-12")
             (expected '((t (:family "Arial" :foundry "outline" :slant italic :weight bold :height 100 :width semi-condensed)))))
        (defface my-face-with-non-matching-display
          '((t (:family "Arial" :foundry "outline" :slant italic :weight bold :height 100 :width semi-condensed)))
          "A face with a non-matching display condition.")
        (expect (zenit--make-font-specs face font) :to-equal expected))))


  (describe "zenit-init-fonts-h"
    (it "loads `zenit-font`"
      (spy-on 'zenit--make-font-specs :and-return-value '((t (:family "Arial"))))
      (spy-on 'custom-push-theme)
      (spy-on 'set-fontset-font)
      (spy-on 'run-hooks)
      (spy-on 'display-multi-font-p :and-return-value t)
      (spy-on 'set-face-attribute)
      (spy-on 'frame-list :and-return-value '(nil))

      (let ((zenit-font "Arial-12")
            (zenit-serif-font "Courier-12")
            (zenit-variable-pitch-font "Arial-12")
            (zenit-symbol-fallback-font-families '("Arial"))
            (zenit-emoji-fallback-font-families '("Arial"))
            (zenit-unicode-font "Arial-12"))

        (zenit-init-fonts-h)

        ;; Check if the spies were called with the expected arguments
        (expect 'zenit--make-font-specs :to-have-been-called-times 4)
        (expect 'custom-push-theme :to-have-been-called-times 4)
        (expect 'set-fontset-font :to-have-been-called-times 2)
        (expect 'run-hooks :to-have-been-called-with 'after-setting-font-hook)
        (expect 'set-face-attribute :to-have-been-called-times 4)
        (expect 'frame-list :to-have-been-called-times 4)
        (expect 'display-multi-font-p :to-have-been-called-times 4))))


  (describe "zenit-init-theme-h"
    (it "loads the theme specified by `zenit-theme` if not already enabled"
      (spy-on 'custom-theme-enabled-p :and-return-value nil)
      (spy-on 'load-theme)

      (let ((zenit-theme 'some-theme))
        ;; The theme is not already enabled, so zenit-init-theme-h should load it.
        (zenit-init-theme-h)
        (expect 'load-theme :to-have-been-called-with zenit-theme t)))

    (it "does not load the theme specified by `zenit-theme` if already enabled"
      (spy-on 'custom-theme-enabled-p :and-return-value t)
      (spy-on 'load-theme)

      (let ((zenit-theme 'some-theme))
        ;; This time, zenit-init-theme-h should not try to load the theme.
        (zenit-init-theme-h)
        (expect 'load-theme :not :to-have-been-called))))


  (describe "zenit--load-theme-a"
    (it "is defined"
      (expect (fboundp 'zenit--load-theme-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p 'zenit--load-theme-a 'load-theme) :to-be-truthy)))


  (describe "zenit-init-ui-h"
    (it "is defined"
      (expect (fboundp 'zenit-init-ui-h) :to-be-truthy))

    (it "is attached to correct hook"
      (expect (member 'zenit-init-ui-h window-setup-hook) :to-be-truthy))))

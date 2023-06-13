;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-core-zenit-editor.el

(describe "core/zenit-editor"

  (require 'zenit-start)
  (require 'zenit-use-package)
  (load! "zenit-editor" zenit-core-dir)

  (describe "zenit--prepare-for-large-files-a"
    (it "is defined"
      (expect (fboundp 'zenit--prepare-for-large-files-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p 'zenit--prepare-for-large-files-a 'abort-if-file-too-large) :to-be-truthy)))


  (describe "zenit-optimize-for-large-files-h"
    (it "is defined"
      (expect (fboundp 'zenit-optimize-for-large-files-h) :to-be-truthy))

    (it "is attached to correct hook"
      (expect (member 'zenit-optimize-for-large-files-h find-file-hook) :to-be-truthy)))


  (describe "zenit--record-symlink-origin-a"
    (it "is defined"
      (expect (fboundp 'zenit--record-symlink-origin-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p 'zenit--record-symlink-origin-a 'vc-follow-link) :to-be-truthy)))


  (describe "zenit-create-missing-directories-h"
    (it "is defined"
      (expect (fboundp 'zenit-create-missing-directories-h) :to-be-truthy))

    (it "is attached to correct hook"
      (expect (member 'zenit-create-missing-directories-h find-file-not-found-functions) :to-be-truthy)))


  (describe "zenit-guess-mode-h"
    (it "is defined"
      (expect (fboundp 'zenit-guess-mode-h) :to-be-truthy))

    (it "is attached to correct hook"
      (expect (member 'zenit-guess-mode-h after-save-hook) :to-be-truthy)))


  (describe "zenit--shut-up-autosave-a"
    (it "is defined"
      (expect (fboundp 'zenit--shut-up-autosave-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p 'zenit--shut-up-autosave-a 'after-find-file) :to-be-truthy)))


  (describe "zenit-make-hashed-auto-save-file-name-a"
    (it "is defined"
      (expect (fboundp 'zenit-make-hashed-auto-save-file-name-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p 'zenit-make-hashed-auto-save-file-name-a 'make-auto-save-file-name) :to-be-truthy)))


  (describe "zenit-make-hashed-backup-file-name-a"
    (it "is defined"
      (expect (fboundp 'zenit-make-hashed-backup-file-name-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p 'zenit-make-hashed-backup-file-name-a 'make-backup-file-name-1) :to-be-truthy)))


  (describe "zenit-cache-hashed-backup-file-name-a"
    (it "is defined"
      (expect (fboundp 'zenit-cache-hashed-backup-file-name-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p 'zenit-cache-hashed-backup-file-name-a 'backup-buffer) :to-be-truthy)))


  (describe "autorevert package config"
    (describe "zenit-auto-revert-buffer-h"
      (it "is defined"
        (expect (fboundp 'zenit-auto-revert-buffer-h) :to-be-truthy))

      (it "is attached to correct hook"
        (expect (member 'zenit-auto-revert-buffer-h zenit-switch-buffer-hook) :to-be-truthy)
        (expect (member 'zenit-auto-revert-buffer-h zenit-switch-window-hook) :to-be-truthy)))

    (describe "zenit-auto-revert-buffers-h"
      (it "is defined"
        (expect (fboundp 'zenit-auto-revert-buffers-h) :to-be-truthy))

      (it "is attached to correct hook"
        (expect (member 'zenit-auto-revert-buffers-h after-save-hook) :to-be-truthy))))


  (describe "recentf package config"
    (before-all
      (require 'recentf))

    (describe "zenit--recentf-file-truename-fn"
      (it "returns the abbreviated true name for local files"
        (let ((temp-file (make-temp-file "local-file")))
          (spy-on 'tramp-file-name-localname :and-return-value temp-file)
          (expect (zenit--recentf-file-truename-fn temp-file) :to-equal (abbreviate-file-name (file-truename temp-file)))))

      (it "returns the abbreviated true name for sudo remote files"
        (let ((temp-file "/sudo::/tmp/sudo-file"))
          (spy-on 'file-remote-p :and-return-value "sudo")
          (spy-on 'tramp-file-name-localname :and-return-value "/tmp/sudo-file")
          (expect (zenit--recentf-file-truename-fn temp-file) :to-equal (abbreviate-file-name (file-truename "/tmp/sudo-file")))))

      (it "returns the original file name for non-sudo remote files"
        (let ((temp-file "/ssh:user@example.com:/remote/file"))
          (spy-on 'file-remote-p :and-return-value "ssh")
          (expect (zenit--recentf-file-truename-fn temp-file) :to-equal temp-file))))


    (describe "zenit--recentf-touch-buffer-h"
      (it "is defined"
        (expect (fboundp 'zenit--recentf-touch-buffer-h) :to-be-truthy))

      (it "is attached to correct hook"
        (expect (member 'zenit--recentf-touch-buffer-h zenit-switch-window-hook) :to-be-truthy)
        (expect (member 'zenit--recentf-touch-buffer-h write-file-functions) :to-be-truthy)))

    (describe "zenit--recentf-add-dired-directory-h"
      (it "is defined"
        (expect (fboundp 'zenit--recentf-add-dired-directory-h) :to-be-truthy))

      (it "is attached to correct hook"
        (expect (member 'zenit--recentf-add-dired-directory-h dired-mode-hook) :to-be-truthy))))


  (describe "savehist package config"
    (before-all
      (require 'savehist))

    (describe "zenit-savehist-unpropertize-variables-h"
      (it "is defined"
        (expect (fboundp 'zenit-savehist-unpropertize-variables-h) :to-be-truthy))

      (it "is attached to correct hook"
        (expect (member 'zenit-savehist-unpropertize-variables-h savehist-save-hook) :to-be-truthy)))

    (describe "zenit-savehist-remove-unprintable-registers-h"
      (it "is defined"
        (expect (fboundp 'zenit-savehist-remove-unprintable-registers-h) :to-be-truthy))

      (it "is attached to correct hook"
        (expect (member 'zenit-savehist-remove-unprintable-registers-h savehist-save-hook) :to-be-truthy))))


  (describe "saveplace package config"
    (before-all
      (require 'saveplace))

    (describe "zenit--recenter-on-load-saveplace-a"
      (it "is defined"
        (expect (fboundp 'zenit--recenter-on-load-saveplace-a) :to-be-truthy))

      (it "advises the correct function"
        (expect (advice-member-p 'zenit--recenter-on-load-saveplace-a 'save-place-find-file-hook) :to-be-truthy)))

    (describe "zenit--inhibit-saveplace-in-long-files-a"
      (it "is defined"
        (expect (fboundp 'zenit--inhibit-saveplace-in-long-files-a) :to-be-truthy))

      (it "advises the correct function"
        (expect (advice-member-p 'zenit--inhibit-saveplace-in-long-files-a 'save-place-to-alist) :to-be-truthy)))

    (describe "zenit--dont-prettify-saveplace-cache-a"
      (it "is defined"
        (expect (fboundp 'zenit--dont-prettify-saveplace-cache-a) :to-be-truthy))

      (it "advises the correct function"
        (expect (advice-member-p 'zenit--dont-prettify-saveplace-cache-a 'save-place-alist-to-file) :to-be-truthy))))


  (describe "better-jumper package config"
    (describe "zenit-set-jump-a"
      (it "is defined"
        (expect (fboundp 'zenit-set-jump-a) :to-be-truthy)))

    (describe "zenit-set-jump-maybe-a"
      (it "is defined"
        (expect (fboundp 'zenit-set-jump-maybe-a) :to-be-truthy)))

    (describe "zenit-set-jump-h"
      (it "is defined"
        (expect (fboundp 'zenit-set-jump-h) :to-be-truthy))))


  (describe "dtrt-indent package config"
    (describe "zenit-detect-indentation-h"
      (before-all
        (setq noninteractive nil)
        (load! "zenit-editor" zenit-core-dir)
        (require 'dtrt-indent))

      (before-each
        (setq after-init-time t)
        (setq zenit-inhibit-indent-detection nil)
        (setq zenit-large-file-p nil)
        (setq zenit-detect-indentation-excluded-modes nil)
        (setq init-file-debug nil)
        (spy-on 'dtrt-indent-mode))

      (after-all
        (setq noninteractive t))

      (it "enables dtrt-indent-mode for regular files"
        (with-temp-buffer
          (rename-buffer (make-temp-file "test"))
          (zenit-detect-indentation-h)
          (expect 'dtrt-indent-mode :to-have-been-called)))

      (it "does not enable dtrt-indent-mode for large files"
        (setq zenit-large-file-p t)
        (with-temp-buffer
          (rename-buffer (make-temp-file "test"))
          (zenit-detect-indentation-h)
          (expect 'dtrt-indent-mode :not :to-have-been-called)))

      (it "does not enable dtrt-indent-mode before init time"
        (setq after-init-time nil)
        (with-temp-buffer
          (rename-buffer (make-temp-file "test"))
          (zenit-detect-indentation-h)
          (expect 'dtrt-indent-mode :not :to-have-been-called)))

      (it "does not enable dtrt-indent-mode when indentation detection is inhibited"
        (with-temp-buffer
          (setq zenit-inhibit-indent-detection t)
          (rename-buffer (make-temp-file "test"))
          (zenit-detect-indentation-h)
          (expect 'dtrt-indent-mode :not :to-have-been-called)))

      (it "does not enable dtrt-indent-mode for excluded modes"
        (with-temp-buffer
          (setq zenit-detect-indentation-excluded-modes '(text-mode))
          (rename-buffer (make-temp-file "test"))
          (text-mode)
          (zenit-detect-indentation-h)
          (expect 'dtrt-indent-mode :not :to-have-been-called)))

      (it "does not enable dtrt-indent-mode for special buffer names"
        (with-temp-buffer
          (rename-buffer "*test-buffer*")
          (zenit-detect-indentation-h)
          (expect 'dtrt-indent-mode :not :to-have-been-called))))

    (describe "zenit--fix-broken-smie-modes-a"
      (it "is defined"
        (expect (fboundp 'zenit--fix-broken-smie-modes-a) :to-be-truthy))

      (it "advises the correct function"
        (expect (advice-member-p 'zenit--fix-broken-smie-modes-a 'dtrt-indent-mode) :to-be-truthy))))


  (describe "helpful package config"
    (before-all
      (require 'helpful))

    (describe "zenit-use-helpful-a"
      (it "is defined"
        (expect (fboundp 'zenit-use-helpful-a) :to-be-truthy)))

    (describe "zenit--clone-emacs-source-maybe"
      (before-each
        (setq source-directory (make-temp-file "emacs-src" t))
        (spy-on 'yes-or-no-p :and-return-value t)
        (spy-on 'make-directory)
        (spy-on 'compile))

      (it "does nothing when source directory exists"
        (zenit--clone-emacs-source-maybe)
        (expect 'yes-or-no-p :not :to-have-been-called))

      (it "does nothing when *clone-emacs-src* buffer exists"
        (let ((buf (get-buffer-create "*clone-emacs-src*")))
          (with-current-buffer buf
            (zenit--clone-emacs-source-maybe)
            (expect 'yes-or-no-p :not :to-have-been-called))
          (kill-buffer buf)))

      (it "clones the source when user confirms"
        (setq source-directory "/tmp/non-existent-dir")
        (zenit--clone-emacs-source-maybe)
        (expect 'make-directory :to-have-been-called-with (file-name-directory source-directory) 'parents)
        (expect 'compile :to-have-been-called))

      (it "does not clone the source when user denies"
        (setq source-directory "/tmp/non-existent-dir")
        (spy-on 'yes-or-no-p)
        (zenit--clone-emacs-source-maybe)
        (expect 'make-directory :not :to-have-been-called)
        (expect 'compile :not :to-have-been-called)))

    (describe "+find-func--clone-emacs-source-a"
      (it "is defined"
        (expect (fboundp '+find-func--clone-emacs-source-a) :to-be-truthy))

      (it "advises the correct function"
        (expect (advice-member-p '+find-func--clone-emacs-source-a 'find-function-C-source) :to-be-truthy)))

    (describe "+helpful--clone-emacs-source-a"
      (it "is defined"
        (expect (fboundp '+helpful--clone-emacs-source-a) :to-be-truthy))

      (it "advises the correct function"
        (expect (advice-member-p '+helpful--clone-emacs-source-a 'helpful--library-path) :to-be-truthy))))


  (describe "smartparens package config"
    (before-all
      (require 'smartparens))

    (describe "zenit-init-smartparens-in-eval-expression-h"
      (it "is defined"
        (expect (fboundp 'zenit-init-smartparens-in-eval-expression-h) :to-be-truthy))

      (it "is attached to correct hook"
        (expect (member 'zenit-init-smartparens-in-eval-expression-h eval-expression-minibuffer-setup-hook) :to-be-truthy)))

    (describe "zenit-init-smartparens-in-minibuffer-maybe-h"
      (it "is defined"
        (expect (fboundp 'zenit-init-smartparens-in-minibuffer-maybe-h) :to-be-truthy))

      (it "is attached to correct hook"
        (expect (member 'zenit-init-smartparens-in-minibuffer-maybe-h minibuffer-setup-hook) :to-be-truthy)))

    (describe "zenit-enable-smartparens-mode-maybe-h"
      (it "is defined"
        (expect (fboundp 'zenit-enable-smartparens-mode-maybe-h) :to-be-truthy))

      (it "is attached to correct hook"
        (expect (member 'zenit-enable-smartparens-mode-maybe-h evil-replace-state-exit-hook) :to-be-truthy)))

    (describe "zenit-disable-smartparens-mode-maybe-h"
      (it "is defined"
        (expect (fboundp 'zenit-disable-smartparens-mode-maybe-h) :to-be-truthy))

      (it "is attached to correct hook"
        (expect (member 'zenit-disable-smartparens-mode-maybe-h evil-replace-state-entry-hook) :to-be-truthy)))))

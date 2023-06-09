;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-core-zenit-projects.el

(describe "core/zenit-projects"

  (require 'zenit-use-package)
  (load! "zenit-projects" zenit-core-dir)
  (require 'projectile)

  (describe "zenit--projectile-dirconfig-file-a"
    (it "is defined"
      (expect (fboundp 'zenit--projectile-dirconfig-file-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p 'zenit--projectile-dirconfig-file-a 'projectile-dirconfig-file) :to-be-truthy)))


  (describe "zenit-cleanup-project-cache-h"
    (it "is defined"
      (expect (fboundp 'zenit-cleanup-project-cache-h) :to-be-truthy))

    (it "is attached to correct hook"
      (expect (member 'zenit-cleanup-project-cache-h kill-emacs-hook) :to-be-truthy)))


  (describe "zenit--projectile-default-generic-command-a"
    (it "is defined"
      (expect (fboundp 'zenit--projectile-default-generic-command-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p 'zenit--projectile-default-generic-command-a 'projectile-default-generic-command) :to-be-truthy)))


  (describe "def-project-mode!"
    (it "defines a project minor mode"
      (def-project-mode! zenit-test-mode
        :modes '(emacs-lisp-mode)
        :files '("test.el")
        :match "test"
        :on-load (message "Loading zenit-test-mode...")
        :on-enter (message "Entering zenit-test-mode...")
        :on-exit (message "Exiting zenit-test-mode..."))

      (expect (fboundp 'zenit-test-mode) :to-be-truthy)
      (expect (boundp 'zenit-test-mode-hook) :to-be-truthy)
      (expect (boundp 'zenit-test-mode-map) :to-be-truthy))))

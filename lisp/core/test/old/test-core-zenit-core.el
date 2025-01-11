;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-core-zenit-core.el


(describe "core/zenit-core"
  :var ((noninteractive nil))

  (load! "zenit-core" zenit-core-dir)

  (describe "zenit--reset-file-handler-alist-h"
    (it "is defined"
      (expect (fboundp 'zenit--reset-file-handler-alist-h) :to-be-truthy))

    (it "is attached to correct hook"
      (expect (member 'zenit--reset-file-handler-alist-h emacs-startup-hook) :to-be-truthy)))


  (describe "zenit--reset-load-suffixes-h"
    (it "is defined"
      (expect (fboundp 'zenit--reset-load-suffixes-h) :to-be-truthy))

    (it "is attached to correct hook"
      (expect (member 'zenit--reset-load-suffixes-h zenit-before-init-hook) :to-be-truthy)))


  (describe "zenit--reset-custom-dont-initialize-h"
    (it "is defined"
      (expect (fboundp 'zenit--reset-custom-dont-initialize-h) :to-be-truthy))

    (it "is attached to correct hook"
      (expect (member 'zenit--reset-custom-dont-initialize-h zenit-before-init-hook) :to-be-truthy)))


  (describe "zenit-context--check"
    (it "does not signal an error for valid contexts"
      (dolist (context '(cli compile eval init modules packages reload sandbox tests))
        (expect (zenit-context--check context) :not :to-throw)))

    (it "signals an error for invalid contexts"
      (dolist (context '(invalid1 invalid2))
        (expect (zenit-context--check context) :to-throw 'zenit-context-error))))


  (describe "zenit-context-p"
    (it "returns t if the context is active"
      (let ((zenit-context '(cli compile)))
        (expect (zenit-context-p 'cli) :to-be t)
        (expect (zenit-context-p 'compile) :to-be t)))

    (it "returns nil if the context is not active"
      (let ((zenit-context '(cli compile)))
        (expect (zenit-context-p 'eval) :to-be nil)
        (expect (zenit-context-p 'init) :to-be nil))))


  (describe "zenit-context-push"
    (it "adds the context if it isn't already present"
      (let ((zenit-context '(cli compile)))
        (zenit-context-push 'eval)
        (expect (zenit-context-p 'eval) :to-be t)))

    (it "does not add the context if it is already present"
      (let ((zenit-context '(cli compile eval)))
        (zenit-context-push 'eval)
        (expect (equal zenit-context '(cli compile eval)) :to-be t)))

    (it "throws an error if the context is invalid"
      (let ((zenit-context '(cli compile)))
        (expect (zenit-context-push 'invalid) :to-throw 'zenit-context-error))))


  (describe "zenit-context-pop"
    (it "removes the context if it is present"
      (let ((zenit-context '(cli compile eval)))
        (zenit-context-pop 'eval)
        (expect (zenit-context-p 'eval) :to-be nil)))

    (it "does nothing if the context is not present"
      (let ((zenit-context '(cli compile)))
        (zenit-context-pop 'eval)
        (expect (equal zenit-context '(cli compile)) :to-be t)))

    (it "throws an error if the context is not present and strict mode is on"
      (let ((zenit-context '(cli compile)))
        (expect (zenit-context-pop 'eval t) :to-throw 'zenit-context-error))))


  (describe "zenit-context-with"
    (it "adds the context for the duration of the body"
      (let ((zenit-context '(cli compile)))
        (zenit-context-with 'eval
          (expect (zenit-context-p 'eval) :to-be t))
        (expect (zenit-context-p 'eval) :to-be nil)))

    (it "works with multiple contexts"
      (let ((zenit-context '(cli compile)))
        (zenit-context-with '(eval init)
          (expect (zenit-context-p 'eval) :to-be t)
          (expect (zenit-context-p 'init) :to-be t))
        (expect (zenit-context-p 'eval) :to-be nil)
        (expect (zenit-context-p 'init) :to-be nil))))


  (describe "zenit--write-to-etc-dir-a"
    (it "is defined"
      (expect (fboundp 'zenit--write-to-etc-dir-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p 'zenit--write-to-etc-dir-a 'locate-user-emacs-file) :to-be-truthy)))


  (describe "zenit--save-enabled-commands-to-custom-file-a"
    (it "is defined"
      (expect (fboundp 'zenit--save-enabled-commands-to-custom-file-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p 'zenit--save-enabled-commands-to-custom-file-a 'en/disable-command) :to-be-truthy)
      (expect (advice-member-p 'zenit--save-enabled-commands-to-custom-file-a 'locate-user-emacs-file) :to-be-truthy)))


  (describe "zenit--begin-init-h"
    (it "is defined"
      (expect (fboundp 'zenit--begin-init-h) :to-be-truthy))

    (it "is attached to correct hook"
      (expect (member 'zenit--begin-init-h zenit-before-init-hook) :to-be-truthy)))


  (describe "zenit--end-init-h"
    (it "is defined"
      (expect (fboundp 'zenit--end-init-h) :to-be-truthy))

    (it "is attached to correct hook"
      (expect (member 'zenit--end-init-h zenit-after-init-hook) :to-be-truthy))))

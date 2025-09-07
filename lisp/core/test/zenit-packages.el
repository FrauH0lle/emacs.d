;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/zenit-packages.el

(require 'zenit-test)
(require 'zenit-packages)
(require 'straight)

(zenit-deftest zenit-init-packages-p
  (:doc "`zenit-init-packages-p' is defined")
  (should (boundp 'zenit-init-packages-p)))

(zenit-deftest zenit-mandatory-packages
  (:doc "`zenit-mandatory-packages' is defined")
  (should (boundp 'zenit-mandatory-packages)))

(zenit-deftest zenit-packages
  (:doc "`zenit-packages' is defined")
  (should (boundp 'zenit-packages)))

(zenit-deftest zenit-disabled-packages
  (:doc "`zenit-disabled-packages' is defined")
  (should (boundp 'zenit-disabled-packages)))

(zenit-deftest zenit-packages-file
  (:doc "`zenit-packages-file' is defined")
  (should (boundp 'zenit-packages-file)))

(zenit-deftest +straight--normalize-profiles
  (:doc "`+straight--normalize-profiles' is defined")
  (should (fboundp '+straight--normalize-profiles)))

(zenit-deftest +straight--auto-options
  (:doc "`+straight--auto-options' is defined")
  (should (boundp '+straight--auto-options)))

(zenit-deftest straight-vc-git--popup-raw
  (:doc "`straight-vc-git--popup-raw' advises `straight--popup-raw'")
  (skip-unless noninteractive)
  (should (advice-member-p 'straight-vc-git--popup-raw #'straight--popup-raw)))

(zenit-deftest +straight--fallback-to-y-or-n-prompt-a
  (:doc "`+straight--fallback-to-y-or-n-prompt-a' advises `straight-are-you-sure'")
  (progn
    (should (fboundp '+straight--fallback-to-y-or-n-prompt-a))
    (should (advice-member-p '+straight--fallback-to-y-or-n-prompt-a #'straight-are-you-sure))))

(zenit-deftest +straight--recommended-option-p
  (:doc "`+straight--recommended-option-p' is defined")
  (should (fboundp '+straight--recommended-option-p)))

(zenit-deftest +straight--fallback-to-tty-prompt-a
  (:doc "`+straight--fallback-to-tty-prompt-a' advises `straight--popup-raw'")
  (progn
    (should (fboundp '+straight--fallback-to-tty-prompt-a))
    (should (advice-member-p '+straight--fallback-to-tty-prompt-a #'straight--popup-raw))))

(zenit-deftest +straight--respect-print-indent-a
  (:doc "`+straight--respect-print-indent-a' advises `straight-use-package'")
  (progn
    (should (fboundp '+straight--respect-print-indent-a))
    (should (advice-member-p '+straight--respect-print-indent-a #'straight-use-package))))

(zenit-deftest zenit-read-packages
  (:doc "`zenit-read-packages' is defined")
  (should (fboundp 'zenit-read-packages)))

(zenit-deftest zenit-initialize-packages
  (:doc "`zenit-initialize-packages' is defined")
  (should (fboundp 'zenit-initialize-packages)))

(zenit-deftest zenit-bootstrap-straight
  (:doc "`zenit-bootstrap-straight' is defined")
  (should (fboundp 'zenit-bootstrap-straight)))

(zenit-deftest zenit-package-get
  (:doc "`zenit-package-get' returns package recipe")
  (let ((zenit-packages '((ws-butler :lockfile
                           (core)
                           :recipe
                           (:host github :repo "emacsmirror/nongnu_elpa" :branch "elpa/ws-butler" :local-repo "ws-butler")
                           :modules
                           ((:core))))))
    (should
     (equal
      '(:lockfile (core)
        :recipe (:host github :repo "emacsmirror/nongnu_elpa" :branch "elpa/ws-butler" :local-repo "ws-butler")
        :modules ((:core)))
      (zenit-package-get 'ws-butler)))
    (should (equal '((:core)) (zenit-package-get 'ws-butler :modules)))
    (should (equal 'default (zenit-package-get 'ws-butler :nosuchkey 'default)))))

(zenit-deftest zenit-package-set
  (:doc "`zenit-package-set' sets property in package recipe")
  (let ((zenit-packages '((ws-butler :lockfile
                           (core)
                           :recipe
                           (:host github :repo "emacsmirror/nongnu_elpa" :branch "elpa/ws-butler" :local-repo "ws-butler")
                           :modules
                           ((:core))))))
    (should
     (equal
      '(:lockfile (core)
        :recipe (:host github :repo "emacsmirror/nongnu_elpa" :branch "elpa/ws-butler" :local-repo "ws-butler")
        :modules ((:other)))
      (zenit-package-set 'ws-butler :modules '((:other)))))))

(zenit-deftest zenit-package-recipe-repo
  (:doc "`zenit-package-recipe-repo' returns package's local-repo property")
  (should (equal "emacs" (zenit-package-recipe-repo 'emacs))))

(zenit-deftest zenit-package-build-recipe
  (:doc "`zenit-package-build-recipe' returns package's `straight' recipe"
   :before-each (straight--load-build-cache))
  (should
   (equal
    '(:type git :host github :repo "radian-software/straight.el" :files ("straight*.el")
      :branch "develop" :package "straight" :local-repo "straight.el")
    (zenit-package-build-recipe 'straight))))

(zenit-deftest zenit-package-built-in-p
  (:doc "`zenit-package-built-in-p' returns non-nil if package is built-in"
   :before-each (straight--load-build-cache))
  (should-not (zenit-package-built-in-p 'straight)))

(zenit-deftest zenit-package-backend
  (:doc "`zenit-package-backend' returns the package backend"
   :before-each (straight--load-build-cache))
  (should (eq 'straight (zenit-package-backend 'straight))))

(zenit-deftest zenit-packages--read
  (:doc "`zenit-packages--read' is defined")
  (should (fboundp 'zenit-packages--read)))

(zenit-deftest zenit-package-list
  (:doc "`zenit-package-list' is defined")
  (should (fboundp 'zenit-package-list)))

(zenit-deftest zenit--process-pkg-dependencies
  (:doc "`zenit--process-pkg-dependencies' is defined")
  (should (fboundp 'zenit--process-pkg-dependencies)))

(zenit-deftest package!
  (:doc "`package!' is defined")
  (should (fboundp 'package!)))

(zenit-deftest disable-packages!
  (:doc "`disable-packages!' is defined")
  (should (fboundp 'disable-packages!)))

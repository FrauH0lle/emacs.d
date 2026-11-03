;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/zenit-projects.el

(require 'cl-lib)
(require 'zenit-test)
(require 'zenit-use-package)
(zenit-require 'zenit-lib 'files)
(require 'zenit-projects)
(require 'projectile)

(zenit-deftest zenit--projectile-dirconfig-file-a
  ()
  (let ((root (make-temp-file "zenit-project-dirconfig-" t))
        (projectile-dirconfig-file ".projectile"))
    (unwind-protect
        (cl-letf (((symbol-function 'projectile-project-root)
                   (lambda ()
                     (when ,explicit-root
                       (error "Explicit roots must not be rediscovered"))
                     root)))
          (dolist (file ',files)
            (write-region "" nil (expand-file-name file root) nil 'silent))
          (should (advice-member-p #'zenit--projectile-dirconfig-file-a
                                   'projectile-dirconfig-file))
          (should (equal (expand-file-name ,expected root)
                         (if ,explicit-root
                             (projectile-dirconfig-file root)
                           (projectile-dirconfig-file)))))
      (delete-directory root t)))
  (explicit-root files expected)
  :doc "An explicit root works without discovering the current project"
  t (".projectile") ".projectile"
  :doc "The current project remains the default root"
  nil (".projectile") ".projectile"
  :doc "The Projectile config takes precedence over the generic config"
  t (".projectile" ".project") ".projectile"
  :doc "A generic project config is supported"
  t (".project") ".project"
  :doc "Missing configs resolve to the generic project config path"
  t nil ".project")

(zenit-deftest zenit-fd-executable
  (:doc "`zenit-fd-executable' is defined")
  (should (boundp 'zenit-fd-executable)))

(zenit-deftest zenit-ripgrep-executable
  (:doc "`zenit-ripgrep-executable' is defined")
  (should (boundp 'zenit-ripgrep-executable)))

(zenit-deftest zenit-projectile-cache-dir
  (:doc "`zenit-projectile-cache-dir' is defined")
  (should (boundp 'zenit-projectile-cache-dir)))

(zenit-deftest zenit-project-hook
  (:doc "`zenit-project-hook' is defined")
  (should (boundp 'zenit-project-hook)))

(zenit-deftest def-project-mode!
  (:doc "`def-project-mode!' is defined")
  (should (fboundp 'def-project-mode!)))

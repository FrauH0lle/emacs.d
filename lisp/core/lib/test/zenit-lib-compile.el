;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-compile.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'compile)

(zenit-deftest zenit-compile-generate-args
  (:doc "`zenit-compile-generate-args' returns compilation args")
  (should (equal ',out (zenit-compile-generate-args ,in)))
  (in out)
  "zenit-lib.el" nil
  "zenit-core.el" nil
  (file-name-concat zenit-core-dir "lib/test.el") (:req-core t)
  "zenit-modules.el" (:req-core t)
  "zenit-start.el" (:req-core t)
  "zenit-use-package.el" (:req-core t)
  "zenit-el-patch.el" (:req-core t)
  "zenit-keybinds.el" (:req-core t :req-extra (zenit-modules zenit-use-package zenit-el-patch))
  "zenit-ui.el" (:req-core t :req-extra (zenit-use-package))
  "zenit-projects.el" (:req-core t :req-extra (zenit-use-package))
  "zenit-editor.el" (:req-core t :req-extra (zenit-use-package))
  (expand-file-name (file-name-concat user-emacs-directory "init.el"))
  (:req-core-lib t :req-core t :req-core-libs all
   :req-extra (cl-lib zenit-modules zenit-use-package zenit-el-patch
                      zenit-keybinds zenit-projects zenit-editor)
   :modulep t :autoloads t))

(zenit-deftest zenit-compile--generate-modules
  (:doc "`zenit-compile--generate-modules' is defined")
  (should (fboundp 'zenit-compile--generate-modules)))

(zenit-deftest zenit-compile-setup-env
  (:doc "`zenit-compile-setup-env' is defined")
  (should (fboundp 'zenit-compile-setup-env)))

(zenit-deftest zenit-async-byte-compile-file
  (:doc "`zenit-async-byte-compile-file' is defined")
  (should (fboundp 'zenit-async-byte-compile-file)))

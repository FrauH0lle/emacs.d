;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-autoloads.el

(require 'cl-lib)
(require 'bytecomp)
(require 'zenit-test)
(require 'zenit-packages)
(zenit-require 'zenit-lib 'autoloads)
(zenit-require 'zenit-lib 'compile)
(zenit-require 'zenit-lib 'modules)

(zenit-deftest zenit-autoloads-excluded-packages
  (:doc "`zenit-autoloads-excluded-packages' is defined")
  (should (boundp 'zenit-autoloads-excluded-packages)))

(zenit-deftest zenit-autoloads-excluded-files
  (:doc "`zenit-autoloads-excluded-files' is defined")
  (should (boundp 'zenit-autoloads-excluded-files)))

(zenit-deftest zenit-autoloads-cached-vars
  (:doc "`zenit-autoloads-cached-vars' is defined")
  (should (boundp 'zenit-autoloads-cached-vars)))

(zenit-deftest zenit-autoloads-files
  (:doc "`zenit-autoloads-files' is defined")
  (should (boundp 'zenit-autoloads-files)))

(zenit-deftest zenit-autoloads--compile-file
  ()
  ,test
  (test)
  :doc "`zenit-autoloads--compile-file' is defined"
  (should (fboundp 'zenit-autoloads--compile-file))

  :doc "Inlined module config can define project modes in a fresh compiler"
  (let ((file (make-temp-file "zenit-autoload-project-" nil ".el"))
        (zenit-modules (make-hash-table :test 'equal))
        (minor-mode-list nil)
        (minor-mode-map-alist nil)
        (byte-compile-warnings nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";; -*- lexical-binding: t; -*-\n"
                    "(def-project-mode! zenit-test-autoload-project-mode)\n"))
          (cl-letf (((symbol-function 'zenit-module-p)
                     (lambda (category module &rest _)
                       (and (eq category :config) (eq module 'compile)))))
            (should (zenit-autoloads--compile-file file)))
          (should (boundp 'zenit-test-autoload-project-mode))
          (should (fboundp 'zenit-test-autoload-project-mode)))
      (delete-file file)
      (delete-file (byte-compile-dest-file file))
      (fmakunbound 'zenit-test-autoload-project-mode)
      (dolist (symbol '(zenit-test-autoload-project-mode
                        zenit-test-autoload-project-mode-map
                        zenit-test-autoload-project-mode-hook))
        (makunbound symbol)))))

(zenit-deftest zenit-autoloads--cleanup-form
  (:doc "`zenit-autoloads--cleanup-form' is defined")
  (should (fboundp 'zenit-autoloads--cleanup-form)))

(zenit-deftest zenit-autoloads--scan-autodefs
  (:doc "`zenit-autoloads--scan-autodefs' is defined")
  (should (fboundp 'zenit-autoloads--scan-autodefs)))

(zenit-deftest zenit-loaddefs-temp-file
  (:doc "`zenit-loaddefs-temp-file' is defined")
  (should (boundp 'zenit-loaddefs-temp-file)))

(zenit-deftest zenit-autoloads--scan-file
  (:doc "`zenit-autoloads--scan-file' is defined")
  (should (fboundp 'zenit-autoloads--scan-file)))

(zenit-deftest zenit-autoloads--scan
  (:doc "`zenit-autoloads--scan' is defined")
  (should (fboundp 'zenit-autoloads--scan)))

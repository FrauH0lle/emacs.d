;; config/compilation/config.el -*- lexical-binding: t; -*-

;; WIP module for byte- and native-compilation of the emacs config.

;; IDEA: Instead of compiling each init.el and config.el file separately, use a
;;       similar mechanism as radian emacs and embed the content of these files
;;       into the init.el file which is created dynamically anyways.

;; - DONE Solve the issue with the modulep! and load! macros. Both of them try to
;;   figure out the path of the file they run in, which would be wrong if called
;;   in init.el
;; - DONE Similar as above, load-history will record defuns etc. from init.el, but
;;   this is not where the actual source is.

;; A bit of a rough hack, however, the async native compiler does its work in a
;; pristine emacs process. This is good for packages, but not for our config
;; files. Thus, we patch this function so it loads our config environment
;; beforehand in case the file to be compiled is part of `zenit-core-dir'.

(cl-eval-when (compile)
  (require 'el-patch))

(el-patch-feature comp)
(after! comp

  ;; Add our generated "init.el" manually to the native-comp queue
  (pushnew! comp-files-queue `(,(file-name-concat user-emacs-directory "init.el") . late))

  (el-patch-defun comp-run-async-workers ()
    "Start compiling files from `comp-files-queue' asynchronously.
When compilation is finished, run `native-comp-async-all-done-hook' and
display a message."
    (cl-assert (null comp-no-spawn))
    (if (or comp-files-queue
            (> (comp-async-runnings) 0))
        (unless (>= (comp-async-runnings) (comp-effective-async-max-jobs))
          (cl-loop
           for (source-file . load) = (pop comp-files-queue)
           while source-file
           do (cl-assert (string-match-p comp-valid-source-re source-file) nil
                         "`comp-files-queue' should be \".el\" files: %s"
                         source-file)
           when (or native-comp-always-compile
                    load ; Always compile when the compilation is
                                        ; commanded for late load.
                    ;; Skip compilation if `comp-el-to-eln-filename' fails
                    ;; to find a writable directory.
                    (with-demoted-errors "Async compilation :%S"
                      (file-newer-than-file-p
                       source-file (comp-el-to-eln-filename source-file))))
           do (let* ((expr `((require 'comp)
                             (setq comp-async-compilation t
                                   warning-fill-column most-positive-fixnum)
                             ,(let ((set (list 'setq)))
                                (dolist (var '(comp-file-preloaded-p
                                               native-compile-target-directory
                                               native-comp-speed
                                               native-comp-debug
                                               native-comp-verbose
                                               comp-libgccjit-reproducer
                                               native-comp-eln-load-path
                                               native-comp-compiler-options
                                               native-comp-driver-options
                                               load-path
                                               backtrace-line-length
                                               byte-compile-warnings
                                               ;; package-load-list
                                               ;; package-user-dir
                                               ;; package-directory-list
                                               ))
                                  (when (boundp var)
                                    (push var set)
                                    (push `',(symbol-value var) set)))
                                (nreverse set))
                             ;; FIXME: Activating all packages would align the
                             ;; functionality offered with what is usually done
                             ;; for ELPA packages (and thus fix some compilation
                             ;; issues with some ELPA packages), but it's too
                             ;; blunt an instrument (e.g. we don't even know if
                             ;; we're compiling such an ELPA package at
                             ;; this point).
                             ;;(package-activate-all)
                             ,native-comp-async-env-modifier-form
                             (message "Compiling %s..." ,source-file)
                             (comp--native-compile ,source-file ,(and load t))))
                     (source-file1 source-file) ;; Make the closure works :/
                     (temp-file (make-temp-file
                                 (concat "emacs-async-comp-"
                                         (file-name-base source-file) "-")
                                 nil ".el"))
                     (expr-strings (let ((print-length nil)
                                         (print-level nil))
                                     (mapcar #'prin1-to-string expr)))
                     (_ (progn
                          (with-temp-file temp-file
                            (mapc #'insert expr-strings))
                          (comp-log "\n")
                          (mapc #'comp-log expr-strings)))
                     (load1 load)
                     (default-directory invocation-directory)
                     (process (make-process
                               :name (concat "Compiling: " source-file)
                               :buffer (with-current-buffer
                                           (get-buffer-create
                                            comp-async-buffer-name)
                                         (setf buffer-read-only t)
                                         (current-buffer))
                               :command (el-patch-swap
                                          (list
                                           (expand-file-name invocation-name
                                                             invocation-directory)
                                           "-no-comp-spawn" "-Q" "--batch"
                                           "--eval"
                                           ;; Suppress Abort dialogs on MS-Windows
                                           "(setq w32-disable-abort-dialog t)"
                                           "-l" temp-file)
                                          (delq nil `(
                                                      ,(expand-file-name invocation-name
                                                                         invocation-directory)
                                                      "-no-comp-spawn" "-Q" "--batch"
                                                      ,@(when (or (file-in-directory-p source-file (file-name-concat user-emacs-directory "lisp" "core/"))
                                                                  (file-in-directory-p source-file (file-name-concat user-emacs-directory "lisp" "modules/"))
                                                                  (equal source-file (file-name-concat user-emacs-directory "init.el")))
                                                          `("--eval" ,(concat "(message \"Zenit Emacs file %s detected! Loading compiler setup!\" \"" source-file "\")")
                                                            "--load" ,(file-name-concat user-emacs-directory "early-init.el")
                                                            "--eval" "(zenit-cli-compile-setup-env)"))
                                                      "--eval"
                                                      ;; Suppress Abort dialogs on MS-Windows
                                                      "(setq w32-disable-abort-dialog t)"
                                                      "-l" ,temp-file)))
                               :sentinel
                               (lambda (process _event)
                                 (run-hook-with-args
                                  'native-comp-async-cu-done-functions
                                  source-file)
                                 (comp-accept-and-process-async-output process)
                                 (ignore-errors (delete-file temp-file))
                                 (let ((eln-file (comp-el-to-eln-filename
                                                  source-file1)))
                                   (when (and load1
                                              (zerop (process-exit-status
                                                      process))
                                              (file-exists-p eln-file))
                                     (native-elisp-load eln-file
                                                        (eq load1 'late))))
                                 (comp-run-async-workers))
                               :noquery (not native-comp-async-query-on-exit))))
                (puthash source-file process comp-async-compilations))
           when (>= (comp-async-runnings) (comp-effective-async-max-jobs))
           do (cl-return)))
      ;; No files left to compile and all processes finished.
      (run-hooks 'native-comp-async-all-done-hook)
      (with-current-buffer (get-buffer-create comp-async-buffer-name)
        (save-excursion
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "Compilation finished.\n"))))
      ;; `comp-deferred-pending-h' should be empty at this stage.
      ;; Reset it anyway.
      (clrhash comp-deferred-pending-h))))

;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-lib-debug.el

(describe "core/lib/debug"

  (load! "lib/debug" zenit-core-dir)

  (describe "zenit/toggle-profiler"
    :var (zenit--profiler)

    (before-each
      (spy-on 'profiler-start)
      (spy-on 'profiler-report)
      (spy-on 'profiler-stop))

    (it "toggles Emacs profiler on"
      (zenit/toggle-profiler)
      (expect 'profiler-start :to-have-been-called-with 'cpu+mem))

    (it "toggles Emacs profiler off and reports"
      (setq zenit--profiler t)
      (zenit/toggle-profiler)
      (expect 'profiler-report :to-have-been-called)
      (expect 'profiler-stop :to-have-been-called)))


  (describe "zenit--watch-debug-vars-h"
    :var (zenit--debug-vars-undefined
          zenit-inhibit-log)

    (before-each
      (setq zenit--debug-vars-undefined nil
            zenit-inhibit-log nil))

    (after-each
      (makunbound 'a))

    (it "should log new variables when they become available"
      (spy-on 'zenit--log)
      (spy-on 'zenit-debug-mode)
      (setq zenit--debug-vars-undefined '(a b c))
      (defvar a t)
      (zenit--watch-debug-vars-h)
      (expect 'zenit--log :to-have-been-called-with "New variables available: %s" '(a))
      (expect 'zenit-debug-mode :to-have-been-called-with -1)
      (expect 'zenit-debug-mode :to-have-been-called-with +1))

    (it "should not log new variables if they are not bound"
      (spy-on 'zenit--log)
      (spy-on 'zenit-debug-mode)
      (setq zenit--debug-vars-undefined '(a b c))
      (zenit--watch-debug-vars-h)
      (expect 'zenit-log :not :to-have-been-called)
      (expect 'zenit-debug-mode :not :to-have-been-called)))


  (describe "zenit-debug-mode"
    :var (zenit--debug-vars-undefined)

    (before-each
      (setq zenit--debug-vars-undefined nil))

    (after-each
      (let ((inhibit-message t))
        (zenit-debug-mode -1)))

    (it "populates zenit--debug-vars-undefined on activation"
      (spy-on 'add-variable-watcher)
      (let ((inhibit-message t))
        (zenit-debug-mode +1))
      (expect zenit--debug-vars-undefined :not :to-be nil)
      (expect 'add-variable-watcher :to-have-been-called-with 'zenit-debug-variables #'zenit--watch-debug-vars-h))

    (it "removes zenit--watch-debug-vars-h from after-load-functions on deactivation"
      (spy-on 'remove-variable-watcher)
      (let ((inhibit-message t))
        (zenit-debug-mode +1)
        (zenit-debug-mode -1))
      (expect after-load-functions :not :to-contain #'zenit--watch-debug-vars-h)
      (expect 'remove-variable-watcher :to-have-been-called-with 'zenit-debug-variables #'zenit--watch-debug-vars-h)))


  (describe "zenit-run-all-startup-hooks-h"
    (it "should set after-init-time to the current time"
      (spy-on 'current-time :and-return-value '(0 0 0))
      (spy-on 'zenit-try-run-hook)
      (zenit-run-all-startup-hooks-h)
      (expect after-init-time :to-equal '(0 0 0)))

    (it "should run all startup hooks"
      (spy-on 'run-hook-wrapped)
      (zenit-run-all-startup-hooks-h)
      (expect 'run-hook-wrapped :to-have-been-called-with 'after-init-hook #'zenit-try-run-hook)
      (expect 'run-hook-wrapped :to-have-been-called-with 'delayed-warnings-hook #'zenit-try-run-hook)
      (expect 'run-hook-wrapped :to-have-been-called-with 'emacs-startup-hook #'zenit-try-run-hook)
      (expect 'run-hook-wrapped :to-have-been-called-with 'tty-setup-hook #'zenit-try-run-hook)
      (expect 'run-hook-wrapped :to-have-been-called-with 'window-setup-hook #'zenit-try-run-hook))))

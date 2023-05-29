;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-core-zenit-start.el

(describe "core/zenit-start"

  (load! "zenit-start" zenit-core-dir)

  (describe "zenit-run-local-var-hooks-h"
    :var ((local-test-var nil))

    (before-each
      (setq zenit-inhibit-local-var-hooks nil))

    (it "does not run the hook if zenit-inhibit-local-var-hooks is non-nil"
      (add-hook 'text-mode-local-vars-hook (lambda () (setq local-test-var t)))
      (setq zenit-inhibit-local-var-hooks t)
      (with-temp-buffer
        (text-mode)
        (zenit-run-local-var-hooks-h)
        (expect local-test-var :to-be nil)))

    (it "does not run the hook if delay-mode-hooks is non-nil"
      (add-hook 'text-mode-local-vars-hook (lambda () (setq local-test-var t)))
      (with-temp-buffer
        (let ((inhibit-message t)
              (delay-mode-hooks t))
          (text-mode)
          (zenit-run-local-var-hooks-h)
          (expect local-test-var :to-be nil))))

    (it "runs the hook if zenit-inhibit-local-var-hooks and delay-mode-hooks are nil"
      (add-hook 'text-mode-local-vars-hook (lambda () (setq local-test-var t)))
      (with-temp-buffer
        (text-mode)
        (zenit-run-local-var-hooks-h)
        (expect local-test-var :to-be t))))


  (describe "zenit-run-local-var-hooks-maybe-h"
    (it "runs zenit-run-local-var-hooks-h when enable-local-variables is nil"
      (spy-on 'zenit-run-local-var-hooks-h)
      (let ((enable-local-variables nil))
        (zenit-run-local-var-hooks-maybe-h)
        (expect 'zenit-run-local-var-hooks-h :to-have-been-called)))

    (it "doesn't run zenit-run-local-var-hooks-h when enable-local-variables is non-nil"
      (spy-on 'zenit-run-local-var-hooks-h)
      (let ((enable-local-variables t))
        (zenit-run-local-var-hooks-maybe-h)
        (expect 'zenit-run-local-var-hooks-h :not :to-have-been-called))))


  (describe "zenit-load-packages-incrementally"

    (it "appends packages to zenit-incremental-packages when now is nil"
      (let ((zenit-incremental-packages '(t))
            (packages '(package1 package2)))
        (zenit-load-packages-incrementally packages)
        (expect zenit-incremental-packages :to-equal '(t package1 package2))))

    (it "loads packages incrementally when now is non-nil"
      (spy-on 'run-at-time)
      (let ((packages '(package1 package2)))
        (zenit-load-packages-incrementally packages t)
        (expect 'run-at-time :to-have-been-called)
        (expect 'run-at-time :to-have-been-called-with
                zenit-incremental-first-idle-timer nil #'zenit-load-packages-incrementally packages t))))


  (describe "zenit-load-packages-incrementally-h"
    (it "loads packages incrementally after idle time"
      (let ((zenit-incremental-packages '(t package1 package2))
            (zenit-incremental-first-idle-timer 2.0))
        (spy-on 'run-with-idle-timer)
        (zenit-load-packages-incrementally-h)
        (expect 'run-with-idle-timer :to-have-been-called-with
                zenit-incremental-first-idle-timer
                nil #'zenit-load-packages-incrementally
                '(package1 package2) t)))

    (it "loads packages immediately when zenit-incremental-first-idle-timer is zero"
      (let ((zenit-incremental-packages '(t package1 package2))
            (zenit-incremental-first-idle-timer 0))
        (spy-on 'require :and-return-value t)
        (zenit-load-packages-incrementally-h)
        (expect 'require :to-have-been-called-times 2)
        (expect 'require :to-have-been-called-with 'package1)
        (expect 'require :to-have-been-called-with 'package2)))

    (it "does not load packages when zenit-incremental-first-idle-timer is not a number"
      (let ((zenit-incremental-packages '(t package1 package2))
            (zenit-incremental-first-idle-timer "not a number"))
        (spy-on 'require)
        (spy-on 'run-with-idle-timer)
        (zenit-load-packages-incrementally-h)
        (expect 'require :not :to-have-been-called)
        (expect 'run-with-idle-timer :not :to-have-been-called))))


  (describe "zenit-display-benchmark-h"
    (it "returns the benchmark message as a string when return-p is non-nil"
      (let ((load-path '(p1 p2 p3 p4))
            (zenit-modules (make-hash-table :test #'equal))
            (zenit-init-time 0.12345))
        (put 'load-path 'initial-value '(p1))
        (puthash 'module1 t zenit-modules)
        (puthash 'module2 t zenit-modules)
        (expect (zenit-display-benchmark-h t)
                :to-equal
                "Loaded 3 packages across 2 modules in 0.123s")))

    (it "displays the benchmark message when return-p is nil"
      (let ((load-path '(p1 p2 p3 p4))
            (zenit-modules (make-hash-table :test #'equal))
            (zenit-init-time 0.12345))
        (put 'load-path 'initial-value '(p1))
        (puthash 'module1 t zenit-modules)
        (puthash 'module2 t zenit-modules)
        (spy-on 'message)
        (zenit-display-benchmark-h)
        (expect 'message :to-have-been-called-with
                "Loaded %d packages across %d modules in %.03fs" 3 2 zenit-init-time))))


  (describe "zenit-init-local-var-hooks-h"
    (it "is defined"
      (expect (fboundp 'zenit-init-local-var-hooks-h) :to-be-truthy))

    (it "is attached to correct hook"
      (expect (member 'zenit-init-local-var-hooks-h after-init-hook) :to-be-truthy))))

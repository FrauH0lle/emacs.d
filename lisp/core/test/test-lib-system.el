;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-lib-system.el

(describe "core/lib/system"

  (load! "lib/system" zenit-core-dir)

  (describe "zenit-system-cpus returns the max number of processing units on this system"

    (after-each
      (put 'zenit-system-cpus 'cached-value nil))

    (it "for windows systems"
      (spy-on 'w32-get-nproc :and-return-value 2)
      (expect (zenit-system-cpus) :to-equal 2))

    (it "when NUMBER_OF_PROCESSORS environment varialbe is bound"
      (spy-on 'getenv :and-return-value "4")
      (expect (zenit-system-cpus) :to-equal 4))

    (it "when nproc is installed"
      (spy-on 'executable-find :and-return-value t)
      (spy-on 'zenit-call-process :and-return-value '(0 . "3"))
      (expect (zenit-system-cpus) :to-equal 3))

    (it "when sysctl is installed"
      (spy-on 'executable-find :and-return-value t)
      (spy-on 'zenit-call-process :and-return-value '(0 . "3"))
      (expect (zenit-system-cpus) :to-equal 3))

    (it "returns 1 if the number of processing units cannot be determined"
      (spy-on 'w32-get-nproc :and-return-value nil)
      (spy-on 'getenv :and-return-value nil)
      (spy-on 'executable-find :and-return-value nil)
      (expect (zenit-system-cpus) :to-equal 1))))

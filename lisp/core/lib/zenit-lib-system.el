;; lisp/core/lib/zenit-lib-system.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

;; `zenit-lib-process'
(declare-function zenit-call-process "zenit-lib-process")


;;;###autoload
(defun zenit-system-cpus ()
  "Return the max number of processing units on this system.
Tries to be portable. Returns 1 if cannot be determined."
  (or (get 'zenit-system-cpus 'cached-value)
      (put 'zenit-system-cpus 'cached-value
           (let ((cpus
                  (cond ((fboundp 'w32-get-nproc)
                         (w32-get-nproc))
                        ((getenv "NUMBER_OF_PROCESSORS"))
                        ((executable-find "nproc")
                         (zenit-call-process "nproc"))
                        ((executable-find "sysctl")
                         (zenit-call-process "sysctl" "-n" "hw.ncpu")))))
             (max
              1 (or (cl-typecase cpus
                      (integer cpus)
                      (string
                       (condition-case _
                           (string-to-number cpus)
                         (wrong-type-argument
                          (user-error "NUMBER_OF_PROCESSORS contains an invalid value: %S"
                                      cpus))))
                      (cons
                       (if (zerop (car cpus))
                           (string-to-number (cdr cpus))
                         (user-error "Failed to look up number of processors, because:\n\n%s"
                                     (cdr cpus)))))
                    1))))))

(provide 'zenit-lib '(system))

;; -*- no-byte-compile: t; -*-
;; tools/magit/packages.el

(when (package! magit :lockfile tools-magit)
  (package! compat :lockfile tools-magit)
  (package! magit-todos :lockfile tools-magit))

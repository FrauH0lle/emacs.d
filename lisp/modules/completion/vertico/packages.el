;; -*- no-byte-compile: t; -*-
;; completion/vertico/packages.el

(package! vertico :lockfile completion_vertico)

(package! orderless :lockfile completion_vertico)

(package! consult :lockfile completion_vertico)
(package! consult-dir :lockfile completion_vertico)
(when (modulep! :checkers syntax -flymake)
  (package! consult-flycheck :lockfile completion_vertico))

(package! embark :lockfile completion_vertico)
(package! embark-consult :lockfile completion_vertico)

(package! marginalia :lockfile completion_vertico)

(package! wgrep :lockfile completion_vertico)

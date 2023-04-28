;; -*- no-byte-compile: t; -*-
;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :lockfile completion-vertico)

(package! orderless :lockfile completion-vertico)

(package! consult :lockfile completion-vertico)
(package! compat :lockfile completion-vertico)
(package! consult-dir :lockfile completion-vertico)
(when (modulep! :checkers syntax)
  (package! consult-flycheck :lockfile completion-vertico))

(package! embark :lockfile completion-vertico)
(package! embark-consult :lockfile completion-vertico)

(package! marginalia :lockfile completion-vertico)

(package! wgrep :lockfile completion-vertico)

;; -*- no-byte-compile: t; -*-
;; tools/magit/packages.el

(when (package! magit :lockfile tools_magit)
  (package! compat :lockfile tools_magit)
  (package! magit-todos :lockfile tools_magit)
  (when (modulep! +forge)
    (package! forge :lockfile tools_magit_forge)
    (package! code-review
      :recipe (:host github
               :repo "doomelpa/code-review"
               :files ("graphql" "code-review*.el"))
      :lockfile tools_magit_forge)))

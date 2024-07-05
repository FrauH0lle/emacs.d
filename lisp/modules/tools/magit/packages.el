;; -*- no-byte-compile: t; -*-
;; tools/magit/packages.el

(when (package! magit :lockfile tools-magit)
  (package! compat :lockfile tools-magit)
  (package! magit-todos :lockfile tools-magit)
  (when (modulep! +forge)
    (package! forge :lockfile tools-magit-forge)
    (package! code-review
      :recipe (:host github
               :repo "doomelpa/code-review"
               :files ("graphql" "code-review*.el"))
      :lockfile tools-magit-forge)))

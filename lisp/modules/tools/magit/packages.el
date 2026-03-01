;; -*- no-byte-compile: t; -*-
;; tools/magit/packages.el

(package! transient :lockfile tools_magit)
(package! cond-let :lockfile tools_magit)

(package! magit :lockfile tools_magit)
(package! magit-todos :lockfile tools_magit)

(when (modulep! +forge)
  (package! forge :lockfile tools_magit)
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :lockfile tools_magit))

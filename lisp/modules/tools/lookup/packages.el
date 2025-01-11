;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; tools/lookup/packages.el

(package! dumb-jump :lockfile tools_lookup)

;; For dictionary and online lookup
(package! request :lockfile tools_lookup)

(when (modulep! +docsets)
  (package! dash-docs :lockfile tools_lookup_docsets))

(when (modulep! +dictionary)
  (if (featurep :system 'macos)
      (package! osx-dictionary :lockfile tools_lookup_dictionary)
    (package! define-word :lockfile tools_lookup_dictionary)
    ;; REVIEW: This fork fixes SavchenkoValeriy/emacs-powerthesaurus#40.
    (package! powerthesaurus
      :recipe (:host github
               :repo "doomelpa/powerthesaurus")
      :lockfile tools_lookup_dictionary)
    (when (modulep! +offline)
      (package! wordnut :lockfile tools_lookup_dictionary_offline)
      (package! synosaurus :lockfile tools_lookup_dictionary_offline))))

;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; os/gentoo/packages.el

(package! ebuild-mode
  :recipe (:host nil :type git
           :repo "https://gitweb.gentoo.org/proj/ebuild-mode.git")
  :lockfile os_gentoo)

(package! company-ebuild
  :recipe (:host nil :type git
           :repo "https://gitweb.gentoo.org/proj/company-ebuild.git"
           :pre-build
           ;; HACK 2024-08-31: Remove the dependency on `company' which is only
           ;;   used for a single function.
           (with-temp-file "company-ebuild.el"
             (insert-file-contents "company-ebuild.el")
             (save-excursion
               (goto-char (point-min))
               (when (search-forward "(require 'company)" nil t)
                 (beginning-of-line)
                 (insert ";; ")))))
  :lockfile os_gentoo)

(package! flycheck-pkgcheck
  :recipe (:host github
           :repo "pkgcore/pkgcheck"
           :files ("contrib/emacs/flycheck-pkgcheck.el"))
  :lockfile os_gentoo)

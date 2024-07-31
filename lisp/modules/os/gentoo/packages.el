;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; os/gentoo/packages.el

(package! ebuild-mode
  :recipe (:host nil :type git
           :repo "https://gitweb.gentoo.org/proj/ebuild-mode.git"))

(package! company-ebuild
  :recipe (:host nil :type git
           :repo "https://gitweb.gentoo.org/proj/company-ebuild.git"
           :pre-build
           (with-temp-file "company-ebuild.el"
             (insert-file-contents "company-ebuild.el")
             (save-excursion
               (goto-char (point-min))
               (when (search-forward "(require 'company)" nil t)
                 (beginning-of-line)
                 (insert ";; "))))))

;; tools/arch-linux/config.el -*- lexical-binding: t; -*-

(use-package! pacfiles-mode
  :commands pacfiles
  :config
  (static-when (modulep! :ui popup)
    (set-popup-rule! "^\\*pacfiles" :ignore t))
  (static-when (modulep! :editor evil)
    (after! evil
      (set-evil-initial-state! 'pacfiles-mode 'emacs))))

(use-package! pkgbuild-mode
  :mode ("/PKGBUILD\\'" . pkgbuild-mode)
  :config
  (map! :map pkgbuild-mode-map
        :localleader
        "r" 'pkgbuild-increase-release-tag
        "b" 'pkgbuild-makepkg
        "a" 'pkgbuild-tar
        "u" 'pkgbuild-browse-url
        "m" 'pkgbuild-update-sums-line
        "s" 'pkgbuild-update-srcinfo
        "e" 'pkgbuild-etags))

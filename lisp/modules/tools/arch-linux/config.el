;; tools/arch-linux/config.el -*- lexical-binding: t; -*-

(use-package! pacfiles-mode
  :commands pacfiles
  :config
  (eval-when! (modulep! :ui popup)
    (set-popup-rule! "^\\*pacfiles" :ignore t))
  (eval-when! (modulep! :editor evil)
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

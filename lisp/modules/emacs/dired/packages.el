;; -*- no-byte-compile: t; -*-
;; emacs/dired/packages.el

(package! diredfl :lockfile emacs-dired)
(package! dired-git-info :lockfile emacs-dired)
(package! dired-rsync :lockfile emacs-dired)
(when (modulep! +dirvish)
  (package! dirvish :lockfile emacs-dired))
(package! fd-dired :lockfile emacs-dired)

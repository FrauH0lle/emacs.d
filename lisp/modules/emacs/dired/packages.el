;; -*- no-byte-compile: t; -*-
;; emacs/dired/packages.el

(package! diredfl :lockfile emacs_dired)
(package! dired-git-info :lockfile emacs_dired)
(package! dired-rsync :lockfile emacs_dired)
(when (modulep! +dirvish)
  (package! dirvish :lockfile emacs_dired))
(package! fd-dired :lockfile emacs_dired)

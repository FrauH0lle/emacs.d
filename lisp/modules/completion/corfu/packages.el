;; -*- no-byte-compile: t; -*-
;; completion/corfu/packages.el

(package! corfu :lockfile completion-corfu)
(package! cape :lockfile completion-corfu)
(when (modulep! +icons)
  (package! nerd-icons-corfu :lockfile completion-corfu))
(when (and (not (modulep! :completion vertico))
           (modulep! +orderless))
  (package! orderless :lockfile completion-corfu))
(when (modulep! :os tty)
  (package! corfu-terminal :lockfile completion-corfu))

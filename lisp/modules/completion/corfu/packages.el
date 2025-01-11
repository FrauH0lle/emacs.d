;; -*- no-byte-compile: t; -*-
;; completion/corfu/packages.el

(package! corfu :lockfile completion_corfu)
(package! cape :lockfile completion_corfu)
(when (modulep! +icons)
  (package! nerd-icons-corfu :lockfile completion_corfu))
(when (and (not (modulep! :completion vertico))
           (modulep! +orderless))
  (package! orderless :lockfile completion_corfu))
(when (modulep! :os tty)
  (package! corfu-terminal :lockfile completion_corfu))

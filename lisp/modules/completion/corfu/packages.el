;; -*- no-byte-compile: t; -*-
;; completion/corfu/packages.el

(package! corfu :recipe (:files ("*.el" "extensions/*.el")) :lockfile completion-corfu)
(package! cape :lockfile completion-corfu)
(when (modulep! +icons)
  (package! nerd-icons-completion :lockfile completion-corfu)
  (package! nerd-icons-corfu :lockfile completion-corfu))
(when (modulep! +orderless)
  (package! orderless :lockfile completion-corfu))
(when (modulep! :os tty)
  (package! corfu-terminal :lockfile completion-corfu))
(when (modulep! :editor snippets)
  (package! yasnippet-capf :lockfile completion-corfu))

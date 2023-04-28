;; -*- no-byte-compile: t; -*-
;; config/default/packages.el

(package! avy :lockfile config-default)
(package! drag-stuff :lockfile config-default)
(package! link-hint :lockfile config-default)

(unless (modulep! :editor evil)
  (package! expand-region :lockfile config-default))

;; -*- no-byte-compile: t; -*-
;; config/default/packages.el

(package! avy :lockfile config_default)
(package! link-hint :lockfile config_default)

(unless (modulep! :editor evil)
  (package! expand-region :lockfile config_default))

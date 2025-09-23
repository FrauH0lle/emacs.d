;; ui/doom-theming/config.el -*- lexical-binding: t; -*-

;;;###package pos-tip
(setq pos-tip-internal-border-width 6
      pos-tip-border-width 1)


(use-package! doom-themes
  ;; improve integration w/ org-mode
  :hook (zenit-load-theme . doom-themes-org-config)
  :init
  (setq zenit-theme 'doom-one)
  ;; more Atom-esque file icons for neotree/treemacs
  (static-when (modulep! :ui neotree)
    (add-hook 'zenit-load-theme-hook #'doom-themes-neotree-config)
    (setq doom-themes-neotree-enable-variable-pitch t
          doom-themes-neotree-file-icons 'simple
          doom-themes-neotree-line-spacing 2))
  (static-when (modulep! :ui treemacs)
    (add-hook 'zenit-load-theme-hook #'doom-themes-treemacs-config)))


(use-package! solaire-mode
  :hook (zenit-load-theme . solaire-global-mode)
  :hook (+popup-buffer-mode . turn-on-solaire-mode))

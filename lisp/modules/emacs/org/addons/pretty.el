;; lang/org/contrib/pretty.el -*- lexical-binding: t; -*-
;;;###if (modulep! +pretty)

(after! org
  (setq org-highlight-latex-and-related '(native script entities)))


(use-package! org-appear ; better markup edit
  :hook (org-mode . org-appear-mode))


(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :hook (org-agenda-finalize . org-modern-mode)
  :hook (org-modern-mode . +org-pretty-mode)
  :config
  (setq org-modern-star 'replace))

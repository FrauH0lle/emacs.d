;; -*- no-byte-compile: t; -*-
;; emacs/org/packages.el

(package! org
  :recipe (:host github
           ;; REVIEW I intentionally avoid git.savannah.gnu.org because of SSL
           ;;   issues (see #5655), uptime issues, download time, and lack of
           ;;   shallow clone support.
           :repo "emacs-straight/org-mode")
  :lockfile 'pinned
  :pin "630f86dfc42472aafd9a4f305e1965cbe92b2891")
(package! org-contrib
  :recipe (:host github
           :repo "emacsmirror/org-contrib")
  :lockfile 'pinned
  :pin "d0cebebb301b5de93e9c5228a91e3e4f5d41902b")

(package! avy :lockfile emacs-org)
(package! htmlize :lockfile emacs-org)

(package! avy :lockfile emacs-org)
(package! htmlize :lockfile emacs-org)
(package! org-superstar :lockfile emacs-org)
(package! ox-clip :lockfile emacs-org)
(package! toc-org :lockfile emacs-org)
(when (modulep! :editor evil)
  (package! evil-org
    :recipe (:host github :repo "hlissner/evil-org-mode")
    :lockfile emacs-org-evil))

;; babel
(package! ob-async :lockfile emacs-org)

;; dragndrop
(when (modulep! :org org +dragndrop)
  (package! org-download :lockfile emacs-org-dragndrop))
;; journal
(when (modulep! :org org +journal)
  (package! org-journal :lockfile emacs-org-journal))
;; jupyter
(when (modulep! :org org +jupyter)
  (package! jupyter :lockfile emacs-org-jupyter))
;; pomodoro
(when (modulep! :org org +pomodoro)
  (package! org-pomodoro :lockfile emacs-org-pomodoro))
;; present
(when (modulep! :org org +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode")
    :lockfile emacs-org-present)
  (package! org-tree-slide :lockfile emacs-org-present)
  (package! ox-reveal :lockfile emacs-org-present))
;; tufte
(when (modulep! :org org +tufte)
  (package! ox-gfm :lockfile emacs-org-tufte)
  (package! ox-tufte-latex
    :recipe (:host github :repo "tsdye/tufte-org-mode"
             :fork (:host github :repo "DonHugo69/tufte-org-mode"))
    :lockfile emacs-org-tufte))

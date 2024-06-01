;; -*- no-byte-compile: t; -*-
;; emacs/org/packages.el

(package! org
  :recipe (:host github
           ;; REVIEW I intentionally avoid git.savannah.gnu.org because of SSL
           ;;   issues (see #5655), uptime issues, download time, and lack of
           ;;   shallow clone support.
           :repo "emacs-straight/org-mode")
  :lockfile 'pinned
  :pin "ab730145ba67596151f60cdc30a7963520832d7d")
(package! org-contrib
  :recipe (:host github
           :repo "emacsmirror/org-contrib")
  :lockfile 'pinned
  :pin "351c71397d893d896a47ad7e280607b4d59b84e4")

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

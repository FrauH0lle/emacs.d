;; -*- no-byte-compile: t; -*-
;; emacs/org/packages.el

(package! org
  :recipe (:host github
           ;; The mirror is a bit faster.
           :repo "emacs-straight/org-mode")
  :lockfile 'pinned
  :lockfile emacs-org)
(package! org-contrib
  :recipe (:host github
           :repo "emacsmirror/org-contrib")
  :lockfile 'pinned
  :lockfile emacs-org)

(package! avy :lockfile emacs-org)
(package! htmlize :lockfile emacs-org)
(package! ox-clip :lockfile emacs-org)
(package! toc-org :lockfile emacs-org)
(package! org-cliplink :lockfile emacs-org)

(when (modulep! :editor evil)
  (package! evil-org
    :recipe (:host github :repo "hlissner/evil-org-mode")
    :lockfile emacs-org-evil))

(when (modulep! :tools pdf)
  (package! org-pdftools :lockfile emacs-org))
(when (modulep! :tools magit)
  (package! orgit :lockfile emacs-org)
  (when (modulep! :tools magit +forge)
    (package! orgit-forge :lockfile emacs-org)))

(when (modulep! +dragndrop)
  (package! org-download :lockfile emacs-org-dragndrop))

(when (modulep! +gnuplot)
  (package! gnuplot :lockfile emacs-org-gnuplot)
  (package! gnuplot-mode :lockfile emacs-org-gnuplot))

(when (modulep! +jupyter)
  (package! jupyter :lockfile emacs-org-jupyter))

(when (modulep! +pomodoro)
  (package! org-pomodoro :lockfile emacs-org-pomodoro))

(when (modulep! +pretty)
  (package! org-appear :lockfile emacs-org-pretty)
  (package! org-superstar :lockfile emacs-org-pretty)
  (package! org-fancy-priorities :lockfile emacs-org-pretty))

(when (modulep! +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode")
    :lockfile emacs-org-present)
  (package! org-tree-slide :lockfile emacs-org-present)
  (package! org-re-reveal :lockfile emacs-org-present)
  (package! revealjs
    :recipe (:host github :repo "hakimel/reveal.js"
             :files ("css" "dist" "js" "plugin"))
    :lockfile emacs-org-present))


;;
;;; Babel
(package! ob-async :lockfile emacs-org)


;;
;;; Export
(when (modulep! +pandoc)
  (package! ox-pandoc :lockfile emacs-org-pandoc))

(when (modulep! +hugo)
  (package! ox-hugo
    :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)
    :lockfile emacs-org))
(when (modulep! :lang rst)
  (package! ox-rst :lockfile emacs-org-hugo))

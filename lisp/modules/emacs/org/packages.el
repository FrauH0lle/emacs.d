;; -*- no-byte-compile: t; -*-
;; emacs/org/packages.el

(package! org
  :recipe (:host github
           ;; The mirror is a bit faster.
           :repo "emacs-straight/org-mode"
           :depth 1)
  :lockfile emacs_org)

(package! org-contrib
  :recipe (:host github
           :repo "emacsmirror/org-contrib")
  :lockfile emacs_org)

(package! avy :lockfile emacs_org)
(package! htmlize :lockfile emacs_org)
(package! ox-clip :lockfile emacs_org)
(package! toc-org :lockfile emacs_org)
(package! org-cliplink :lockfile emacs_org)

(when (modulep! :editor evil)
  (package! evil-org
    :recipe (:host github :repo "hlissner/evil-org-mode")
    :lockfile emacs_org))

(when (modulep! :tools pdf)
  (package! org-pdftools :lockfile emacs_org))
(when (modulep! :tools magit)
  (package! orgit :lockfile emacs_org)
  (when (modulep! :tools magit +forge)
    (package! orgit-forge :lockfile emacs_org)))

(when (modulep! +dragndrop)
  (package! org-download :lockfile emacs_org))

(when (modulep! +gnuplot)
  (package! gnuplot :lockfile emacs_org)
  (package! gnuplot-mode :lockfile emacs_org))

(when (modulep! +jupyter)
  (package! jupyter :lockfile emacs_org))

(when (modulep! +pomodoro)
  (package! org-pomodoro :lockfile emacs_org))

(when (modulep! +pretty)
  (package! org-appear :lockfile emacs_org)
  (package! org-modern :lockfile emacs_org))

(when (modulep! +present)
  (package! centered-window
    :recipe (:host github :repo "emacsmirror/centered-window")
    :lockfile emacs_org)
  (package! org-tree-slide :lockfile emacs_org)
  (package! org-re-reveal :lockfile emacs_org)
  (package! revealjs
    :recipe (:host github :repo "hakimel/reveal.js"
             :files ("css" "dist" "js" "plugin"))
    :lockfile emacs_org))


;;
;;; Babel
(package! ob-async :lockfile emacs_org)


;;
;;; Export
(when (modulep! +pandoc)
  (package! ox-pandoc :lockfile emacs_org))

(when (modulep! +hugo)
  (package! ox-hugo
    :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)
    :lockfile emacs_org))
(when (modulep! :lang rst)
  (package! ox-rst :lockfile emacs_org))

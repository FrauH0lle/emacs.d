;; lang/org/config.el -*- lexical-binding: t; -*-

(defvar +org-babel-native-async-langs '(python)
  "Languages that will use `ob-comint' instead of `ob-async' for
 `:async'.")

(defvar +org-babel-mode-alist
  '((c . C)
    (cpp . C)
    (C++ . C)
    (D . C)
    (elisp . emacs-lisp)
    (sh . shell)
    (bash . shell)
    (matlab . octave)
    (rust . rustic-babel)
    (amm . ammonite))
  "An alist mapping languages to babel libraries. This is
 necessary for babel libraries (ob-*.el) that don't match the
 name of the language.

For example, (fish . shell) will cause #+begin_src fish blocks to
load ob-shell.el when executed.")

(defvar +org-babel-load-functions ()
  "A list of functions executed to load the current executing src
block. They take one argument (the language specified in the src
block, as a string). Stops at the first function to return
non-nil.")

(defvar +org-capture-todo-file "todo.org"
  "Default target for todo entries.

Is relative to `org-directory', unless it is absolute. Is used in
default `org-capture-templates'.")

(defvar +org-capture-changelog-file "changelog.org"
  "Default target for changelog entries.

Is relative to `org-directory' unless it is absolute. Is used in
default `org-capture-templates'.")

(defvar +org-capture-notes-file "notes.org"
  "Default target for storing notes.

Used as a fall back file for org-capture.el, for templates that
do not specify a target file.

Is relative to `org-directory', unless it is absolute. Is used in
default `org-capture-templates'.")

(defvar +org-capture-journal-file "journal.org"
  "Default target for storing timestamped journal entries.

Is relative to `org-directory', unless it is absolute. Is used in
default `org-capture-templates'.")

(defvar +org-capture-projects-file "projects.org"
  "Default, centralized target for org-capture templates.")

(defvar +org-habit-graph-padding 2
  "The padding added to the end of the consistency graph")

(defvar +org-habit-min-width 30
  "Hides the consistency graph if the `org-habit-graph-column' is
less than this value")

(defvar +org-habit-graph-window-ratio 0.3
  "The ratio of the consistency graphs relative to the window width")

(defvar +org-startup-with-animated-gifs nil
  "If non-nil, and the cursor is over a gif inline-image preview,
animate it!")


;;
;;; `org-load' hooks
(compile-along! "org-load-hooks")
(autoload! "org-load-hooks"
  #'+org-init-org-directory-h
  #'+org-init-appearance-h
  ;; #'+org-init-agenda-h
  #'+org-init-attachments-h
  #'+org-init-babel-h
  #'+org-init-babel-lazy-loader-h
  #'+org-init-capture-defaults-h
  #'+org-init-capture-frame-h
  #'+org-init-custom-links-h
  #'+org-init-export-h
  ;; #'+org-init-habit-h
  #'+org-init-hacks-h
  #'+org-init-keybinds-h
  #'+org-init-popup-rules-h
  #'+org-init-smartparens-h)


;;
;;; Packages

(compile-along! "org-config")
(autoload! "org-config" #'+zenit-org--init-config)

(use-package! org
  :defer-incrementally
  +zenit-org--init-config calendar find-func format-spec org-macs org-compat org-faces org-entities
  org-list org-pcomplete org-src org-footnote org-macro ob org org-agenda
  org-capture
  :preface
  (add-hook! 'org-load-hook
             #'+org-init-org-directory-h
             #'+org-init-appearance-h
             ;; #'+org-init-agenda-h
             #'+org-init-attachments-h
             #'+org-init-babel-h
             #'+org-init-babel-lazy-loader-h
             #'+org-init-capture-defaults-h
             #'+org-init-capture-frame-h
             #'+org-init-custom-links-h
             #'+org-init-export-h
             ;; #'+org-init-habit-h
             #'+org-init-hacks-h
             #'+org-init-keybinds-h
             #'+org-init-popup-rules-h
             #'+org-init-smartparens-h))

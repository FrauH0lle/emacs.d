;; lisp/core/lib/zenit-lib-help.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))


;;;###autoload
(defvar zenit-docs-dir (file-name-concat zenit-emacs-dir "docs/")
  "Where Doom's documentation files are stored. Must end with a slash.")

(defvar zenit--help-major-mode-module-alist
  '((dockerfile-mode :tools docker)
    (agda2-mode      :lang agda)
    (c-mode          :lang cc)
    (c++-mode        :lang cc)
    (objc++-mode     :lang cc)
    (crystal-mode    :lang crystal)
    (lisp-mode       :lang common-lisp)
    (csharp-mode     :lang csharp)
    (clojure-mode    :lang clojure)
    (clojurescript-mode :lang clojure)
    (json-mode       :lang json)
    (yaml-mode       :lang yaml)
    (csv-mode        :lang data)
    (erlang-mode     :lang erlang)
    (elixir-mode     :lang elixir)
    (elm-mode        :lang elm)
    (emacs-lisp-mode :lang emacs-lisp)
    (ess-r-mode      :lang ess)
    (ess-julia-mode  :lang ess)
    (go-mode         :lang go)
    (haskell-mode    :lang haskell)
    (hy-mode         :lang hy)
    (idris-mode      :lang idris)
    (java-mode       :lang java)
    (js2-mode        :lang javascript)
    (rjsx-mode       :lang javascript)
    (typescript-mode :lang javascript)
    (typescript-tsx-mode :lang javascript)
    (coffee-mode     :lang javascript)
    (julia-mode      :lang julia)
    (kotlin-mode     :lang kotlin)
    (latex-mode      :lang latex)
    (LaTeX-mode      :lang latex)
    (ledger-mode     :lang ledger)
    (lua-mode        :lang lua)
    (moonscript-mode :lang lua)
    (markdown-mode   :lang markdown)
    (gfm-mode        :lang markdown)
    (nim-mode        :lang nim)
    (nix-mode        :lang nix)
    (tuareg-mode     :lang ocaml)
    (org-mode        :emacs org)
    (raku-mode       :lang raku)
    (php-mode        :lang php)
    (hack-mode       :lang php)
    (plantuml-mode   :lang plantuml)
    (purescript-mode :lang purescript)
    (python-mode     :lang python)
    (restclient-mode :lang rest)
    (ruby-mode       :lang ruby)
    (rust-mode       :lang rust)
    (rustic-mode     :lang rust)
    (scala-mode      :lang scala)
    (scheme-mode     :lang scheme)
    (sh-mode         :lang sh)
    (swift-mode      :lang swift)
    (web-mode        :lang web)
    (css-mode        :lang web)
    (scss-mode       :lang web)
    (sass-mode       :lang web)
    (less-css-mode   :lang web)
    (stylus-mode     :lang web)
    (terra-mode      :lang terra))
  "An alist mapping major modes to Emacs modules.

This is used by `zenit/help-modules' to auto-select the module
corresponding to the current major-modea.")


;;
;;; Helpers

;;;###autoload
(defun zenit-active-minor-modes ()
  "Return a list of active minor-mode symbols."
  (cl-loop for mode in minor-mode-list
           if (and (boundp mode) (symbol-value mode))
           collect mode))


;;
;;; Custom describe commands

;;;###autoload (defalias 'zenit/describe-autodefs #'zenit/help-autodefs)
;;;###autoload (defalias 'zenit/describe-module   #'zenit/help-modules)
;;;###autoload (defalias 'zenit/describe-package  #'zenit/help-packages)

;;;###autoload
(defun zenit/describe-active-minor-mode (mode)
  "Get information on an active minor mode.

 Use `describe-minor-mode' for a selection of all minor-modes,
active or not."
  (interactive
   (list (completing-read "Describe active mode: " (zenit-active-minor-modes))))
  (let ((symbol
         (cond ((stringp mode) (intern mode))
               ((symbolp mode) mode)
               ((error "Expected a symbol/string, got a %s" (type-of mode)))))
        (fn (if (fboundp symbol) #'describe-function #'describe-variable)))
    (funcall (or (command-remapping fn) fn)
             symbol)))


;;
;;; Documentation commands

(defvar org-agenda-files)
(defvar org-inhibit-startup)
(cl-defun zenit--org-headings (files &key depth mindepth include-files &allow-other-keys)
  "TODO"
  (require 'org)
  (let* ((default-directory zenit-docs-dir)
         (org-agenda-files (mapcar #'expand-file-name (ensure-list files)))
         (depth (if (integerp depth) depth))
         (mindepth (if (integerp mindepth) mindepth))
         (org-inhibit-startup t))
    (message "Loading search results...")
    (unwind-protect
        (delq
         nil
         (org-map-entries
          (lambda ()
            (cl-destructuring-bind (level _reduced-level _todo _priority text tags)
                (org-heading-components)
              (when (and (or (null depth)
                             (<= level depth))
                         (or (null mindepth)
                             (>= level mindepth))
                         (or (null tags)
                             (not (string-match-p ":TOC" tags))))
                (let ((path  (org-get-outline-path))
                      (title (org-collect-keywords '("TITLE") '("TITLE"))))
                  (list (string-join
                         (list (string-join
                                (append (when include-files
                                          (list (or (cdr (assoc "TITLE" title))
                                                    (file-relative-name (buffer-file-name)))))
                                        path
                                        (when text
                                          (list (replace-regexp-in-string org-link-any-re "\\4" text))))
                                " > ")
                               tags)
                         " ")
                        (buffer-file-name)
                        (point))))))
          t 'agenda))
      (mapc #'kill-buffer org-agenda-new-buffers)
      (setq org-agenda-new-buffers nil))))

;;;###autoload
(cl-defun zenit-completing-read-org-headings
    (prompt files &rest plist &key depth mindepth include-files initial-input extra-candidates action)
  "TODO"
  (let ((alist
         (append (apply #'zenit--org-headings files plist)
                 extra-candidates)))
    (if-let (result (completing-read prompt alist nil nil initial-input))
        (cl-destructuring-bind (file &optional location)
            (cdr (assoc result alist))
          (if action
              (funcall action file location)
            (find-file file)
            (cond ((functionp location)
                   (funcall location))
                  (location
                   (goto-char location)))
            (ignore-errors
              (when (memq (get-char-property (point) 'invisible)
                          '(outline org-fold-outline))
                (save-excursion
                  (outline-previous-visible-heading 1)
                  (org-show-subtree))))))
      (user-error "Aborted"))))

;;;###autoload
(defun zenit/homepage ()
  "Open the Zenit Emacs homepage in the browser."
  (interactive)
  (browse-url "https://github.com/FrauH0lle/emacs.d"))

;;;###autoload
(defun zenit/issue-tracker ()
  "Open Zenit Emacs' global issue tracker."
  (interactive)
  (browse-url "https://github.com/FrauH0lle/emacs.d/issues"))

;;;###autoload
(defun zenit/report-bug ()
  "Open the browser on the issue tracker."
  (interactive)
  (browse-url "https://github.com/FrauH0lle/emacs.d/issues"))

;;;###autoload
(defun zenit/help-search-headings (&optional initial-input)
  "Search Zenit's documentation and jump to a headline."
  (interactive)
  (zenit-completing-read-org-headings
   "Find in Zenit help: "
   (list "conventions.org")
   :depth 3
   :include-files t
   :initial-input initial-input
   :extra-candidates
   (mapcar (lambda (x)
             (setcar x (concat "Zenit Modules > " (car x)))
             x)
           (zenit--help-modules-list))))

;;;###autoload
(defun zenit/help-search (&optional initial-input)
  "Perform a text search on all of Zenit's documentation."
  (interactive)
  (funcall (cond ((fboundp '+vertico-file-search)
                  #'+vertico-file-search)
                 ((rgrep
                   (read-regexp
                    "Search for" (or initial-input 'grep-tag-default)
                    'grep-regexp-history)
                   "*.org" zenit-emacs-dir)
                  #'ignore))
           :query initial-input
           :args '("-t" "org")
           :in zenit-emacs-dir
           :prompt "Search documentation for: "))

;;;###autoload
(defun zenit/help-autodefs (autodef)
  "Open documentation for an autodef.

An autodef is a function or macro that is always defined, whether
or not its containing module is disabled (in which case it will
safely no-op without evaluating its arguments). This syntactic
sugar lets you use them without needing to check if they are
available."
  (interactive
   (let* ((settings
           (cl-loop with case-fold-search = nil
                    for sym being the symbols of obarray
                    for sym-name = (symbol-name sym)
                    if (and (or (functionp sym)
                                (macrop sym))
                            (string-match-p "[a-z]!$" sym-name))
                    collect sym))
          (sym (symbol-at-point))
          (autodef
           (completing-read
            "Describe setter: "
            ;; TODO Could be cleaner (refactor me!)
            (cl-loop with maxwidth = (apply #'max (mapcar #'length (mapcar #'symbol-name settings)))
                     for def in (sort settings #'string-lessp)
                     if (get def 'zenit-module)
                     collect
                     (format (format "%%-%ds%%s" (+ maxwidth 4))
                             def (propertize (format "%s %s" (car it) (cdr it))
                                             'face 'font-lock-comment-face))
                     else if (and (string-match-p "^set-.+!$" (symbol-name def))
                                  (symbol-file def)
                                  (file-in-directory-p (symbol-file def) zenit-core-dir))
                     collect
                     (format (format "%%-%ds%%s" (+ maxwidth 4))
                             def (propertize (format "lisp/%s.el" (file-name-sans-extension (file-relative-name (symbol-file def) zenit-core-dir)))
                                             'face 'font-lock-comment-face)))
            nil t
            (when (and (symbolp sym)
                       (string-match-p "!$" (symbol-name sym)))
              (symbol-name sym)))))
     (list (and autodef (car (split-string autodef " "))))))
  (or (stringp autodef)
      (functionp autodef)
      (signal 'wrong-type-argument (list '(stringp functionp) autodef)))
  (let ((fn (if (functionp autodef)
                autodef
              (intern-soft autodef))))
    (or (fboundp fn)
        (error "'%s' is not a valid autodef" autodef))
    (if (fboundp 'helpful-callable)
        (helpful-callable fn)
      (describe-function fn))))

(defun zenit--help-modules-list ()
  (cl-loop for (cat . mod) in (zenit-module-list 'all)
           for readme-path = (or (zenit-module-locate-path cat mod "README.org")
                                 (zenit-module-locate-path cat mod))
           for format = (if mod (format "%s %s" cat mod) (format "%s" cat))
           if (zenit-module-p cat mod)
           collect (list format readme-path)
           else if (and cat mod)
           collect (list (propertize format 'face 'font-lock-comment-face)
                         readme-path)))

(defun zenit--help-current-module-str ()
  (cond ((save-excursion
           (ignore-errors
             (thing-at-point--beginning-of-sexp)
             (unless (eq (char-after) ?\()
               (backward-char))
             (let ((sexp (sexp-at-point)))
               ;; DEPRECATED `featurep!'
               (when (memq (car-safe sexp) '(modulep!))
                 (format "%s %s" (nth 1 sexp) (nth 2 sexp)))))))
        ((when buffer-file-name
           (when-let (mod (zenit-module-from-path buffer-file-name))
             (unless (memq (car mod) '(:core :local-conf))
               (format "%s %s" (car mod) (cdr mod))))))
        ((when-let (mod (cdr (assq major-mode zenit--help-major-mode-module-alist)))
           (format "%s %s"
                   (symbol-name (car mod))
                   (symbol-name (cadr mod)))))))

;;;###autoload
(defun zenit/help-modules (category module &optional visit-dir)
  "Open the documentation for a Zenit module.

CATEGORY is a keyword and MODULE is a symbol. e.g. :editor and
\\='evil.

If VISIT-DIR is non-nil, visit the module's directory rather than
its documentation.

Automatically selects a) the module at point (in private init
files), b) the module derived from a `modulep!' call, c) the
module that the current file is in, or d) the module associated
with the current major mode (see
`zenit--help-major-mode-module-alist')."
  (interactive
   (nconc
    (mapcar #'intern
            (split-string
             (completing-read "Describe module: "
                              (zenit--help-modules-list)
                              nil t nil nil
                              (zenit--help-current-module-str))
             " " t))
    (list current-prefix-arg)))
  (cl-check-type category symbol)
  (cl-check-type module symbol)
  (cl-destructuring-bind (module-string path)
      (or (assoc (format "%s %s" category module) (zenit--help-modules-list))
          (and (memq category '(:core :local-conf))
               (assoc (format "%s" category) (zenit--help-modules-list)))
          (user-error "'%s %s' is not a valid module" category module))
    (setq module-string (substring-no-properties module-string))
    (unless (file-readable-p path)
      (error "Can't find or read %S module at %S" module-string path))
    (cond ((not (file-directory-p path))
           (if visit-dir
               (zenit-project-browse (file-name-directory path))
             (find-file path)))
          (visit-dir
           (zenit-project-browse path))
          ((y-or-n-p (format "The %S module has no README file. Explore its directory?"
                             module-string))
           (zenit-project-browse (file-name-as-directory path)))
          ((user-error "Aborted module lookup")))))

(defun zenit--help-variable-p (sym)
  "TODO"
  (or (get sym 'variable-documentation)
      (and (boundp sym)
           (not (keywordp sym))
           (not (memq sym '(t nil))))))

;;;###autoload
(defun zenit/help-custom-variable (var)
  "Look up documentation for a custom variable.

Unlike `describe-variable' or `helpful-variable', which casts a
wider net that includes internal variables, this only lists
variables that exist to be customized (defined with `defcustom')."
  (interactive
   (list
    (intern (completing-read
             "Custom variable: " obarray
             (lambda (sym)
               (and (zenit--help-variable-p sym)
                    (custom-variable-p sym)
                    ;; Exclude minor mode state variables, which aren't meant to
                    ;; be modified directly, but through their associated
                    ;; function.
                    (not (or (and (string-suffix-p "-mode" (symbol-name sym))
                                  (fboundp sym))
                             (eq (get sym 'custom-set) 'custom-set-minor-mode)))))
             t nil nil (let ((var (variable-at-point)))
                         ;; `variable-at-point' uses 0 rather than nil to
                         ;; signify no symbol at point (presumably because 'nil
                         ;; is a symbol).
                         (unless (symbolp var)
                           (setq var nil))
                         (when (zenit--help-variable-p var)
                           var))))))
  (funcall (or (command-remapping #'describe-variable)
               #'describe-variable)
           var))


;;
;;; `zenit/help-packages'

(defun zenit--help-insert-button (label &optional uri line)
  "Helper function to insert a button at point.

The button will have the text LABEL. If URI is given, the button
will open it, otherwise the LABEL will be used. If the uri to
open is a url it will be opened in a browser. If LINE is
given (and the uri to open is not a url), then the file will open
with point on that line."
  (let ((uri (or uri label)))
    (insert-text-button
     label
     'face 'link
     'follow-link t
     'action
     (if (string-match-p "^https?://" uri)
         (lambda (_) (browse-url uri))
       (unless (file-exists-p uri)
         (error "Path does not exist: %S" uri))
       (lambda (_)
         (when (window-dedicated-p)
           (other-window 1))
         (find-file uri)
         (when line
           (goto-char (point-min))
           (forward-line (1- line))
           (recenter)))))))

(defun zenit--help-package-configs (package)
  (let ((default-directory zenit-emacs-dir))
    (split-string
     (cdr (zenit-call-process
           zenit-ripgrep-executable
           "--no-heading" "--line-number" "--iglob" "!*.org"
           (format "%s %s($| )"
                   "(^;;;###package|\\(after!|\\(use-package!)"
                   package)))
     "\n" t)))

(defvar zenit--help-packages-list nil)
;;;###autoload
(defun zenit/help-packages (package)
  "Like `describe-package', but for packages installed by modules.

Only shows installed packages. Includes information about where
packages are defined and configured.

If prefix arg is present, refresh the cache."
  (interactive
   (let ((guess (or (function-called-at-point)
                    (symbol-at-point))))
     (require 'finder-inf nil t)
     (require 'package)
     (require 'straight)
     (let ((packages
            (if (and zenit--help-packages-list (null current-prefix-arg))
                zenit--help-packages-list
              (message "Generating packages list for the first time...")
              (redisplay)
              (setq zenit--help-packages-list
                    (delete-dups
                     (append (mapcar #'car package-alist)
                             (mapcar #'car package--builtins)
                             (mapcar #'intern
                                     (hash-table-keys straight--build-cache))
                             (mapcar #'car (zenit-package-list 'all))
                             nil))))))
       (unless (memq guess packages)
         (setq guess nil))
       (list
        (intern
         (completing-read (format "Describe Zenit package (%s): "
                                  (concat (when guess
                                            (format "default '%s', " guess))
                                          (format "total %d" (length packages))))
                          packages nil t nil nil
                          (when guess (symbol-name guess))))))))
  (require 'zenit-packages)
  (zenit-initialize-packages)
  (help-setup-xref (list #'zenit/help-packages package)
                   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (when (or (package-desc-p package)
                (and (symbolp package)
                     (or (assq package package-alist)
                         (assq package package--builtins))))
        (describe-package-1 package))
      (let ((indent (make-string 13 ? )))
        (goto-char (point-min))
        (if (re-search-forward "     Status: .*$" nil t)
            (insert "\n")
          (search-forward "\n\n" nil t))

        (package--print-help-section "Package")
        (insert (symbol-name package) "\n")
        (format "%s %s" 1 nil)
        (package--print-help-section "Source")
        (pcase (zenit-package-backend package)
          (`straight
           (insert "Straight\n")
           (package--print-help-section "Pinned")
           (if-let ((lockfiles (delq nil (mapcar #'straight--versions-lockfile (zenit-package-get package :lockfile)))))
               (dotimes (i (length lockfiles))
                 (let* ((lf (nth i lockfiles))
                        (pin (alist-get (symbol-name package) (straight--lockfile-read lf) nil nil #'equal)))
                   (unless (zerop i)
                       (insert indent))
                   (zenit--help-insert-button (format "%s" pin) lf)
                   (let ((cat-mod (split-string (file-name-base lf) "_" t)))
                     (insert (apply #'format (if (nth 1 cat-mod) " (:%s %s)" " (:%s)") cat-mod))))
                 (insert "\n"))
             (insert "unpinned" "\n"))
           (package--print-help-section "Build")
           (let ((default-directory (straight--repos-dir (symbol-name package))))
             (if (file-exists-p default-directory)
                 (insert (cdr (zenit-call-process "git" "log" "-1" "--format=%D %h %ci")))
               (insert "n/a")))
           (insert "\n" indent)

           (package--print-help-section "Build location")
           (let ((build-dir (straight--build-dir (symbol-name package))))
             (if (file-exists-p build-dir)
                 (zenit--help-insert-button (abbreviate-file-name build-dir))
               (insert "n/a")))
           (insert "\n" indent)

           (package--print-help-section "Repo location")
           (let* ((local-repo (zenit-package-recipe-repo package))
                  (repo-dir (straight--repos-dir local-repo)))
             (if (file-exists-p repo-dir)
                 (zenit--help-insert-button (abbreviate-file-name repo-dir))
               (insert "n/a"))
             (insert "\n"))

           (let ((recipe (zenit-package-build-recipe package)))
             (package--print-help-section "Recipe")
             (insert
              (replace-regexp-in-string "\n" (concat "\n" indent)
                                        (pp-to-string recipe))))

           (package--print-help-section "Homepage")
           (let ((homepage (zenit-package-homepage package)))
             (if homepage
                 (zenit--help-insert-button homepage)
               (insert "n/a"))))

          (`elpa (insert "[M]ELPA ")
                 (zenit--help-insert-button (zenit-package-homepage package))
                 (package--print-help-section "Location")
                 (zenit--help-insert-button
                  (abbreviate-file-name
                   (file-name-directory
                    (locate-library (symbol-name package))))))
          (`builtin (insert "Built-in\n")
                    (package--print-help-section "Location")
                    (zenit--help-insert-button
                     (abbreviate-file-name
                      (file-name-directory
                       (locate-library (symbol-name package))))))
          (`other (zenit--help-insert-button
                   (abbreviate-file-name
                    (or (symbol-file package)
                        (locate-library (symbol-name package))))))
          (_ (insert "Not installed")))
        (insert "\n")

        (when-let
            (modules
             (if (gethash (symbol-name package) straight--build-cache)
                 (zenit-package-get package :modules)
               (plist-get (cdr (assq package (zenit-package-list 'all)))
                          :modules)))
          (package--print-help-section "Modules")
          (insert "Declared by the following Zenit modules:\n")
          (dolist (m modules)
            (let* ((module-path (pcase (car m)
                                  (:core zenit-core-dir)
                                  (:local-conf zenit-local-conf-dir)
                                  (category
                                   (zenit-module-locate-path category
                                                            (cdr m)))))
                   (readme-path (expand-file-name "README.org" module-path)))
              (insert indent)
              (zenit--help-insert-button
               (format "%s %s" (car m) (or (cdr m) ""))
               module-path)
              (insert " (")
              (if (file-exists-p readme-path)
                  (zenit--help-insert-button "readme" readme-path)
                (insert "no readme"))
              (insert ")\n"))))

        (package--print-help-section "Configs")
        (if-let ((configs (zenit--help-package-configs package)))
            (progn
              (insert "This package is configured in the following locations:")
              (dolist (location configs)
                (insert "\n" indent)
                (cl-destructuring-bind (file line _match &rest)
                    (split-string location ":")
                  (zenit--help-insert-button location
                                            (expand-file-name file zenit-emacs-dir)
                                            (string-to-number line)))))
          (insert "This package is not configured anywhere"))
        (goto-char (point-min))))))

(defvar zenit--package-cache nil)
(defun zenit--package-list (&optional prompt)
  (let* ((guess (or (function-called-at-point)
                    (symbol-at-point))))
    (require 'finder-inf nil t)
    (unless package--initialized
      (package-initialize t))
    (let ((packages (or zenit--package-cache
                        (progn
                          (message "Reading packages...")
                          (cl-delete-duplicates
                           (append (mapcar 'car package-alist)
                                   (mapcar 'car package--builtins)
                                   (mapcar #'(lambda (x) (plist-get x :package))
                                           (let (recipes)
                                             (dolist (recipe (hash-table-values straight--recipe-cache))
                                               (cl-destructuring-bind (&key local-repo type &allow-other-keys)
                                                   recipe
                                                 (unless (or (null local-repo)
                                                             (eq type 'built-in))
                                                   (push recipe recipes))))
                                             (nreverse recipes)))
                                   (mapcar 'car package-archive-contents)))))))
      (setq zenit--package-cache packages)
      (unless (memq guess packages)
        (setq guess nil))
      (intern (completing-read (or prompt
                                   (if guess
                                       (format "Select package to search for (default %s): "
                                               guess)
                                     "Describe package: "))
                               packages nil t nil nil
                               (if guess (symbol-name guess)))))))

;;;###autoload
(defun zenit/help-package-config (package)
  "Jump to any `use-package!', `after!' or ;;;###package block for PACKAGE.

This only searches `zenit-emacs-dir' (typically ~/.emacs.d) and does not include
config blocks in your private config."
  (interactive (list (zenit--package-list "Find package config: ")))
  (cl-destructuring-bind (file line _match)
      (split-string
       (completing-read
        "Jump to config: "
        (or (zenit--help-package-configs package)
            (user-error "This package isn't configured by you or Zenit")))
       ":")
    (find-file (expand-file-name file zenit-emacs-dir))
    (goto-char (point-min))
    (forward-line (1- line))
    (recenter)))

;;;###autoload
(defalias 'zenit/help-package-homepage #'straight-visit-package-website)

(defun zenit--help-search-prompt (prompt)
  (let ((query (zenit-thing-at-point-or-region)))
    (if (featurep 'counsel)
        query
      (read-string prompt query 'git-grep query))))

(defun zenit--help-search (dirs query prompt)
  (unless zenit-ripgrep-executable
    (user-error "Can't find ripgrep on your system"))
  (cond ((fboundp 'consult--grep)
         (consult--grep prompt #'consult--ripgrep-make-builder (cons data-directory dirs) query))
        ((grep-find
          (string-join
           (append (list zenit-ripgrep-executable
                         "-L" "--search-zip" "--no-heading" "--color=never"
                         (shell-quote-argument query))
                   (mapcar #'shell-quote-argument dirs))
           " ")))))

;;;###autoload
(defun zenit/help-search-load-path (query)
  "Perform a text search on your `load-path'.
Uses the symbol at point or the current selection, if available."
  (interactive
   (list (zenit--help-search-prompt "Search load-path: ")))
  (zenit--help-search (cl-remove-if-not #'file-directory-p load-path)
                     query "Search load-path: "))

;;;###autoload
(defun zenit/help-search-loaded-files (query)
  "Perform a text search on your `load-path'.
Uses the symbol at point or the current selection, if available."
  (interactive
   (list (zenit--help-search-prompt "Search loaded files: ")))
  (zenit--help-search
   (cl-loop for (file . _) in (cl-remove-if-not #'stringp load-history :key #'car)
            for filebase = (file-name-sans-extension file)
            if (file-exists-p! (or (format "%s.el.gz" filebase)
                                   (format "%s.el" filebase)))
            collect it)
   query "Search loaded files: "))

(provide 'zenit-lib '(help))

;; lisp/core/lib/help.el -*- lexical-binding: t; -*-

(defvar zenit--help-major-mode-module-alist
  '((lisp-mode       :lang common-lisp)
    (json-mode       :lang json)
    (yaml-mode       :lang yaml)
    (csv-mode        :lang data)
    (emacs-lisp-mode :lang emacs-lisp)
    (ess-r-mode      :lang ess)
    (ess-julia-mode  :lang ess)
    (latex-mode      :lang latex)
    (LaTeX-mode      :lang latex)
    (markdown-mode   :lang markdown)
    (gfm-mode        :lang markdown)
    (org-mode        :emacs org)
    (python-mode     :lang python)
    (scheme-mode     :lang scheme)
    (sh-mode         :lang sh))
  "An alist mapping major modes to modules.
This is used by `zenit/help-module-locate' to auto-select the
module corresponding to the current major-modea.")


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

;;;###autoload
(defun zenit/describe-active-minor-mode (mode)
  "Get information on an active minor mode. Use `describe-minor-mode' for a
selection of all minor-modes, active or not."
  (interactive
   (list (completing-read "Describe active mode: " (zenit-active-minor-modes))))
  (let ((symbol
         (cond ((stringp mode) (intern mode))
               ((symbolp mode) mode)
               ((error "Expected a symbol/string, got a %s" (type-of mode))))))
    (if (fboundp symbol)
        (helpful-function symbol)
      (helpful-variable symbol))))


;;
;;; Documentation commands

(defvar org-agenda-files)
(cl-defun zenit--org-headings (files &key depth mindepth include-files &allow-other-keys)
  "Extract headings from FILES. DEPTH defines the maximum level,
MINDEPTH the minimum level and INCLUDE-FILES puts the filename in
front. Returns an alist."
  (require 'org)
  (let* ((default-directory)
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

(defvar ivy-sort-functions-alist)
;;;###autoload
(cl-defun zenit-completing-read-org-headings
    (prompt files &rest plist &key depth mindepth include-files initial-input extra-candidates action)
  "TODO"
  (let ((alist
         (append (apply #'zenit--org-headings files plist)
                 extra-candidates))
        ivy-sort-functions-alist)
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
              (when (outline-invisible-p)
                (save-excursion
                  (outline-previous-visible-heading 1)
                  (org-fold-show-subtree))))))
      (user-error "Aborted"))))

(defun zenit-modules-find ()
  "Find MODULEINFO section in file defining modules."
  (let ((case-fold-search nil)
        module-files
        plist)
    ;; Collect files which contain module definitions
    (dolist (dir zenit-modules-dirs module-files)
      (pushnew! module-files (concat dir "modules.el")))
    (pushnew! module-files (concat zenit-core-dir "init-core-modules.el"))
    (let (modstring
          line)
      ;; Loop over module files
      (dolist (file module-files)
        (with-temp-buffer
          (when (file-exists-p file)
            (insert-file-contents file))
          (goto-char (point-min))
          (while
              ;; Search for "(module! (:CATEGORY MODULE +FLAG))"
              (re-search-forward
               (rx
                "(module! ("
                (group ":"
                       (zero-or-more (any "a-z"))
                       space
                       (zero-or-more (any "a-z" "-"))
                       (zero-or-more space "+" (zero-or-more (any "a-z" "-"))))
                ")") nil t)
            (save-excursion
              (setq modstring (match-string-no-properties 0))
              (let ((line (line-number-at-pos))
                    (str (split-string
                          (string-remove-suffix ")" (string-remove-prefix "(module! (" modstring)) " ")))
                ;; Collect module name, file name and line number into plist
                (setq plist (append plist `((,(cons (intern (nth 0 str)) (intern (nth 1 str)))
                                             (:flags ,(nthcdr 2 str)
                                              :files ,(list (format "%s:%d" file line)))))))))))))
    ;; Merge entries
    (let ((result ())
          kvs)
      ;; assign to kvs
      (while (setq kvs (pop plist))
        (let* ((module (car kvs))
               ;; module plist
               (mplist (nth 1 kvs))
               (flags (plist-get mplist :flags))
               ;; check resulting alist if an entry for module is already there
               (entry (funcall #'assoc module result))
               (entryflags (plist-get (cadr entry) :flags)))
          ;; If entry is present and has the same flags
          (if (and entry (equal flags entryflags))
              ;; Append new file to entry's file list
              (setcdr entry `((:flags ,entryflags
                               :files ,(append (plist-get (cadr entry) :files) (plist-get mplist :files)))))
            ;; Else create new entry
            (push (cons module (list mplist)) result))))
      ;; Reverse and sort result
      (sort (nreverse result) (lambda (a b) (string< (format "%s" (car a)) (format "%s" (car b))))))))

(defun zenit--help-modules-list ()
  (cl-loop for module in (zenit-modules-find)
           for (cat . mod) = (nth 0 module)
           for files = (plist-get (nth 1 module) :files)
           for flag  = (car (plist-get (nth 1 module) :flags))
           collect (cl-loop for file in files
                            for position = (nth 1 (split-string file ":"))
                            for file = (file-relative-name (nth 0 (split-string file ":")) zenit-emacs-dir)
                            for format = (concat
                                          (format "%s " cat)
                                          (format "%s " mod)
                                          (when flag (format "%s " flag))
                                          (propertize (format "%s %s" file position) 'face 'font-lock-comment-face))
                            if (zenit-module-p cat mod (when flag (intern flag)))
                            collect format
                            else if (and cat mod)
                            collect (propertize format 'face 'font-lock-comment-face))))

(defun zenit--help-current-module-str ()
  (cond ((save-excursion
           (ignore-errors
             (thing-at-point--beginning-of-sexp)
             (unless (eq (char-after) ?\()
               (backward-char))
             (let ((sexp (sexp-at-point)))
               (when (memq (car-safe sexp) '(modulep!))
                 (concat (format "%s " (nth 1 sexp))
                         (format "%s " (nth 2 sexp))
                         (when (nth 3 sexp)
                           (format "%s" (nth 3 sexp)))))))))
        ((when-let (mod (cdr (assq major-mode zenit--help-major-mode-module-alist)))
           (format "%s %s"
                   (symbol-name (car mod))
                   (symbol-name (cadr mod)))))))

;;;###autoload
(defun zenit/help-module-locate (category module file position)
  "Locate the declaration of a module.
CATEGORY is a keyword and MODULE, FILE and POSITION is a symbol.
When called with `universal-argument', automatically selects

a) the module at point derived from a `modules!' or `modulep!'
call, or

b) the module associated with the current major mode (see
`zenit--help-major-mode-module-alist')."
  (interactive
   (mapcar #'intern
           (split-string
            (completing-read "Locate module: "
                             (apply #'append (zenit--help-modules-list))
                             nil t (when current-prefix-arg
                                     (or (when-let ((pointmodule (zenit-lisp--module-at-point)))
                                           (message "%s" pointmodule)
                                           (concat (format "%s " (nth 0 pointmodule))
                                                   (format "%s " (nth 1 pointmodule))
                                                   (when (nth 2 pointmodule)
                                                     (format "%s" (nth 2 pointmodule)))))
                                         (zenit--help-current-module-str))))
            " " t)))
  (cl-check-type category symbol)
  (cl-check-type module symbol)
  (find-file (expand-file-name (symbol-name file) zenit-emacs-dir))
  (forward-line (1- (string-to-number (symbol-name position))))
  (recenter))

;;;###autoload
(defun zenit/help-module-info (&optional arg)
  "Find information of a module.
When called with `universal-argument', automatically selects

a) the module at point derived from a `modules!' or `modulep!'
call, or

b) the module associated with the current major mode (see
`zenit--help-major-mode-module-alist')."
  (interactive "P")
  (zenit-completing-read-org-headings
   "Go to module: "
   (list (concat zenit-core-dir "Modinfo.org")
         (concat zenit-modules-dir "Modinfo.org")
         (concat zenit-local-conf-dir "Modinfo.org"))
   :depth 2 :include-files t
   :initial-input
   (when current-prefix-arg
     (or (when-let ((pointmodule (zenit-lisp--module-at-point)))
           (message "%s" pointmodule)
           (concat (format "%s " (nth 0 pointmodule))
                   (format "%s " (nth 1 pointmodule))
                   (when (nth 2 pointmodule)
                     (format "%s" (nth 2 pointmodule)))))
         (zenit--help-current-module-str)))))


(defun zenit--help-package-configs (package)
  (append
   ;; Find in ~/.emacs.d
   (let ((default-directory zenit-emacs-dir))
     (split-string
      (cdr (zenit-call-process
            "rg" "-H" "-S" "--no-heading" "--line-number" "-g" "*.el"
            (format "%s %s($| )"
                    "(^;;;###package|\\(after\!|\\(use-package\!)"
                    package)))
      "\n" t))
   ;; Find in ~/.emacs.d/site-lisp
   (let ((default-directory zenit-local-conf-dir)
         result)
     (dolist (entry
              (split-string
               (cdr (zenit-call-process
                     "rg" "-uu" "-H" "-S" "--no-heading" "--line-number" "--follow" "-g" "*.el"
                     (format "%s %s($| )"
                             "(^;;;###package|\\(after\!|\\(use-package\!)"
                             package)))
               "\n" t) result)
       ;; Append site-lisp to all results, otherwise opening the correct file will not work
       (push (string-join (list "site-lisp/" entry)) result))
     (nreverse result))))

(defvar zenit--package-cache nil)
(defun zenit--package-list (&optional prompt)
  (let* ((guess (or (function-called-at-point)
                    (symbol-at-point))))
    (require 'finder-inf nil t)
    (require 'package)
    (require 'straight)
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
  "Jump to any `use-package!', `after!' or ;;;###package block for
PACKAGE.

This only searches `zenit-emacs-dir' (typically ~/.emacs.d) and
does not include config blocks in your private config."
  (interactive (list (zenit--package-list "Find package config: ")))
  (cl-destructuring-bind (file line _match)
      (split-string
       (completing-read
        "Jump to config: "
        (or (zenit--help-package-configs package)
            (user-error "This package isn't configured.")))
       ":")
    (find-file (expand-file-name file zenit-emacs-dir))
    (goto-char (point-min))
    (forward-line (1- (string-to-number line)))
    (recenter)))

;;;###autoload
(defalias 'zenit/help-package-homepage #'straight-visit-package-website)

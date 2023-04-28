;; lisp/core/zenit-setup.el -*- lexical-binding: t; -*-

(defvar zenit--deferred-packages-alist '(t))
(autoload 'use-package "use-package-core" nil nil t)


;;
;;; use-package modifications

(setq use-package-compute-statistics init-file-debug
      use-package-verbose init-file-debug
      use-package-minimum-reported-time (if init-file-debug 0 0.1)
      use-package-expand-minimally (not noninteractive))

;; Disable `:ensure', because we don't use package.el.
(setq use-package-ensure-function
      (lambda (name &rest _)
        (message "Ignoring ':ensure t' in '%s' config" name)))
;; But restore old behavior if `package' is loaded.
(add-transient-hook! 'package-initialize
  (when (eq use-package-ensure-function #'ignore)
    (setq use-package-ensure-function #'use-package-ensure-elpa)))

(with-eval-after-load 'use-package-core
  ;; `use-package' adds syntax highlighting for the `use-package' macro, but
  ;; Emacs 26+ already highlights macros, so it's redundant.
  (font-lock-remove-keywords 'emacs-lisp-mode use-package-font-lock-keywords)

  ;; We define :minor and :magic-minor from the `auto-minor-mode' package here
  ;; so we don't have to load `auto-minor-mode' so early.
  (dolist (keyword '(:minor :magic-minor))
    (setq use-package-keywords
          (use-package-list-insert keyword use-package-keywords :commands)))

  (defalias 'use-package-normalize/:minor #'use-package-normalize-mode)
  (defun use-package-handler/:minor (name _ arg rest state)
    (use-package-handle-mode name 'auto-minor-mode-alist arg rest state))

  (defalias 'use-package-normalize/:magic-minor #'use-package-normalize-mode)
  (defun use-package-handler/:magic-minor (name _ arg rest state)
    (use-package-handle-mode name 'auto-minor-mode-magic-alist arg rest state))

  ;; HACK Fix `:load-path' so it resolves relative paths to the containing file,
  ;;      rather than `user-emacs-directory'. This is a done as a convenience.
  (defadvice! zenit--resolve-load-path-from-containg-file-a (fn label arg &optional recursed)
    "Resolve :load-path from the current directory."
    :around #'use-package-normalize-paths
    ;; `use-package-normalize-paths' resolves paths relative to
    ;; `user-emacs-directory', so we change that.
    (let ((user-emacs-directory
           (or (and (stringp arg)
                    (not (file-name-absolute-p arg))
                    (ignore-errors (dir!)))
               zenit-emacs-dir)))
      (funcall fn label arg recursed)))

  ;; Adds two keywords to `use-package' to expand its lazy-loading capabilities:
  ;;
  ;;   :after-call SYMBOL|LIST
  ;;   :defer-incrementally SYMBOL|LIST|t
  ;;
  ;; Check out `use-package!'s documentation for more about these two.
  (dolist (keyword '(:defer-incrementally :after-call))
    (push keyword use-package-deferring-keywords)
    (setq use-package-keywords
          (use-package-list-insert keyword use-package-keywords :after)))

  (defalias 'use-package-normalize/:defer-incrementally #'use-package-normalize-symlist)
  (defun use-package-handler/:defer-incrementally (name _keyword targets rest state)
    (use-package-concat
     `((zenit-load-packages-incrementally
        ',(if (equal targets '(t))
              (list name)
            (append targets (list name)))))
     (use-package-process-keywords name rest state)))

  (defalias 'use-package-normalize/:after-call #'use-package-normalize-symlist)
  (defun use-package-handler/:after-call (name _keyword hooks rest state)
    (if (plist-get state :demand)
        (use-package-process-keywords name rest state)
      (let ((fn (make-symbol (format "zenit--after-call-%s-h" name))))
        (use-package-concat
         `((fset ',fn
                 (lambda (&rest _)
                   (zenit-log "use-package: lazy loading %s from %s" ',name ',fn)
                   (condition-case e
                       ;; If `default-directory' is a directory that doesn't
                       ;; exist or is unreadable, Emacs throws up file-missing
                       ;; errors, so we set it to a directory we know exists and
                       ;; is readable.
                       (let ((default-directory zenit-emacs-dir))
                         (require ',name))
                     ((debug error)
                      (message "Failed to load deferred package %s: %s" ',name e)))
                   (when-let (deferral-list (assq ',name zenit--deferred-packages-alist))
                     (dolist (hook (cdr deferral-list))
                       (advice-remove hook #',fn)
                       (remove-hook hook #',fn))
                     (delq! deferral-list zenit--deferred-packages-alist)
                     (unintern ',fn nil)))))
         (let (forms)
           (dolist (hook hooks forms)
             (push (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
                       `(add-hook ',hook #',fn)
                     `(advice-add #',hook :before #',fn))
                   forms)))
         `((unless (assq ',name zenit--deferred-packages-alist)
             (push '(,name) zenit--deferred-packages-alist))
           (nconc (assq ',name zenit--deferred-packages-alist)
                  '(,@hooks)))
         (use-package-process-keywords name rest state))))))

;;
;;; Macros

(defvar zenit-disabled-packages)
(defmacro use-package! (name &rest plist)
  "Declares and configures a package.

This is a thin wrapper around `use-package', and is ignored if
the NAME package is disabled (with `package!').

See `use-package' to see what properties can be provided. Adds
support for two extra properties:

:after-call SYMBOL|LIST
  Takes a symbol or list of symbols representing functions or
  hook variables. The first time any of these functions or hooks
  are executed, the package is loaded.

:defer-incrementally SYMBOL|LIST|t
  Takes a symbol or list of symbols representing packages that
  will be loaded incrementally at startup before this one. This
  is helpful for large packages like magit or org, which load a
  lot of dependencies on first load. This lets you load them
  piece-meal during idle periods, so that when you finally do
  need the package, it'll load quicker.

  NAME is implicitly added if this property is present and
  non-nil. No need to specify it. A value of `t' implies NAME."
  (declare (indent 1))
  (unless (or (memq name (bound-and-true-p zenit-disabled-packages))
              ;; At compile-time, use-package will forcibly load packages to
              ;; prevent compile-time errors. However, if a package is disabled,
              ;; you get file-missing package errors, so it's necessary to check
              ;; for packages at compile time:
              (and (bound-and-true-p byte-compile-current-file)
                   (not (locate-library (symbol-name name)))))
    `(use-package ,name ,@plist)))

(provide 'zenit-setup)

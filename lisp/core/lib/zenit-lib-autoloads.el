;; lisp/core/lib/zenit-lib-autoloads.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defvar zenit-autoloads-excluded-packages '()
  "Which packages to exclude from autoloads files.

Use this for packages with problematic autoloads; e.g. they
autoload too much or hoist buggy forms into autoloads.")

(defvar zenit-autoloads-excluded-files '()
  "List of regexps whose matching files won't be indexed for
autoloads.")

(defvar zenit-autoloads-cached-vars
  '(load-path
    auto-mode-alist
    interpreter-mode-alist
    magic-mode-alist
    magic-fallback-mode-alist
    Info-directory-list)
  "A list of variables to be cached in `zenit-config-init-file'.")

(defvar zenit-autoloads-files ()
  "A list of additional files or file globs to scan for
autoloads.")

(require 'loaddefs-gen)


;;
;;; Library

(defun zenit-autoloads--compile-file (file)
  (condition-case-unless-debug e
      (let ((byte-compile-warnings (if init-file-debug byte-compile-warnings))
            success)
        (and (pcase-let ((`(,status . ,msg)
                          (async-get
                           (zenit-async-byte-compile-file
                            file
                            :req-core t
                            :req-core-libs (when (zenit-module-p :config 'compile)
                                             '(files))
                            :req-extra (if (zenit-module-p :config 'compile)
                                           '(cl-lib zenit-use-package zenit-el-patch zenit-keybinds)
                                         '(cl-lib))
                            :modulep (zenit-module-p :config 'compile)
                            :warnings byte-compile-warnings))))
               (when msg
                 (with-output-to!
                     `((t . ,(alist-get 'complog zenit-cli-log-buffers)))
                   (let ((zenit-print-indent 0))
                     (print! msg))))
               (setq success status))
             (if success t (signal 'error "Failed to byte-compile init file"))
             (load (byte-compile-dest-file file) nil t)))
    (error
     (delete-file (byte-compile-dest-file file))
     (signal 'zenit-autoload-error (list file e)))))

(defun zenit-autoloads--cleanup-form (form &optional expand)
  (let ((func (car-safe form)))
    (cond ((memq func '(provide custom-autoload register-definition-prefixes))
           nil)
          ;; HACK: Remove modifications to `auto-mode-alist' and
          ;;   `interpreter-mode-alist' in *-ts-mode package. They are applied
          ;;   twice and often overwrite user or module configuration.
          ((equal (list func (car (cdr-safe form))) '(when (treesit-available-p)))
           (setf (nth 2 form)
                 (cl-loop for form in (nth 2 form)
                          if (or (not (eq (car-safe form) 'add-to-list))
                                 (not (memq (nth 1 form) '(auto-mode-alist interpreter-mode-alist))))
                          collect form))
           form)
          ((and (eq func 'add-to-list)
                (memq (zenit-unquote (cadr form))
                      zenit-autoloads-cached-vars))
           nil)
          ((not (eq func 'autoload))
           form)
          ((and expand (not (file-name-absolute-p (nth 2 form))))
           (defvar zenit--autoloads-path-cache nil)
           (setf (nth 2 form)
                 (let ((path (nth 2 form)))
                   (or (cdr (assoc path zenit--autoloads-path-cache))
                       (when-let* ((libpath
                                    (or (locate-library path)
                                        (locate-library (expand-file-name path temporary-file-directory))))
                                   (libpath (file-name-sans-extension libpath))
                                   (libpath (abbreviate-file-name libpath)))
                         (push (cons path libpath) zenit--autoloads-path-cache)
                         libpath)
                       path)))
           form)
          (form))))

(defun zenit-autoloads--scan-autodefs (file buffer module &optional module-enabled-p)
  (with-file-contents! file
    (while (re-search-forward "^;;;###autodef *\\([^\n]+\\)?\n" nil t)
      (let* ((standard-output buffer)
             (form    (read (current-buffer)))
             (altform (match-string 1))
             (definer (car-safe form))
             (symbol  (zenit-unquote (cadr form))))
        (cond ((and (not module-enabled-p) altform)
               (print (read altform)))
              ((memq definer '(defun defmacro cl-defun cl-defmacro))
               (print
                (if module-enabled-p
                    (loaddefs-generate--make-autoload form (abbreviate-file-name file))
                  (seq-let (_ _ arglist &rest body) form
                    (if altform
                        (read altform)
                      (append
                       (list (pcase definer
                               (`defun 'defmacro)
                               (`cl-defun `cl-defmacro)
                               (_ type))
                             symbol arglist
                             (format "THIS FUNCTION DOES NOTHING BECAUSE %s IS DISABLED\n\n%s"
                                     module (if (stringp (car body))
                                                (pop body)
                                              "No documentation.")))
                       (cl-loop for arg in arglist
                                if (symbolp arg)
                                if (not (keywordp arg))
                                if (not (memq arg cl--lambda-list-keywords))
                                collect arg into syms
                                else if (listp arg)
                                collect (car arg) into syms
                                finally return (if syms `((ignore ,@syms)))))))))
               (print `(put ',symbol 'zenit-module ',module)))
              ((eq definer 'defalias)
               (seq-let (_ _ target docstring) form
                 (unless module-enabled-p
                   (setq target #'ignore
                         docstring
                         (format "THIS FUNCTION DOES NOTHING BECAUSE %s IS DISABLED\n\n%s"
                                 module docstring)))
                 (print `(put ',symbol 'zenit-module ',module))
                 (print `(defalias ',symbol #',(zenit-unquote target) ,docstring))))
              (module-enabled-p (print form)))))))

(defvar generated-autoload-load-name)
(defvar zenit-loaddefs-temp-file nil
  "Temporary file used to store output from `loaddefs-generate'.")
(defun zenit-autoloads--scan-file (file)
  (let* (;; Prevent `autoload-find-file' from firing file hooks, e.g. adding to
         ;; recentf.
         find-file-hook
         write-file-functions
         ;; Prevent a possible source of crashes when there's a syntax error in
         ;; the autoloads file. See
         ;; https://github.com/radian-software/straight.el/issues/434.
         debug-on-error
         ;; Non-nil interferes with autoload generation in Emacs < 29. See
         ;; https://github.com/radian-software/straight.el/issues/904.
         (left-margin 0)
         ;; The following bindings are in `package-generate-autoloads'.
         ;; Presumably for a good reason, so I just copied them.
         (backup-inhibited t)
         (version-control 'never)
         ;; Reduce magic
         case-fold-search
         autoload-compute-prefixes
         ;; So `autoload-generate-file-autoloads' knows where to write it
         (target-buffer (current-buffer))
         (module (zenit-module-from-path file))
         (generated-autoload-load-name (abbreviate-file-name (file-name-sans-extension file)))
         (module-enabled-p (and (zenit-module-p (car module) (cdr module))
                                (zenit-file-cookie-p file "if" t))))
    (save-excursion
      (when module-enabled-p
        (let (;; Don't be noisy
              (message-log-max nil)
              (inhibit-message t))
          (loaddefs-generate
           (file-name-directory file) zenit-loaddefs-temp-file
           ;; The string used for EXTRA-DATA forces the creation of a file
           (remove file (zenit-glob (file-name-directory file) "*.el")) ";; zenit-loaddefs" nil t))
        (zenit-file-read zenit-loaddefs-temp-file :by 'insert))
      (goto-char (point-max))
      (zenit-autoloads--scan-autodefs
       file target-buffer module module-enabled-p))))

(defun zenit-autoloads--scan (files &optional exclude literal)
  "Scan and return all autoloaded forms in FILES.

Autoloads will be generated from autoload cookies in FILES (except those that
match one of the regexps in EXCLUDE -- a list of strings). If LITERAL is
non-nil, treat FILES as pre-generated autoload files instead."
  (let ((zenit-loaddefs-temp-file (make-temp-file "zenit-loaddefs"))
        autoloads)
    (dolist (file files (nreverse (delq nil autoloads)))
      (when (and (not (seq-find (zenit-rpartial #'string-match-p file) exclude))
                 (file-readable-p file))
        (zenit-log "loaddefs:scan: %s" file)

        (zenit--with-prepared-file-buffer file (or coding-system-for-read 'utf-8) nil
          (if literal
              (insert-file-contents file)
            (zenit-autoloads--scan-file file))
          (save-excursion
            (while (re-search-forward "\\_<load-file-name\\_>" nil t)
              ;; `load-file-name' is meaningless in a concatenated
              ;; mega-autoloads file, but also essential in isolation, so we
              ;; replace references to it with the file they came from.
              (let ((ppss (save-excursion (syntax-ppss))))
                (or (nth 3 ppss)
                    (nth 4 ppss)
                    (replace-match (prin1-to-string (abbreviate-file-name file)) t t)))))
          (let ((load-file-name file)
                (load-path
                 (append (list zenit-local-conf-dir)
                         zenit-modules-load-path
                         load-path)))
            (condition-case _
                (while t
                  (let ((form (zenit-autoloads--cleanup-form (read (current-buffer))
                                                             (not literal))))
                    (unless (member form autoloads)
                      (push form autoloads))))
              (end-of-file))))))))

(provide 'zenit-lib '(autoloads))

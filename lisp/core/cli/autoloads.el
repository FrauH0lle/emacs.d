;; lisp/core/cli/autoloads.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defvar zenit-autoloads-excluded-packages '()
  "Packages that have silly or destructive autoload files that
try to load everyone in the universe and their dog, causing
errors that make babies cry. No one wants that.")

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

(defvar zenit-cache-generators
  '(("zenit-cached-vars.el"        . zenit--generate-vars)
    ("zenit-autoloads.el"          . zenit--generate-autoloads)
    ("zenit-packages-autoloads.el" . zenit--generate-package-autoloads)
    ("zenit-load-modules.el"       . zenit--generate-load-modules))
  "An alist mapping file names to generator functions.")

(defvar zenit-autoloads-files ()
  "A list of additional files or file globs to scan for
autoloads.")


;;
;;; Library

(defun zenit--generate-vars ()
  ;; Make sure `Info-directory-list' is populated
  (dolist (dir (let ((files (straight--directory-files (straight--build-dir) nil t)))
                 (mapcar #'file-name-nondirectory
                         (cl-remove-if-not #'file-directory-p files))))
    (straight--add-package-to-info-path `(:package ,dir)))
  `((when (zenit-context-p 'init)
      ,@(cl-loop for var in zenit-autoloads-cached-vars
                 if (boundp var)
                 collect `(set-default ',var ',(symbol-value var))))))

(defun zenit--generate-autoloads ()
  (zenit-autoloads--scan
   (append (zenit-glob zenit-core-dir "lib/*.el")
           (cl-loop for dir
                    in (append (zenit-module-load-path zenit-modules-dirs)
                               (list zenit-local-conf-dir))
                    if (zenit-glob dir "autoload.el") collect it
                    if (zenit-glob dir "autoload/*.el") append it)
           (mapcan #'zenit-glob zenit-autoloads-files))
   nil))

(defun zenit--generate-package-autoloads ()
  (zenit-autoloads--scan
   (mapcar #'straight--autoloads-file
           (nreverse (seq-difference (hash-table-keys straight--build-cache)
                                     zenit-autoloads-excluded-packages)))
   zenit-autoloads-excluded-files 'literal))

(defun zenit--generate-load-modules ()
  (let* ((init-modules-list (zenit-module-list nil t))
         (config-modules-list (zenit-module-list))
         (pre-init-modules
          (seq-filter (fn! (<= (zenit-module-depth (car %) (cdr %) t) -100))
                      (remove '(:user) init-modules-list)))
         (init-modules
          (seq-filter (fn! (<= 0 (zenit-module-depth (car %) (cdr %) t) 100))
                      init-modules-list))
         (config-modules
          (seq-filter (fn! (<= 0 (zenit-module-depth (car %) (cdr %)) 100))
                      config-modules-list))
         (post-config-modules
          (seq-filter (fn! (>= (zenit-module-depth (car %) (cdr %)) 100))
                      config-modules-list))
         (init-file   zenit-module-init-file)
         (config-file zenit-module-config-file))
    (letf! ((defun module-loader (group name file &optional noerror)
              (zenit-module-context-with (cons group name)
                `(let ((zenit-module-context ,zenit-module-context))
                   ,@(if (not (modulep! :config compile))
                         `((zenit-load ,(abbreviate-file-name (file-name-sans-extension file))))
                       `((cl-eval-when (compile) (setq zenit--embed-current-file ,file))
                         (zenit-embed ,file)
                         (cl-eval-when (compile) (setq zenit--embed-current-file nil)))
                       ))))
            (defun module-list-loader (modules file &optional noerror)
              (cl-loop for (cat . mod) in modules
                       if (zenit-module-locate-path cat mod file)
                       collect (module-loader cat mod it noerror))))
      ;; FIX: Same as above (see `zenit-profile--generate-init-vars').
      `((if (or (zenit-context-p 'init)
                (zenit-context-p 'reload))
            (zenit-context-with 'modules
              (set 'zenit-modules ',zenit-modules)
              (set 'zenit-disabled-packages ',zenit-disabled-packages)
              ;; Cache module state and flags in symbol plists for quick lookup by
              ;; `modulep!' later.
              ,@(cl-loop
                 for (category . modules) in (seq-group-by #'car config-modules-list)
                 collect
                 `(setplist ',category
                   (quote ,(cl-loop for (_ . module) in modules
                                    nconc `(,module ,(get category module))))))
              (let ((old-custom-file custom-file))
                ,@(module-list-loader pre-init-modules init-file)
                (zenit-run-hooks 'zenit-before-modules-init-hook)
                ,@(module-list-loader init-modules init-file)
                (zenit-run-hooks 'zenit-after-modules-init-hook)
                (zenit-run-hooks 'zenit-before-modules-config-hook)
                ,@(module-list-loader config-modules config-file)
                (zenit-run-hooks 'zenit-after-modules-config-hook)
                ,@(module-list-loader post-config-modules config-file t)
                (when (eq custom-file old-custom-file)
                  (zenit-load custom-file 'noerror)))))))))

(defun zenit-autoloads-reload (&optional file)
  "Regenerates autoloads and writes them to FILE."
  (unless file
    (setq file zenit-config-init-file))
  (print! (start "(Re)generating autoloads file..."))
  (print-group!
   (cl-check-type file string)
   (zenit-initialize-packages)
   (print! (start "Generating autoloads file..."))
   (pcase-dolist (`(,file . ,fn) zenit-cache-generators)
     (let ((file (file-name-concat zenit-local-dir file)))
       (zenit-log "Building %s..." file)
       (zenit-autoloads--write file nil (funcall fn))))
   (let ((forms nil))
     (with-temp-buffer
       (pcase-dolist (`(,file . ,fn) zenit-cache-generators)
         (let ((file (file-name-concat zenit-local-dir file)))
           (insert-file-contents file)
           (condition-case _
               (while t
                 (let ((form (read (current-buffer))))
                   (push form forms)))
             (end-of-file)))))
     (setq forms (nreverse forms))
     (zenit-autoloads--write file t
      `((unless (equal emacs-major-version ,emacs-major-version)
          (signal 'zenit-error
                  (list "The installed version of Emacs has changed since last refresh")))
        ,@forms)))
   (print! (start "Byte-compiling autoloads file..."))
   (quiet! (zenit-autoloads--compile-file file))
   (print! (success "Generated %s")
           (relpath (byte-compile-dest-file file)
                    zenit-emacs-dir))))

(defun zenit-autoloads--write (file header &rest forms)
  (make-directory (file-name-directory file) 'parents)
  (condition-case-unless-debug e
      (with-temp-file file
        (setq-local coding-system-for-write 'utf-8)
        (let ((standard-output (current-buffer))
              (print-quoted t)
              (print-level nil)
              (print-length nil))
          (when header
            (insert ";; -*- coding: utf-8; lexical-binding: t; -*-\n"
                    ";; This file was autogenerated; do not edit it by hand!\n"))
          (dolist (form (delq nil forms))
            (mapc #'prin1 form))
          t))
    (error (delete-file file)
           (signal 'zenit-autoload-error (list file e)))))

(defun zenit-autoloads--compile-file (file)
  ;; Make sure the cached definitions are loaded for the byte-compiler
  (dolist (cache-file (butlast (mapcar #'car zenit-cache-generators) 1))
    (load (file-name-concat zenit-local-dir cache-file) nil (not init-file-debug)))
  ;; Setup environment for compilation
  (require 'zenit-start)
  ;; If we are embedding the configuration, we need to load a bit more,
  (when (modulep! :config compile)
    (require 'zenit-use-package)
    (require 'use-package)
    (require 'zenit-el-patch)
    (require 'zenit-keybinds)
    (require 'zenit-ui)
    (require 'zenit-projects)
    (require 'zenit-editor)

    ;; Prevent packages from being loaded at compile time if they
    ;; don't meet their own predicates.
    (push (list :no-require t
                (lambda (_name args)
                  (or (when-let (pred (or (plist-get args :if)
                                          (plist-get args :when)))
                        (not (eval pred t)))
                      (when-let (pred (plist-get args :unless))
                        (eval pred t)))))
          use-package-defaults))

  (condition-case-unless-debug e
      (let ((byte-compile-warnings (if init-file-debug byte-compile-warnings)))
        (and (byte-compile-file file)
             (load (byte-compile-dest-file file) nil t)))
    (error
     (delete-file (byte-compile-dest-file file))
     (signal 'zenit-autoload-error (list file e)))))

(defun zenit-autoloads--cleanup-form (form &optional expand)
  (let ((func (car-safe form)))
    (cond ((memq func '(provide custom-autoload register-definition-prefixes))
           nil)
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
                       (when-let* ((libpath (locate-library path))
                                   (libpath (file-name-sans-extension libpath))
                                   (libpath (abbreviate-file-name libpath)))
                         (push (cons path libpath) zenit--autoloads-path-cache)
                         libpath)
                       path)))
           form)
          (form))))

(defun zenit-autoloads--scan-autodefs (file buffer module &optional module-enabled-p)
  (with-temp-buffer
    (insert-file-contents file)
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
                    (make-autoload form (abbreviate-file-name file))
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

(defvar autoload-timestamps)
(defvar generated-autoload-load-name)
(defun zenit-autoloads--scan-file (file)
  (let* (;; Prevent `autoload-find-file' from firing file hooks, e.g. adding
         ;; to recentf.
         find-file-hook
         write-file-functions
         ;; Prevent a possible source of crashes when there's a syntax error
         ;; in the autoloads file
         debug-on-error
         ;; The following bindings are in `package-generate-autoloads'.
         ;; Presumably for a good reason, so I just copied them
         ;; Non-nil interferes with autoload generation in Emacs < 29. See
         ;; radian-software/straight.el#904.
         (left-margin 0)
         (backup-inhibited t)
         (version-control 'never)
         case-fold-search    ; reduce magic
         autoload-timestamps ; reduce noise in generated files
         ;; So `autoload-generate-file-autoloads' knows where to write it
         (target-buffer (current-buffer))
         (module (zenit-module-from-path file))
         (generated-autoload-load-name (abbreviate-file-name (file-name-sans-extension file)))
         (module-enabled-p (and (zenit-module-p (car module) (cdr module))
                                (zenit-file-cookie-p file "if" t))))
    (save-excursion
      (when module-enabled-p
        (quiet! (autoload-generate-file-autoloads file target-buffer)))
      (zenit-autoloads--scan-autodefs
       file target-buffer module module-enabled-p))))

(defun zenit-autoloads--scan (files &optional exclude literal)
  (require 'autoload)
  (let (autoloads)
    (dolist (file files (nreverse (delq nil autoloads)))
      (when (and (not (seq-find (zenit-rpartial #'string-match-p file) exclude))
                 (file-readable-p file))
        (zenit-log "Scanning %s" file)
        (setq file (file-truename file))
        (with-temp-buffer
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
                         zenit-modules-dirs
                         load-path)))
            (condition-case _
                (while t
                  (push (zenit-autoloads--cleanup-form (read (current-buffer))
                                                       (not literal))
                        autoloads))
              (end-of-file))))))))

(defun zenit-autoloads--delete-file (file)
  "Delete FILE (an autoloads file) and accompanying *.elc file, if any."
  (cl-check-type file string)
  (when (file-exists-p file)
    (when-let (buf (find-buffer-visiting file))
      (with-current-buffer buf
        (set-buffer-modified-p nil))
      (kill-buffer buf))
    (ignore-errors (delete-file (comp-el-to-eln-filename file)))
    (ignore-errors (delete-file (byte-compile-dest-file file)))
    (delete-file file)
    t))

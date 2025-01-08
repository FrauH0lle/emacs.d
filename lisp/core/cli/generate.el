;; lisp/core/cli/generate.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defvar zenit-cache-generators
  '(("zenit-cached-vars.el"        . zenit--generate-vars)
    ("zenit-autoloads.el"          . zenit--generate-autoloads)
    ("zenit-packages-autoloads.el" . zenit--generate-package-autoloads)
    ("zenit-load-modules.el"       . zenit--generate-load-modules))
  "An alist mapping file names to generator functions.")


;;
;;; Generators

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

(defun zenit--generate-load-modules ()
  (let* ((init-modules-list (zenit-module-list nil t))
         (config-modules-list (zenit-module-list))
         (pre-init-modules
          (seq-filter (fn! (<= (zenit-module-depth (car %) (cdr %) t) -100))
                      (remove '(:local-conf) init-modules-list)))
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
    (letf! ((defun module-loader (group name file &optional noerror no-include)
              (zenit-module-context-with (cons group name)
                `(let ((zenit-module-context ,zenit-module-context))
                   ,@(if (or (not (zenit-module-p :config 'compile))
                             no-include)
                         `((zenit-load ,(abbreviate-file-name (file-name-sans-extension file))))
                       `((cl-eval-when (compile) (setq zenit-include--current-file ,file))
                         (zenit-include ,file)
                         (cl-eval-when (compile) (setq zenit-include--current-file nil)))))))
            (defun module-list-loader (modules file &optional noerror no-include)
              (cl-loop for (cat . mod) in modules
                       if (zenit-module-locate-path cat mod file)
                       collect (module-loader cat mod it noerror no-include))))
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
                ,@(module-list-loader post-config-modules config-file t t)
                (when (eq custom-file old-custom-file)
                  (zenit-load custom-file 'noerror)))))))))

(defun zenit--generate-autoloads ()
  "Generate autoloads for core and module files.

This function scans the core directory and module directories to
find all relevant files for generating autoloads. It then
processes these files to generate the necessary autoload
definitions."
  (zenit-autoloads--scan
   ;; Remove empty strings and append core library files, module autoload files,
   ;; and any additional files specified in `zenit-autoloads-files`.
   (delete "" (append (zenit-glob zenit-core-dir "lib/*.el")
                      (cl-loop for dir
                               in (append (zenit-module-load-path zenit-modules-dirs)
                                          (list zenit-local-conf-dir))
                               if (zenit-glob dir "autoload.el") append it
                               if (zenit-glob dir "autoload/*.el") append it)
                      (mapcan #'zenit-glob zenit-autoloads-files)))
   nil))

(defun zenit--generate-package-autoloads ()
  "Generate autoloads for packages.

Autoloads are generated by walking the package dependency tree
depth-first. This ensures any load-order constraints in package
autoloads are always met."
  (zenit-autoloads--scan
   ;; Create a list of packages starting with the Nth-most dependencies by
   ;; walking the package dependency tree depth-first. This ensures any
   ;; load-order constraints in package autoloads are always met.
   (let (packages)
     ;; Define a local function to walk through the package list and collect packages
     (letf! (defun* walk-packages (pkglist)
              (cond ((null pkglist) nil)
                    ((stringp pkglist)
                     ;; Recursively walk through dependencies and add to packages list
                     (walk-packages (nth 1 (gethash pkglist straight--build-cache)))
                     (cl-pushnew pkglist packages :test #'equal))
                    ((listp pkglist)
                     ;; Walk through each package in the list
                     (mapc #'walk-packages (reverse pkglist)))))
       ;; Start walking from the root packages
       (walk-packages (mapcar #'symbol-name (mapcar #'car zenit-packages))))
     ;; Map the collected packages to their autoload files
     (mapcar #'straight--autoloads-file (nreverse packages)))
   zenit-autoloads-excluded-files 'literal))

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
       (zenit-file-write file (funcall fn))))
   (with-file! file
     (insert ";; -*- coding: utf-8; lexical-binding: t; -*-\n"
             ";; This file was autogenerated; do not edit it by hand!\n")
     (prin1 `(unless (equal emacs-major-version ,emacs-major-version)
               (signal 'zenit-error
                       (list "The installed version of Emacs has changed since last refresh")))
            (current-buffer))
     (pcase-dolist (`(,file . ,fn) zenit-cache-generators)
       (let ((file (file-name-concat zenit-local-dir file)))
         (zenit-file-read file :by 'insert)))
     (insert "\n(defun zenit-foo-bla () (message \"done!\"))\n"))
   (print! (start "Byte-compiling autoloads file..."))
   (zenit-autoloads--compile-file file)
   (print! (success "Generated %s")
           (relpath (byte-compile-dest-file file)
                    zenit-emacs-dir))))

(defun zenit-autoloads--delete-file (file)
  "Delete FILE (an autoloads file) and accompanying *.elc file, if any."
  (cl-check-type file string)
  (when (file-exists-p file)
    (when-let* ((buf (find-buffer-visiting file)))
      (with-current-buffer buf
        (set-buffer-modified-p nil))
      (kill-buffer buf))
    (ignore-errors (delete-file (comp-el-to-eln-filename file)))
    (ignore-errors (delete-file (byte-compile-dest-file file)))
    (delete-file file)
    t))

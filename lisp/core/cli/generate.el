;; lisp/core/cli/generate.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defvar zenit-init-generators
  '(("05-zenit-cached-vars.auto.el"        . zenit--generate-vars)
    ("80-zenit-autoloads.auto.el"          . zenit--generate-autoloads)
    ("90-zenit-packages-autoloads.auto.el" . zenit--generate-package-autoloads)
    ("95-zenit-load-modules.auto.el"       . zenit--generate-load-modules))
  "An alist mapping file names to generator functions.")


;;
;;; Generators

(defun zenit--generate-vars ()
  `((when (zenit-context-p 'reload)
      (set-default-toplevel-value 'load-path (get 'load-path 'initial-value)))
    ,@(cl-loop for var in '(auto-mode-alist
                            interpreter-mode-alist
                            magic-mode-alist
                            magic-fallback-mode-alist)
               collect `(set-default-toplevel-value ',var ',(symbol-value var)))
    ,@(cl-loop with site-run-dir =
               (ignore-errors
                 (directory-file-name (file-name-directory
                                       (locate-library site-run-file))))
               for path in load-path
               unless (and site-run-dir (file-in-directory-p path site-run-dir))
               unless (file-in-directory-p path data-directory)
               unless (file-equal-p path zenit-core-dir)
               collect `(add-to-list 'load-path ,path))))

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
    (letf! ((defun module-loader (key file &optional noerror no-include)
              (let ((noextfile (file-name-sans-extension file)))
                `(zenit-module-context-with ',key
                   ,@(pcase key
                       ('(:core . nil)
                        `((zenit-load
                           (file-name-concat
                            zenit-core-dir ,(file-name-nondirectory noextfile))
                           t)))
                       ('(:user . nil)
                        `((zenit-load
                           (file-name-concat
                            zenit-local-conf-dir ,(file-name-nondirectory noextfile))
                           t)))
                       (_
                        (when (zenit-file-cookie-p file "if" t)
                          (if (or (not (zenit-module-p :config 'compile))
                                  no-include)
                              `((zenit-load ,(abbreviate-file-name noextfile) t))
                            `((cl-eval-when (compile) (setq zenit-include--current-file ,file))
                              (zenit-include ,file)
                              (cl-eval-when (compile) (setq zenit-include--current-file nil))))))))))
            (defun module-list-loader (modules file &optional noerror no-include)
              (cl-loop for (cat . mod) in modules
                       if (zenit-module-locate-path cat mod file)
                       collect (module-loader (cons cat mod) it noerror no-include))))
      ;; Make sure this only runs at startup to protect us Emacs' interpreter
      ;; re-evaluating this file when lazy-loading dynamic docstrings from the
      ;; byte-compiled init file.
      `((when (or (zenit-context-p 'startup)
                  (zenit-context-p 'reload))
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
          (zenit-context-with 'module
            (let ((old-custom-file custom-file))
              (zenit-context-with 'init
                ,@(module-list-loader pre-init-modules init-file)
                (zenit-run-hooks 'zenit-before-modules-init-hook)
                ,@(module-list-loader init-modules init-file)
                (zenit-run-hooks 'zenit-after-modules-init-hook))
              (zenit-context-with 'config
                (zenit-run-hooks 'zenit-before-modules-config-hook)
                ,@(module-list-loader config-modules config-file)
                (zenit-run-hooks 'zenit-after-modules-config-hook)
                ,@(module-list-loader post-config-modules config-file t 'no-include))
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
  `(,@(zenit-autoloads--scan
       ;; Create a list of packages starting with the Nth-most dependencies
       ;; by walking the package dependency tree depth-first. This ensures
       ;; any load-order constraints in package autoloads are always met.
       (let (packages)
         (letf! (defun* walk-packages (pkglist)
                  (cond ((null pkglist) nil)
                        ((stringp pkglist)
                         (walk-packages (nth 1 (gethash pkglist straight--build-cache)))
                         (cl-pushnew pkglist packages :test #'equal))
                        ((listp pkglist)
                         (mapc #'walk-packages (reverse pkglist)))))
           (walk-packages (mapcar #'symbol-name (mapcar #'car zenit-packages))))
         (mapcar #'straight--autoloads-file (nreverse packages)))
       zenit-autoloads-excluded-files
       'literal)
    ,@(when-let* ((info-dirs
                   (cl-loop for dir in load-path
                            if (file-exists-p (zenit-path dir "dir"))
                            collect dir)))
        `((with-eval-after-load 'info
            (info-initialize)
            (dolist (path ',(delete-dups info-dirs))
              (add-to-list 'Info-directory-list path)))))))

(defun zenit-autoloads-reload (&optional file)
  "Regenerates autoloads and writes them to FILE."
  (unless file
    (setq file zenit-config-init-file))
  (print! (start "(Re)generating init file..."))
  (print-group!
    (cl-check-type file string)
    (zenit-initialize-packages)
    (let ((init-dir zenit-autogen-dir))
      (with-file-modes #o750
        (print-group!
          (make-directory init-dir t)
          (let ((auto-files (zenit-glob init-dir "*.auto.el")))
            (print! (start "Generating %d init files...") (length zenit-init-generators))
            (print-group! :level 'info
              (dolist (file auto-files)
                (print! (item "Deleting %s...") file)
                (delete-file file))
              (pcase-dolist (`(,file . ,fn) zenit-init-generators)
                (let ((file (file-name-concat init-dir file)))
                  (zenit-log "Building %s..." file)
                  (zenit-file-write file (concat "\n;;;; START " file " ;;;;\n") :printfn #'prin1)
                  (zenit-file-write file (funcall fn) :printfn #'prin1 :append t)
                  (zenit-file-write file (concat "\n;;;; END " file " ;;;;\n") :printfn #'prin1 :append t)))))
          (with-file! file
            (insert ";; -*- coding: utf-8; lexical-binding: t; -*-\n"
                    ";; This file was autogenerated; do not edit it by hand!\n")
            (prin1 `(unless (equal emacs-major-version ,emacs-major-version)
                      (signal 'zenit-error
                              (list "The installed version of Emacs has changed since last refresh")))
                   (current-buffer))
            (dolist (file (zenit-glob init-dir "*.el"))
              (print-group! :level 'info
                (print! (start "Reading %s...") file))
              (zenit-file-read file :by 'insert)))
          (print! (start "Byte-compiling %s...")
                  (relpath file zenit-emacs-dir))
          (zenit-autoloads--compile-file file)
          (print! (success "Generated %s")
                  (relpath (byte-compile-dest-file file)
                           zenit-emacs-dir)))))))

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

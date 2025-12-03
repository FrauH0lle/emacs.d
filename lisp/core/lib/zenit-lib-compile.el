;; lisp/core/lib/zenit-lib-compile.el -*- lexical-binding: t; no-byte-compile: t; -*-

(autoload #'async-inject-variables "async")
(autoload #'async-get "async")

;;;###autoload
(defun zenit-compile-generate-args (target)
  "Generate compilation arguments specific for TARGET.

TARGET is the file path to be compiled. This function determines
which core libraries and dependencies need to be loaded before
compiling the target file.

Returns a plist with the following possible keys:
  :req-core-lib   - Whether to require zenit-lib
  :req-core       - Whether to require zenit-core
  :req-core-libs  - List of core libraries to require
  :req-extra      - List of extra libraries to require
  :modulep        - Whether to generate module information
  :autoloads      - Whether to load autoloads files"
  (cond
   ;; Core library files that don't need any special handling
   ((string-suffix-p "zenit-compat.el" target)
    nil)
   ((string-suffix-p "zenit-lib.el" target)
    nil)
   ((string-suffix-p "zenit-core.el" target)
    nil)

   ;; Files in core/lib directory only need zenit-core
   ((file-in-directory-p target (file-name-concat zenit-core-dir "lib/"))
    '(:req-core t))

   ;; Core system files that require zenit-core
   ((string-suffix-p "zenit-start.el" target)
    '(:req-core t))
   ((string-suffix-p "zenit-use-package.el" target)
    '(:req-core t))
   ((string-suffix-p "zenit-el-patch.el" target)
    '(:req-core t))

   ;; Feature-specific files that require additional dependencies
   ((string-suffix-p "zenit-keybinds.el" target)
    '(:req-core t :req-extra (zenit-use-package zenit-el-patch)))
   ((string-suffix-p "zenit-ui.el" target)
    '(:req-core t :req-extra (zenit-use-package)))
   ((string-suffix-p "zenit-projects.el" target)
    '(:req-core t :req-extra (zenit-use-package)))
   ((string-suffix-p "zenit-editor.el" target)
    '(:req-core t :req-extra (zenit-use-package)))

   ;; User configuration files that need full environment
   ((or (file-in-directory-p target (file-name-concat user-emacs-directory "lisp" "core/"))
        (file-in-directory-p target (file-name-concat user-emacs-directory "lisp" "modules/"))
        (file-in-directory-p target (file-name-concat user-emacs-directory "site-lisp"))
        (equal (expand-file-name target) (expand-file-name (file-name-concat user-emacs-directory "init.el"))))
    '(:req-core-lib t :req-core t :req-core-libs all
      :req-extra (cl-lib zenit-use-package zenit-el-patch
                         zenit-keybinds zenit-projects zenit-editor)
      :modulep t :autoloads t))

   ;; Default case for unknown files
   (t
    nil)))

(defun zenit-compile--generate-modules ()
  "Generate module configuration for compilation environment.

This function processes the current module configuration and
creates a form that sets up module properties for use during
compilation. The properties are stored in symbol plists for
efficient lookup by `modulep!'.

Returns a list of forms that set up module properties when
evaluated."
  (let ((config-modules-list (zenit-module-list)))
    ;; Cache module state and flags in symbol plists for quick lookup by
    ;; `modulep!' later.
    `(,@(cl-loop
         ;; Group modules by their category
         for (category . modules) in (seq-group-by #'car config-modules-list)
         collect
         `(setplist ',category
           (quote ,(cl-loop for (_ . module) in modules
                            ;; Collect module properties into plist
                            nconc `(,module ,(get category module)))))))))

;;;###autoload
(cl-defun zenit-compile-setup-env (&key
                                   (req-core-lib nil)
                                   (req-core nil)
                                   (req-core-libs nil)
                                   (req-extra nil)
                                   (modulep nil)
                                   (autoloads nil))
  "Set up the compilation environment with specified dependencies.

This function generates a form that sets up the necessary
environment for compiling Zenit files. It handles loading core
libraries, extra dependencies, module configuration, and
autoloads as specified by the keyword arguments.

Keyword arguments:
:req-core-lib   - Whether to require zenit-lib
:req-core       - Whether to require zenit-core

:req-core-libs  - List of core libraries to require (or \\='all for all
                  core libs)

:req-extra      - List of extra libraries to require
:modulep        - Whether to generate module configuration
:autoloads      - Whether to load autoloads files

Returns a progn form that sets up the compilation environment
when evaluated."
  `(progn
     ;; Load zenit-lib if requested
     ,(when req-core-lib
        `(require 'zenit-lib))
     
     ;; Load zenit-core if either core or core libs are needed
     ,(when (or req-core req-core-libs)
        `(require 'zenit-core))
     
     ;; Load specified core libraries
     ,@(when req-core-libs
         (cond ((eq req-core-libs 'all)
                ;; Load all core libraries from lib directory
                (cl-loop for lib in (zenit-files-in
                                     (file-name-concat zenit-core-dir "lib")
                                     :match ".el$")
                         collect `(zenit-require
                                   'zenit-lib
                                   ',(intern
                                      (string-remove-prefix
                                       "zenit-lib-" (file-name-base lib))))))
               (t
                ;; Load specific core libraries
                (cl-loop for lib in (ensure-list req-core-libs)
                         collect `(zenit-require 'zenit-lib ',lib)))))
     
     ;; Load any extra required libraries
     ,@(when req-extra
         (cl-loop for lib in (ensure-list req-extra)
                  collect `(require ',lib)))
     
     ;; Generate module configuration if needed
     ,@(when modulep
         (zenit-compile--generate-modules))

     ;; Load autoloads files if requested
     ,@(when autoloads
         (let (forms)
           (dolist (file (zenit-glob zenit-bootstrap-dir "*loaddefs*.el") forms)
             (when (file-exists-p file)
               (push `(load ,file) forms)))
           (nreverse forms)))
     
     ;; Push compile context if zenit-core is loaded
     (when (featurep 'zenit-core)
       (zenit-context-push 'compile))

     ;; Prevent packages from being loaded at compile time if they don't meet
     ;; their own predicates.
     (with-eval-after-load 'use-package
       (push (list :no-require t
                   (lambda (_name args)
                     (or (when-let* ((pred (or (plist-get args :if)
                                               (plist-get args :when))))
                           (not (eval pred t)))
                         (when-let* ((pred (plist-get args :unless)))
                           (eval pred t)))))
             use-package-defaults))))

;;;###autoload
(cl-defun zenit-async-byte-compile-file
    (file &key
          (req-core-lib nil)
          (req-core nil)
          (req-core-libs nil)
          (req-extra nil)
          (modulep nil)
          (autoloads nil)
          (warnings (if init-file-debug
                        byte-compile-warnings
                      '(not make-local noruntime))))
  "Byte compile Lisp code FILE asynchronously.

This function compiles a Lisp file in a separate Emacs process,
setting up the required Zenit environment before compilation. It
handles dependency loading, module configuration, and autoloads
as specified by the keyword arguments.

Arguments:
FILE - The Lisp file to compile

Keyword arguments:
:req-core-lib   - Whether to require zenit-lib
:req-core       - Whether to require zenit-core
:req-core-libs  - List of core libraries to require (or \\='all for all
                  core libs)
:req-extra      - List of extra libraries to require
:modulep        - Whether to generate module configuration
:autoloads      - Whether to load autoloads files
:warnings       - List of byte-compile warnings to
                  enable (defaults to basic warnings)

Returns nil immediately, with compilation happening in the
background. Compilation results are logged to
`async-byte-compile-log-file'."
  ;; Ensure async-bytecomp is loaded for asynchronous compilation
  (require 'async-bytecomp)
  
  ;; Start asynchronous compilation process
  (async-start
   `(lambda ()
      ;; Set up byte compilation environment
      (require 'bytecomp)
      
      ;; Inject necessary variables into the async process
      ,(async-inject-variables async-bytecomp-load-variable-regexp)
      ,(async-inject-variables "\\`zenit-modules\\'")
      ,(async-inject-variables "\\`zenit-disabled-packages\\'")
      
      ;; Configure compilation settings
      (setq load-prefer-newer t)
      (setq byte-compile-warnings ',warnings)
      
      ;; Set up Zenit compilation environment
      ,(zenit-compile-setup-env
        :req-core-lib req-core-lib :req-core req-core
        :req-core-libs req-core-libs :req-extra req-extra
        :modulep modulep :autoloads autoloads)
      
      ;; Compile the file and handle results
      (let ((default-directory ,default-directory)
            error-data status compiler-log)
        ;; Perform the actual compilation
        (pcase (byte-compile-file ,file)
          (`no-byte-compile (setq status 'no-byte-compile))
          (`nil (setq status nil))
          (_ (setq status t)))
        
        ;; Capture and process compilation errors
        (when (get-buffer byte-compile-log-buffer)
          (setq error-data (with-current-buffer byte-compile-log-buffer
                             (buffer-substring-no-properties (point-min) (point-max))))
          ;; If there were errors, format them and append to log file
          (unless (string= error-data "")
            (with-temp-buffer
              (insert error-data ?\n)
              (goto-char (point-min))
              (insert ,file ":\n")
              (setq compiler-log (buffer-string))
              (append-to-file (point-min) (point-max) ,async-byte-compile-log-file))))
        
        ;; Return compilation status and log
        (cons status compiler-log)))
   nil))

(provide 'zenit-lib '(compile))

;; lisp/core/cli/compile.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defun zenit--byte-compile-ignore-file-p (path)
  (let ((filename (file-name-nondirectory path)))
    (or (string-prefix-p "." filename)
        (string-prefix-p "test-" filename)
        (string-suffix-p ".example.el" filename)
        (string-prefix-p "flycheck_" filename)
        (not (equal (file-name-extension path) "el"))
        (not (zenit-file-cookie-p path "if" t))
        (member filename
                (list
                 ;; These module file should be either embedded or ignored
                 "init.el" "config.el" "packages.el"
                 ;; Ignore
                 "custom.el"
                 ;; Core files which are not used in the interactive session
                 "zenit-cli.el" "zenit-packages.el")))))

(defun zenit-cli-compile-setup-env ()
  "Load required files for the byte-compilation."
  (dolist (cache-file (butlast (mapcar #'car zenit-cache-generators) 1))
    (load (file-name-concat zenit-local-dir cache-file) nil (not init-file-debug)))
  ;; Setup environment for compilation
  (require 'zenit-start)
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

(cl-defun zenit-cli-compile ()
  "Byte and native compiles your emacs configuration."
  (let ((default-directory zenit-emacs-dir)
        (byte-compile-verbose init-file-debug)
        (byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

        ;; In case it is changed during compile-time
        (auto-mode-alist auto-mode-alist)

        (targets (list (zenit-glob zenit-core-dir "zenit-core.el"))))

    ;; But first we must be sure that configs have been fully loaded. Which
    ;; usually aren't so in an noninteractive session.
    (let ((load-prefer-newer t)
          kill-emacs-query-functions
          kill-emacs-hook)
      (zenit-cli-compile-setup-env)
      (zenit-initialize-packages))

    (delq nil targets)
    ;; Assemble el files we want to compile
    (appendq! targets
              (append
               ;; Collect files in core dir
               (zenit-files-in zenit-core-dir
                               :match "\\.el$"
                               :filter #'zenit--byte-compile-ignore-file-p
                               :depth 0)
               ;; Collect core autoloads
               (zenit-files-in (file-name-concat zenit-core-dir "lib/")
                               :match "\\.el$"
                               :filter #'zenit--byte-compile-ignore-file-p
                               :depth 0)))

    (unless targets
      (print!
       (if targets
           (warn "Couldn't find any valid targets")
         (item "No targets to compile")))
      (cl-return nil))

    (print!
     (item "Compiling your config (may take a while)..."))
    (print-group!
     (condition-case e
         (let ((total-ok   0)
               (total-fail 0)
               (total-noop 0)
               kill-emacs-hook kill-buffer-query-functions)

           (dolist (target (delete-dups (delq nil targets)))
             (when init-file-debug
               (print! (start "Byte-compiling %s") (relpath target)))
             (cl-incf
              (if (let ((elc-file (byte-compile-dest-file target)))
                    (and (file-exists-p elc-file)
                         (file-newer-than-file-p elc-file target)))
                  total-noop
                (pcase (byte-compile-file target)
                  (`no-byte-compile
                   (print! (item "Ignored %s") (relpath target))
                   total-noop)
                  (`nil
                   (print! (error "Failed to compile %s") (relpath target))
                   total-fail)
                  (_
                   (print! (success "Compiled %s") (relpath target))
                   (load target t t)
                   total-ok)))))
           (print! (class (if (= total-fail 0) 'success 'error)
                          "%s %d/%d file(s) (%d ignored)")
                   "Compiled"
                   total-ok (- (length targets) total-noop)
                   total-noop)
           t)
       ((debug error)
        (print! (error "\nThere were breaking errors.\n\n%s")
                "Reverting changes...")
        (signal 'zenit-error (list 'byte-compile e)))))))

(defun zenit-cli-clean-compiled-files ()
  "Delete all the compiled elc eln files in your Emacs configuration
and private module.

This does not include third party packages.'"
  (print! (start "Cleaning .elc and .eln files"))
  (print-group!
   (cl-loop with default-directory = zenit-emacs-dir
            with success = nil
            for path
            in (append (zenit-files-in zenit-local-conf-dir :match "\\.el$")
                       (zenit-files-in zenit-core-dir :match "\\.el$")
                       (zenit-files-in zenit-modules-dirs :match "\\.el$"))
            if (file-exists-p (byte-compile-dest-file path))
            do (delete-file (byte-compile-dest-file path))
            and do (print! (success "Deleted %s") (relpath (byte-compile-dest-file path)))
            and do (setq success t)
            if (file-exists-p (zenit--eln-output-file (zenit--eln-file-name path)))
            do (delete-file (zenit--eln-output-file (zenit--eln-file-name path)))
            and do (print! (success "Deleted %s") (relpath (zenit--eln-output-file (zenit--eln-file-name path))))
            finally do
            (print! (if success
                        (success "All elc and eln files deleted")
                      (item "No files to clean"))))))

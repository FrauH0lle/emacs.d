;; lisp/core/lib/byte-compile.el -*- lexical-binding: t; -*-

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

;;;###autoload
(defun zenit-config-byte-compile (&optional report-progress)
  "Byte-compile configuration. For interactive usage.
REPORT-PROGRESS non-nil (or interactively) means to print more
messages."
  (interactive (list 'report-progress))
  (cl-block nil
    ;; Check if any .el file the relevant directories was updated and if yes,
    ;; trigger the compilation.
    (unless (cl-loop with outdated = nil
                     for file
                     in (append
                         ;; Collect files in core dir
                         (zenit-files-in zenit-core-dir
                                         :match "\\.el$"
                                         :filter #'zenit--byte-compile-ignore-file-p
                                         :depth 0)
                         ;; Collect core autoloads
                         (zenit-files-in (file-name-concat zenit-core-dir "lib/")
                                         :match "\\.el$"
                                         :filter #'zenit--byte-compile-ignore-file-p
                                         :depth 0)
                         ;; Collect files in modules dir
                         ;; (zenit-files-in (seq-filter
                         ;;                  ;; Only compile activated modules,
                         ;;                  ;; currently excluding local ones
                         ;;                  (zenit-rpartial #'file-in-directory-p zenit-emacs-dir)
                         ;;                  (zenit-module-load-path))
                         ;;                 :match "\\.el$"
                         ;;                 :filter #'zenit--byte-compile-ignore-file-p)
                         ;; TODO Collect files in local conf dir, but this be
                         ;; behind a switch
                         )
                     for elc-file = (concat file "c")
                     if (file-newer-than-file-p file elc-file)
                     do (delete-file elc-file)
                     if (bound-and-true-p native-comp-eln-load-path)
                     do (let ((eln-file (concat (car (bound-and-true-p native-comp-eln-load-path))
                                                (concat comp-native-version-dir "/"
                                                        (file-name-nondirectory
                                                         (comp-el-to-eln-filename file))))))
                          (when (file-exists-p eln-file)
                            (delete-file eln-file)))
                     and do (setq outdated t)
                     finally return outdated)
      (when report-progress
        (print! (success "Byte-compiled configuration already up to date")))
      (cl-return))
    (when report-progress
      (print! (start "Byte-compiling configuration...")))

    (let* ((evil-collection-mode-list nil)
           (default-directory zenit-emacs-dir)
           (buf (get-buffer-create " *zenit-conf-compile*"))
           (zenit-format-backend 'ansi)
           (ignore-window-parameters t)
           (noninteractive t))
      (with-current-buffer buf
        (erase-buffer)
        (make-process
         :name "zenit-config-compile"
         :buffer "*zenit-conf-compile*"
         :command '("make" "compile")
         :noquery t
         :sentinel
         (lambda (proc _event)
           (unless (process-live-p proc)
             (with-current-buffer (process-buffer proc)
               (require 'ansi-color)
               (ansi-color-apply-on-region (point-min) (point-max))
               (if (= 0 (process-exit-status proc))
                   (progn
                     (message
                      (if report-progress
                          "Byte-compiling updated configuration...done. Restart emacs for changes to take effect."
                        "Byte-compiled updated configuration. Restart emacs for changes to take effect.")))
                 (message "Failed to byte-compile"))))))))))

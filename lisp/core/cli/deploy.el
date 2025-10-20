;; lisp/core/cli/deploy.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defun zenit-cli-deploy (&optional config-p envfile-p fonts-p)
  (print! (green "Deploying Emacs config!\n"))

  ;; Create `zenit-local-conf-dir'
  (if (eq config-p :no)
      (print! (warn "Skipping local config template, as requested"))
    (if (file-directory-p zenit-local-conf-dir)
        (print! (item "Skipping %s (already exists)") (path zenit-local-conf-dir))
      (make-directory zenit-local-conf-dir 'parents)
      (print! (success "Created %s") (path zenit-local-conf-dir)))

    ;; Create init.el, config.el and packages.el
    (print-group!
      (mapc (lambda (file)
              (cl-destructuring-bind (filename . template) file
                (setq filename (zenit-path zenit-local-conf-dir filename))
                (if (file-exists-p filename)
                    (print! (item "Skipping %s (already exists)...") (path filename))
                  (print! (item "Creating %s...") (path filename))
                  (with-temp-file filename (insert template))
                  (print! (success "Done!")))))
            `((,zenit-module-init-file
               . ,(concat
                   (format ";; %sinit.el -*- lexical-binding: t; -*-\n" (abbreviate-file-name zenit-local-conf-dir))
                   "\n"
                   ";; The place to declare your module configuration.\n"
                   ";; Remember to run 'emacs-config refresh' after modifying it!\n\n"
                   "(modules!\n"
                   " :completion"
                   " vertico)\n"))
              (,zenit-module-config-file
               . ,(concat
                   (format ";; %sconfig.el -*- lexical-binding: t; -*-\n" (abbreviate-file-name zenit-local-conf-dir))
                   "\n"
                   ";; The place for your local configuration.\n\n"
                   ";; Optional user identification:\n"
                   ";; (setq user-full-name \"John Doe\"\n"
                   ";;       user-mail-address \"john@doe.com\")\n"))
              (,zenit-module-packages-file
               . ,(concat
                   (format ";; %spackages.el -*- lexical-binding: t; -*-\n" (abbreviate-file-name zenit-local-conf-dir))
                   "\n"
                   ";; The place for your local package declaration.\n"))))))

  ;; Env file
  (if (eq envfile-p :no)
      (print! (warn "Skipping envvars file generation, as requested"))
    (if (file-exists-p zenit-env-file)
        (print! (item "Envvar file already exists, skipping"))
      (when (or zenit-cli-auto-accept (y-or-n-p "Generate an envvar file?"))
        (zenit-cli-reload-env-file))))

  ;; Initialize packages
  (print! "Installing packges")
  (zenit-cli-packages-ensure)

  (print! "Regenerating autoloads files")
  (zenit-autoloads-reload)

  (when (zenit-module-p :config 'compile)
    (zenit-cli-compile))

  ;; Install `nerd-icons' fonts
  (if (eq fonts-p :no)
      (print! (warn "Skipping nerd-icon's fonts installation, as requested"))
    (when (or zenit-cli-auto-accept
              (y-or-n-p "Download and install nerd-icon's fonts?"))
      (require 'nerd-icons)
      (let ((window-system (cond (zenit--system-macos-p 'ns)
                                 (zenit--system-linux-p 'x))))
        (nerd-icons-install-fonts 'yes))))

  (when (file-exists-p "~/.emacs")
    (print! (warn "A ~/.emacs file was detected. This should be deleted!")))

  (print! (success "Finished! Emacs is ready to go!\n"))
  (with-temp-buffer
    (insert
     (concat
      "Some things which are good to know:\n\n"
      "1. Run 'emacs-config refresh' and restart Emacs after making any changes to your init.el\n"
      "   or packages.el in ~/.emacs.d/site-lisp/.\n\n"
      "2. Use 'git pull' to update this config and run 'emacs-config sync -u' to update packages.\n\n"
      "3. Have fun :)\n"))
    (print! "%s" (buffer-string))))

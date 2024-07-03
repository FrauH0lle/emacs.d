;; lisp/core/cli/deploy.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defun zenit-cli-deploy (&optional debug-p)
  (when (or debug-p (getenv "DEBUG"))
    (zenit-debug-mode +1))

  (print! (green "Deploying Emacs config!\n"))
  ;; Env file
  (if (file-exists-p zenit-env-file)
      (print! (info "Envvar file already exists, skipping"))
    (when (or zenit-auto-accept
              (y-or-n-p "Generate an envvar file?"))
      (zenit-cli-reload-env-file 'force-p)))

  ;; Initialize packages
  (print! "Installing packges")
  (zenit-cli-packages-install)

  (print! "Regenerating autoloads files")
  (zenit-autoloads-reload)

  (when (modulep! :config compile)
    (zenit-cli-compile))

  ;; Install `nerd-icons' fonts
  (when (or zenit-auto-accept
            (y-or-n-p "Download and install nerd-icon's fonts?"))
    (require 'nerd-icons)
    (let ((window-system (cond (zenit--system-macos-p 'ns)
                               (zenit--system-linux-p 'x))))
      (nerd-icons-install-fonts 'yes)))

  (when (file-exists-p "~/.emacs")
    (print! (warn "A ~/.emacs file was detected. This should be deleted!")))

  (print! (success "Finished! Emacs is ready to go!\n")))

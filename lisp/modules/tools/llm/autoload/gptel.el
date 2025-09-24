;; tools/llm/autoload/gptel.el -*- lexical-binding: t; -*-

;; From `gptel'
;;;###autoload
(defun +llm-api-key-from-auth-source (host &optional user)
  "Lookup api key for HOST in the auth source.
By default \"apikey\" is used as USER."
  (if-let* ((secret
             (plist-get
              (car (auth-source-search
                    :host host
                    :user (or user "apikey")
                    :require '(:secret)))
              :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)
    (user-error "No API key for %s found in the auth source" host)))

;;;###autoload
(defun +llm/aider-set-model (model)
  "Set the MODEL for Aider.
This function prompts the user to choose a model from a
predefined list, retrieves the corresponding API key from the
auth source, and sets the environment variable with the API key.
It also updates the Aider arguments with the selected model's
configuration."
  (interactive
   (list
    ;; Prompt the user to choose a model from a predefined list
    (completing-read "Choose model: " (mapcar #'car +llm-aider-arg-list) nil t)))
  ;; Retrieve the configuration plist for the selected model
  (let* ((plist (alist-get model +llm-aider-arg-list nil nil #'equal))
         ;; Extract the arguments, host, user, and environment variable name for
         ;; the API key lookup
         (args (plist-get plist :args))
         (host (plist-get plist :host))
         (user (plist-get plist :user))
         (env (plist-get plist :env)))
    ;; Set the environment variable with the API key
    (setenv env (+llm-api-key-from-auth-source host user))
    ;; Update the Aider arguments with the selected model's configuration
    (setq aidermacs-args args)))

;;;###autoload
(defun +gptel/commit-summary ()
  "Insert a commit message header line in the format I use.

The header is followed by a standard magit (GNU style) changelog.

Don't get the LLM to write the commit message itself, because it's bad
at inferring my intent.

Intended to be placed in `git-commit-setup-hook'."
  (interactive)
  (require 'gptel)
  (gptel-with-preset 'commit-summary
    ;; Commit message buffer
    (let ((commit-buffer (current-buffer)))
      ;; Heuristic for blank message
      (when (looking-at-p "[\n[:blank:]]+")
        (with-temp-buffer
          ;; Insert diff
          (vc-git-command
           (current-buffer) 1 nil
           "diff-index" "--exit-code" "--patch"
           (and (magit-anything-staged-p) "--cached")
           "HEAD" "--")

          ;; Run request on diff buffer contents
          (gptel-request nil
            :context commit-buffer
            :callback
            (lambda (resp info)
              (if (not (stringp resp))
                  (message "Git commit summary generation failed")
                (with-current-buffer (plist-get info :context)
                  (save-excursion
                    (goto-char (point-min))
                    (insert resp "\n\n")
                    (magit-generate-changelog)))))))))))

;; tools/llm/autoload.el -*- lexical-binding: t; -*-

;; From `gptel'
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
predefined list, retrieves the corresponding API key from the auth
source, and sets the environment variable with the API key. It
also updates the Aider arguments with the selected model's
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
    (setq aider-args args)))

;; tools/llm/autoload/glm.el -*- lexical-binding: t; -*-

(defvar glm-models '((glm-5
                      :description "High Performance, Strong Reasoning, More Versatile"
                      :capabilities (tool-use reasoning)
                      :context-window 200
                      :input-cost 1
                      :output-cost 3.2)
                     (glm-4.7
                      :description "High Performance, Strong Reasoning, More Versatile"
                      :capabilities (tool-use reasoning)
                      :context-window 200
                      :input-cost 0.6
                      :output-cost 2.2)
                     (glm-4.7-flash
                      :description "Lightweight, High Performance"
                      :capabilities (tool-use)
                      :context-window 200
                      :input-cost 0
                      :output-cost 0)))

(cl-defstruct (gptel-glm-openai
               (:include gptel-openai)
               (:copier nil)
               (:constructor gptel--make-glm-openai)))

;;;###autoload
(cl-defun gptel-make-glm-openai
    (name &key curl-args stream key request-params
          (header (lambda () (when-let* ((key (gptel--get-api-key)))
                               `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.z.ai")
          (protocol "https")
          (endpoint "/api/paas/v4/chat/completions")
          (models glm-models))
  "Register a GLM backend for gptel with NAME.

For the meanings of the keyword arguments, see `gptel-make-openai'."
  (declare (indent 1))
  (let ((backend (gptel--make-glm-openai
                  :name name
                  :host host
                  :header header
                  :key key
                  :models (gptel--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :curl-args curl-args
                  :url (concat protocol "://" host endpoint))))
    (setf (alist-get name gptel--known-backends nil nil #'equal) backend)
    backend))

(cl-defmethod gptel--parse-buffer :around ((_backend gptel-glm-openai) _max-entries)
  "Merge split tool calls into single assistant messages.

The GLM API with preserved thinking requires all tool_calls from
one model turn in a single assistant message, with
reasoning_content preserved."
  (let ((prompts (cl-call-next-method))
        (result nil))
    (while prompts
      (let ((msg (car prompts)))
        (if (and (equal (plist-get msg :role) "assistant")
                 (plist-get msg :tool_calls))
            ;; Merge consecutive assistant(tool_calls)+tool groups
            (let ((all-tool-calls
                   (append (plist-get msg :tool_calls) nil))
                  (reasoning (plist-get msg :reasoning_content))
                  (content (plist-get msg :content))
                  (tool-results nil))
              (setq prompts (cdr prompts))
              (catch 'done
                (while prompts
                  (let ((next (car prompts)))
                    (cond
                     ((equal (plist-get next :role) "tool")
                      (push next tool-results)
                      (setq prompts (cdr prompts)))
                     ((and (equal (plist-get next :role) "assistant")
                           (plist-get next :tool_calls))
                      (setq all-tool-calls
                            (append all-tool-calls
                                    (append (plist-get next :tool_calls) nil)))
                      (unless reasoning
                        (setq reasoning (plist-get next :reasoning_content)))
                      (unless content
                        (setq content (plist-get next :content)))
                      (setq prompts (cdr prompts)))
                     (t (throw 'done nil))))))
              ;; Build merged assistant message
              (let ((merged (list :role "assistant"
                                  :tool_calls (vconcat all-tool-calls))))
                (when content
                  (plist-put merged :content content))
                (when reasoning
                  (plist-put merged :reasoning_content reasoning))
                (push merged result))
              ;; Add tool results in original order
              (dolist (tr (nreverse tool-results))
                (push tr result)))
          (push msg result)
          (setq prompts (cdr prompts)))))
    (nreverse result)))

(cl-defstruct (gptel-glm-anthropic
               (:include gptel-anthropic)
               (:copier nil)
               (:constructor gptel--make-glm-anthropic)))

;;;###autoload
(cl-defun gptel-make-glm-anthropic
    (name &key curl-args stream key request-params
          (header (lambda () (when-let* ((key (gptel--get-api-key)))
                               `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.z.ai")
          (protocol "https")
          (endpoint "/api/anthropic/v1/messages")
          (models glm-models))
  "Register a GLM backend for gptel with NAME.

For the meanings of the keyword arguments, see `gptel-make-anthropic'."
  (declare (indent 1))
  (let ((backend (gptel--make-glm-anthropic
                  :name name
                  :host host
                  :header header
                  :key key
                  :models (gptel--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :curl-args curl-args
                  :url (concat protocol "://" host endpoint))))
    (setf (alist-get name gptel--known-backends nil nil #'equal) backend)
    backend))

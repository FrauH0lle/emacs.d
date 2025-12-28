;; tools/llm/autoload/glm.el -*- lexical-binding: t; -*-

(cl-defstruct (gptel-glm (:include gptel-openai)
                         (:copier nil)
                         (:constructor gptel--make-glm)))

;;;###autoload
(cl-defun gptel-make-glm
    (name &key curl-args stream key request-params
          (header (lambda () (when-let* ((key (gptel--get-api-key)))
                               `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.z.ai")
          (protocol "https")
          (endpoint "/api/paas/v4/chat/completions")
          (models '((glm-4.7
                     :description "High Performance, Strong Reasoning, More Versatile"
                     :capabilities (tool-use reasoning)
                     :context-window 200
                     :input-cost 0.6
                     :output-cost 2.2)
                    (glm-4.5-x
                     :description "High Performance, Strong Reasoning, Ultra-Fast Response"
                     :capabilities (tool-use reasoning)
                     :context-window 128
                     :input-cost 2.2
                     :output-cost 8.9)
                    (glm-4.5-air
                     :description "Cost-Effective, Lightweight, High Performance"
                     :capabilities (tool-use)
                     :context-window 128
                     :input-cost 0.2
                     :output-cost 1.1)
                    (glm-4.5-airx
                     :description "Lightweight, High Performance, Ultra-Fast Response"
                     :capabilities (tool-use)
                     :context-window 128
                     :input-cost 1.1
                     :output-cost 4.5)
                    (glm-4.5-flash
                     :description "Lightweight, High Performance"
                     :capabilities (tool-use)
                     :context-window 128
                     :input-cost 0
                     :output-cost 0))))
  "Register a GLM backend for gptel with NAME.

For the meanings of the keyword arguments, see `gptel-make-openai'."
  (declare (indent 1))
  (let ((backend (gptel--make-glm
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

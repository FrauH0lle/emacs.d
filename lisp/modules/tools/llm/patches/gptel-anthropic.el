;; tools/llm/patches/gptel-anthropic.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'gptel-anthropic))

;; PATCH Support dedicated reasoning property (from gptel upstream)

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-anthropic) info)
  "Parse an Anthropic data stream.

Return the text response accumulated since the last call to this
function.  Additionally, mutate state INFO to add tool-use
information if the stream contains it.  Not my best work, I know."
  (let* ((content-strs)
         (pt (point)))
    (condition-case nil
        (while (re-search-forward "^event: " nil t)
          (setq pt (match-beginning 0))
          (if (equal (line-end-position) (point-max))
              (error "Data block incomplete"))
          (cond
           ((looking-at "content_block_delta") ;collect incremental
            (forward-line 1) (forward-char 5)  ;text, tool or thinking block
            (when-let* ((delta (plist-get (gptel--json-read) :delta)))
              (if-let* ((content (plist-get delta :text))
                        ((not (eq content :null))))
                  (push content content-strs) ;collect text
                (if-let* ((partial-json (plist-get delta :partial_json)))
                    (plist-put          ;collect partial tool input
                     info :partial_json
                     (cons partial-json (plist-get info :partial_json)))
                  (if-let* ((thinking (plist-get delta :thinking)))
                      (plist-put info :reasoning
                                 (concat (plist-get info :reasoning) thinking))
                    (if-let* ((signature (plist-get delta :signature)))
                        (plist-put info :signature signature)
                      ;; REVIEW: Why concatenate here?
                        ;; (plist-put info :signature
                        ;;            (concat (plist-get info :signature) signature))
                        ))))))

           ((looking-at "content_block_start") ;Is the following block text or tool-use?
            (forward-line 1) (forward-char 5)
            (when-let* ((cblock (plist-get (gptel--json-read) :content_block)))
              (pcase (plist-get cblock :type)
                ("text" (push (plist-get cblock :text) content-strs))
                ("tool_use" (plist-put info :tool-use
                                       (cons (list :id (plist-get cblock :id)
                                                   :name (plist-get cblock :name))
                                             (plist-get info :tool-use))))
                ("thinking" (plist-put info :reasoning (plist-get cblock :thinking))
                 (when-let* ((signature (plist-get cblock :signature)))
                   (plist-put info :signature signature))
                 (plist-put info :reasoning-block 'in)))))

           ((looking-at "content_block_stop")
            (cond
             ((plist-get info :partial_json)   ;End of tool block
              (condition-case-unless-debug nil ;Combine partial tool inputs
                  (let* ((args-json (apply #'concat (nreverse (plist-get info :partial_json))))
                         (args-decoded  ;Handle blank argument strings
                          (if (string-empty-p args-json)
                              nil (gptel--json-read-string args-json))))
                    ;; Add the input to the tool-call spec
                    (plist-put (car (plist-get info :tool-use)) :input args-decoded))
                ;; If there was an error in reading that tool, we ignore it:
                ;; TODO(tool) handle this error better
                (error (pop (plist-get info :tool-use)))) ;TODO: nreverse :tool-use list
              (plist-put info :partial_json nil))

             ((eq (plist-get info :reasoning-block) 'in) ;End of reasoning block
              (plist-put info :reasoning-block t)))) ;Signal end of reasoning stream to filter

           ((looking-at "message_delta")
            ;; collect stop_reason, usage_tokens and prepare tools
            (forward-line 1) (forward-char 5)
            (when-let* ((tool-use (plist-get info :tool-use))
                        (response (gptel--json-read)))
              (let* ((data (plist-get info :data))
                     (prompts (plist-get data :messages)))
                (plist-put ; Append a COPY of response text + tool-use to the prompts list
                 data :messages
                 (vconcat
                  prompts
                  `((:role "assistant"
                     :content ,(vconcat ;Insert any LLM text and thinking text
                                (and-let* ((reasoning (plist-get info :partial_reasoning)))
                                 `((:type "thinking" :thinking ,reasoning
                                    :signature ,(plist-get info :signature))))
                                (and-let* ((strs (plist-get info :partial_text)))
                                 `((:type "text" :text ,(apply #'concat (nreverse strs)))))
                                (mapcar (lambda (tool-call) ;followed by the tool calls
                                          (append (list :type "tool_use")
                                           (copy-sequence tool-call)))
                                 tool-use))))))
                (plist-put info :partial_text nil) ; Clear any captured text
                ;; Then shape the tool-use block by adding args so we can call the functions
                (mapc (lambda (tool-call)
                        (plist-put tool-call :args (plist-get tool-call :input))
                        (plist-put tool-call :input nil)
                        (plist-put tool-call :id (plist-get tool-call :id)))
                      tool-use))
              (plist-put info :output-tokens
                         (map-nested-elt response '(:usage :output_tokens)))
              (plist-put info :stop-reason
                         (map-nested-elt response '(:delta :stop_reason)))))))
      (error (goto-char pt)))
    (let ((response-text (apply #'concat (nreverse content-strs))))
      (unless (string-empty-p response-text)
        (plist-put info :partial_text
                   (cons response-text (plist-get info :partial_text))))
      (when (plist-get info :tools)
        (when-let* ((reasoning (plist-get info :reasoning)))
          (plist-put info :partial_reasoning
                     (concat (plist-get info :partial_reasoning) reasoning))))
      response-text)))

(cl-defmethod gptel--parse-buffer ((backend gptel-anthropic) &optional max-entries)
  (let ((prompts) (prev-pt (point)))
    (if (or gptel-mode gptel-track-response)
        (while (and (or (not max-entries) (>= max-entries 0))
                    (goto-char (previous-single-property-change
                                (point) 'gptel nil (point-min)))
                    (not (= (point) prev-pt)))
          ;; HACK Until we can find a more robust solution for editing
          ;; responses, ignore prompts containing only whitespace, as the
          ;; Anthropic API can't handle it.  See #452, #409, #406, #351 and #321
          ;; We check for blank prompts by skipping whitespace and comparing
          ;; point against the previous.
          (unless (save-excursion (skip-syntax-forward " ") (>= (point) prev-pt))
            (pcase (get-char-property (point) 'gptel)
              ;; Reasoning block (Anthropic thinking): prepend to following assistant message
              (`(reasoning . ,signature)
               (let* ((raw-content (buffer-substring-no-properties (point) prev-pt))
                      ;; Check if we're merging with an existing thinking block
                      (merging-p (and prompts
                                      (equal (plist-get (car prompts) :role) "assistant")
                                      (let ((old-content (plist-get (car prompts) :content)))
                                        (and (vectorp old-content)
                                             (> (length old-content) 0)
                                             (equal (plist-get (aref old-content 0) :type) "thinking")
                                             (equal (plist-get (aref old-content 0) :signature)
                                                    (or signature nil))))))
                      ;; Only trim if not merging (to preserve spaces at boundaries)
                      (content (if merging-p
                                   raw-content
                                 (gptel--trim-prefixes raw-content))))
                 (unless (string-blank-p content)
                   ;; Check if the most recent message is an assistant message
                   ;; (we iterate backwards, so this is the one that follows
                   ;; reasoning in buffer order)
                   (if (and prompts
                            (equal (plist-get (car prompts) :role) "assistant"))
                       ;; Check if we can merge with existing thinking block
                       (let* ((msg (car prompts))
                              (old-content (plist-get msg :content)))
                         (if (and (vectorp old-content)
                                  (> (length old-content) 0)
                                  (equal (plist-get (aref old-content 0) :type) "thinking")
                                  (equal (plist-get (aref old-content 0) :signature)
                                         (or signature nil)))
                             ;; Merge with existing thinking block (prepend text, preserving spaces)
                             (plist-put (aref old-content 0) :thinking
                                        (concat content
                                                (plist-get (aref old-content 0) :thinking)))
                           ;; Create new thinking block and prepend
                           (let ((thinking-block
                                  `(:type "thinking" :thinking ,content
                                    :signature ,(or signature nil))))
                             (plist-put
                              msg :content
                              (if (vectorp old-content)
                                  (vconcat (vector thinking-block) old-content)
                                (vector thinking-block
                                        `(:type "text" :text ,old-content)))))))
                     ;; No assistant message yet - create one with just thinking
                     (push (list :role "assistant"
                                 :content (vector `(:type "thinking" :thinking ,content
                                                    :signature ,(or signature nil))))
                           prompts)))))
              ('response
               (when-let* ((content
                            (gptel--trim-prefixes
                             (buffer-substring-no-properties (point) prev-pt))))
                 (when (not (string-blank-p content))
                   (push (list :role "assistant" :content content) prompts))))
              (`(tool . ,id)
               (save-excursion
                 (condition-case nil
                     (let* ((tool-call (read (current-buffer)))
                            ;; (id (gptel--anthropic-format-tool-id id))
                            (name (plist-get tool-call :name))
                            (arguments (plist-get tool-call :args)))
                       (unless id (setq id (gptel--anthropic-format-tool-id nil)))
                       (plist-put tool-call :id id)
                       (plist-put tool-call :result
                                  (string-trim (buffer-substring-no-properties
                                                (point) prev-pt)))
                       (push (gptel--parse-tool-results backend (list tool-call))
                             prompts)
                       (push (list :role "assistant"
                                   :content `[( :type "tool_use" :id ,id :name ,name
                                                :input ,arguments)])
                             prompts))
                   ((end-of-file invalid-read-syntax)
                    (message (format "Could not parse tool-call %s on line %s"
                                     id (line-number-at-pos (point))))))))
              ('ignore)
              ('nil                     ; user role: possibly with media
               (if gptel-track-media
                   (when-let* ((content (gptel--anthropic-parse-multipart
                                         (gptel--parse-media-links major-mode (point) prev-pt))))
                     (when (> (length content) 0)
                       (push (list :role "user" :content content) prompts)))
                 (when-let* ((content (gptel--trim-prefixes
                                       (buffer-substring-no-properties (point) prev-pt))))
                   (push (list :role "user" :content content) prompts))))))
          (setq prev-pt (point))
          (and max-entries (cl-decf max-entries)))
      (when-let* ((content (string-trim (buffer-substring-no-properties
                                         (point-min) (point-max)))))
        ;; XXX fails if content is empty.  The correct error behavior is left to
        ;; a future discussion.
        (push (list :role "user" :content content) prompts)))
    ;; Cache messages if required: add cache_control to the last message
    (if (and (or (eq gptel-cache t) (memq 'message gptel-cache))
             (gptel--model-capable-p 'cache))
        (let ((last-message (plist-get (car (last prompts)) :content)))
          (if (stringp last-message)
              (plist-put
               (car (last prompts)) :content
               `[(:type "text" :text ,last-message
                  :cache_control (:type "ephemeral"))])
            (nconc (aref (plist-get (car (last prompts)) :content) 0)
                   '(:cache_control (:type "ephemeral"))))))
    prompts))

;; tools/llm/patches/gptel.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'el-patch)
  (require 'gptel))

;; PATCH Support dedicated reasoning property (from gptel upstream)

(el-patch-defun gptel-highlight--margin-prefix (type)
  (el-patch-concat "Create margin prefix string for TYPE.\n\nSupported TYPEs are response, ignore"
                   (el-patch-add ", reasoning")
                   " and tool calls.")
  (propertize ">" 'display
              `( (margin left-margin)
                 ,(propertize "▎" 'face
                              (pcase type
                                ('response 'gptel-response-fringe-highlight)
                                ('ignore 'shadow)
                                (el-patch-add
                                  (`(reasoning . ,_) 'gptel-response-fringe-highlight))
                                (`(tool . ,_) 'shadow))))))

(el-patch-defun gptel-highlight--fringe-prefix (type)
  (el-patch-concat "Create fringe prefix string for TYPE.\n\nSupported TYPEs are response, ignore"
                   (el-patch-add ", reasoning")
                   " and tool calls.")
  (propertize ">" 'display
              `( left-fringe gptel-highlight-fringe
                 ,(pcase type
                    ('response 'gptel-response-fringe-highlight)
                    ('ignore 'shadow)
                    (el-patch-add
                      (`(reasoning . ,_) 'gptel-response-fringe-highlight))
                    (`(tool . ,_) 'shadow)))))

(el-patch-defun gptel-highlight--decorate (ov &optional val)
  "Decorate gptel indicator overlay OV whose type is VAL."
  (overlay-put ov 'evaporate t)
  (overlay-put ov 'gptel-highlight t)
  (when (memq 'face gptel-highlight-methods)
    (overlay-put ov 'font-lock-face
                 (pcase val
                   ('response 'gptel-response-highlight)
                   ('ignore 'shadow)
                   (el-patch-add
                     (`(reasoning . ,_) 'gptel-response-highlight))
                   (`(tool . ,_) 'shadow))))
  (when-let* ((prefix
               (cond ((memq 'margin gptel-highlight-methods)
                      (gptel-highlight--margin-prefix (or val 'response)))
                     ((memq 'fringe gptel-highlight-methods)
                      (gptel-highlight--fringe-prefix (or val 'response))))))
    (overlay-put ov 'line-prefix prefix)
    (overlay-put ov 'wrap-prefix prefix)))

(el-patch-defun gptel-highlight--update (beg end)
  "JIT-lock function: mark gptel response/reasoning regions.

BEG and END delimit the region to refresh."
  (save-excursion                ;Scan across region for the gptel text property
    (let ((prev-pt (goto-char end)))
      (while (and (goto-char (previous-single-property-change
                              (point) 'gptel nil beg))
                  (/= (point) prev-pt))
        (pcase (get-char-property (point) 'gptel)
          ((and (or 'response 'ignore (el-patch-add `(reasoning . ,_)) `(tool . ,_)) val)
           (if-let* ((ov (or (cdr-safe (get-char-property-and-overlay
                                        (point) 'gptel-highlight))
                             (cdr-safe (get-char-property-and-overlay
                                        prev-pt 'gptel-highlight))))
                     (from (overlay-start ov)) (to (overlay-end ov)))
               (unless (<= from (point) prev-pt to)
                 (move-overlay ov (min from (point)) (max to prev-pt)))
             (gptel-highlight--decorate ;Or make new overlay covering just region
              (make-overlay (point) prev-pt nil t) val)))
          ('nil                     ;If there's an overlay, we need to split it.
           (when-let* ((ov (cdr-safe (get-char-property-and-overlay
                                      (point) 'gptel-highlight)))
                       (from (overlay-start ov)) (to (overlay-end ov)))
             (move-overlay ov from (point)) ;Move overlay to left side
             (gptel-highlight--decorate     ;Make a new one on the right
              (make-overlay prev-pt to nil t)
              (get-char-property prev-pt 'gptel)))))
        (setq prev-pt (point)))))
  `(jit-lock-bounds ,beg . ,end))

(el-patch-defun gptel--insert-response (response info &optional raw)
  "Insert the LLM RESPONSE into the gptel buffer.

INFO is a plist containing information relevant to this buffer.
See `gptel--url-get-response' for details.

Optional RAW disables text properties and transformation."
  (let* ((gptel-buffer (plist-get info :buffer))
         (start-marker (plist-get info :position))
         (tracking-marker (plist-get info :tracking-marker)))
    (pcase response
      ((pred stringp)                ;Response text
       (with-current-buffer gptel-buffer
         (when tracking-marker           ;separate from previous response
           (setq response (concat gptel-response-separator response)))
         (save-excursion
           (with-current-buffer (marker-buffer start-marker)
             (goto-char (or tracking-marker start-marker))
             ;; (run-hooks 'gptel-pre-response-hook)
             (unless (or (bobp) (plist-get info :in-place)
                         tracking-marker)
               (insert gptel-response-separator)
               (when gptel-mode
                 (insert (gptel-response-prefix-string)))
               (move-marker start-marker (point)))
             (unless raw
               (when-let* ((transformer (plist-get info :transformer)))
                 (setq response (funcall transformer response)))
               (add-text-properties
                0 (length response) '(gptel response front-sticky (gptel)) response))
             (insert response)
             (plist-put info :tracking-marker (setq tracking-marker (point-marker)))
             ;; for uniformity with streaming responses
             (set-marker-insertion-type tracking-marker t)))))
      (`(reasoning . ,text)
       (when-let* ((include (plist-get info :include-reasoning)))
         (if (stringp include)
             (with-current-buffer (get-buffer-create
                                   (plist-get info :include-reasoning))
               (save-excursion (goto-char (point-max)) (insert text)))
           (with-current-buffer (marker-buffer start-marker)
             (let ((separator         ;Separate from response prefix if required
                    (and (not tracking-marker) gptel-mode
                         (not (string-suffix-p "\n" (gptel-response-prefix-string)))
                         "\n"))
                   (blocks (if (derived-mode-p 'org-mode)
                               `("#+begin_reasoning\n" . ,(concat "\n#+end_reasoning"
                                                           gptel-response-separator))
                             ;; TODO(reasoning) remove properties and strip instead
                             (cons (propertize "``` reasoning\n" 'gptel 'ignore
                                               'keymap gptel--markdown-block-map)
                                   (concat (propertize "\n```" 'gptel 'ignore
                                                       'keymap gptel--markdown-block-map)
                                           gptel-response-separator)))))
               (if (eq include 'ignore)
                   (progn
                     (add-text-properties
                      0 (length text) '(gptel ignore front-sticky (gptel)) text)
                     (gptel--insert-response
                      (concat (car blocks) text (cdr blocks)) info t))
                 (gptel--insert-response (concat separator (car blocks)) info t)
                 (el-patch-remove
                   (gptel--insert-response text info))
                 (el-patch-add
                   ;; Apply reasoning property with signature
                   (let ((sig (plist-get info :signature)))
                     (add-text-properties
                      0 (length text)
                      `(gptel (reasoning . ,sig) front-sticky (gptel)) text))
                   (gptel--insert-response text info t))
                 (gptel--insert-response (cdr blocks) info t))
               (save-excursion
                 (goto-char (plist-get info :tracking-marker))
                 (if (derived-mode-p 'org-mode) ;fold block
                     (progn (search-backward "#+end_reasoning" start-marker t)
                            (when (looking-at "^#\\+end_reasoning")
                              (org-cycle)))
                   (when (re-search-backward "^```" start-marker t)
                     (gptel-markdown-cycle-block)))))))))
      (`(tool-call . ,tool-calls)
       (gptel--display-tool-calls tool-calls info))
      (`(tool-result . ,tool-results)
       (gptel--display-tool-results tool-results info)))))

(el-patch-defun gptel--display-reasoning-stream (text info)
  "Show reasoning TEXT in an appropriate location.

INFO is the request INFO, see `gptel--url-get-response'.  This is
for streaming responses only."
  (when-let* ((include (plist-get info :include-reasoning)))
    (if (stringp include)
        (unless (eq text t)
          (with-current-buffer (get-buffer-create include)
            (save-excursion (goto-char (point-max))
                            (insert text))))
      (let* ((reasoning-marker (plist-get info :reasoning-marker))
             (tracking-marker (plist-get info :tracking-marker))
             (start-marker (plist-get info :position)))
        (with-current-buffer (marker-buffer start-marker)
          (if (eq text t)               ;end of stream
              (progn
                (el-patch-add
                  ;; Update reasoning property with final signature.  All
                  ;; chunks share the same cached cons cell, so mutating it
                  ;; via `setcdr' updates every position in the buffer at once.
                  (when-let* ((sig (plist-get info :signature))
                              (cached (plist-get info :reasoning-prop)))
                    (let ((current-sig (cdr cached)))
                      (when (or (not current-sig)
                                (and (stringp current-sig) (string-empty-p current-sig)))
                        (setcdr cached sig))))
                  ;; Clear cached cons cell so the next reasoning block
                  ;; (after tool results) gets a fresh one.
                  (plist-put info :reasoning-prop nil))
                (gptel-curl--stream-insert-response
                 (concat (if (derived-mode-p 'org-mode)
                             "\n#+end_reasoning"
                           ;; TODO(reasoning) remove properties and strip instead
                           (propertize "\n```" 'gptel 'ignore
                                       'keymap gptel--markdown-block-map))
                         gptel-response-separator)
                 info t)
                (ignore-errors          ;fold block
                  (save-excursion
                    (goto-char tracking-marker)
                    (if (derived-mode-p 'org-mode)
                        (progn (search-backward "#+end_reasoning" start-marker t)
                               (when (looking-at "^#\\+end_reasoning")
                                 (org-cycle)))
                      (when (re-search-backward "^```" start-marker t)
                        (gptel-markdown-cycle-block))))))
            (unless (and reasoning-marker tracking-marker
                         (= reasoning-marker tracking-marker))
              (let ((separator        ;Separate from response prefix if required
                     (and (not tracking-marker) gptel-mode
                          (not (string-suffix-p
                                "\n" (gptel-response-prefix-string)))
                          "\n")))
                (gptel-curl--stream-insert-response
                 (concat separator
                         (if (derived-mode-p 'org-mode)
                             "#+begin_reasoning\n"
                           ;; TODO(reasoning) remove properties and strip instead
                           (propertize "``` reasoning\n" 'gptel 'ignore
                                       'keymap gptel--markdown-block-map)))
                 info t)))
            (if (eq include 'ignore)
                (progn
                  (add-text-properties
                   0 (length text) '(gptel ignore front-sticky (gptel)) text)
                  (gptel-curl--stream-insert-response text info t))
              (el-patch-remove
                (gptel-curl--stream-insert-response text info))
              (el-patch-add
                ;; Apply reasoning property (signature might be updated at end of
                ;; block).  Reuse the same cons cell so that adjacent chunks
                ;; are `eq' and `previous-single-property-change' treats
                ;; them as one contiguous region.
                (let ((reasoning-prop
                       (or (plist-get info :reasoning-prop)
                           (let* ((sig (plist-get info :signature))
                                  (sig (if (and (stringp sig) (string-empty-p sig))
                                           nil sig))
                                  (prop (cons 'reasoning sig)))
                             (plist-put info :reasoning-prop prop)
                             prop))))
                  (add-text-properties
                   0 (length text)
                   `(gptel ,reasoning-prop front-sticky (gptel)) text))
                (gptel-curl--stream-insert-response text info t))))
          (setq tracking-marker (plist-get info :tracking-marker))
          (if reasoning-marker
              (move-marker reasoning-marker tracking-marker)
            (plist-put info :reasoning-marker
                       (copy-marker tracking-marker nil))))))))

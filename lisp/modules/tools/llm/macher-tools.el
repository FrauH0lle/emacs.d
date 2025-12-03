;; tools/llm/macher-tools.el -*- lexical-binding: t; -*-

(defun +macher-tool--xref-find-references (context identifier file-path)
  "Find references to IDENTIFIER in the current session's project.
FILE-PATH specifies which file's buffer context to use for the search.
This function uses the session context to operate in the correct project."
  (require 'xref)
  (if (not file-path)
      (error "file_path parameter is required. Please specify the file where you want to search for %s" identifier)
    (let* ((workspace (macher-context-workspace context))
           (resolve-workspace-path (apply-partially #'macher--resolve-workspace-path workspace))
           (full-path (funcall resolve-workspace-path file-path))
           (target-buffer (or (find-buffer-visiting full-path)
                              (find-file-noselect full-path)))
           (identifier-str (format "%s" identifier)))

      (unless (file-exists-p full-path)
        (error "File %s does not exist in the workspace" file-path))

      (with-current-buffer target-buffer
        (condition-case err
            (let ((backend (xref-find-backend)))
              (if (not backend)
                  (format "No xref backend available for %s" file-path)
                (let ((xref-items (xref-backend-references backend identifier-str)))
                  (if xref-items
                      (mapcar (lambda (item)
                                (let* ((location (xref-item-location item))
                                       (file (xref-location-group location))
                                       (marker (xref-location-marker location))
                                       (line (with-current-buffer (marker-buffer marker)
                                               (save-excursion
                                                 (goto-char marker)
                                                 (line-number-at-pos))))
                                       (summary (xref-item-summary item)))
                                  (format "%s:%d: %s" file line summary)))
                              xref-items)
                    (format "No references found for '%s'" identifier-str)))))
          (error
           (format "Error searching for '%s' in %s: %s"
                   identifier-str file-path (error-message-string err))))))))

(defun +macher-tool--xref-find-apropos (context pattern file-path)
  "Find symbols matching PATTERN across the entire project.
FILE-PATH specifies which file's buffer context to use for the search.
This function uses the session context to operate in the correct
project."
  (require 'xref)
  (if (not file-path)
      (error "file_path parameter is required. Please specify the file where you want to search for pattern %s" pattern)

    (let* ((workspace (macher-context-workspace context))
           (resolve-workspace-path (apply-partially #'macher--resolve-workspace-path workspace))
           (full-path (funcall resolve-workspace-path file-path))
           (target-buffer (or (find-buffer-visiting full-path)
                              (find-file-noselect full-path)))
           (pattern-str (format "%s" pattern)))

      (unless (file-exists-p full-path)
        (error "File %s does not exist in the workspace" file-path))

      (with-current-buffer target-buffer
        (condition-case err
            (let ((backend (xref-find-backend)))
              (cond
               ((not backend)
                (format "No xref backend available for %s" file-path))
               ;; Special handling for etags without tags table
               ((and (eq backend 'etags)
                     (not (or (and (boundp 'tags-file-name) tags-file-name
                                   (file-exists-p tags-file-name))
                              (and (boundp 'tags-table-list) tags-table-list
                                   (cl-some #'file-exists-p tags-table-list)))))
                (format "No tags table available for %s" file-path))
               (t
                (let ((xref-items (xref-backend-apropos backend pattern-str)))
                  (if xref-items
                      (mapcar (lambda (item)
                                (let* ((location (xref-item-location item))
                                       (file (xref-location-group location))
                                       (marker (xref-location-marker location))
                                       (line (with-current-buffer (marker-buffer marker)
                                               (save-excursion
                                                 (goto-char marker)
                                                 (line-number-at-pos))))
                                       (summary (xref-item-summary item)))
                                  (format "%s:%d: %s" file line summary)))
                              xref-items)
                    (format "No symbols found matching pattern '%s'" pattern-str))))))
          (error
           (format "Error searching for pattern '%s' in %s: %s"
                   pattern-str file-path (error-message-string err))))))))

(defun +macher-tool--imenu-list-symbols (context file-path)
  "List all symbols in FILE-PATH using imenu.
Returns a list of symbols with their types and positions."
  (require 'imenu)
  (if (not file-path)
      (error "file_path parameter is required")

    (let* ((workspace (macher-context-workspace context))
           (resolve-workspace-path (apply-partially #'macher--resolve-workspace-path workspace))
           (full-path (funcall resolve-workspace-path file-path))
           (target-buffer (or (find-buffer-visiting full-path)
                              (find-file-noselect full-path))))

      (unless (file-exists-p full-path)
        (error "File %s does not exist in the workspace" file-path))

      (condition-case err
          (with-current-buffer target-buffer
            ;; Generate or update imenu index
            (imenu--make-index-alist)
            (if imenu--index-alist
                (let ((results '()))
                  ;; Process the imenu index
                  (dolist (item imenu--index-alist)
                    (cond
                     ;; Skip special entries
                     ((string-match-p "^\\*" (car item)) nil)
                     ;; Handle simple entries (name . position)
                     ((markerp (cdr item))
                      (let ((line (line-number-at-pos (marker-position (cdr item)))))
                        (push (format "%s:%d: %s"
                                      file-path
                                      line
                                      (car item))
                              results)))
                     ;; Handle position numbers
                     ((numberp (cdr item))
                      (let ((line (line-number-at-pos (cdr item))))
                        (push (format "%s:%d: %s"
                                      file-path
                                      line
                                      (car item))
                              results)))
                     ;; Handle nested entries (category . items)
                     ((listp (cdr item))
                      (let ((category (car item)))
                        (dolist (subitem (cdr item))
                          (when (and (consp subitem)
                                     (or (markerp (cdr subitem))
                                         (numberp (cdr subitem))))
                            (let ((line (line-number-at-pos
                                         (if (markerp (cdr subitem))
                                             (marker-position (cdr subitem))
                                           (cdr subitem)))))
                              (push (format "%s:%d: [%s] %s"
                                            file-path
                                            line
                                            category
                                            (car subitem))
                                    results))))))))
                  (if results
                      (nreverse results)
                    (format "No symbols found in %s" file-path)))
              (format "No imenu support or no symbols found in %s" file-path)))
        (error
         (format "Error listing symbols in %s: %s"
                 file-path (error-message-string err)))))))

(defun +macher-tool--treesit-format-tree (node level max-depth)
  "Format NODE and its children as a tree string.
LEVEL is the current indentation level.
MAX-DEPTH is the maximum depth to traverse."
  (if (or (not node) (>= level max-depth))
      ""
    (let* ((indent (make-string (* level 2) ?\s))
           (type (treesit-node-type node))
           (named (if (treesit-node-check node 'named) " (named)" ""))
           (start (treesit-node-start node))
           (end (treesit-node-end node))
           (field-name (treesit-node-field-name node))
           (field-str (if field-name (format " [%s]" field-name) ""))
           (text (treesit-node-text node t))
           (text-preview (if (and (< (length text) 40)
                                  (not (string-match-p "\n" text)))
                             (format " \"%s\"" text)
                           ""))
           (result (format "%s%s%s%s (%d-%d)%s\n"
                           indent type named field-str
                           start end text-preview))
           (child-count (treesit-node-child-count node)))
      ;; Add children
      (dotimes (i child-count)
        (when-let ((child (treesit-node-child node i)))
          (setq result (concat result
                               (+macher-tool--treesit-format-tree
                                child (1+ level) max-depth)))))
      result)))

(defun +macher-tool--treesit-line-column-to-point (line column)
  "Convert LINE and COLUMN to point position in current buffer.
LINE is 1-based, COLUMN is 0-based (Emacs convention)."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)
    (point)))

(defun +macher-tool--treesit-info (context file-path &optional line column whole_file include_ancestors include_children)
  "Get tree-sitter parse tree information for FILE-PATH.
Optional LINE and COLUMN specify the position (1-based line, 0-based column).
If WHOLE_FILE is non-nil, show the entire file's syntax tree.
If neither position is specified, defaults to current cursor position (point).
If INCLUDE_ANCESTORS is non-nil, include parent node hierarchy.
If INCLUDE_CHILDREN is non-nil, include child nodes."
  (if (not file-path)
      (error "file_path parameter is required")

    (let* ((workspace (macher-context-workspace context))
           (resolve-workspace-path (apply-partially #'macher--resolve-workspace-path workspace))
           (full-path (funcall resolve-workspace-path file-path))
           (target-buffer (or (find-buffer-visiting full-path)
                              (find-file-noselect full-path))))

      (unless (file-exists-p full-path)
        (error "File %s does not exist in the workspace" file-path))

      (condition-case err
          (if (not (treesit-available-p))
              "Tree-sitter is not available in this Emacs build"
            (with-current-buffer target-buffer
              (let* ((parsers (treesit-parser-list))
                     (parser (car parsers)))
                (if (not parser)
                    (format "No tree-sitter parser available for %s" file-path)
                  (let* ((root-node (treesit-parser-root-node parser))
                         ;; Determine position from line/column or use current point
                         (pos (cond (whole_file nil)
                                    (line (+macher-tool--treesit-line-column-to-point
                                           line (or column 0)))
                                    ;; Use current point in the target buffer
                                    (t (point))))
                         (node (if whole_file
                                   root-node
                                 (treesit-node-at pos parser)))
                         (results '()))
                    (if (not node)
                        "No tree-sitter node found"
                      ;; For full tree, use a different display function
                      (if whole_file
                          (+macher-tool--treesit-format-tree root-node 0 20)
                        ;; Basic node information for specific position
                        (push (format "Node Type: %s" (treesit-node-type node)) results)
                        (push (format "Range: %d-%d"
                                      (treesit-node-start node)
                                      (treesit-node-end node)) results)
                        (push (format "Text: %s"
                                      (truncate-string-to-width
                                       (treesit-node-text node t)
                                       80 nil nil "...")) results)

                        ;; Check if node is named
                        (when (treesit-node-check node 'named)
                          (push "Named: yes" results))

                        ;; Field name if available
                        (let ((field-name (treesit-node-field-name node)))
                          (when field-name
                            (push (format "Field: %s" field-name) results)))

                        ;; Include ancestors if requested
                        (when include_ancestors
                          (push "\nAncestors:" results)
                          (let ((parent (treesit-node-parent node))
                                (level 1))
                            (while (and parent (< level 10))
                              (push (format "  %s[%d] %s (%d-%d)"
                                            (make-string level ?-)
                                            level
                                            (treesit-node-type parent)
                                            (treesit-node-start parent)
                                            (treesit-node-end parent))
                                    results)
                              (setq parent (treesit-node-parent parent))
                              (cl-incf level))))

                        ;; Include children if requested
                        (when include_children
                          (push "\nChildren:" results)
                          (let ((child-count (treesit-node-child-count node))
                                (i 0))
                            (if (= child-count 0)
                                (push "  (no children)" results)
                              (while (< i (min child-count 20))
                                (let ((child (treesit-node-child node i)))
                                  (when child
                                    (push (format "  [%d] %s%s (%d-%d)"
                                                  i
                                                  (treesit-node-type child)
                                                  (if (treesit-node-check child 'named)
                                                      " (named)" "")
                                                  (treesit-node-start child)
                                                  (treesit-node-end child))
                                          results)))
                                (cl-incf i))
                              (when (> child-count 20)
                                (push (format "  ... and %d more children"
                                              (- child-count 20))
                                      results)))))

                        ;; Return formatted results
                        (string-join (nreverse results) "\n"))))))))
        (error
         (format "Error getting tree-sitter info for %s: %s"
                 file-path (error-message-string err)))))))

(defun +macher--extra-tools (context make-tool-function)
  (list
   (funcall
    make-tool-function
    :name "xref_find_references"
    :description "Find where a function, variable, or class is used throughout your codebase. Perfect for understanding code dependencies and impact analysis"
    :function (apply-partially #'+macher-tool--xref-find-references context)
    :confirm nil
    :include nil
    :args
    '((:name "identifier"
       :type string
       :description "The identifier to find references for")
      (:name "file_path"
       :type string
       :description "File path to use as context for the search")))
   (funcall
    make-tool-function
    :name "xref_find_apropos"
    :description "Search for functions, variables, or classes by name pattern across your project. Helps you discover code elements when you know part of the name"
    :function (apply-partially #'+macher-tool--xref-find-apropos context)
    :confirm nil
    :include nil
    :args
    '((:name "pattern"
       :type string
       :description "The pattern to search for symbols")
      (:name "file_path"
       :type string
       :description "File path to use as context for the search")))
   (funcall
    make-tool-function
    :name "imenu_list_symbols"
    :description "Find where a function, variable, or class is used throughout your codebase. Perfect for understanding code dependencies and impact analysis"
    :function (apply-partially #'+macher-tool--imenu-list-symbols context)
    :confirm nil
    :include nil
    :args
    '((:name "file_path"
       :type string
       :description "Path to the file to analyze for symbols")))
   (funcall
    make-tool-function
    :name "treesit_info"
    :description "Find where a function, variable, or class is used throughout your codebase. Perfect for understanding code dependencies and impact analysis"
    :function (apply-partially #'+macher-tool--treesit-info context)
    :confirm nil
    :include nil
    :args
    '((:name "file_path"
       :type string
       :description "Path to the file to analyze")
      (:name "line"
       :type number
       :optional t
       :description "Line number (1-based)")
      (:name "column"
       :type number
       :optional t
       :description "Column number (0-based)")
      (:name "whole_file"
       :type boolean
       :optional t
       :description "Show the entire file's syntax tree")
      (:name "include_ancestors"
       :type boolean
       :optional t
       :description "Include parent node hierarchy")
      (:name "include_children"
       :type boolean
       :optional t
       :description "Include child nodes")))))

(setq macher-read-tools-function #'macher--read-tools)
(add-function :around macher-read-tools-function (lambda (orig-fn &rest args)
                                                   (let ((tools (append (apply #'+macher--extra-tools args)
                                                           (apply orig-fn args))))
                                                     (gptel-mcp-connect '("duckduckgo") 'sync)
                                                     (cl-loop for (_name . tooldef) in (alist-get "mcp-duckduckgo" gptel--known-tools nil nil #'equal)
                                                              do (push tooldef tools))
                                                     (cl-loop for (_name . tooldef) in (alist-get "task-management" gptel--known-tools nil nil #'equal)
                                                              do (push tooldef tools))
                                                     tools)))

;; lisp/core/lib/zenit-lib-imenu.el -*- lexical-binding: t; -*-

;; Adapted from https://github.com/redguardtoo/imenu-extra/ and
;; `markdown-imenu-create-nested-index'.

(eval-when-compile
  (require 'cl-lib))

;; `imenu'
(declare-function imenu--generic-function "imenu" (patterns))
(defvar imenu-create-index-function)

;;;###autoload
(defun +imenu-create-nested-index (regexp &optional menu-title)
  "Create a nested imenu index alist for the current buffer.
REGEXP is a regular expression matching the imenu entries. It
must contain two numbered capturing groups:

- (?1:...): This matches the symbol denoting the level, for
  exampe (?1:[;]+) matches one or more \\=';' and their number
  determines the level

- (?2:...): This matches the heading itself, for
  example (?2:[a-z])

MENU-TITLE (a string) specifies the title of a submenu into which
the matches are put. If nil, the matches for this element are put
in the top level of the buffer index.

See `imenu-create-index-function' and `imenu-generic-expression'
for details."
  ;; Initialize variables for building the index tree
  (let* (;; Root node of the index tree
         (root (list nil))
         ;; Track minimum level for normalization
         (min-level 9999)
         ;; Temporary storage for level and headers
         level-idx headers)

    ;; Scan through the buffer to find all matching headers
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp (point-max) t)
        (when (setq level-idx (string-trim (match-string-no-properties 1)))
          ;; Update minimum level and store header information
          (setq min-level (min min-level (length level-idx)))
          (push (list :heading (match-string-no-properties 2)
                      :point (match-beginning 1)
                      :level (- (length level-idx) (1- min-level)))
                headers))))

    ;; Build the nested index structure from collected headers
    (cl-loop
     ;; Current nesting level
     with cur-level = 0
     ;; Current position in the tree
     with cur-alist = nil
     ;; Placeholder for empty headings
     with empty-heading = "-"
     ;; Placeholder for self-reference
     with self-heading = "."
     ;; Process headers in order
     for header in (reverse headers)
     for level = (plist-get header :level)
     do
     (let ((alist (list (cons (plist-get header :heading)
                              (plist-get header :point)))))
       (cond
        ;; Case 1: New sibling at same level
        ((= cur-level level)
         (setcdr cur-alist alist)
         (setq cur-alist alist))

        ;; Case 2: First child at deeper level
        ((< cur-level level)
         ;; Create intermediate levels if needed
         (dotimes (_ (- level cur-level 1))
           (setq alist (list (cons empty-heading alist))))
         (if cur-alist
             ;; Add self-reference to parent
             (let* ((parent (car cur-alist))
                    (self-pos (cdr parent)))
               (setcdr parent (cons (cons self-heading self-pos) alist)))
           ;; Handle root level case
           (setcdr root alist))
         (setq cur-alist alist)
         (setq cur-level level))

        ;; Case 3: New sibling of an ancestor
        (t
         (let ((sibling-alist (last (cdr root))))
           ;; Traverse to the correct level
           (dotimes (_ (1- level))
             (setq sibling-alist (last (cdar sibling-alist))))
           (setcdr sibling-alist alist)
           (setq cur-alist alist))
         (setq cur-level level)))))

    ;; Return the completed index tree
    (when (length> root 1)
      (if menu-title
          (list (cons menu-title (cdr root)))
        (cdr root)))))

(defun +imenu-extra-line-range (position)
  "Get the start and end positions of the line at POSITION.
Returns a cons cell (START . END) containing the buffer positions
of the start and end of the line containing POSITION."
  (let (rlt)
    (save-excursion
      (goto-char position)
      (setq rlt (cons (line-beginning-position) (line-end-position))))
    rlt))

(defun +imenu-extra-position (item)
  "Extract the first buffer position from an imenu ITEM.

ITEM can be:
- An integer (direct buffer position)
- A marker
- A plist with position as second element
- An alist with position in cdr
Returns the buffer position or nil if not found."
  (cond
   ;; Handle nil case - return nil for non-existent items
   ((not item)
    nil)

   ;; Handle direct integer positions - return as-is
   ((integerp item)
    item)

   ;; Handle marker objects - extract the numeric position
   ((markerp item)
    (marker-position item))

   ;; Handle lists where position is second element
   ;; e.g. ("name" 42 ...)
   ((and (listp item) (listp (cdr item)))
    (+imenu-extra-position (cadr item)))

   ;; Handle association lists where position is in cdr
   ;; e.g. ("name" . 42)
   ((and (listp item) (not (listp (cdr item))))
    (+imenu-extra-position (cdr item)))))

(defun +imenu-extract-ranges (items)
  "Extract line ranges from imenu ITEMS.
Returns a list of cons cells (start . end) representing line
ranges."
  (let (ranges)
    (dolist (item items ranges)
      (cond
       ;; Skip pure strings or nil
       ((or (null item) (stringp item))
        nil)

       ;; Handle nested lists
       ((and (listp item) (listp (cdr item)))
        (setq ranges
              (nconc ranges
                     (+imenu-extract-ranges item))))

       ;; Handle direct items
       (t
        (when-let* ((pos (+imenu-extra-position item)))
          (push (+imenu-extra-line-range pos) ranges)))))))

(autoload 'imenu--generic-function "imenu")
;;;###autoload
(defun +imenu-merge-new-items (original-items patterns &optional keep)
  "Merge ORIGINAL-ITEMS and extra imenu items from PATTERNS.
Duplicates will be removed unless KEEP is non-nil and results
from the new PATTERNS take precedence over ORIGINAL-ITEMS.
PATTERNS should be an alist where each pattern can be either be
an index generating function (see `imenu-create-index-function')
or as in `imenu-generic-expression'. Example:

\\='((\"tdd.it\" \"^[ \t]*it('\\([^']+\\)\" 1))
  (\"tdd.desc\" \"^[ \t]*describe('\\([^']+\\)\" 1))
  (my-index-fn optional-args))
"
  ;; Collect extra items
  (let ((extra-items (save-excursion
                       (cl-loop for pattern in patterns
                                if (functionp (car pattern))
                                append (save-excursion (apply (car pattern) (cdr pattern)))
                                else
                                append (save-excursion (imenu--generic-function (list pattern)))))))
    (cond
     (extra-items
      (unless keep
        ;; Extract the line numbers for the new imenu items.
        (let ((ranges (+imenu-extract-ranges extra-items)))

          ;; Remove duplicate items from original-items
          (setq original-items
                (+imenu-filter-index original-items ranges))))

      ;; EXTRA-ITEMS sample:
      ;; ((function ("hello" . #<marker 63>) ("bye" . #<marker 128>))
      ;;  (controller ("MyController" . #<marker 128))
      ;;  (hello . #<marker 161>))
      (append original-items extra-items))

     (t
      original-items))))

(defun +imenu-filter-index (lst positions)
  "Remove all elements from LST whose cdr is found in POSITIONS.
If the resulting list only contains a string (at any level),
remove that level. If a list contains only a header and a dot
entry, convert it to a dotted pair."
  (cond
   ;; Base case: nil
   ((null lst) nil)

   ;; If it's a cons but not a list (dotted pair)
   ((and (consp lst) (not (listp (cdr lst))))
    (if (let ((position (+imenu-extra-position (cdr lst))))
          (cl-some (lambda (item-range)
                     (and position
                          (<= position (cdr item-range))
                          (>= position (car item-range))))
                   positions))
        nil
      lst))

   ;; If it's a list
   ((listp lst)
    (let ((filtered
           (delq nil
                 (mapcar (lambda (elem)
                           (+imenu-filter-index elem positions))
                         lst))))
      ;; Check various conditions
      (cond
       ;; Empty list becomes nil
       ((null filtered) nil)
       ;; Only a string becomes nil
       ((and (= (length filtered) 1)
             (stringp (car filtered)))
        nil)
       ;; Special case: list with header and single dot entry
       ((and (= (length filtered) 2)
             (stringp (car filtered))
             (consp (cadr filtered))
             (equal (caadr filtered) ".")
             (numberp (cdadr filtered)))
        (cons (car filtered) (cdadr filtered)))
       ;; Otherwise return the filtered list
       (t filtered))))

   ;; For non-list elements (like strings), return as is
   (t lst)))

;;;###autoload
(defun +imenu-add-items (patterns)
  "Add extra imenu items extracted from PATTERNS.

PATTERNS should be an alist where each pattern can be either be
an index generating function (see `imenu-create-index-function')
or as in `imenu-generic-expression'. Example:

\\='((\"tdd.it\" \"^[ \t]*it('\\([^']+\\)\" 1))
  (\"tdd.desc\" \"^[ \t]*describe('\\([^']+\\)\" 1))
  (my-index-fn optional-args))
"
  (add-function :around (local 'imenu-create-index-function)
                (lambda (orig-fn)
                  (+imenu-merge-new-items (funcall orig-fn) patterns))
                '((depth . -90))))

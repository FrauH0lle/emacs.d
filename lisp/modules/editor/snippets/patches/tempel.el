;; editor/snippets/patches/tempel.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'el-patch)
  (require 'tempel))

;; PATCH Allow specified minor modes to match the buffer local variable
;;   `+snippets--extra-modes'
(el-patch-defun tempel--condition-p (mode cond)
  "Return non-nil if MODE matches and COND is satisfied."
  (and
   (or (eq mode #'fundamental-mode)
       (el-patch-add
          (and (bound-and-true-p +snippets--extra-modes)
               (memq m +snippets--extra-modes)))
       (derived-mode-p mode)
       (when-let* ((remap (alist-get mode major-mode-remap-alist)))
         (derived-mode-p remap)))
   (or tempel--ignore-condition
       (eq cond t)
       (save-excursion
         (save-restriction
           (save-match-data
             (eval cond 'lexical)))))))

;; PATCH Ask user to choose if a LSP snippet offers choices. This is pretty
;;   hacky and there should be a better way. See
;;   https://github.com/minad/tempel/issues/105
(defvar +tempel--last-motion nil)
(defun +tempel--navigate-after-choice (ov direction)
  "Navigate to next/previous field after choosing an LSP completion."
  (goto-char (if (eq direction 'forward)
                 (overlay-end ov)
               (overlay-start ov)))
  (when-let* ((found (tempel--find (if (eq direction 'forward) 1 -1))))
    (funcall (if (eq direction 'forward)
                 #'tempel-next
               #'tempel-previous)
             1)))

(defadvice! +tempel-record-motion-direction-a (fn arg)
  "Record motion direction."
  :around #'tempel-next
  (if (> arg 0)
      (setq +tempel--last-motion 'forward)
    (setq +tempel--last-motion 'backward))
  (funcall fn arg))

;; PATCH Make `r' without a region behave like `p' and insert region as editable
;;   field, if present.
;;   Furthermore, add `lsp-choice'.
(el-patch-defun tempel--element (region elt)
  "Add template ELT given the REGION.
A template can consist of elements of several types:

- string: The string is inserted in the buffer.
- nil: It is ignored.
- `p': An empty and unnamed placeholder field is inserted.
- `r': Inserts the currently active region.  If no region is active, a
  placeholder field is inserted.  If `tempel-done-on-region' is non-nil,
  the template is finished when you jump to the field like `q'.
- `r>': Like `r', but it also indents the region.
- `n': Inserts a newline.
- `n>': Inserts a newline and indents line.
- `>': The line is indented using `indent-according-to-mode'.  Note
  that you often should place this item after the text you want on the
  line.
- `&': If there is only whitespace between the line start and point,
  nothing happens.  Otherwise a newline is inserted.
- `%': If there is only whitespace between point and end of line,
  nothing happens.  Otherwise a newline is inserted.
- `o': Like `%' but leaves the point before the newline.
- (s NAME): Inserts a named field.
- (p PROMPT <NAME> <NOINSERT>): Insert an optionally named field with a
  prompt.  The PROMPT is displayed directly in the buffer as default
  value.  The field value is bound to NAME and updated dynamically.  If
  NOINSERT is non-nil, no field is inserted and the minibuffer is used
  for prompting.  For clarity, the symbol `noinsert' should be used as
  argument.
- (r PROMPT <NAME> <NOINSERT>): Like (p ..), but if there is a current
  region, it is placed here.
- (r> PROMPT <NAME> <NOINSERT>): Like (r ..), but is also indents the
  region.
- (l ELEMENTS..): Insert multiple elements.
- Anything else is passed to each function in `tempel-user-elements'
  until one of the functions returns non-nil, and the result is
  inserted.  If all of them return nil, the form is evaluated.  The
  result can either be a string or any other element.  If the return
  value is a string it is dynamically updated on modification of other
  fields.  Other return values are treated as elements and inserted
  according to the rules.  The element (l ..) is useful to return
  multiple elements.

Tempel extends the Tempo syntax with the following elements:

- (p FORM <NAME> <NOINSERT>): Like (p ..) described above, but FORM is
  evaluated.  FORM can for example call `completing-read' to select
  among various elements.
- (FORM ..): If a Lisp form evaluates to a string, it is inserted as
  overlay and the overlay is updated on modifications of other fields.
- `q': Like `p', but the template is finished if the user jumps to the
  field.  Similarly `r' finishes the template if `tempel-done-on-region'
  is non-nil.

Use caution with templates which execute arbitrary code!"
  (pcase elt
    ('nil)
    ('n (insert "\n"))
    ;; `indent-according-to-mode' fails sometimes in Org. Ignore errors.
    ('n> (insert "\n") (tempel--protect (indent-according-to-mode)))
    ('> (tempel--protect (indent-according-to-mode)))
    ((pred stringp) (insert elt))
    ('& (unless (or (bolp) (save-excursion (re-search-backward "^\\s-*\\=" nil t)))
          (insert "\n")))
    ('% (unless (or (eolp) (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
          (insert "\n")))
    ('o (unless (or (eolp) (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
          (open-line 1)))
    (`(s ,name) (tempel--field name))
    (`(l . ,lst) (dolist (e lst) (tempel--element region e)))
    ((or 'p `(,(or 'p 'P) . ,rest)) (apply #'tempel--placeholder rest))
    ((or 'r 'r> `(,(or 'r 'r>) . ,rest))
     (if (not region)
         (el-patch-remove
           (when-let* ((ov (apply #'tempel--placeholder rest))
                       ((not rest))
                       (tempel-done-on-region))
             (overlay-put ov 'tempel--enter #'tempel--done))
           (goto-char (cdr region))
           (when (eq (or (car-safe elt) elt) 'r>)
             (indent-region (car region) (cdr region) nil)))
       (el-patch-add
         ;; When no region is active, behave like p
         (let ((p-elt (cons 'p rest)))
           (tempel--protect (indent-according-to-mode))
           (tempel--element nil p-elt))
         ;; Create an editable field with region content
         (let* ((reg-content (buffer-substring-no-properties (car region) (cdr region)))
                (name (and (listp elt) (nth 2 elt)))
                ;; Construct a p-style element with the region content
                (p-elt (if rest
                           `(p ,@(if (stringp (car rest))
                                     (cons reg-content (cdr rest))
                                   (cons reg-content rest)))
                         `(p ,reg-content ,name))))
           ;; Delete the region content since we'll reinsert it in the field
           (delete-region (car region) (cdr region))
           ;; Recurse using the p pattern with our constructed element
           (tempel--element nil p-elt)
           ;; Handle indentation if needed
           (when (eq (or (car-safe elt) elt) 'r>)
             (let ((end (point)))
               (indent-region (- end (length reg-content)) end nil)))))))
    ;; TEMPEL EXTENSION: Quit template immediately
    ('q (overlay-put (tempel--field) 'tempel--enter #'tempel--done))
    (el-patch-add
      (`(lsp-choice ,choices ,_name)
       (unless (and (listp choices) (seq-every-p #'stringp choices))
         (error "Invalid LSP choices format"))
       (overlay-put (tempel--placeholder "CHOICES") 'tempel--enter
                    (lambda (&rest _)
                      (let* ((ov (tempel--field-at-point))
                             (content (buffer-substring-no-properties
                                       (overlay-start ov) (overlay-end ov)))
                             (res (letf! ((#'lsp-completion--annotate #'ignore))
                                    (completing-read "Choose: "
                                                     choices
                                                     nil t
                                                     (when (member content choices)
                                                       content)))))
                        ;; Replace content if different choice selected
                        (unless (equal res content)
                          (tempel-kill)
                          (insert res))
                        ;; Navigate based on last motion direction
                        (+tempel--navigate-after-choice
                         ov
                         (or +tempel--last-motion 'forward)))))))
    (_ (let* ((uel (tempel--user-element elt))
              (val (unless uel
                     ;; Ignore errors since variables may not be defined yet.
                     (condition-case nil
                         (eval elt (cdar tempel--active))
                       (void-variable "")))))
         (if (or uel (not (stringp val)))
             (tempel--element region (or uel val))
           ;; TEMPEL EXTENSION: Evaluate forms
           (tempel--form elt val))))))

;; PATCH Modified `tempel--insert' to capture region bounds early. Necessary for
;;   the patch for `tempel--element'.
(el-patch-defun tempel--insert (template region)
  "Insert TEMPLATE given the current REGION."
  (let ((plist (tempel--template-plist template))
        ;; Capture fixed region bounds if region is active
        (el-patch-add
          (fixed-region (when region
                          ;; Make a copy of the region markers that won't move
                          (cons
                           ;; t = rear-advance
                           (copy-marker (car region) t)
                           ;; nil = front-advance
                           (copy-marker (cdr region) nil))))))
    (eval (plist-get plist :pre) 'lexical)
    (unless (eq buffer-undo-list t)
      (push '(apply tempel--disable) buffer-undo-list))
    (setf (alist-get 'tempel--active minor-mode-overriding-map-alist) tempel-map)
    (save-excursion
      ;; Split existing overlays, do not expand within existing field.
      (dolist (st tempel--active)
        (dolist (ov (cdar st))
          (when (and (<= (overlay-start ov) (point))
                     (>= (overlay-end ov) (point)))
            (setf (overlay-end ov) (point)))))
      ;; Activate template
      (let* ((range (make-overlay (point) (point) nil t))
             (st (cons (list range) nil))
             (tempel--inhibit-hooks t))
        (push st tempel--active)
        (cl-loop for x in template until (keywordp x)
                 do (tempel--element (el-patch-swap region fixed-region) x))
        (move-overlay range (overlay-start range) (point))
        ;; (overlay-put range 'face 'region) ;; Enable for debugging
        (overlay-put range 'modification-hooks (list #'tempel--range-modified))
        (overlay-put range 'tempel--range st)
        (overlay-put range 'tempel--post (plist-get plist :post))))
    ;; Clean up markers
    (el-patch-add
      (when fixed-region
        (set-marker (car fixed-region) nil)
        (set-marker (cdr fixed-region) nil)))
    (cond
     ((cl-loop for ov in (caar tempel--active)
               never (overlay-get ov 'tempel--field))
      (goto-char (overlay-end (caaar tempel--active)))
      (tempel--done)) ;; Finalize right away
     ((cl-loop for ov in (caar tempel--active)
               never (and (overlay-get ov 'tempel--field)
                          (eq (point) (overlay-start ov))))
      (tempel-next)))))

;; editor/snippets/patches/tempel.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'el-patch)
  (require 'tempel))

;; PATCH Allow specified minor modes to match the buffer local variable
;;   `+snippets--extra-modes'
(el-patch-defun tempel--condition-p (modes plist)
  "Return non-nil if one of MODES matches and the PLIST condition is satisfied."
  (and
   (cl-loop
    for m in modes thereis
    (or (eq m #'fundamental-mode)
        (el-patch-add
          (and (bound-and-true-p +snippets--extra-modes)
               (memq m +snippets--extra-modes)))
        (derived-mode-p m)
        (when-let (((eval-when-compile (> emacs-major-version 28)))
                   (remap (alist-get m (bound-and-true-p major-mode-remap-alist))))
          (derived-mode-p remap))))
   (or tempel--ignore-condition
       (not (plist-member plist :when))
       (save-excursion
         (save-restriction
           (save-match-data
             (eval (plist-get plist :when) 'lexical)))))))

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
(el-patch-defun tempel--element (st region elt)
  "Add template ELT to ST given the REGION."
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
    (`(s ,name) (tempel--field st name))
    (`(l . ,lst) (dolist (e lst) (tempel--element st region e)))
    ((or 'p `(,(or 'p 'P) . ,rest)) (apply #'tempel--placeholder st rest))
    ((or 'r 'r> `(,(or 'r 'r>) . ,rest))
     (if (not region)
         (el-patch-remove
           (when-let ((ov (apply #'tempel--placeholder st rest))
                      ((not rest)))
             (overlay-put ov 'tempel--enter #'tempel--done))
           (goto-char (cdr region))
           (when (eq (or (car-safe elt) elt) 'r>)
             (indent-region (car region) (cdr region) nil)))
       (el-patch-add
         ;; When no region is active, behave like p
         (let ((p-elt (cons 'p rest)))
           (tempel--protect (indent-according-to-mode))
           (tempel--element st nil p-elt))
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
           (tempel--element st nil p-elt)
           ;; Handle indentation if needed
           (when (eq (or (car-safe elt) elt) 'r>)
             (let ((end (point)))
               (indent-region (- end (length reg-content)) end nil)))))))
    ;; TEMPEL EXTENSION: Quit template immediately
    ('q (overlay-put (tempel--field st) 'tempel--enter #'tempel--done))
    (el-patch-add
      (`(lsp-choice ,choices ,_name)
       (unless (and (listp choices) (seq-every-p #'stringp choices))
         (error "Invalid LSP choices format"))
       (overlay-put (tempel--placeholder st "CHOICES") 'tempel--enter
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
    (_ (if-let ((ret (run-hook-wrapped 'tempel-user-elements
                                       (lambda (hook elt fields)
                                         (condition-case nil
                                             (funcall hook elt)
                                           (wrong-number-of-arguments
                                            (funcall hook elt fields))))
                                       elt (cdr st))))
           (tempel--element st region ret)
         ;; TEMPEL EXTENSION: Evaluate forms
         (tempel--form st elt)))))

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
          (when (and (<= (overlay-start ov) (point)) (>= (overlay-end ov) (point)))
            (setf (overlay-end ov) (point)))))
      ;; Activate template
      (let ((st (cons nil nil))
            (ov (point))
            (tempel--inhibit-hooks t))
        (cl-loop for x in template until (keywordp x)
                 do (tempel--element st (el-patch-swap region fixed-region) x))
        (setq ov (make-overlay ov (point) nil t))
        (push ov (car st))
        (overlay-put ov 'modification-hooks (list #'tempel--range-modified))
        (overlay-put ov 'tempel--range st)
        (overlay-put ov 'tempel--post (plist-get plist :post))
        (push st tempel--active)))
    ;; Clean up markers
    (el-patch-add
      (when fixed-region
        (set-marker (car fixed-region) nil)
        (set-marker (cdr fixed-region) nil)))
    (cond
     ((cl-loop for ov in (caar tempel--active)
               never (overlay-get ov 'tempel--field))
      (goto-char (overlay-end (caaar tempel--active)))
      (tempel--done))
     ((cl-loop for ov in (caar tempel--active)
               never (and (overlay-get ov 'tempel--field)
                          (eq (point) (overlay-start ov))))
      (tempel-next 1)))))

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
        (when-let ((remap (alist-get m (bound-and-true-p major-mode-remap-alist))))
          (derived-mode-p remap))))
   (or (not (plist-member plist :when))
       (save-excursion
         (save-restriction
           (save-match-data
             (eval (plist-get plist :when) 'lexical)))))))

;; PATCH Ask user to choose if a LSP snippet offers choices. This is pretty
;;   hacky and there should be a better way. See
;;   https://github.com/minad/tempel/issues/105
(defvar +tempel--last-motion nil)
(defadvice! +tempel-record-motion-direction (fn arg)
  "Record motion direction."
  :around #'tempel-next
  (if (> arg 0)
      (setq +tempel--last-motion 'forward)
    (setq +tempel--last-motion 'backward))
  (funcall fn arg))

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
         (when-let ((ov (apply #'tempel--placeholder st rest))
                    ((not rest)))
           (overlay-put ov 'tempel--enter #'tempel--done))
       (goto-char (cdr region))
       (when (eq (or (car-safe elt) elt) 'r>)
         (indent-region (car region) (cdr region) nil))))
    ;; TEMPEL EXTENSION: Quit template immediately
    ('q (overlay-put (tempel--field st) 'tempel--enter #'tempel--done))
    (el-patch-add
      (`(lsp-choice ,choices ,name)
       (overlay-put (tempel--placeholder st "CHOICES") 'tempel--enter
                    (lambda (&rest _)
                      (let* ((ov (tempel--field-at-point))
                             (content (buffer-substring-no-properties
                                       (overlay-start ov) (overlay-end ov))))
                        ;; BUG `lsp-completion--annotate' causes a strange error when vertico opens:
                        ;;   Error in post-command-hook (vertico--exhibit): (wrong-type-argument hash-table-p nil)
                        ;;   The approach below is not nice, but works.
                        (let ((res (letf! ((#'lsp-completion--annotate #'ignore))
                                     (completing-read "Choose: " choices nil t (when (member content choices) content)))))
                          ;; Somethink different was chose than what was
                          ;; written before, replace it.
                          (unless (equal res content)
                            (tempel-kill)
                            (insert res))

                          ;; Go to the next template field, but only if there
                          ;; is one. This way we do not automatically end the
                          ;; template expansion.
                          (cond
                           ;; Forward motion
                           ((eq +tempel--last-motion 'forward)
                            (goto-char (overlay-end ov))
                            (when (tempel--find 1)
                              (message "going forward!")
                              (tempel-next 1)))
                           ;; Backward motion
                           ((eq +tempel--last-motion 'backward)
                            (goto-char (overlay-start ov))
                            (when (tempel--find -1)
                              (message "going backward!")
                              (tempel-previous 1)))
                           ;; Go forward if we don't know
                           (t
                            (message "no previous motion data!")
                            (tempel-next 1)))))))))
    (_ (if-let ((ret (run-hook-with-args-until-success 'tempel-user-elements elt)))
           (tempel--element st region ret)
         ;; TEMPEL EXTENSION: Evaluate forms
         (tempel--form st elt)))))

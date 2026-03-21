;; ui/indent-guides/config.el -*- lexical-binding: t; -*-

(defcustom +indent-guides-inhibit-functions ()
  "A list of predicate functions.

Each function will be run in the context of a buffer where
`indent-bars' should be enabled. If any function returns non-nil,
the mode will not be activated."
  :type 'hook
  :group '+indent-guides)


;;
;;; Packages

(use-package! indent-bars
  :unless noninteractive
  :defer t
  :init
  (defun +indent-guides-init-maybe-h ()
    "Enable `indent-bars-mode' depending on `+indent-guides-inhibit-functions'."
    (unless (or (eq major-mode 'fundamental-mode)
                (zenit-temp-buffer-p (current-buffer))
                (run-hook-with-args-until-success '+indent-guides-inhibit-functions))
      (indent-bars-mode +1)))
  :config
  (setq!
   indent-bars-treesit-support (modulep! :tools tree-sitter)
   ;; Show indent guides starting from the first column.
   indent-bars-starting-column 0
   ;; Slightly thinner bars
   indent-bars-width-frac 0.2
   ;; Make indent guides subtle.
   indent-bars-color-by-depth nil
   ;; Tone down the highlighting
   indent-bars-color '(highlight :face-bg t :blend 0.2)
   ;; But increase the highlighting of the current depth
   indent-bars-highlight-current-depth (plist-put indent-bars-highlight-current-depth :blend 0.5)
   ;; Do not highlight on empty lines
   indent-bars-display-on-blank-lines 'least)

  (unless (boundp 'enable-theme-functions)
    (add-hook 'zenit-load-theme-hook #'indent-bars-reset-styles))

  (add-hook! '+indent-guides-inhibit-functions
    (defun +inodent-guides-in-special-buffers-p ()
      ;; Buffers that may have special fontification or may be invisible to the
      ;; user. Particularly src blocks, org agenda, or special modes like magit.
      (and (not (derived-mode-p 'text-mode 'prog-mode 'conf-mode))
           (or buffer-read-only
               (bound-and-true-p cursor-intangible-mode)
               (derived-mode-p 'special-mode))))
    ;; Org's virtual indentation messes up indent-guides.
    (defun +indent-guides-in-org-indent-mode-p ()
      (bound-and-true-p org-indent-mode))

    ;; Don't display indent guides in childframe popups (not helpful in
    ;; completion or eldoc popups).
    #'frame-parent)


  ;; `indent-bars-mode' interacts with some packages poorly. This section is
  ;; dedicated to package interop fixes.

  ;; HACK: The way `indent-bars-display-on-blank-lines' functions, it places
  ;;   text properties with a display property containing a newline, which
  ;;   confuses `move-to-column'. This breaks `next-line' and `evil-next-line'
  ;;   without this advice (See jdtsmith/indent-bars#22). Advising
  ;;   `line-move-to-column' isn't enough for `move-to-column' calls in various
  ;;   Evil operators (`evil-delete', `evil-change', etc).
  (defadvice! +indent-guides--prevent-passing-newline-a (fn col &rest args)
    :around #'move-to-column
    (if-let* ((indent-bars-mode)
              (indent-bars-display-on-blank-lines)
              (nlp (line-end-position))
              (dprop (get-text-property nlp 'display))
              ((seq-contains-p dprop ?\n))
              ((> col (- nlp (point)))))
        (goto-char nlp)
      (apply fn col args)))

  (static-when (modulep! :tools magit)
    (after! magit-blame
      (add-to-list 'magit-blame-disable-modes 'indent-bars-mode)))

  (let ((hide
         (lambda (beg end)
           (save-excursion
             (let ((indent-bars--display-function #'ignore)
                   (indent-bars--display-blank-lines-function #'ignore))
               (indent-bars--fontify beg (1+ end) nil)))))
        (restore
         (lambda (beg end)
           (save-excursion
             (indent-bars--fontify beg (1+ end) nil)))))
    (static-when (modulep! :tools lsp)
      (defadvice! +indent-guides--remove-after-lsp-ui-peek-a (&rest _)
        :after #'lsp-ui-peek--peek-new
        (when (and indent-bars-mode
                   (not indent-bars-prefer-character)
                   (overlayp lsp-ui-peek--overlay))
          (funcall hide
                   (overlay-start lsp-ui-peek--overlay)
                   (overlay-end lsp-ui-peek--overlay))))
      (defadvice! +indent-guides--restore-after-lsp-ui-peek-a (&rest _)
        :before #'lsp-ui-peek--peek-hide
        (when (and indent-bars-mode indent-bars-prefer-character)
          (funcall restore
                   (overlay-start lsp-ui-peek--overlay)
                   (overlay-end lsp-ui-peek--overlay)))))

    (static-when (modulep! :editor fold)
      (defadvice! +indent-guides--remove-overlays-in-vimish-fold-a (beg end)
        :after #'vimish-fold
        (when (and indent-bars-mode (not indent-bars-prefer-character))
          (cl-destructuring-bind (beg . end) (vimish-fold--correct-region beg end)
            (dolist (ov (vimish-fold--folds-in beg end))
              (funcall hide (overlay-start ov) (overlay-end ov))))))
      (defadvice! +indent-guides--fix-overlays-after-unfold-a (fn overlay)
        :around #'vimish-fold--unfold
        (when (vimish-fold--vimish-overlay-folded-p overlay)
          (let ((beg (overlay-start overlay))
                (end (overlay-end overlay)))
            (prog1 (funcall fn overlay)
              (when (and indent-bars-mode (not indent-bars-prefer-character))
                (funcall restore beg end))))))))

  ;; Integrate with `editorconfig'
  ;; From https://github.com/jdtsmith/indent-bars/wiki/integration-with-Editorconfig
  (static-when (modulep! :tools editorconfig)
    (defun +indent-guides--with-editorconfig (size)
      (when (bound-and-true-p indent-bars-mode)
        (setq indent-bars-spacing-override size)
        (indent-bars-reset)))

    (add-hook! 'indent-bars-mode-hook
      (defun +indent-bars-use-editorconfig-h ()
        (when (and (bound-and-true-p editorconfig-mode)
                   (bound-and-true-p editorconfig-indentation-alist))
          (dolist (mode editorconfig-indentation-alist)
            (let ((varlist (cdr mode)))
              (setcdr mode (if (listp varlist)
                               (cl-pushnew '(_ . +indent-guides--with-editorconfig) varlist)
                             (append '((_ . +indent-guides--with-editorconfig))
                                     (ensure-list varlist)))))))))))

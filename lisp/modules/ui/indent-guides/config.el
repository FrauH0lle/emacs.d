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

;; TODO 2024-07-10: Add tree-sitter support
(use-package! indent-bars
  :unless noninteractive
  :defer t
  :init
  (defun +indent-guides-init-maybe-h ()
    "Enable `indent-bars-mode' depending on `+indent-guides-inhibit-functions'."
    (unless (run-hook-with-args-until-success '+indent-guides-inhibit-functions)
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
   indent-bars-display-on-blank-lines nil)

  (unless (boundp 'enable-theme-functions)
    (add-hook 'zenit-load-theme-hook #'indent-bars-reset-styles))

  (add-hook! '+indent-guides-inhibit-functions
    ;; Org's virtual indentation messes up indent-guides.
    (defun +indent-guides-in-org-indent-mode-p ()
      (bound-and-true-p org-indent-mode))

    ;; Don't display indent guides in childframe popups (not helpful in
    ;; completion or eldoc popups).
    (defun +indent-guides-in-childframe-p ()
      (frame-parent))

    ;; indent-guides in src blocks can cause syntax highlighting to fail
    ;; abruptly for some major modes (particularly *-ts-modes or rustic-mode).
    ;; Since it's already working on the super org buffer, it's redundant to let
    ;; it work on the contents of each babel block.
    (defun +indent-guides-in-org-src-block-p ()
      (string-prefix-p " *org-src-fontification:" (buffer-name))))


  ;; HACK 2024-09-15: `indent-bars-mode' interacts with some packages poorly.
  ;;   This section is dedicated to package interop fixes.

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

  (eval-when! (modulep! :tools magit)
    (after! magit-blame
      (add-to-list 'magit-blame-disable-modes 'indent-bars-mode)))

  (eval-when! (modulep! :tools lsp)
    ;; HACK: lsp-ui-peek uses overlays, and indent-bars doesn't know how to deal
    ;;   with all the whitespace it uses to format its popups, spamming it with
    ;;   indent guides. Making the two work together is a project for another
    ;;   day, so disable `indent-bars-mode' while its active instead. Doesn't
    ;;   affect character bars though.
    (defadvice! +indent-guides--remove-after-lsp-ui-peek-a (&rest _)
      :after #'lsp-ui-peek--peek-new
      (when (and indent-bars-mode
                 (not indent-bars-prefer-character)
                 (overlayp lsp-ui-peek--overlay))
        (save-excursion
          (let ((indent-bars--display-function #'ignore)
                (indent-bars--display-blank-lines-function #'ignore))
            (indent-bars--fontify (overlay-start lsp-ui-peek--overlay)
                                  (1+ (overlay-end lsp-ui-peek--overlay))
                                  nil)))))
    (defadvice! +indent-guides--restore-after-lsp-ui-peek-a (&rest _)
      :after #'lsp-ui-peek--peek-hide
      (unless indent-bars-prefer-character
        (indent-bars-setup))))

  ;; Integrate with `editorconfig'
  ;; From https://github.com/jdtsmith/indent-bars/wiki/integration-with-Editorconfig
  (eval-when! (modulep! :tools editorconfig)
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

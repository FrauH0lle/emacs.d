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
  :defer t
  :init
  (defun +indent-guides-init-maybe-h ()
    "Enable `indent-bars-mode' depending on `+indent-guides-inhibit-functions'."
    (unless (run-hook-with-args-until-success '+indent-guides-inhibit-functions)
      (indent-bars-mode +1)))
  :config
  (setq!
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
      (frame-parent)))

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

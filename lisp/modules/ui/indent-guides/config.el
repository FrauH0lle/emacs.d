;; ui/indent-guides/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

;; TODO 2024-07-10: Add tree-sitter support
(use-package! indent-bars
  :defer t
  :config
  (setq!
   ;; Tone down the highlighting
   indent-bars-color '(highlight :face-bg t :blend 0.2)
   ;; But increase the highlighting of the current depth
   indent-bars-highlight-current-depth (plist-put indent-bars-highlight-current-depth :blend 0.5)
   ;; Do not highlight on empty lines
   indent-bars-display-on-blank-lines nil)

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

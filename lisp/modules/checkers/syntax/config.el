;; checkers/syntax/config.el -*- lexical-binding: t; -*-

;;
;;; Flycheck


(defvar +flycheck-disabled-modes '())

(use-package! flycheck
  :unless (modulep! +flymake)
  :commands flycheck-list-errors flycheck-buffer
  :hook (zenit-first-buffer . global-flycheck-mode)
  :config
  (setq! flycheck-global-modes (cons 'not +flycheck-disabled-modes))

  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically)
  ;; And don't recheck on idle as often
  (setq flycheck-idle-change-delay 1.0)

  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly. This allows "refreshing" the syntax check state for several
  ;; buffers quickly after e.g. changing a config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.25)

  ;; Don't commandeer input focus if the error message pops up (happens when
  ;; tooltips and childframes are disabled).
  (set-popup-rules!
    '(("^\\*Flycheck error messages\\*" :select nil)
      ("^\\*Flycheck errors\\*" :size 0.25)))

  (add-hook! 'zenit-escape-hook :append
    (defun +syntax-check-buffer-h ()
      "Flycheck buffer on ESC in normal mode."
      (when flycheck-mode
        (ignore-errors (flycheck-buffer))
        nil)))

  (map! :map flycheck-error-list-mode-map
        :n "C-n"    #'flycheck-error-list-next-error
        :n "C-p"    #'flycheck-error-list-previous-error
        :n "j"      #'flycheck-error-list-next-error
        :n "k"      #'flycheck-error-list-previous-error
        :n "RET"    #'flycheck-error-list-goto-error
        :n [return] #'flycheck-error-list-goto-error))


(use-package! flycheck-popup-tip
  :unless (modulep! +flymake)
  :commands flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup
  :hook (flycheck-mode . +syntax-init-popups-h)
  :config
  (setq flycheck-popup-tip-error-prefix (if (modulep! +icons) "⚠ " "[!] "))

  ;; HACK: Only display the flycheck popup if we're in normal mode (for evil
  ;;   users) or if no selection or completion is active. This popup can
  ;;   interfere with the active evil mode, clear active regions, and other
  ;;   funny business (see #7242).
  (defadvice! +syntax--disable-flycheck-popup-tip-maybe-a (&rest _)
    :before-while #'flycheck-popup-tip-show-popup
    (if (and (bound-and-true-p evil-local-mode)
             (not (evil-emacs-state-p)))
        (evil-normal-state-p)
      (and (not (region-active-p))
           (not (ignore-errors (>= corfu--index 0)))))))


;;
;;; Flymake

(use-package! flymake
  :when (or (modulep! +flymake) (modulep! :tools lsp +lsp-flymake))
  :defer t
  :init
  (static-when (modulep! +flymake)
    (add-hook! '(prog-mode text-mode) #'flymake-mode))
  :config
  (setq flymake-indicator-type 'margins
        flymake-margin-indicator-position 'right-margin)
  (setq! flymake-margin-indicators-string
         `((error ,(nerd-icons-faicon "nf-fa-remove_sign") compilation-error)
           (warning ,(nerd-icons-faicon "nf-fa-warning") compilation-warning)
           (note ,(nerd-icons-faicon "nf-fa-circle_info") compilation-info))))


(use-package! flymake-popon
  :when (or (modulep! +flymake) (modulep! :tools lsp +lsp-flymake))
  :hook (flymake-mode . flymake-popon-mode)
  :config
  (setq flymake-popon-method (static-if (modulep! +childframe)
                                 'posframe
                               'popon)))

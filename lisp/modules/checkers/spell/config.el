;; checkers/spell/config.el -*- lexical-binding: t; -*-

;;
;;; Ispell

;; `ispell' is loaded at startup. In order to lazy load its config we need to
;; pretend it isn't loaded
(delq! 'ispell features)

(global-set-key [remap ispell-word] #'+spell/correct)

(after! ispell
  ;; Don't spellcheck org blocks
  (pushnew! ispell-skip-region-alist
            '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
            '("#\\+BEGIN_SRC" . "#\\+END_SRC")
            '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

  ;; Enable either aspell, hunspell or enchant.
  ;;   If no module flags are given, enable either aspell, hunspell or enchant
  ;;     if their binary is found.
  ;;   If one of the flags `+aspell', `+hunspell' or `+enchant' is given, only
  ;;     enable that spell checker.
  (pcase (cond ((modulep! +aspell)   'aspell)
               ((modulep! +hunspell) 'hunspell)
               ((modulep! +enchant)  'enchant)
               ((executable-find "aspell")    'aspell)
               ((executable-find "hunspell")  'hunspell)
               ((executable-find "enchant-2") 'enchant))
    (`aspell
     (setq ispell-program-name "aspell"
           ispell-extra-args '("--sug-mode=ultra"
                               "--run-together"))

     (unless ispell-aspell-dict-dir
       (setq ispell-aspell-dict-dir
             (ispell-get-aspell-config-value "dict-dir")))
     (unless ispell-aspell-data-dir
       (setq ispell-aspell-data-dir
             (ispell-get-aspell-config-value "data-dir")))
     (unless ispell-personal-dictionary
       (setq ispell-personal-dictionary
             (expand-file-name (concat "ispell/" ispell-dictionary ".pws")
                               zenit-data-dir)))

     (defhook! +spell-remove-run-together-switch-for-aspell-h ()
       'text-mode-hook
       (setq-local ispell-extra-args (remove "--run-together" ispell-extra-args)))

     (defadvice! +spell-init-ispell-extra-args-a (orig-fun &rest args)
       :around '(ispell-word flyspell-auto-correct-word)
       (let ((ispell-extra-args (remove "--run-together" ispell-extra-args)))
         (ispell-kill-ispell t)
         (apply orig-fun args)
         (ispell-kill-ispell t))))

    (`hunspell
     (setq ispell-program-name "hunspell"))

    (`enchant
     (setq ispell-program-name "enchant-2"))

    (_ (zenit-log "Spell checker not found. Either install `aspell', `hunspell' or `enchant'"))))


;;
;;; Implementations

(eval-if! (executable-find "enchant-2")

    (use-package! jinx
      :hook (text-mode . jinx-mode)
      :general ([remap ispell-word] #'+spell/correct)
      :init
      (eval-when! (modulep! +everywhere)
        (add-hook! '(yaml-mode-hook
                     conf-mode-hook
                     prog-mode-hook)
                   #'jinx-mode))
      :config
      (eval-when! (modulep! :completion vertico)
        (add-to-list 'vertico-multiform-categories
                     '(jinx grid (vertico-grid-annotate . 20))))

      (eval-when! (modulep! :editor evil)
        (defadvice! +spell-jinx-fix-off-by-one-a (&rest _)
          "Fix off-by-one cursor position after `jinx-next' or
`jinx-previous'."
          :after #'jinx-next
          (evil-backward-char)))

      (map! :map jinx-overlay-map
            "RET"     #'jinx-correct
            [return]  #'jinx-correct
            [mouse-1] #'jinx-correct))

  (use-package! flyspell ; built-in
    :defer t
    :preface
    ;; `flyspell' is loaded at startup. In order to lazy load its config we need
    ;; to pretend it isn't loaded.
    (defer-feature! flyspell flyspell-mode flyspell-prog-mode)
    :init
    (add-hook! '(org-mode-hook
                 markdown-mode-hook
                 TeX-mode-hook
                 rst-mode-hook
                 mu4e-compose-mode-hook
                 message-mode-hook
                 git-commit-mode-hook)
               #'flyspell-mode)

    (eval-when! (modulep! +everywhere)
      (add-hook! '(yaml-mode-hook
                   conf-mode-hook
                   prog-mode-hook)
                 #'flyspell-prog-mode))

    :config
    (provide 'ispell) ; forcibly load ispell configs

    (setq flyspell-issue-welcome-flag nil
          ;; Significantly speeds up flyspell, which would otherwise print
          ;; messages for every word when checking the entire buffer
          flyspell-issue-message-flag nil)

    (defhook! +spell-inhibit-duplicate-detection-maybe-h ()
      "Don't mark duplicates when style/grammar linters are present.
e.g. proselint and langtool."
      'flyspell-mode-hook
      (and (or (and (bound-and-true-p flycheck-mode)
                    (executable-find "proselint"))
               (featurep 'langtool))
           (setq-local flyspell-mark-duplications-flag nil)))

    ;; Ensure mode-local predicates declared with `set-flyspell-predicate!' are
    ;; used in their respective major modes.
    (add-hook 'flyspell-mode-hook #'+spell-init-flyspell-predicate-h)

    (let ((flyspell-correct
           (general-predicate-dispatch
               (and (not mark-active)
                    (not (and (bound-and-true-p evil-local-mode)
                              (or (evil-insert-state-p)
                                  (evil-emacs-state-p))))
                    (memq 'flyspell-incorrect (face-at-point nil t)))
             #'flyspell-correct-at-point)))
      (map! :map flyspell-mouse-map
            "RET"    flyspell-correct
            [return] flyspell-correct
            [mouse-1] #'flyspell-correct-at-point)))


  (use-package! flyspell-correct
    :commands flyspell-correct-previous
    :general ([remap ispell-word] #'flyspell-correct-at-point)
    :config
    (cond ((modulep! :completion vertico)) ; vertico doesn't need any extra configuration
          ((require 'flyspell-correct-popup nil t) ; only use popup if no compatible completion UI is enabled
           (setq flyspell-popup-correct-delay 0.8)
           (define-key popup-menu-keymap [escape] #'keyboard-quit))))


  (use-package! flyspell-lazy
    :after flyspell
    :config
    (setq flyspell-lazy-idle-seconds 1
          flyspell-lazy-window-idle-seconds 3)
    ;; flyspell-lazy inhibits flyspell entirely in message-mode derivatives (e.g.
    ;; for notmuch users).
    (setq-hook! 'message-mode-hook flyspell-lazy-disallow-buffers nil)
    (flyspell-lazy-mode +1)))

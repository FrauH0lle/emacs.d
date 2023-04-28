;; completion/company/config.el -*- lexical-binding: t; -*-

(use-package! company
  :commands (company-complete-common
             company-complete-common-or-cycle
             company-manual-begin
             company-grab-line)
  :hook (zenit-first-input . global-company-mode)
  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes
        '(not erc-mode
          circe-mode
          message-mode
          help-mode
          gud-mode
          vterm-mode)
        company-frontends
        '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
          company-echo-metadata-frontend)  ; show selected candidate docs in echo area

        ;; Buffer-local backends will be computed when loading a major mode, so
        ;; only specify a global default here.
        company-backends '(company-capf)

        ;; These auto-complete the current selection when
        ;; `company-auto-commit-chars' is typed. This is too magical. We already
        ;; have the much more explicit RET and TAB.
        company-auto-commit nil

        ;; Only search the current buffer for `company-dabbrev' (a backend that
        ;; suggests text your open buffers). This prevents Company from causing
        ;; lag once you have a lot of buffers open.
        company-dabbrev-other-buffers nil
        ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
        ;; domain-specific words with particular casing.
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)

  :config
  (when (modulep! :editor evil)
    (after! evil
      (add-hook 'company-mode-hook #'evil-normalize-keymaps)
      ;; Don't persist company popups when switching back to normal mode.
      ;; `company-box' aborts on mode switch so it doesn't need this.
      (defhook! +company-abort-h ()
        'evil-normal-state-entry-hook
        ;; HACK `company-abort' doesn't no-op if company isn't active; causing
        ;;      unwanted side-effects, like the suppression of messages in the
        ;;      echo-area.
        ;; REVIEW Revisit this to refactor; shouldn't be necessary!
        (when company-candidates
          (company-abort)))
      ;; Allow users to switch between backends on the fly. E.g. C-x C-s followed
      ;; by C-x C-n, will switch from `company-yasnippet' to
      ;; `company-dabbrev-code'.
      (defadvice! +company--abort-previous-a (&rest _)
        :before #'company-begin-backend
        (company-abort))))

  (add-hook 'after-change-major-mode-hook #'+company-init-backends-h 'append)


  ;; NOTE Fix #1335: ensure `company-emulation-alist' is the first item of
  ;;      `emulation-mode-map-alists', thus higher priority than keymaps of
  ;;      evil-mode. We raise the priority of company-mode keymaps
  ;;      unconditionally even when completion is not activated. This should not
  ;;      cause problems, because when completion is activated, the value of
  ;;      `company-emulation-alist' is ((t . company-my-keymap)), when
  ;;      completion is not activated, the value is ((t . nil)).
  (add-hook! 'evil-local-mode-hook
    (when (memq 'company-emulation-alist emulation-mode-map-alists)
      (company-ensure-emulation-alist)))

  ;; Fix #4355: allow eldoc to trigger after completions.
  (after! eldoc
    (eldoc-add-command 'company-complete-selection
                       'company-complete-common
                       'company-capf
                       'company-abort)))


;;
;;; Packages

(after! company-files
  ;; Fix `company-files' completion for org file:* links
  (add-to-list 'company-files--regexps "file:\\(\\(?:\\.\\{1,2\\}/\\|~/\\|/\\)[^\]\n]*\\)"))


(use-package! company-dict
  :defer t
  :config
  (setq company-dict-dir (expand-file-name "dicts" zenit-local-conf-dir))
  (defhook! +company-enable-project-dicts-h (mode &rest _)
    "Enable per-project dictionaries."
    'zenit-project-hook
    (if (symbol-value mode)
        (add-to-list 'company-dict-minor-mode-list mode nil #'eq)
      (setq company-dict-minor-mode-list (delq mode company-dict-minor-mode-list)))))

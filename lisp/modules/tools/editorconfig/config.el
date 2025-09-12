;; tools/editorconfig/config.el -*- lexical-binding: t; -*-

;; REVIEW 2025-09-12: Revisit the module in Emacs v31. `editorconfig' is part of
;;  Emacs v30 but it behaved rather different and caused some issues not worth
;;  investigating.
(use-package! editorconfig
  :hook (zenit-first-buffer . editorconfig-mode)
  :config
  (when (require 'ws-butler nil t)
    (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))

  ;; Archives don't need editorconfig settings, and they may otherwise interfere
  ;; with the process of opening them (office formats are zipped XML formats).
  (add-to-list 'editorconfig-exclude-regexps
               "\\.\\(zip\\|\\(doc\\|xls\\|ppt\\)x\\)\\'")

  (add-hook! 'editorconfig-after-apply-functions
    (defun +editorconfig-disable-indent-detection-h (props)
      "Inhibit `dtrt-indent' if an explicit indent_style and indent_size is
specified by editorconfig."
      (when (and (not zenit-inhibit-indent-detection)
                 (or (gethash 'indent_style props)
                     (gethash 'indent_size props)))
        (setq zenit-inhibit-indent-detection 'editorconfig)))
    ;; Use a hook over `editorconfig-exclude-modes' because the option inhibits
    ;; all settings, and I only want to inhibit indent_size. Plus modes in that
    ;; option won't apply to derived modes, so we'd have to add *all* possible
    ;; org-mode derivatives to it.
    (defun +editorconfig-unset-tab-width-in-org-mode-h (props)
      "A tab-width != 8 is an error state in `org-mode', so prevent changing it."
      (when (and (gethash 'indent_size props)
                 (derived-mode-p 'org-mode))
        (setq tab-width 8)))))

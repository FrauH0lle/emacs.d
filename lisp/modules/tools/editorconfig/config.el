;; tools/editorconfig/config.el -*- lexical-binding: t; -*-

;; REVIEW 2025-04-28: Revisit the module in Emacs v31.
;; Handles whitespace (tabs/spaces) settings externally. This way projects can
;; specify their own formatting rules.
(use-package! editorconfig
  :hook (zenit-first-buffer . editorconfig-mode)
  :init
  ;; REVIEW 2025-04-27: Check again in Emacs v31+. For whatever reason, the
  ;;   included `editorconfig' implementatin does not implement
  ;;   `editorconfig-exclude-regexps' and `editorconfig-exclude-modes',
  ;;   eventhough it mentions them in the source docstrings. We bring them back
  ;;   on our own for now.
  (defvar editorconfig-exclude-regexps ()
    "List of regexp for buffer filenames `editorconfig-mode-apply' will not run.

When variable `buffer-file-name' matches any of the regexps, then
`editorconfig-mode-apply' will not do its work.")

  (defvar editorconfig-exclude-modes ()
    "Modes in which `editorconfig-mode-apply' will not run.")

  :config
  ;; HACK 2025-04-28: `editorconfig' gives `dir-locals.el' a higher precedence
  ;;   than settings from a .editorconfig file. I disagree, it should be the
  ;;   other way around.
  (add-hook! 'editorconfig-mode-hook
    (defun +editorconfig-change-precedence-h ()
      (when editorconfig-mode
        (remove-hook 'hack-dir-local-get-variables-functions #'editorconfig--get-dir-local-variables))

      ;; Taken from `editorconfig'
      (let ((modehooks '(prog-mode-hook
                         text-mode-hook
                         rpm-spec-mode-hook)))
        (if editorconfig-mode
            (progn
              (advice-add 'find-file-noselect :around #'editorconfig--advice-find-file-noselect)
              (dolist (hook modehooks)
                (add-hook hook
                          #'editorconfig-major-mode-hook
                          t)))
          (advice-remove 'find-file-noselect #'editorconfig--advice-find-file-noselect)
          (dolist (hook modehooks)
            (remove-hook hook #'editorconfig-major-mode-hook))))))


  (when (require 'ws-butler nil t)
    (setq! editorconfig-trim-whitespaces-mode 'ws-butler-mode))

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
      "A tab-width != 8 is an error state in org-mode, so prevent
changing it."
      (when (and (gethash 'indent_size props)
                 (derived-mode-p 'org-mode))
        (setq tab-width 8)))))

;; PATCH 2025-04-27: `editorconfig'
(el-patch-feature editorconfig-tools)
(after! editorconfig-tools
  (defun editorconfig--disabled-for-filename (filename)
    "Return non-nil when EditorConfig is disabled for FILENAME."
    (cl-assert (stringp filename))
    (cl-loop for regexp in editorconfig-exclude-regexps
             if (string-match regexp filename) return t
             finally return nil))

  (defun editorconfig--disabled-for-majormode (majormode)
    "Return non-nil when Editorconfig is disabled for MAJORMODE."
    (cl-assert majormode)
    (or (provided-mode-derived-p majormode 'special-mode)
        ;; Some special modes (like `archive-mode') are not derived from
        ;; `special-mode'
        (eq (get majormode 'mode-class) 'special)
        (memq majormode
              editorconfig-exclude-modes)))

  (el-patch-defun editorconfig-mode-apply ()
    "Get and apply EditorConfig properties to current buffer.

This function does nothing when the major mode is listed in
`editorconfig-exclude-modes', or variable `buffer-file-name' matches
any of regexps in `editorconfig-exclude-regexps'."
    (interactive)
    (when (and major-mode
               (el-patch-add (not (editorconfig--disabled-for-majormode major-mode)))
               buffer-file-name
               (el-patch-add (not (editorconfig--disabled-for-filename buffer-file-name))))
      (editorconfig-apply))))

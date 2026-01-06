;; lang/rust/autoload/compat.el -*- lexical-binding: t; -*-

;;;###autoload
(progn
  ;; HACK: Ensure `rust-mode' derives from `rust-ts-mode', b/c `rustic-mode'
  ;;   derives from `rust-mode'. This way, `rustic-mode' is the only major mode
  ;;   we have to worry about for Rust support in Emacs.
  (setq rust-mode-treesitter-derive (modulep! :lang rust +tree-sitter))

  ;; HACK: Prevent `auto-mode-alist' conflicts between rust-mode, rust-ts-mode,
  ;;   and rustic
  (cl-callf2 rassq-delete-all 'rust-mode auto-mode-alist)
  (cl-callf2 rassq-delete-all 'rustic-mode auto-mode-alist)

  ;; HACK: Load order is important for these two packages. `rustic' needs to be
  ;;   loaded *after* `rust-mode'.
  (after! (:or rust-prog-mode rust-mode-treesitter)
    (let (auto-mode-alist)
      (require 'rustic nil t))))

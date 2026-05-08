;; lang/rust/autoload/compat.el -*- lexical-binding: t; -*-

;;;###autoload
(progn
  ;; HACK: Ensure `rust-mode' derives from `rust-ts-mode', b/c `rustic-mode'
  ;;   derives from `rust-mode'. This way, `rustic-mode' is the only major mode
  ;;   we have to worry about for Rust support in Emacs.
  (setq rust-mode-treesitter-derive (zenit-module-p :lang 'rust '+tree-sitter))

  ;; HACK: Emacs 31's built-in `rust-ts-mode' calls (derived-mode-add-parents
  ;;   'rust-ts-mode '(rust-mode)) so that hooks on `rust-mode' also fire in
  ;;   `rust-ts-mode' buffers. But when `rust-mode-treesitter-derive' is
  ;;   non-nil, `rust-mode' derives FROM `rust-ts-mode', creating a cycle in
  ;;   `derived-mode-all-parents': rust-mode → rust-ts-mode → rust-mode. Remove
  ;;   the extra parent to break the cycle. The derive direction already ensures
  ;;   `rust-mode' hooks fire (since `rust-mode' IS a `rust-ts-mode').
  (after! rust-mode-treesitter
    (cl-callf2 delq 'rust-mode (get 'rust-ts-mode 'derived-mode-extra-parents))
    (put 'rust-mode 'derived-mode--all-parents nil)
    (put 'rust-ts-mode 'derived-mode--all-parents nil))

  ;; HACK: Prevent `auto-mode-alist' conflicts between rust-mode, rust-ts-mode,
  ;;   and rustic
  (cl-callf2 rassq-delete-all 'rust-mode auto-mode-alist)
  (cl-callf2 rassq-delete-all 'rustic-mode auto-mode-alist)

  ;; HACK: Load order is important for these two packages. `rustic' needs to be
  ;;   loaded *after* `rust-mode'.
  (after! (:or rust-prog-mode rust-mode-treesitter)
    (let (auto-mode-alist)
      (require 'rustic nil t))))

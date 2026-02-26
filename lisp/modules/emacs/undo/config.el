;; emacs/undo/config.el -*- lexical-binding: t; -*-

(use-package! undo-tree
  ;; Branching & persistent undo
  :hook (zenit-first-buffer . global-undo-tree-mode)
  :custom (undo-tree-history-directory-alist `(("." . ,(concat zenit-cache-dir "undo-tree-hist/"))))
  :config
  (setq undo-tree-visualizer-diff nil
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        ;; Increase undo limits to avoid emacs prematurely truncating the undo
        ;; history and corrupting the tree. This is larger than the undo-fu
        ;; defaults because undo-tree trees consume exponentially more space,
        ;; and then some when `undo-tree-enable-undo-in-region' is involved. See
        ;; syl20bnr/spacemacs#12110
        undo-limit 800000           ; 800kb (default is 160kb)
        undo-strong-limit 12000000  ; 12mb  (default is 240kb)
        undo-outer-limit 128000000) ; 128mb (default is 24mb)

  ;; Compress undo-tree history files with zstd, if available. File size isn't
  ;; the (only) concern here: the file IO barrier is slow for Emacs to cross;
  ;; reading a tiny file and piping it in-memory through zstd is *slightly*
  ;; faster than Emacs reading the entire undo-tree file from the get go (on
  ;; SSDs). Whether or not that's true in practice, we still enjoy zstd's ~80%
  ;; file savings (these files add up over time and zstd is so incredibly fast).
  (when (executable-find "zstd")
    (defadvice! +undo--append-zst-extension-to-file-name-a (file)
      :filter-return #'undo-tree-make-history-save-file-name
      (concat file ".zst")))

  ;; Strip text properties from undo-tree data to stave off bloat. File size
  ;; isn't the concern here; undo cache files bloat easily, which can cause
  ;; freezing, crashes, GC-induced stuttering or delays when opening files.
  (defadvice! +undo--strip-text-properties-a (&rest _)
    :before #'undo-list-transfer-to-tree
    (dolist (item buffer-undo-list)
      (and (consp item)
           (stringp (car item))
           (setcar item (substring-no-properties (car item))))))

  ;; Undo-tree is too chatty about saving its history files. This doesn't
  ;; totally suppress it logging to *Messages*, it only stops it from appearing
  ;; in the echo-area.
  (advice-add #'undo-tree-save-history :around #'zenit-shut-up-a)

  ;; HACK: If undo-tree creates its diff window next to a popup/side window, the
  ;;   `balance-window' calls in `undo-tree-visualizer-update-diff' can wreck
  ;;   havoc on the window tree, making the diff window an unclosable "root"
  ;;   window (which emacs will happily throw errors about when you call
  ;;   `undo-tree-visualizer-quit'). Breakage ensues.
  (defadvice! +undo-tree--show-visualizer-diff-safely-a (&optional node)
    :override #'undo-tree-visualizer-show-diff
    (setq undo-tree-visualizer-diff t)
    (let ((buff (with-current-buffer undo-tree-visualizer-parent-buffer
                  (undo-tree-diff node)))
          (display-buffer-mark-dedicated 'soft)
          (win (split-window (get-buffer-window undo-tree-visualizer-parent-buffer))))
      (with-current-buffer buff
        (hide-mode-line-mode +1))
      (set-window-buffer win buff)
      (shrink-window-if-larger-than-buffer win)))

  (defadvice! +undo-tree--suppress-balance-windows-a (fn &rest args)
    :around #'undo-tree-visualizer-update-diff
    (letf! ((#'balance-windows #'ignore))
      (apply fn args))))

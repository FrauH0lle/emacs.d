;; editor/snippets/autoload/settings.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-tempel-minor-mode! (modes)
  "Register minor MODES (one mode symbol or a list of them) with
`tempel' so it can have its own snippets category."
  (dolist (mode (ensure-list modes))
    (let ((fn (intern (format "+snippets-register-%s-h" mode))))
      (fset fn (lambda () (+snippets-activate-extra-mode mode)))
      (add-hook (intern (format "%s-hook" mode)) fn))))

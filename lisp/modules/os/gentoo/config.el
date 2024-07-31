;; os/gentoo/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(use-package! ebuild-mode
  :defer t
  :init
  (remove-hook 'find-file-hook #'ebuild-repo-mode-maybe-enable)
  :config
  (set-electric! 'ebuild-mode :words '("else" "elif" "fi" "done" "then" "do" "esac" ";;"))
  (set-formatter! 'shfmt '("shfmt" "-ci"
                           (unless indent-tabs-mode
                             (list "-i" (number-to-string tab-width))))
    :modes '(ebuild-mode))
  (map! :map ebuild-mode-map
        :localleader
        :desc "Run ebuild"               "e" #'ebuild-run-command
        :desc "Manipulate keyword"       "k" #'ebuild-mode-keyword
        :desc "Manipulate ekeyword"      "y" #'ebuild-mode-ekeyword
        :desc "Run pkgdev"               "p" #'ebuild-mode-run-pkgdev
        :desc "Run pkgcheck"             "q" #'ebuild-mode-run-pkgcheck
        :desc "Insert skeletion"         "n" #'ebuild-mode-insert-skeleton
        :desc "Insert tag line"          "t" #'ebuild-mode-insert-tag-line
        :desc "Find workdir"             "d" #'ebuild-mode-find-workdir
        :desc "Unstabilize all keywords" "b" #'ebuild-mode-all-keywords-unstable))


(use-package! company-ebuild
  :when (modulep! :completion corfu)
  :after ebuild-mode
  :preface
  ;; HACK 2024-07-25: This function is the only one required from `company-mode'
  (defun company-grab-symbol ()
    "Return buffer substring from the beginning of the symbol until
point."
    (buffer-substring (point) (save-excursion (skip-syntax-backward "w_")
                                              (point))))
  :config/el-patch
  ;; PATCH 2024-07-26: Remove `company' dependency
  (defun company-ebuild-setup ()
    "Setup for Company-Ebuild.

To setup the integration correctly, add this function to ‘ebuild-mode-hook’
in your config:
\(add-hook 'ebuild-mode-hook 'company-ebuild-setup)
or `require' Company-Ebuild:
\(require 'company-ebuild)"
    ;; Force-enable `company-mode'.
    (el-patch-remove
      (when (null company-mode)
        (company-mode +1)))
    ;; Regenerate dynamic keywords.
    (company-ebuild--regenerate-dynamic-keywords)
    ;; Add the `company-ebuild' backend.
    (el-patch-remove
      (setq-local company-backends
                  `((company-ebuild
                     company-capf  ; standard fallback
                     ,@(cond
                        ((fboundp 'company-yasnippet)  ; YAS for easier setup
                         '(company-yasnippet))
                        (t
                         '())))
                    ,@company-backends))
      (setq-local company-require-match nil)))
  :config
  ;; Use Company backend as Capfs.
  (setq-hook! 'ebuild-mode-hook completion-at-point-functions
              (cons (cape-company-to-capf #'company-ebuild)
                    completion-at-point-functions)))


(use-package! flycheck-pkgcheck
  :when (modulep! :checkers syntax)
  :hook (ebuild-mode . flycheck-pkgcheck-setup))

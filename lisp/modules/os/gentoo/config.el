;; os/gentoo/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(use-package! ebuild-mode
  :defer t
  :init
  ;; HACK 2024-08-02: `find-file-hook' fires as soon as we try to open any file
  ;;   which then causes `ebuild-mode' to load, which in turn pulls in
  ;;   `sh-script'.
  (remove-hook 'find-file-hook #'ebuild-repo-mode-maybe-enable)
  :config
  (set-electric! 'ebuild-mode :words '("else" "elif" "fi" "done" "then" "do" "esac" ";;"))

  (after! apheleia
    (add-to-list 'apheleia-mode-alist '(ebuild-mode . shfmt)))

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


;; PATCH 2024-08-02: `company-ebuild'
(el-patch-feature company-ebuild)
(compile-along! "patches")

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
  :config
  (load! "patches")

  ;; Use Company backend as Capfs.
  (setq-hook! 'ebuild-mode-hook completion-at-point-functions
              (cons (cape-company-to-capf #'company-ebuild)
                    completion-at-point-functions)))


(use-package! flycheck-pkgcheck
  :when (modulep! :checkers syntax -flymake)
  :hook (ebuild-mode . flycheck-pkgcheck-setup))

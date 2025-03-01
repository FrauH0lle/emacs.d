;; editor/file-templates/config.el -*- lexical-binding: t; -*-

(defvar +file-templates-dirs
  `(,(file-name-concat zenit-emacs-dir "templates" "file-templates")
    ,(file-name-concat zenit-local-conf-dir "templates" "file-templates"))
  "Folders searched for file templates.
Defaults to the folders templates/file-templates/ located in
`zenit-emacs-dir' and `zenit-local-conf-dir'. Templates defined
in `zenit-local-conf-dir' take precedence.")

(defvar +file-templates-default-trigger "__"
  "The default trigger key (a string) for file template rules that
don't have a :trigger property in `+file-templates-alist'.")

(defvar +file-templates-inhibit nil
  "If non-nil, inhibit file template expansion.")

(defvar +file-templates-alist
  '(;; General
    (gitignore-mode)
    (dockerfile-mode)
    ("/docker-compose\\.yml$" :mode yaml-mode)
    ("/Makefile$"             :mode makefile-gmake-mode)
    ;; elisp
    ("/\\.dir-locals\\.el$")
    ("/control\\.el$" :when +file-templates-in-emacs-dirs-p
     :trigger "__zenit-control"
     :mode emacs-lisp-mode)
    ("/packages\\.el$" :when +file-templates-in-emacs-dirs-p
     :trigger "__zenit-packages"
     :mode emacs-lisp-mode)
    ("/test/.+\\.el$" :when +file-templates-in-emacs-dirs-p
     :trigger "__zenit-test"
     :mode emacs-lisp-mode)
    ("\\.el$" :when +file-templates-in-emacs-dirs-p
     :trigger "__zenit-module"
     :mode emacs-lisp-mode)
    ("-test\\.el$" :mode emacs-ert-mode)
    (emacs-lisp-mode :trigger "__")
    (snippet-mode)
    ;; C/C++
    ("/main\\.c\\(?:c\\|pp\\)$"   :trigger "__main.cpp"    :mode c++-mode)
    ("/win32_\\.c\\(?:c\\|pp\\)$" :trigger "__winmain.cpp" :mode c++-mode)
    ("\\.c\\(?:c\\|pp\\)$"        :trigger "__cpp" :mode c++-mode)
    ("\\.h\\(?:h\\|pp\\|xx\\)$"   :trigger "__hpp" :mode c++-mode)
    ("\\.h$" :trigger "__h" :mode c-mode)
    (c-mode  :trigger "__c")
    ;; direnv
    ("/\\.envrc$" :trigger "__envrc" :mode direnv-envrc-mode)
    ;; go
    ("/main\\.go$" :trigger "__main.go" :mode go-mode :project t)
    (go-mode :trigger "__.go")
    ;; web-mode
    ("/normalize\\.scss$" :trigger "__normalize.scss" :mode scss-mode)
    ("/master\\.scss$" :trigger "__master.scss" :mode scss-mode)
    ("\\.html$" :trigger "__.html" :mode web-mode)
    (scss-mode)
    ;; java
    ("/main\\.java$" :trigger "__main" :mode java-mode)
    ("/build\\.gradle$" :trigger "__build.gradle" :mode android-mode)
    ("/src/.+\\.java$" :mode java-mode)
    ;; javascript
    ("/package\\.json$"        :trigger "__package.json" :mode json-mode)
    ("/bower\\.json$"          :trigger "__bower.json" :mode json-mode)
    ("/gulpfile\\.js$"         :trigger "__gulpfile.js" :mode js-mode)
    ("/webpack\\.config\\.js$" :trigger "__webpack.config.js" :mode js-mode)
    ;; Lua
    ("/main\\.lua$" :trigger "__main.lua" :mode love-mode)
    ("/conf\\.lua$" :trigger "__conf.lua" :mode love-mode)
    ;; Markdown
    (markdown-mode)
    ;; Markdown
    (nxml-mode)
    ;; Nix
    ("/shell\\.nix$" :trigger "__shell.nix")
    (nix-mode)
    ;; Org
    (org-journal-mode :ignore t)
    ("/README\\.org$" :trigger +file-templates-insert-zenit-module-docs-fn :mode org-mode)
    (org-mode)
    ;; PHP
    ("\\.class\\.php$" :trigger "__.class.php" :mode php-mode)
    (php-mode)
    ;; Python
    ;; TODO ("tests?/test_.+\\.py$" :trigger "__" :mode nose-mode)
    ;; TODO ("/setup\\.py$" :trigger "__setup.py" :mode python-mode)
    (python-mode)
    ;; Ruby
    ("/lib/.+\\.rb$"      :trigger "__module"   :mode ruby-mode :project t)
    ("/spec_helper\\.rb$" :trigger "__helper"   :mode rspec-mode :project t)
    ("_spec\\.rb$"                              :mode rspec-mode :project t)
    ("/\\.rspec$"         :trigger "__.rspec"   :mode rspec-mode :project t)
    ("\\.gemspec$"        :trigger "__.gemspec" :mode ruby-mode :project t)
    ("/Gemfile$"          :trigger "__Gemfile"  :mode ruby-mode :project t)
    ("/Rakefile$"         :trigger "__Rakefile" :mode ruby-mode :project t)
    (ruby-mode)
    ;; Rust
    ("/Cargo\\.toml$" :trigger "__Cargo.toml" :mode rust-mode)
    ("/main\\.rs$" :trigger "__main.rs" :mode rust-mode)
    ;; Slim
    ("/\\(?:index\\|main\\)\\.slim$" :mode slim-mode)
    ;; Shell scripts
    ("\\.zunit$" :trigger "__zunit" :mode sh-mode)
    (fish-mode)
    (sh-mode)
    ;; Solidity
    (solidity-mode :trigger "__sol"))
  "An alist of file template rules. The CAR of each rule is either a major mode
symbol or regexp string. The CDR is a plist. See `set-file-template!' for more
information.")


;;
;;; Library

(defun +file-templates-in-emacs-dirs-p (file)
  "Returns t if FILE is in core or local directory."
  (or (file-in-directory-p file zenit-local-conf-dir)
      (file-in-directory-p file zenit-emacs-dir)))

(defun +file-template-p (rule)
  "Return t if RULE applies to the current buffer."
  (let ((pred (car rule))
        (plist (cdr rule)))
    (and (or (and (symbolp pred)
                  (eq major-mode pred))
             (and (stringp pred)
                  (stringp buffer-file-name)
                  (string-match-p pred buffer-file-name)))
         (or (not (plist-member plist :when))
             (funcall (plist-get plist :when)
                      buffer-file-name))
         rule)))

(defun +file-templates-check-h ()
  "Check if the current buffer is a candidate for file template expansion. It
must be non-read-only, empty, and there must be a rule in
`+file-templates-alist' that applies to it."
  (and (not +file-templates-inhibit)
       buffer-file-name
       (not buffer-read-only)
       (bobp) (eobp)
       (not (member (substring (buffer-name) 0 1) '("*" " ")))
       (not (file-exists-p buffer-file-name))
       (not (buffer-modified-p))
       (null (buffer-base-buffer))
       (+file-templates/apply)))


(defun +file-templates/apply ()
  "Expand a file template if one exists."
  (interactive)
  (when-let* ((rule (cl-find-if #'+file-template-p +file-templates-alist)))
    (apply #'+file-templates--expand rule)))


;;
;;; Bootstrap

(after! tempel
  (eval-unless! (modulep! :editor snippets)
    ;; Keybinds, if not set up already
    (map! :map tempel-map
          "<tab>"     #'tempel-next
          "TAB"       #'tempel-next
          "<backtab>" #'tempel-previous
          "C-g"       #'tempel-abort
          "C-<home>"  #'tempel-beginning
          "C-<end>"   #'tempel-end))

  ;; Exit snippets on ESC from normal mode
  (add-hook 'zenit-escape-hook #'tempel-abort))

;;
(add-hook 'zenit-switch-buffer-hook #'+file-templates-check-h)

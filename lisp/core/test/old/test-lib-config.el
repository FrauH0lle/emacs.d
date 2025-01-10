;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-lib-config.el


(describe "core/lib/config"

  (load! "lib/config" zenit-core-dir)

  (describe "zenit/open-local-config"
    :var (zenit-local-conf-dir)

    (before-each
      (spy-on 'zenit-project-browse)
      (spy-on 'make-directory))

    (it "creates the zenit-local-conf-dir if it doesn't exist"
      (setq zenit-local-conf-dir "/path/to/nonexistent/dir")
      (zenit/open-local-config)
      (expect 'make-directory :to-have-been-called-with "/path/to/nonexistent/dir" t))

    (it "calls `zenit-project-browse` with `zenit-local-conf-dir`"
      (setq zenit-local-conf-dir "/path/to/existing/dir")
      (zenit/open-local-config)
      (expect 'zenit-project-browse :to-have-been-called-with "/path/to/existing/dir")))


  (describe "zenit/find-file-in-local-config"
    :var (zenit-local-conf-dir)

    (before-each
      (spy-on 'zenit-project-find-file))

    (it "calls `zenit-project-browse` with `zenit-local-conf-dir`"
      (setq zenit-local-conf-dir "/path/to/existing/dir")
      (zenit/find-file-in-local-config)
      (expect 'zenit-project-find-file :to-have-been-called-with "/path/to/existing/dir")))


  (describe "zenit/goto-local-init-file"
    :var (zenit-local-conf-dir)

    (before-each
      (setq zenit-local-conf-dir temporary-file-directory)
      (with-temp-file (file-name-concat zenit-local-conf-dir "init.el")
        (erase-buffer)
        (prin1 '(modules! :test foo) (current-buffer)))
      (setq buf (with-current-buffer (get-buffer-create "test")
                  (insert-file-contents
                   (expand-file-name "init.el" zenit-local-conf-dir))
                  (current-buffer))))

    (after-each
      (kill-buffer buf))

    (it "calls find-file with the correct file path"
      (spy-on 'find-file)
      (zenit/goto-local-init-file)
      (expect 'find-file :to-have-been-called-with (expand-file-name "init.el" zenit-local-conf-dir)))

    (it "jumps to the `(modules!` block in the file"
      (spy-on 'find-file :and-call-fake (lambda (filename &optional wildcards)
                                          (pop-to-buffer-same-window buf)))
      (expect (zenit/goto-local-init-file) :to-be 10)))


  (describe "zenit/goto-local-config-file"
    :var (zenit-local-conf-dir)

    (before-each
      (setq zenit-local-conf-dir "/path/to/existing/dir"))

    (it "calls find-file with the correct file path"
      (spy-on 'find-file)
      (zenit/goto-local-config-file)
      (expect 'find-file :to-have-been-called-with (expand-file-name "config.el" zenit-local-conf-dir))))


  (describe "zenit/goto-local-packages-file"
    :var (zenit-local-conf-dir)

    (before-each
      (setq zenit-local-conf-dir "/path/to/existing/dir"))

    (it "calls find-file with the correct file path"
      (spy-on 'find-file)
      (zenit/goto-local-packages-file)
      (expect 'find-file :to-have-been-called-with (expand-file-name "packages.el" zenit-local-conf-dir))))


  (describe "zenit/open-in-magit"
    (it "calls magit-status with correct path if available"
      (spy-on 'magit-status)
      (zenit/open-in-magit)
      (expect 'magit-status :to-have-been-called-with zenit-emacs-dir))

    (it "throws error if magit-status is not available"
      (expect (zenit/open-in-magit) :to-throw)))


  (describe "zenit/search-in-emacsd"
    (it "calls +vertico/project-search-from-cwd if :completion vertico is enabled"
      (defmacro modulep! (category &optional module flag) t)
      (spy-on 'call-interactively)
      (zenit/search-in-emacsd)
      (expect 'call-interactively :to-have-been-called-with #'+vertico/project-search-from-cwd))

    (it "calls projectile-ripgrep if :completion vertico is not enabled"
      (defmacro modulep! (category &optional module flag) nil)
      (spy-on 'call-interactively)
      (zenit/search-in-emacsd)
      (expect 'call-interactively :to-have-been-called-with #'projectile-ripgrep)))


  (describe "zenit/browse-emacsd"
    (it "calls zenit-project-browse with correct path"
      (spy-on 'zenit-project-browse)
      (zenit/browse-emacsd)
      (expect 'zenit-project-browse :to-have-been-called-with zenit-emacs-dir)))


  (describe "zenit/find-in-emacsd"
    (it "calls zenit-project-find-file with correct path"
      (spy-on 'zenit-project-find-file)
      (zenit/find-in-emacsd)
      (expect 'zenit-project-find-file :to-have-been-called-with zenit-emacs-dir)))


  (describe "zenit/insert-date"
    (it "inserts the current date in the default format (YYYY-MM-DD) when called without arguments"
      (with-temp-buffer
        (zenit/insert-date)
        (expect (buffer-string) :to-match (format-time-string "%Y-%m-%d"))))

    (it "inserts the current date in the alternative format (DD.MM.YYYY) when called with a prefix argument"
      (with-temp-buffer
        (zenit/insert-date t)
        (expect (buffer-string) :to-match (format-time-string "%d.%m.%Y"))))))

;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-lib-projects.el

(describe "core/lib/projects"

  (require 'projectile)

  (before-each
    (setq projectile-enable-caching nil)
    (projectile-mode +1))

  (after-each
    (projectile-mode -1))

  (load! "lib/files" zenit-core-dir)
  (load! "lib/projects" zenit-core-dir)

  (describe "set-project-type!"
    (before-each
      (spy-on 'add-to-list :and-call-through)
      (setq projectile-project-types nil))

    (it "adds a new project type to projectile-project-types"
      (set-project-type! 'my-test-type
        :predicate (lambda () t)
        :compile "make"
        :run "./my-test-type"
        :test "make test"
        :configure "make configure"
        :dir "build")
      (expect (assq 'my-test-type projectile-project-types) :not :to-be nil))

    (it "sets the appropriate properties for the new project type"
      (set-project-type! 'my-test-type
        :predicate (lambda () t)
        :compile "make"
        :run "./my-test-type"
        :test "make test"
        :configure "make configure"
        :dir "build")

      (let ((project-type (assq 'my-test-type projectile-project-types)))
        (expect (plist-get (cdr project-type) 'marker-files) :to-equal (lambda () t))
        (expect (plist-get (cdr project-type) 'compilation-dir) :to-equal "build")
        (expect (plist-get (cdr project-type) 'configure-command) :to-equal "make configure")
        (expect (plist-get (cdr project-type) 'compile-command) :to-equal "make")
        (expect (plist-get (cdr project-type) 'test-command) :to-equal "make test")
        (expect (plist-get (cdr project-type) 'run-command) :to-equal "./my-test-type")))

    (it "updates an existing project type"
      (set-project-type! 'my-test-type
        :predicate (lambda () t)
        :compile "make"
        :run "./my-test-type"
        :test "make test"
        :configure "make configure"
        :dir "build")

      (set-project-type! 'my-test-type
        :predicate (lambda () t)
        :compile "make"
        :run "./my-test-type"
        :test "make test"
        :configure "make configure"
        :dir "build-2")

      (let ((project-type (assq 'my-test-type projectile-project-types)))
        (expect (plist-get (cdr project-type) 'marker-files) :to-equal (lambda () t))
        (expect (plist-get (cdr project-type) 'compilation-dir) :to-equal "build-2")
        (expect (plist-get (cdr project-type) 'configure-command) :to-equal "make configure")
        (expect (plist-get (cdr project-type) 'compile-command) :to-equal "make")
        (expect (plist-get (cdr project-type) 'test-command) :to-equal "make test")
        (expect (plist-get (cdr project-type) 'run-command) :to-equal "./my-test-type"))))


  (describe "project-file-exists-p!"
    (it "checks if the file exists in the project root"
      (let ((default-directory user-emacs-directory))
        (expect (project-file-exists-p! "init.el") :to-be-truthy)))

    (it "checks if the absolute file exists for absolute paths"
      (expect (project-file-exists-p! (file-name-concat user-emacs-directory "init.el")) :to-be-truthy)))


  (describe "zenit/find-file-in-other-project"
    (before-each
      (spy-on 'file-directory-p :and-return-value t)
      (spy-on 'zenit-project-find-file))

    (it "calls zenit-project-find-file with the selected project root"
      (zenit/find-file-in-other-project "/selected/project/root")
      (expect 'zenit-project-find-file :to-have-been-called-with "/selected/project/root")))


  (describe "zenit/browse-in-other-project"
    (before-each
      (spy-on 'file-directory-p :and-return-value t)
      (spy-on 'zenit-project-browse))

    (it "calls zenit-project-browse with the selected project root"
      (zenit/browse-in-other-project "/selected/project/root")
      (expect 'zenit-project-browse :to-have-been-called-with "/selected/project/root")))


  (describe "zenit/add-directory-as-project"
    :var (temp-dir)

    (before-each
      (setq temp-dir (make-temp-file "test-dir" t)))

    (after-each
      (delete-directory temp-dir t))

    (before-each
      (spy-on 'message)
      (spy-on 'projectile-add-known-project :and-call-through))

    (it "adds a .project file to a non-project directory"
      (zenit/add-directory-as-project temp-dir)
      (expect (file-exists-p (expand-file-name ".project" temp-dir)) :to-be t))

    (it "calls projectile-add-known-project with the directory"
      (zenit/add-directory-as-project temp-dir)
      (expect 'projectile-add-known-project :to-have-been-called-with temp-dir))

    (it "gives a message when adding a .project file"
      (zenit/add-directory-as-project temp-dir)
      (expect 'message :to-have-been-called-with "%S was not a project; adding .project file to it" (abbreviate-file-name temp-dir))))


  (describe "zenit-project-p"
    (it "returns t for a valid project"
      (expect (zenit-project-p user-emacs-directory) :to-be t))

    (it "returns nil for an invalid project"
      (expect (zenit-project-p temporary-file-directory) :to-be nil)))


  (describe "zenit-project-root"
    (it "returns a path for a valid project"
      (expect (zenit-project-root user-emacs-directory) :to-be-truthy))

    (it "returns nil for an invalid project"
      (expect (zenit-project-root temporary-file-directory) :to-be nil)))


  (describe "zenit-project-name"
    (it "returns the name of the current project"
      (expect (zenit-project-name) :to-be-truthy)
      (expect (zenit-project-name temporary-file-directory) :to-be-truthy)))


  (describe "zenit-project-expand"
    (it "returns the name of the current project"
      (expect (zenit-project-expand "foo" user-emacs-directory) :to-equal (expand-file-name (concat user-emacs-directory "foo")))))


  (describe "zenit-project-find-file"
    (before-each
      (setq temp-dir (make-temp-file "temp-project" t))
      (with-temp-file (concat temp-dir "/.project"))
      (setq test-file (concat temp-dir "/test.txt"))
      (with-temp-file test-file))

    (after-each
      (delete-directory temp-dir t))

    (it "finds a file in a project"
      (cl-letf (((symbol-function 'call-interactively)
                 (lambda (func &optional record-flag keys)
                   (funcall func test-file)))
                ((symbol-function 'projectile-find-file)
                 (lambda (&optional arg)
                   (find-file test-file))))
        (zenit-project-find-file temp-dir)
        (expect (buffer-file-name) :to-equal test-file)
        (kill-buffer))))


  (describe "zenit-project-browse"
    (before-each
      (setq temp-dir (make-temp-file "temp-browse" t))
      (setq test-file (concat temp-dir "/test.txt"))
      (with-temp-file test-file))

    (after-each
      (delete-directory temp-dir t))

    (it "browses a directory"
      (cl-letf (((symbol-function 'call-interactively)
                 (lambda (func &optional record-flag keys)
                   (funcall func test-file))))
        (zenit-project-browse temp-dir)
        (expect (buffer-file-name) :to-equal test-file)
        (kill-buffer))))


  (describe "zenit-project-ignored-p"
    (it "recognizes non-ignored projects"
      (expect (zenit-project-ignored-p user-emacs-directory) :to-be nil))

    (it "recognizes ignored projects in temporary-file-directory"
      (expect (zenit-project-ignored-p temporary-file-directory) :to-be t))

    (it "recognizes ignored projects in zenit-emacs-dir/straight"
      (expect (zenit-project-ignored-p (concat zenit-emacs-dir "straight")) :to-be t))

    (it "recognizes ignored projects in zenit-local-dir"
      (expect (zenit-project-ignored-p zenit-local-dir) :to-be t))))

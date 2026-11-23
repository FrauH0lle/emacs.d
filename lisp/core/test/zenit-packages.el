;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/zenit-packages.el

(require 'zenit-test)
(require 'zenit-packages)
(require 'straight)
(zenit-require 'zenit-lib 'files)

(zenit-deftest zenit-init-packages-p
  (:doc "`zenit-init-packages-p' is defined")
  (should (boundp 'zenit-init-packages-p)))

(zenit-deftest zenit-mandatory-packages
  (:doc "`zenit-mandatory-packages' is defined")
  (should (boundp 'zenit-mandatory-packages)))

(zenit-deftest zenit-packages
  (:doc "`zenit-packages' is defined")
  (should (boundp 'zenit-packages)))

(zenit-deftest zenit-disabled-packages
  (:doc "`zenit-disabled-packages' is defined")
  (should (boundp 'zenit-disabled-packages)))

(zenit-deftest zenit-packages-file
  (:doc "`zenit-packages-file' is defined")
  (should (boundp 'zenit-packages-file)))

(zenit-deftest +straight--fixup-profiles
  (:doc "`+straight--fixup-profiles' is defined")
  (should (fboundp '+straight--fixup-profiles)))

(zenit-deftest +straight--fixup-profiles/ignore
  (:doc "`+straight--fixup-profiles' drops every other profile of an ignored package")
  (let ((straight--recipe-cache (make-hash-table :test #'equal))
        (straight--profile-cache (make-hash-table :test #'equal))
        (straight-recipe-repositories nil))
    (puthash "ignored" '(tools ignore local) straight--profile-cache)
    (puthash "pinned" '(tools local) straight--profile-cache)
    (+straight--fixup-profiles)
    (should (equal '(ignore) (gethash "ignored" straight--profile-cache)))
    (should (equal '(tools local) (gethash "pinned" straight--profile-cache)))))

(zenit-deftest +straight--lockfile-read-all--prefer-local-versions-a
  (:doc "`+straight--lockfile-read-all--prefer-local-versions-a' never pins ignored packages")
  (let ((straight--repo-cache (make-hash-table :test #'equal))
        (straight--profile-cache (make-hash-table :test #'equal))
        (straight-profiles '((tools . "tools.el") (local . "local.el")))
        (zenit-local-versions-dir (make-temp-name
                                   (expand-file-name "zenit-test-versions-" temporary-file-directory))))
    (puthash "ignored-repo" '(:package "ignored") straight--repo-cache)
    (puthash "pinned-repo" '(:package "pinned") straight--repo-cache)
    (puthash "local-repo" '(:package "local") straight--repo-cache)
    (puthash "ignored" '(ignore) straight--profile-cache)
    (puthash "pinned" '(tools) straight--profile-cache)
    (puthash "local" '(local) straight--profile-cache)
    (should (equal '(("pinned-repo" . "b") ("unknown-repo" . "d"))
                   (+straight--lockfile-read-all--prefer-local-versions-a
                    '(("ignored-repo" . "a") ("pinned-repo" . "b")
                      ("local-repo" . "c") ("unknown-repo" . "d")))))))

(zenit-deftest zenit-packages--lockfile-profile
  (:doc "`zenit-packages--lockfile-profile' honours ignore in local-conf")
  (progn
    (should (eq 'ignore (zenit-packages--lockfile-profile 'ignore t)))
    (should (eq 'ignore (zenit-packages--lockfile-profile ''ignore t)))
    (should (eq 'ignore (zenit-packages--lockfile-profile ''ignore nil)))
    (should (eq 'local (zenit-packages--lockfile-profile 'tools t)))
    (should (eq 'local (zenit-packages--lockfile-profile nil t)))
    (should (eq 'tools (zenit-packages--lockfile-profile 'tools nil)))
    (should (eq t (zenit-packages--lockfile-profile t nil)))
    (should-not (zenit-packages--lockfile-profile nil nil))))

(zenit-deftest zenit-packages--read/local-conf-lockfile
  (:doc "`zenit-packages--read' keeps ignore for local-conf declarations")
  (let* ((zenit-local-conf-dir (file-name-as-directory (make-temp-file "zenit-test-conf-" t)))
         (file (expand-file-name "packages.el" zenit-local-conf-dir))
         zenit-packages)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(package! ignored :lockfile ignore)\n"
                    "(package! quoted :lockfile 'ignore)\n"
                    "(package! pinned :lockfile tools)\n"
                    "(package! bare)\n"))
          (letf! ((defun! zenit-module-context-key () '(:local-conf)))
            (zenit-packages--read file))
          (should (equal '(ignore) (plist-get (alist-get 'ignored zenit-packages) :lockfile)))
          (should (equal '(ignore) (plist-get (alist-get 'quoted zenit-packages) :lockfile)))
          (should (equal '(local) (plist-get (alist-get 'pinned zenit-packages) :lockfile)))
          (should (equal '(local) (plist-get (alist-get 'bare zenit-packages) :lockfile))))
      (delete-directory zenit-local-conf-dir t))))

(zenit-deftest +straight--auto-options
  (:doc "`+straight--auto-options' is defined")
  (should (boundp '+straight--auto-options)))

(zenit-deftest straight-vc-git--popup-raw
  (:doc "`straight-vc-git--popup-raw' advises `straight--popup-raw'")
  (skip-unless noninteractive)
  (should (advice-member-p 'straight-vc-git--popup-raw #'straight--popup-raw)))

(zenit-deftest +straight--fallback-to-y-or-n-prompt-a
  (:doc "`+straight--fallback-to-y-or-n-prompt-a' advises `straight-are-you-sure'")
  (progn
    (should (fboundp '+straight--fallback-to-y-or-n-prompt-a))
    (should (advice-member-p '+straight--fallback-to-y-or-n-prompt-a #'straight-are-you-sure))))

(zenit-deftest +straight--recommended-option-p
  (:doc "`+straight--recommended-option-p' is defined")
  (should (fboundp '+straight--recommended-option-p)))

(zenit-deftest +straight--fallback-to-tty-prompt-a
  (:doc "`+straight--fallback-to-tty-prompt-a' advises `straight--popup-raw'")
  (progn
    (should (fboundp '+straight--fallback-to-tty-prompt-a))
    (should (advice-member-p '+straight--fallback-to-tty-prompt-a #'straight--popup-raw))))

(zenit-deftest +straight--respect-print-indent-a
  (:doc "`+straight--respect-print-indent-a' advises `straight-use-package'")
  (progn
    (should (fboundp '+straight--respect-print-indent-a))
    (should (advice-member-p '+straight--respect-print-indent-a #'straight--output))))

(zenit-deftest zenit-read-packages
  (:doc "`zenit-read-packages' is defined")
  (should (fboundp 'zenit-read-packages)))

(zenit-deftest zenit-initialize-packages
  (:doc "`zenit-initialize-packages' is defined")
  (should (fboundp 'zenit-initialize-packages)))

(zenit-deftest zenit-bootstrap-straight
  (:doc "`zenit-bootstrap-straight' is defined")
  (should (fboundp 'zenit-bootstrap-straight)))

(zenit-deftest zenit-package-get
  (:doc "`zenit-package-get' returns package recipe")
  (let ((zenit-packages '((ws-butler :lockfile
                           (core)
                           :recipe
                           (:host github :repo "emacsmirror/nongnu_elpa" :branch "elpa/ws-butler" :local-repo "ws-butler")
                           :modules
                           ((:core))))))
    (should
     (equal
      '(:lockfile (core)
        :recipe (:host github :repo "emacsmirror/nongnu_elpa" :branch "elpa/ws-butler" :local-repo "ws-butler")
        :modules ((:core)))
      (zenit-package-get 'ws-butler)))
    (should (equal '((:core)) (zenit-package-get 'ws-butler :modules)))
    (should (equal 'default (zenit-package-get 'ws-butler :nosuchkey 'default)))))

(zenit-deftest zenit-package-set
  (:doc "`zenit-package-set' sets property in package recipe")
  (let ((zenit-packages '((ws-butler :lockfile
                           (core)
                           :recipe
                           (:host github :repo "emacsmirror/nongnu_elpa" :branch "elpa/ws-butler" :local-repo "ws-butler")
                           :modules
                           ((:core))))))
    (should
     (equal
      '(:lockfile (core)
        :recipe (:host github :repo "emacsmirror/nongnu_elpa" :branch "elpa/ws-butler" :local-repo "ws-butler")
        :modules ((:other)))
      (zenit-package-set 'ws-butler :modules '((:other)))))))

(zenit-deftest zenit-package-recipe-repo
  (:doc "`zenit-package-recipe-repo' returns package's local-repo property")
  (should (equal "emacs" (zenit-package-recipe-repo 'emacs))))

(zenit-deftest zenit-package-build-recipe
  (:doc "`zenit-package-build-recipe' returns package's `straight' recipe"
   :before-each (straight--load-build-cache))
  (should
   (equal
    '(:type git :host github :repo "radian-software/straight.el" :files ("straight*.el")
      :branch "develop" :package "straight" :local-repo "straight.el")
    (zenit-package-build-recipe 'straight))))

(zenit-deftest zenit-package-built-in-p
  (:doc "`zenit-package-built-in-p' returns non-nil if package is built-in"
   :before-each (straight--load-build-cache))
  (should-not (zenit-package-built-in-p 'straight)))

(zenit-deftest zenit-package-backend
  (:doc "`zenit-package-backend' returns the package backend"
   :before-each (straight--load-build-cache))
  (should (eq 'straight (zenit-package-backend 'straight))))

(zenit-deftest zenit-packages--read
  (:doc "`zenit-packages--read' is defined")
  (should (fboundp 'zenit-packages--read)))

(zenit-deftest zenit-package-list
  (:doc "`zenit-package-list' is defined")
  (should (fboundp 'zenit-package-list)))

(zenit-deftest zenit--process-pkg-dependencies
  ()
  (let ((straight--build-cache (make-hash-table :test #'equal))
        (load-path load-path)
        registered
        loaded)
    (dolist (entry ,cache)
      (puthash (car entry)
               (list nil (list :version "1.0" :dependencies (cdr entry)))
               straight--build-cache))
    (cl-letf (((symbol-function 'straight-register-package)
               (lambda (package)
                 (should (eq '_pkg-dependency straight-current-profile))
                 (push package registered)))
              ((symbol-function 'straight--build-dir)
               (lambda (package) (concat "/test-build/" package "/")))
              ((symbol-function 'straight--load-package-autoloads)
               (lambda (package) (push package loaded))))
      (zenit--process-pkg-dependencies "test-root"))
    (should (equal ,expected (sort loaded #'string<)))
    (should (equal (mapcar #'intern (remove "test-root" ,expected))
                   (sort registered #'string<)))
    (dolist (package ,expected)
      (should (member (concat "/test-build/" package) load-path))))
  (cache expected)

  :doc "Packages without dependencies still load their own autoloads"
  '(("test-root")) '("test-root")

  :doc "Versioned Emacs dependencies are registered by name"
  '(("test-root" ("emacs" . "25.1"))) '("emacs" "test-root")

  :doc "Direct and transitive dependencies deduplicate by name, not version"
  '(("test-root" ("test-a" . "1.2") ("test-b" . nil))
    ("test-a" ("test-shared" . "2.0"))
    ("test-b" ("test-shared" . "3.0")))
  '("test-a" "test-b" "test-root" "test-shared"))

(zenit-deftest package!
  (:doc "`package!' is defined")
  (should (fboundp 'package!)))

(zenit-deftest package!/lockfile-profile
  (:doc "`package!' binds the profile derived from :lockfile and the declaring file")
  (let ((straight-profiles nil)
        zenit-disabled-packages)
    (cl-flet ((profile (file form)
                (let ((zenit-include--current-file file))
                  (cadr (assq 'straight-current-profile
                              (cadr (macroexpand-1 form)))))))
      ;; A file that doesn't exist, so a deployed (symlinked) packages.el in
      ;; `zenit-local-conf-dir' can't change how the path is classified.
      (let ((local-file (expand-file-name "zenit-test-packages.el" zenit-local-conf-dir))
            (module-file (expand-file-name "tools/x/packages.el" zenit-modules-dir)))
        (should (equal ''ignore (profile local-file '(package! pkg :lockfile ignore))))
        (should (equal ''ignore (profile local-file '(package! pkg :lockfile 'ignore))))
        (should (equal ''local (profile local-file '(package! pkg :lockfile tools))))
        (should (equal ''local (profile local-file '(package! pkg))))
        (should (equal ''ignore (profile module-file '(package! pkg :lockfile ignore))))
        (should (equal ''tools (profile module-file '(package! pkg :lockfile tools))))
        (should (equal ''nil (profile module-file '(package! pkg :lockfile t))))
        ;; Only real lockfiles become straight profiles.
        (should (equal '((tools . "tools.el")) straight-profiles))))))

(zenit-deftest package!/ignore-recipe-override
  (:doc "`package!' lets an ignored declaration's recipe win over a module's")
  (let ((straight-profiles '((tools . "tools.el") (core . "core.el")
                             (nil . "default.el") (local . "local.el")))
        (straight-recipe-overrides nil)
        (straight--recipe-cache (make-hash-table :test #'equal))
        (zenit-packages nil)
        zenit-disabled-packages)
    (letf! ((defun! zenit-context-p (_context) t)
            (defun! zenit--process-pkg-dependencies (_package))
            (defun! straight-register-package (_recipe)))
      (cl-flet ((declare (file form)
                  (let ((zenit-include--current-file file))
                    (eval (macroexpand-1 form) t))))
        (let ((local-file (expand-file-name "zenit-test-packages.el" zenit-local-conf-dir))
              (module-file (expand-file-name "tools/x/packages.el" zenit-modules-dir))
              (other-file (expand-file-name "tools/y/packages.el" zenit-modules-dir)))
          ;; Same order as `zenit-read-packages': local, modules, local again.
          (declare local-file '(package! pkg :recipe (:repo "local") :lockfile ignore))
          (declare module-file '(package! pkg :recipe (:repo "module") :lockfile tools))
          (should (equal '(pkg :repo "local") (straight--get-overridden-recipe 'pkg)))
          (declare local-file '(package! pkg :recipe (:repo "local") :lockfile ignore))
          (should (equal '(pkg :repo "local") (straight--get-overridden-recipe 'pkg)))
          ;; An ignored module declaration still beats another module's.
          (declare other-file '(package! other :recipe (:repo "other") :lockfile ignore))
          (declare module-file '(package! other :recipe (:repo "module") :lockfile tools))
          (should (equal '(other :repo "other") (straight--get-overridden-recipe 'other))))))))

(zenit-deftest disable-packages!
  (:doc "`disable-packages!' is defined")
  (should (fboundp 'disable-packages!)))

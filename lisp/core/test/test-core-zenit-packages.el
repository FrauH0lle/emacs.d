;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-core-zenit-packages.el


(describe "core/zenit-packages"

  (load! "zenit-packages" zenit-core-dir)
  (autoload 'straight--with-plist "straight")
  (describe "zenit--package-inhibit-custom-file-a"
    (it "is defined"
      (expect (fboundp 'zenit--package-inhibit-custom-file-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p 'zenit--package-inhibit-custom-file-a 'package--save-selected-packages) :to-be-truthy)))


  (describe "+straight--ignore-missing-lockfile-a"
    (it "is defined"
      (expect (fboundp '+straight--ignore-missing-lockfile-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p '+straight--ignore-missing-lockfile-a 'straight--versions-lockfile) :to-be-truthy)))


  (describe "+straight--normalize-profiles"
    (before-each
      (setq straight--profile-cache (make-hash-table :test #'equal)
            straight--recipe-cache (make-hash-table :test #'equal)
            straight-recipe-repositories '(melpa el-get)))

    (it "removes nil from packages with multiple profiles"
      (puthash "package1" '(nil t) straight--profile-cache)
      (puthash "package2" '(t nil t) straight--profile-cache)
      (+straight--normalize-profiles)
      (expect (gethash "package1" straight--profile-cache) :to-equal '(t))
      (expect (gethash "package2" straight--profile-cache) :to-equal '(t t)))

    (it "does not affect packages with one non-nil profile"
      (puthash "package3" '(t) straight--profile-cache)
      (+straight--normalize-profiles)
      (expect (gethash "package3" straight--profile-cache) :to-equal '(t)))

    (it "does not affect packages with one nil profile"
      (puthash "package4" '(nil) straight--profile-cache)
      (+straight--normalize-profiles)
      (expect (gethash "package4" straight--profile-cache) :to-equal '(nil)))

    (it "removes 'dep from packages with multiple profiles"
      (puthash "package1" '(nil t dep) straight--profile-cache)
      (puthash "package2" '(t nil dep t) straight--profile-cache)
      (+straight--normalize-profiles)
      (expect (gethash "package1" straight--profile-cache) :to-equal '(t))
      (expect (gethash "package2" straight--profile-cache) :to-equal '(t t)))

    (it "ensures recipe repositories are registered under core"
      (puthash "melpa" '(dep foo) straight--profile-cache)
      (puthash "el-get" '(t nil dep t) straight--profile-cache)
      (puthash "package3" '(t) straight--profile-cache)
      (+straight--normalize-profiles)
      (expect (gethash "melpa" straight--profile-cache) :to-equal '(core))
      (expect (gethash "el-get" straight--profile-cache) :to-equal '(core))
      (expect (gethash "package3" straight--profile-cache) :to-equal '(t)))

    (it "handles multi-package repositories"
      (puthash "magit" '(foo) straight--profile-cache)
      (puthash "magit-section" '(dep) straight--profile-cache)
      (puthash "magit" '(:flavor melpa :package "magit" :local-repo "magit"
                         :type git :repo "magit/magit" :host github)
               straight--recipe-cache)
      (puthash "magit-section" '(:flavor melpa :package "magit-section" :local-repo "magit"
                                 :type git :repo "magit/magit" :host github)
               straight--recipe-cache)
      (+straight--normalize-profiles)
      (expect (gethash "magit" straight--profile-cache) :to-equal '(foo))
      (expect (gethash "magit-section" straight--profile-cache) :to-equal '(foo))))


  (describe "+straight--fallback-to-y-or-n-prompt-a"
    (it "is defined"
      (expect (fboundp '+straight--fallback-to-y-or-n-prompt-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p '+straight--fallback-to-y-or-n-prompt-a 'straight-are-you-sure) :to-be-truthy)))


  (describe "+straight--recommended-option-p"
    (it "returns true for matching prompt-option pair"
      (add-to-list '+straight--auto-options '("^test-prompt" . "^test-option"))
      (expect (+straight--recommended-option-p "test-prompt" "test-option") :to-be 0))

    (it "returns nil for non-matching prompt-option pair"
      (expect (+straight--recommended-option-p "test-prompt" "wrong-option") :to-be nil))

    (it "returns nil for non-existing prompt"
      (expect (+straight--recommended-option-p "non-existing-prompt" "test-option") :to-be nil)))


  (describe "+straight--fallback-to-tty-prompt-a"
    (it "is defined"
      (expect (fboundp '+straight--fallback-to-tty-prompt-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p '+straight--fallback-to-tty-prompt-a 'straight--popup-raw) :to-be-truthy)))


  (describe "+straight--respect-print-indent-a"
    (it "is defined"
      (expect (fboundp '+straight--respect-print-indent-a) :to-be-truthy))

    (it "advises the correct function"
      (expect (advice-member-p '+straight--respect-print-indent-a 'straight-use-package) :to-be-truthy)))


  (describe "zenit-read-packages"
    (before-all
      (load! "zenit-modules" zenit-core-dir))

    (before-each
      (spy-on 'zenit-module-list :and-return-value '((:core . "core") (:user . "user")))
      (spy-on 'zenit-module-locate-path :and-return-value "path-to-module")
      (spy-on 'load :and-call-fake (lambda (&rest args) nil)))

    (it "loads packages from enabled modules when MODULE-LIST is nil"
      (zenit-read-packages nil)
      (expect 'zenit-module-list :to-have-been-called)
      (expect 'zenit-module-locate-path :to-have-been-called-with :core "core" zenit-module-packages-file)
      (expect 'zenit-module-locate-path :to-have-been-called-with :user "user" zenit-module-packages-file)
      (expect 'load :to-have-been-called-with "path-to-module" 'noerror 'nomessage 'nosuffix))

    (it "loads packages from all modules when MODULE-LIST is a symbol"
      (zenit-read-packages 'all)
      (expect 'zenit-module-list :to-have-been-called-with 'all)
      (expect 'zenit-module-locate-path :to-have-been-called-with :core "core" zenit-module-packages-file)
      (expect 'zenit-module-locate-path :to-have-been-called-with :user "user" zenit-module-packages-file)
      (expect 'load :to-have-been-called-with "path-to-module" 'noerror 'nomessage 'nosuffix))

    (it "loads packages from specific modules when MODULE-LIST is a list of module keys"
      (zenit-read-packages '((:module1 . "module1") (:module2 . "module2")))
      (expect 'zenit-module-locate-path :to-have-been-called-with :module1 "module1" zenit-module-packages-file)
      (expect 'zenit-module-locate-path :to-have-been-called-with :module2 "module2" zenit-module-packages-file)
      (expect 'load :to-have-been-called-with "path-to-module" 'noerror 'nomessage 'nosuffix)))


  (describe "zenit-initialize-packages"
    (before-each
      (spy-on 'require)
      (spy-on 'package-initialize)
      (spy-on 'zenit-bootstrap-straight)
      (spy-on 'straight-use-package)
      (spy-on 'zenit-read-packages)
      (spy-on '+straight--normalize-profiles))

    (it "initializes package.el and straight.el when package--initialized and straight--reset-caches are not bound and true"
      (zenit-initialize-packages nil)
      (expect 'require :to-have-been-called-with 'package)
      (expect 'package-initialize :to-have-been-called)
      (expect 'straight-use-package :to-have-been-called-with 'straight)
      (expect 'zenit-read-packages :to-have-been-called)
      (expect '+straight--normalize-profiles :to-have-been-called))

    (it "initializes package.el and straight.el when FORCE-P is true"
      (zenit-initialize-packages t)
      (expect 'require :to-have-been-called-with 'package)
      (expect 'package-initialize :to-have-been-called)
      (expect 'straight-use-package :to-have-been-called-with 'straight)
      (expect 'zenit-read-packages :to-have-been-called)
      (expect '+straight--normalize-profiles :to-have-been-called)))


  (describe "zenit-package-recipe-repo"
    (before-all
      (load! "lib/files" zenit-core-dir))

    (before-each
      (setq straight-current-profile 'core)
      (zenit-bootstrap-straight)
      (setq straight-current-profile nil))

    (it "returns local-repo property when it exists"
      (expect (zenit-package-recipe-repo 'straight) :to-equal "straight.el"))

    (it "returns symbol-name when local-repo property does not exist"
      (expect (zenit-package-recipe-repo 'package) :to-equal "package")))


  (describe "package!"
    (before-each
      (setq zenit-disabled-packages ()
            straight-profiles ()
            straight--recipe-cache (make-hash-table :test 'equal)
            straight-x-pinned-packages nil)
      (spy-on 'locate-library :and-return-value t)
      (spy-on 'straight-register-package)
      (spy-on 'straight--get-dependencies :and-return-value nil)
      (spy-on 'straight--build-dir :and-return-value "/tmp/package-build-dir")
      (spy-on 'straight--load-package-autoloads)
      (spy-on 'zenit-package-recipe-repo :and-return-value "test/repo"))

    (it "handles the :built-in 'prefer option"
      (package! test-package :built-in 'prefer)
      (expect 'locate-library :to-have-been-called-with "test-package" nil (get 'load-path 'initial-value))
      (expect 'straight-register-package :not :to-have-been-called))

    (it "handles the :disable option"
      (package! test-package :disable t)
      (expect zenit-disabled-packages :to-contain 'test-package)
      (expect 'straight-register-package :not :to-have-been-called))

    (it "handles the :ignore option"
      (package! test-package :ignore t)
      (expect 'straight-register-package :not :to-have-been-called))

    (it "registers the package when it's not ignored"
      (package! test-package)
      (expect 'straight-register-package :to-have-been-called-with 'test-package))

    (it "handles the :lockfile option"
      (package! test-package :lockfile "test-lockfile")
      (expect straight-profiles :to-contain '("test-lockfile" . "test-lockfile.el")))

    (it "handles the :pin option"
      (package! test-package :pin "test-pin")
      (expect straight-x-pinned-packages :to-contain `("test/repo" . "test-pin"))
      (expect straight-profiles :to-contain '(pinned . "pinned.el")))))

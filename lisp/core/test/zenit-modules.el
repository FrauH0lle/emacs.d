;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/zenit-modules.el

(require 'zenit-test)
(require 'zenit-modules)
(zenit-require 'zenit-lib 'files)

(zenit-deftest zenit-modules
  (:doc "`zenit-modules' is defined")
  (should (boundp 'zenit-modules)))

(zenit-deftest zenit-modules-dirs
  (:doc "`zenit-modules-dirs' is defined")
  (should (boundp 'zenit-modules-dirs)))

(zenit-deftest zenit-module-init-file
  (:doc "`zenit-module-init-file' is defined")
  (should (boundp 'zenit-module-init-file)))

(zenit-deftest zenit-module-config-file
  (:doc "`zenit-module-config-file' is defined")
  (should (boundp 'zenit-module-config-file)))

(zenit-deftest zenit-module-packages-file
  (:doc "`zenit-module-packages-file' is defined")
  (should (boundp 'zenit-module-packages-file)))

(zenit-deftest zenit-module-control-file
  (:doc "`zenit-module-control-file' is defined")
  (should (boundp 'zenit-module-control-file)))

(zenit-deftest zenit-inhibit-module-warnings
  (:doc "`zenit-inhibit-module-warnings' is defined")
  (should (boundp 'zenit-inhibit-module-warnings)))

(zenit-deftest zenit-before-modules-init-hook
  (:doc "`zenit-before-modules-init-hook' is defined")
  (should (boundp 'zenit-before-modules-init-hook)))

(zenit-deftest zenit-after-modules-init-hook
  (:doc "`zenit-after-modules-init-hook' is defined")
  (should (boundp 'zenit-after-modules-init-hook)))

(zenit-deftest zenit-before-modules-config-hook
  (:doc "`zenit-before-modules-config-hook' is defined")
  (should (boundp 'zenit-before-modules-config-hook)))

(zenit-deftest zenit-after-modules-config-hook
  (:doc "`zenit-after-modules-config-hook' is defined")
  (should (boundp 'zenit-after-modules-config-hook)))

(zenit-deftest zenit-module-p
  (:doc "`zenit-module-p' returns non-nil if module is enabled")
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (puthash (cons :cat1 'module1) '(:flags (flag1)) zenit-modules)
    (puthash (cons :cat2 'module2) '(:flags nil) zenit-modules)
    (should (zenit-module-p :cat1 'module1))
    (should (zenit-module-p :cat1 'module1 'flag1))
    (should (zenit-module-p :cat2 'module2))
    (should-not (zenit-module-p :cat2 'module2 'flag2))))

(zenit-deftest zenit-module-depth ()
  ,test
  (test)
  :doc "`zenit-module-depth' returns the depth if the category-module pair is present, and no initdepth is specified"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (puthash (cons :cat1 'module1) '(:depth (2 . 3)) zenit-modules)
    (should (equal 3 (zenit-module-depth :cat1 'module1))))
  :doc "`zenit-module-depth' returns the car of the depth if initdepth is specified"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (puthash (cons :cat1 'module1) '(:depth (2 . 3)) zenit-modules)
    (should (equal 2 (zenit-module-depth :cat1 'module1 t))))
  :doc "`zenit-module-depth' returns 0 if the category-module pair is not present"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (puthash (cons :cat1 'module1) '(:depth (2 . 3)) zenit-modules)
    (should (equal 0 (zenit-module-depth :cat2 'module2)))))

(zenit-deftest zenit-module--has-flag-p ()
  ,test
  (test)
  :doc "`zenit-module--has-flag-p' returns t if flags satisfy wanted-flags"
  (let ((flags '(+present +multiple -negated)))
    (should (zenit-module--has-flag-p flags '(+present)))
    (should (zenit-module--has-flag-p flags '(+present +multiple)))
    (should (zenit-module--has-flag-p flags '(-negated)))
    (should (zenit-module--has-flag-p flags '(+present -nonexistent))))

  :doc "`zenit-module--has-flag-p' returns nil if flags don't satisfy wanted-flags"
  (let ((flags '(+present +multiple -negated)))
    (should-not (zenit-module--has-flag-p flags '(+missing)))
    (should-not (zenit-module--has-flag-p flags '(-present)))
    (should-not (zenit-module--has-flag-p flags '(+present -multiple)))
    (should-not (zenit-module--has-flag-p flags '(+present +multiple -negated +extra)))))

(zenit-deftest zenit-module-get ()
  ,test
  (test)
  :doc "`zenit-module-get' retrieves module properties"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (puthash (cons :test 'module) '(:flags (+a +b) :depth 5 :path "/test") zenit-modules)
    (should (equal '(+a +b) (zenit-module-get :test 'module :flags)))
    (should (equal 5 (zenit-module-get :test 'module :depth))))

  :doc "`zenit-module-get' returns nil for missing properties"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (should-not (zenit-module-get :test 'module :nonexistent))))

(zenit-deftest zenit-module-put ()
  ,test
  (test)
  :doc "`zenit-module-put' updates module properties"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (zenit-module-put :test 'module :flags '(a b) :depth 2)
    (should (equal '(:flags (a b) :depth 2) (zenit-module-get :test 'module))))

  :doc "`zenit-module-put' updates existing properties"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (zenit-module-put :test 'module :flags '(a))
    (zenit-module-put :test 'module :flags '(a b))
    (should (equal '(a b) (zenit-module-get :test 'module :flags)))))

(zenit-deftest zenit-module-set ()
  ,test
  (test)
  :doc "`zenit-module-set' adds new modules"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (zenit-module-set :test 'module :flags '(a) :depth 3)
    (should (equal '(:flags (a) :depth 3) (zenit-module-get :test 'module))))

  :doc "`zenit-module-set' updates existing modules"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (zenit-module-set :test 'module :flags '(a))
    (zenit-module-set :test 'module :flags '(b))
    (should (equal '(b) (zenit-module-get :test 'module :flags))))

  :doc "`zenit-module-set' removes modules with nil plist"
  (let ((zenit-modules (make-hash-table :test #'equal)))
    (zenit-module-set :test 'module :flags '(a))
    (zenit-module-set :test 'module nil)
    (should-not (gethash (cons :test 'module) zenit-modules))))

(zenit-deftest zenit-module-list
  (:vars ((zenit-modules-dirs (list (file-name-concat temporary-file-directory "modules")))
          (zenit-modules (make-hash-table :test #'equal)))
   :before-each
   (progn
     (make-directory (file-name-concat temporary-file-directory "modules" "cat1" "mod1") t)
     (make-directory (file-name-concat temporary-file-directory "modules" "cat2" "mod2") t)
     (zenit-module-set :cat1 'mod1 :path (file-name-concat temporary-file-directory "cat1" "mod1") :depth '(2 . 1))
     (zenit-module-set :cat2 'mod2 :path (file-name-concat temporary-file-directory "cat2" "mod2") :depth '(1 . 2)))
   :after-each
   (progn
     (delete-directory (file-name-concat temporary-file-directory "modules") t)))
  ,test
  (test)
  :doc "`zenit-module-list' returns modules sorted by depth"
  (should (equal '((:cat2 . mod2) (:cat1 . mod1)) (zenit-module-list nil)))

  :doc "`zenit-module-list' returns modules sorted by their initdepth when initorder? is non-nil"
  (should (equal '((:cat1 . mod1) (:cat2 . mod2)) (zenit-module-list nil t)))

  :doc "`zenit-module-list' finds modules in directories"
  (should (member '(:cat1 . mod1) (zenit-module-list t))))

(zenit-deftest zenit-module-expand-path
  (:vars ((zenit-modules (make-hash-table :test #'equal)))
   :before-each
   (zenit-module-set :test 'mod :path "/test/path"))
  ,test
  (test)
  :doc "`zenit-module-expand-path' constructs path with file"
  (should (equal "/test/path/file.el"
                 (zenit-module-expand-path :test 'mod "file.el")))
  
  :doc "`zenit-module-expand-path' returns base path without file"
  (should (equal "/test/path"
                 (zenit-module-expand-path :test 'mod)))
  
  :doc "`zenit-module-expand-path' returns nil for disabled modules"
  (should-not (zenit-module-expand-path :disabled 'mod "file.el")))

(zenit-deftest zenit-module-locate-path
  (:vars ((zenit-modules-dirs (list temporary-file-directory)))
   :before-each
   (progn
     (make-directory (file-name-concat temporary-file-directory "test" "mod") t)
     (with-temp-file (file-name-concat temporary-file-directory "test" "mod" "test-file.el")
       (insert "test content")))
   :after-each
   (delete-directory (file-name-concat temporary-file-directory "test") t))
  ,test
  (test)
  :doc "`zenit-module-locate-path' finds existing files"
  (should (zenit-module-locate-path :test 'mod "test-file.el"))
  
  :doc "`zenit-module-locate-path' returns nil for missing files"
  (should-not (zenit-module-locate-path :test 'mod "nonexistent.el"))
  
  :doc "`zenit-module-locate-path' handles directory paths"
  (should (zenit-module-locate-path :test 'mod)))

(zenit-deftest zenit-module-locate-paths
  (:vars ((zenit-modules-dirs (list temporary-file-directory)))
   :before-each
   (progn
     (make-directory (file-name-concat temporary-file-directory "cat1" "mod1") t)
     (make-directory (file-name-concat temporary-file-directory "cat2" "mod2") t)
     (with-temp-file (file-name-concat temporary-file-directory "cat1" "mod1" "common.el")
       (insert "test content"))
     (with-temp-file (file-name-concat temporary-file-directory "cat2" "mod2" "common.el")
       (insert "test content"))
     (zenit-module-set :cat1 'mod1 :path (file-name-concat temporary-file-directory "cat1" "mod1"))
     (zenit-module-set :cat2 'mod2 :path (file-name-concat temporary-file-directory "cat2" "mod2")))
   :after-each
   (progn
     (delete-directory (file-name-concat temporary-file-directory "cat1") t)
     (delete-directory (file-name-concat temporary-file-directory "cat2") t)))
  ,test
  (test)
  :doc "`zenit-module-locate-paths' finds all instances of file"
  (let ((paths (zenit-module-locate-paths '((:cat1 . mod1) (:cat2 . mod2)) "common.el")))
    (should (= 2 (length paths)))))

(zenit-deftest zenit-module-load-path
  (:vars ((zenit-modules (make-hash-table :test #'equal)))
   :before-each
   (progn
     (zenit-module-set :test 'enabled :path "/path/to/enabled")
     (zenit-module-set :test 'disabled nil)))
  ,test
  (test)
  :doc "`zenit-module-load-path' includes enabled modules"
  (should (member "/path/to/enabled" (zenit-module-load-path)))
  
  :doc "`zenit-module-load-path' excludes disabled modules"
  (should-not (member nil (zenit-module-load-path))))

(zenit-deftest zenit-module-from-path
  (:vars ((zenit-modules (make-hash-table :test #'equal))
          (tmp-zenit-core-dir (zenit-test-make-temp-file t))
          (tmp-zenit-local-conf-dir (zenit-test-make-temp-file t)))
   :before-each
   (progn
     (zenit-module-set :lang 'python :path "/modules/lang/python")
     (zenit-module-set :tools 'lsp :path "/modules/tools/lsp")
     (zenit-file-write (file-name-concat tmp-zenit-core-dir "init.el") "Hello World")
     (zenit-file-write (file-name-concat tmp-zenit-local-conf-dir "init.el") "Hello World"))
   :after-each
   (progn
     (delete-directory tmp-zenit-core-dir t)
     (delete-directory tmp-zenit-local-conf-dir t)))
  ,test
  (test)
  :doc "`zenit-module-from-path' extracts module info from path"
  (should (equal '(:lang . python)
                 (zenit-module-from-path "/modules/lang/python/config.el")))
  
  :doc "`zenit-module-from-path' handles core directory"
  (let ((zenit-core-dir tmp-zenit-core-dir))
    (should (equal '(:core) (zenit-module-from-path (file-name-concat tmp-zenit-core-dir "file.el")))))
  
  :doc "`zenit-module-from-path' handles local config directory"
  (let ((zenit-local-conf-dir tmp-zenit-local-conf-dir))
    (should (equal '(:local-conf) (zenit-module-from-path (file-name-concat tmp-zenit-local-conf-dir "init.el")))))
  
  :doc "`zenit-module-from-path' respects enabled-only flag"
  (should-not (zenit-module-from-path "/modules/lang/disabled/config.el" t)))

(zenit-deftest zenit-module-mplist-map
  (:vars ((test-results nil)))
  ,test
  (test)
  :doc "`zenit-module-mplist-map' applies function to each module"
  (let ((mplist '(:cat1 mod1 mod2 :cat2 mod3)))
    (setq test-results nil)
    (zenit-module-mplist-map 
     (lambda (category module &rest plist)
       (push (list category module (plist-get plist :flags)) test-results))
     mplist)
    (should (equal '((:cat1 mod1 nil)
                     (:cat1 mod2 nil)
                     (:cat2 mod3 nil))
                   (reverse test-results))))
  
  :doc "`zenit-module-mplist-map' handles module flags"
  (let ((mplist '(:cat1 (mod1 +flag1 +flag2) mod2 :cat2 (mod3 +flag3))))
    (setq test-results nil)
    (zenit-module-mplist-map 
     (lambda (category module &rest plist)
       (push (list category module (plist-get plist :flags)) test-results))
     mplist)
    (should (equal '((:cat1 mod1 (+flag1 +flag2))
                     (:cat1 mod2 nil)
                     (:cat2 mod3 (+flag3)))
                   (reverse test-results)))))

(zenit-deftest zenit--module-dependencies
  (:doc "`zenit--module-dependencies' is defined")
  (should (boundp 'zenit--module-dependencies)))

(zenit-deftest zenit--module-conflicts
  (:doc "`zenit--module-conflicts' is defined")
  (should (boundp 'zenit--module-conflicts)))

(zenit-deftest zenit-module-resolve
  (:vars* ((zenit--module-dependencies nil)
           (zenit--module-conflicts nil)
           (tmp-dir (zenit-test-make-temp-file t))
           (zenit-modules-dirs (list tmp-dir))
           (zenit-modules (make-hash-table :test #'equal)))
   :before-each
   (progn
     (make-directory (file-name-concat tmp-dir "test" "mod") t)
     (zenit-module-set :test 'mod :path (file-name-concat tmp-dir "test" "mod") :flags '(+flag)))
   :after-each
   (progn
     (delete-directory tmp-dir t)))
  ,test
  (test)
  :doc "`zenit-module-resolve' adds dependencies"
  (progn
    (with-temp-file (file-name-concat tmp-dir "test" "mod" zenit-module-control-file)
      (insert "(:depends ((t :depcat1 depmod1 +flag) (+flag :depcat2 depmod2 +flag)))"))
    (make-directory (file-name-concat tmp-dir "depcat1" "depmod1") t)
    (make-directory (file-name-concat tmp-dir "depcat2" "depmod2") t)

    (zenit-module-resolve '(:test . mod))
    (should (string-match-p "Adding :depcat2 depmod2 (\\+flag) as dependency" 
                            (car zenit--module-dependencies)))
    (should (length= zenit--module-dependencies 2))
    (should (zenit-module-p :depcat1 'depmod1 '+flag))
    (should (zenit-module-p :depcat2 'depmod2 '+flag)))

  :doc "`zenit-module-resolve' handles existing dependencies"
  (progn
    (zenit-module-set :depcat 'depmod :path (file-name-concat tmp-dir "test" "mod") :flags '(+oldflag))
    (with-temp-file (file-name-concat tmp-dir "test" "mod" zenit-module-control-file)
      (insert "(:depends ((t :depcat depmod +newflag)))"))

    (zenit-module-resolve '(:test . mod))
    (should (equal '(+oldflag +newflag) 
                   (zenit-module-get :depcat 'depmod :flags))))

  :doc "`zenit-module-resolve' detects conflicts"
  (progn
    (zenit-module-set :conflict 'mod :path (file-name-concat tmp-dir "test" "mod") :flags '(+flag))
    (with-temp-file (file-name-concat tmp-dir "test" "mod" zenit-module-control-file)
      (insert "(:conflicts ((t :conflict mod) (+flag :conflict mod)))"))
    
    (zenit-module-resolve '(:test . mod) 'conflicts)
    (should (string-match-p "conflicts with :conflict mod" 
                            (car zenit--module-conflicts)))
    (should (length= zenit--module-conflicts 2))))

(zenit-deftest modules!
  (:vars ((zenit-modules (make-hash-table :test #'equal))))
  ,test
  (test)
  :doc "`modules!' creates modules and sets path for each one"
  (letf! ((#'print! #'ignore)
          (defun zenit-module-locate-path (category &optional module file)
            "/test/path"))
    (modules! :test mod1 mod2 mod3)
    (dolist (key (hash-table-keys zenit-modules))
      (should (equal '(:path "/test/path" :flags nil) (gethash key zenit-modules)))))

  :doc "`modules!' creates modules and sets path for each one"
  (letf! ((#'print! #'ignore)
          (defun zenit-module-locate-path (category &optional module file)
            t))
    (should-error (modules! :test mod1 mod2 mod3))))

(zenit-deftest zenit--empty-module-context
  (:doc "`zenit--empty-module-context' is defined")
  (should (boundp 'zenit--empty-module-context)))

(zenit-deftest zenit-module-context
  (:doc "`zenit-module-context' is defined")
  (should (boundp 'zenit-module-context)))

(zenit-deftest zenit-module--context-field
  (:doc "`zenit-module--context-field' retrieves the value of the given field")
  (progn
    (should (= 0 (zenit-module--context-field :index)))
    (should (= 1 (zenit-module--context-field :initdepth)))
    (should (= 2 (zenit-module--context-field :configdepth)))
    (should (= 3 (zenit-module--context-field :group)))
    (should (= 4 (zenit-module--context-field :name)))
    (should (= 5 (zenit-module--context-field :flags)))
    (should (= 6 (zenit-module--context-field :features)))))


;; (modulep! :lang nosuchlanguage -nosuchflag)
;; (modulep! :lang nosuchlanguage -nosuchflag)

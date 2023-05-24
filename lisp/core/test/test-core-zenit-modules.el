;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-core-zenit-modules.el

(describe "core/zenit-modules"

  (load! "zenit-modules" zenit-core-dir)

  (describe "zenit-module-p"
    (it "returns t if the category-module pair is present, and no flag is specified"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (puthash (cons 'category1 'module1) '(:flags (flag1 flag2)) zenit-modules)
        (expect (zenit-module-p 'category1 'module1) :to-be t)))

    (it "returns nil if the category-module pair is not present"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (puthash (cons 'category1 'module1) '(:flags (flag1 flag2)) zenit-modules)
        (expect (zenit-module-p 'category2 'module2) :to-be nil)))

    (it "returns t if the flag is present"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (puthash (cons 'category1 'module1) '(:flags (flag1 flag2)) zenit-modules)
        (expect (zenit-module-p 'category1 'module1 'flag1) :to-be t)))

    (it "returns nil if the flag is not present"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (puthash (cons 'category1 'module1) '(:flags (flag1 flag2)) zenit-modules)
        (expect (zenit-module-p 'category1 'module1 'flag3) :to-be nil))))


  (describe "zenit-module-depth"
    (it "returns the depth if the category-module pair is present, and no initdepth is specified"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (puthash (cons 'category1 'module1) '(:depth (2 . 3)) zenit-modules)
        (expect (zenit-module-depth 'category1 'module1) :to-be 3)))

    (it "returns the car of the depth if initdepth is specified"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (puthash (cons 'category1 'module1) '(:depth (2 . 3)) zenit-modules)
        (expect (zenit-module-depth 'category1 'module1 t) :to-be 2)))

    (it "returns 0 if the category-module pair is not present"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (puthash (cons 'category1 'module1) '(:depth (2 . 3)) zenit-modules)
        (expect (zenit-module-depth 'category2 'module2) :to-be 0))))


  (describe "zenit-module-get"
    (it "returns the plist if the category-module pair is present, and no property is specified"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (puthash (cons 'category1 'module1) '(:depth (2 . 3) :flags (flag1 flag2)) zenit-modules)
        (expect (zenit-module-get 'category1 'module1) :to-equal '(:depth (2 . 3) :flags (flag1 flag2)))))

    (it "returns the value of the specified property if it is present"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (puthash (cons 'category1 'module1) '(:depth (2 . 3) :flags (flag1 flag2)) zenit-modules)
        (expect (zenit-module-get 'category1 'module1 :depth) :to-equal '(2 . 3))))

    (it "returns nil if the category-module pair is not present"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (puthash (cons 'category1 'module1) '(:depth (2 . 3) :flags (flag1 flag2)) zenit-modules)
        (expect (zenit-module-get 'category2 'module2) :to-be nil))))


  (describe "zenit-module-put"
    (it "adds a new property to the plist if the category-module pair is present"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (puthash (cons 'category1 'module1) '(:depth (2 . 3)) zenit-modules)
        (zenit-module-put 'category1 'module1 :flags '(flag1 flag2))
        (expect (zenit-module-get 'category1 'module1) :to-equal '(:depth (2 . 3) :flags (flag1 flag2)))))

    (it "modifies an existing property in the plist if the category-module pair is present"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (puthash (cons 'category1 'module1) '(:depth (2 . 3)) zenit-modules)
        (zenit-module-put 'category1 'module1 :depth '(4 . 5))
        (expect (zenit-module-get 'category1 'module1 :depth) :to-equal '(4 . 5))))

    (it "adds a new category-module pair with the specified plist if it is not present"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (zenit-module-put 'category2 'module2 :depth '(2 . 3) :flags '(flag1 flag2))
        (expect (zenit-module-get 'category2 'module2) :to-equal '(:depth (2 . 3) :flags (flag1 flag2))))))


  (describe "zenit-module-set"
    (it "adds a new module with specified properties"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (zenit-module-set 'category1 'module1 :depth '(2 . 3) :flags '(flag1 flag2))
        (expect (zenit-module-get 'category1 'module1) :to-equal '(:depth (2 . 3) :flags (flag1 flag2)))))

    (it "modifies an existing module's properties"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (puthash (cons 'category1 'module1) '(:depth (2 . 3) :flags (flag1 flag2)) zenit-modules)
        (zenit-module-set 'category1 'module1 :depth '(4 . 5) :flags '(flag3 flag4))
        (expect (zenit-module-get 'category1 'module1) :to-equal '(:depth (4 . 5) :flags (flag3 flag4)))))

    (it "removes a module when plist is a single nil"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (puthash (cons 'category1 'module1) '(:depth (2 . 3) :flags (flag1 flag2)) zenit-modules)
        (zenit-module-set 'category1 'module1 nil)
        (expect (zenit-module-get 'category1 'module1) :to-be nil))))


  (describe "zenit-module-list"
    (it "returns a list of modules in order of their :depth"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (zenit-module-set 'category1 'module1 :depth '(2 . 3))
        (zenit-module-set 'category2 'module2 :depth '(1 . 2))
        (zenit-module-set 'category3 'module3 :depth '(3 . 4))
        (expect (zenit-module-list nil nil) :to-equal '((category2 . module2) (category1 . module1) (category3 . module3)))))

    (it "returns a list of modules sorted by their initdepth when initorder? is non-nil"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (zenit-module-set 'category1 'module1 :depth '(2 . 3))
        (zenit-module-set 'category2 'module2 :depth '(1 . 2))
        (zenit-module-set 'category3 'module3 :depth '(3 . 4))
        (expect (zenit-module-list nil t) :to-equal '((category2 . module2) (category1 . module1) (category3 . module3))))))


  (describe "zenit-module-expand-path"
    (it "returns a path to a file relative to category and module"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (zenit-module-set 'category1 'module1 :path "/path/to/module1")
        (expect (zenit-module-expand-path 'category1 'module1 "file1") :to-equal "/path/to/module1/file1")))

    (it "returns the module path when no file is provided"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (zenit-module-set 'category1 'module1 :path "/path/to/module1")
        (expect (zenit-module-expand-path 'category1 'module1) :to-equal "/path/to/module1")))

    (it "returns nil when the category is not enabled"
      (let ((zenit-modules (make-hash-table :test 'equal)))
        (expect (zenit-module-expand-path 'category1 'module1 "file1") :to-be nil))))


  (describe "zenit-module-locate-path"
    :var ((tmp-file "/tmp/category1/module1/file1"))

    (before-each
      (make-directory (file-name-directory tmp-file) t))

    (after-each
      (delete-directory (file-name-directory tmp-file) t))

    (it "returns a path to a file when the module is enabled"
      (let ((zenit-modules (make-hash-table :test 'equal))
            (zenit-modules-dirs '("/tmp")))
        (zenit-module-set :category1 'module1 :path (file-name-directory tmp-file))
        (write-region "" nil tmp-file)
        (expect (zenit-module-locate-path :category1 'module1 "file1") :to-equal tmp-file)))

    (it "returns a path to a module when the module is enabled and no file is provided"
      (let ((zenit-modules (make-hash-table :test 'equal))
            (zenit-modules-dirs '("/tmp")))
        (zenit-module-set :category1 'module1 :path "/tmp/module1")
        (expect (zenit-module-locate-path :category1 'module1) :to-equal "/tmp/module1")))

    (it "returns nil when the path does not exist"
      (let ((zenit-modules (make-hash-table :test 'equal))
            (zenit-modules-dirs '("/tmp")))
        (expect (zenit-module-locate-path :category1 'module1 "file1") :to-be nil)))

    (it "returns a path to a file when the module is not enabled but the file exists"
      (let ((zenit-modules (make-hash-table :test 'equal))
            (zenit-modules-dirs '("/tmp")))
        (write-region "" nil tmp-file)
        (expect (zenit-module-locate-path :category1 'module1 "file1") :to-equal tmp-file))))


  (describe "zenit-module-locate-paths"
    :var ((tmp-file1 "/tmp/category1/module1/file1")
          (tmp-file2 "/tmp/category2/module2/file1"))

    (before-each
      (make-directory (file-name-directory tmp-file1) t)
      (make-directory (file-name-directory tmp-file2) t)
      (write-region "" nil tmp-file1)
      (write-region "" nil tmp-file2))

    (after-each
      (delete-directory (file-name-directory tmp-file1) t)
      (delete-directory (file-name-directory tmp-file2) t))

    (it "returns paths to files under modules when the modules are enabled"
      (let ((zenit-modules (make-hash-table :test 'equal))
            (zenit-modules-dirs '("/tmp")))
        (zenit-module-set :category1 'module1 :path (file-name-directory tmp-file1))
        (zenit-module-set :category2 'module2 :path (file-name-directory tmp-file2))
        (expect (zenit-module-locate-paths '((:category1 . module1) (:category2 . module2)) "file1")
                :to-equal `(,tmp-file1
                            ,tmp-file2))))

    (it "returns nil when the paths do not exist"
      (let ((zenit-modules (make-hash-table :test 'equal))
            (zenit-modules-dirs '("/tmp")))
        (delete-directory (file-name-directory tmp-file1) t)
        (delete-directory (file-name-directory tmp-file2) t)
        (expect (zenit-module-locate-paths '((:category1 . module1) (:category2 . module2)) "file1")
                :to-be nil)))

    (it "returns paths to existing files under modules when the modules are not enabled"
      (let ((zenit-modules (make-hash-table :test 'equal))
            (zenit-modules-dirs '("/tmp")))
        (expect (zenit-module-locate-paths '((:category1 . module1) (:category2 . module2)) "file1")
                :to-equal (list tmp-file1 tmp-file2))))))

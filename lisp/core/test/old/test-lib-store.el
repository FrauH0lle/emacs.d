;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-lib-store.el

(describe "core/lib/store"

  (load! "lib/store" zenit-core-dir)

  (setq zenit-store-dir (file-name-concat temporary-file-directory "store/"))


  (describe "zenit-save-persistent-store-h"
    (it "persists `zenit-store-persist-alist'"
      (setq var1 1
            var2 2
            var3 3
            zenit-store-persist-alist '(("location1" var1 var2)
                                        ("location2" var3)))

      (spy-on 'zenit-files-in :and-return-value '("location1" "location2"))
      (spy-on 'zenit-store-put)
      (zenit-save-persistent-store-h)
      (expect 'zenit-store-put :to-have-been-called-times 2)
      (expect 'zenit-store-put :to-have-been-called-with 'var1 1 nil "location1" 'noflush)
      (expect 'zenit-store-put :to-have-been-called-with 'var2 2 nil "location1" 'noflush))

    (it "cleans up expired entries"
      (spy-on 'zenit-files-in :and-return-value '("location"))
      (spy-on 'zenit--store-init :and-return-value (make-hash-table :test 'equal))
      (spy-on 'zenit--store-expired-p :and-return-value t)
      (spy-on 'zenit--store-rem)
      (zenit-save-persistent-store-h)
      (expect 'zenit--store-rem :to-have-been-called)))


  (describe "zenit-store-persist"
    (before-each
      (setq zenit-store-persist-alist nil))

    (it "persists variables in a given location"
      (let ((location "test-location")
            (variables '(test-var-1 test-var-2)))
        (zenit-store-persist location variables)
        (expect (alist-get location zenit-store-persist-alist)
                :to-equal variables)))

    (it "populates persisted variables with cached values"
      (let ((location "test-location")
            (variables '(test-var-1 test-var-2))
            (value-1 "value-1")
            (value-2 "value-2"))
        (zenit-store-put 'test-var-1 value-1 nil location)
        (zenit-store-put 'test-var-2 value-2 nil location)
        (zenit-store-persist location variables)
        (expect test-var-1 :to-equal value-1)
        (expect test-var-2 :to-equal value-2))))


  (describe "zenit-store-desist"
    (describe "when unregistering variables from a location"
      (it "should remove the location from zenit-store-persist-alist if no variables are passed"
        (let ((zenit-store-persist-alist '(("location-1" var-1 var-2)
                                           ("location-2" var-3 var-4))))
          (zenit-store-desist "location-1")
          (expect zenit-store-persist-alist :to-equal '(("location-2" var-3 var-4)))))

      (it "should remove the variables from the location in zenit-store-persist-alist if variables are passed"
        (let ((zenit-store-persist-alist '(("location-1" var-1 var-2)
                                           ("location-2" var-3 var-4))))
          (zenit-store-desist "location-1" '(var-1))
          (expect zenit-store-persist-alist :to-equal '(("location-1" var-2)
                                                        ("location-2" var-3 var-4)))))))


  (describe "zenit--store-init"
    (before-each
      ;; Setup temporary store directory
      (setq zenit-store-dir (concat temporary-file-directory "zenit-test-store/"))
      (when (file-exists-p zenit-store-dir)
        (delete-directory zenit-store-dir t))
      (setq zenit--store-table (make-hash-table :test 'equal))
      (setq zenit-store-location "default"))

    (after-each
      ;; Clean up temporary store directory
      (when (file-exists-p zenit-store-dir)
        (delete-directory zenit-store-dir t)))

    (it "initializes a new store when no file exists"
      (let ((store (zenit--store-init)))
        (expect (hash-table-p store) :to-be t)
        (expect (hash-table-count store) :to-be 0)))

    (it "reads existing store data from a file"
      (let ((test-data '((key1 . value1) (key2 . value2))))
        ;; Write test data to the file
        (make-directory zenit-store-dir 'parents)
        (with-temp-file (expand-file-name zenit-store-location zenit-store-dir)
          (prin1 test-data (current-buffer)))
        ;; Test if zenit--store-init reads the data correctly
        (let ((store (zenit--store-init)))
          (expect store :to-equal test-data))))

    (it "initializes store at a custom location"
      (let ((location (concat temporary-file-directory "custom/")))
        (setq zenit-store-location location)
        (let ((store (zenit--store-init)))
          (expect (hash-table-p store) :to-be t)
          (expect (hash-table-count store) :to-be 0)))))


  (describe "zenit--store-expired-p"
    (it "returns nil when TTL is a function that returns t"
      (let ((key :test-key)
            (data (cons (lambda (_ _data) t) :value)))
        (expect (zenit--store-expired-p key data) :to-be nil)))

    (it "returns t when TTL is a function that returns nil"
      (let ((key :test-key)
            (data (cons (lambda (_ _data) nil) :value)))
        (expect (zenit--store-expired-p key data) :to-be t)))

    (it "returns t when TTL is a timestamp in the past"
      (let ((key :test-key)
            (data (cons (time-subtract (current-time) (seconds-to-time 10)) :value)))
        (expect (zenit--store-expired-p key data) :to-be t)))

    (it "returns nil when TTL is a timestamp in the future"
      (let ((key :test-key)
            (data (cons (time-add (current-time) (seconds-to-time 10)) :value)))
        (expect (zenit--store-expired-p key data) :to-be nil))))


  (describe "zenit--store-flush"
    (before-each
      ;; Ensure a clean test environment
      (setq zenit-store-dir (concat temporary-file-directory "zenit-test-store/"))
      (delete-directory zenit-store-dir t))

    (after-each
      ;; Clean up the test environment
      (delete-directory zenit-store-dir t))

    (it "saves the data to a file in zenit-store-dir"
      (let* ((location "test-location")
             (location-path (expand-file-name location zenit-store-dir))
             (key :test-key)
             (value :test-value))
        (puthash key value (zenit--store-init location))
        (zenit--store-flush location)
        (expect (file-exists-p location-path) :to-be t)))

    (it "loads the saved data correctly"
      (let* ((location "test-location")
             (key :test-key)
             (value (cons nil :test-value)))
        (puthash key value (zenit--store-init location))
        (zenit--store-flush location)
        (remhash location zenit--store-table)
        (expect (zenit-store-get key location) :to-equal :test-value))))


  (describe "zenit-store-get"
    (before-each
      ;; Ensure a clean test environment
      (setq zenit-store-dir (concat temporary-file-directory "zenit-test-store/"))
      (delete-directory zenit-store-dir t))

    (after-each
      ;; Clean up the test environment
      (delete-directory zenit-store-dir t))

    (it "returns the value associated with a key if it exists"
      (let* ((location "test-location")
             (key :test-key)
             (value :test-value))
        (zenit-store-put key value nil location)
        (expect (zenit-store-get key location) :to-equal value)))

    (it "returns the default value if the key does not exist"
      (let* ((location "test-location")
             (key :foo)
             (default-value :default-value))
        (expect (zenit-store-get key location default-value) :to-equal default-value)))

    (it "returns the default value if the key's data has expired"
      (let* ((location "test-location")
             (key :test-key)
             (value :test-value)
             (ttl 0) ; expired
             (default-value :default-value))
        (zenit-store-put key value ttl location)
        (expect (zenit-store-get key location default-value) :to-equal default-value))))


  (describe "zenit-store-put"
    (before-each
      ;; Ensure a clean test environment
      (setq zenit-store-dir (concat temporary-file-directory "zenit-test-store/"))
      (delete-directory zenit-store-dir t))

    (after-each
      ;; Clean up the test environment
      (delete-directory zenit-store-dir t))

    (it "associates a value with a key in the store"
      (let* ((location "test-location")
             (key :test-key)
             (value :test-value))
        (zenit-store-put key value nil location)
        (expect (zenit-store-get key location) :to-equal value)))

    (it "sets a TTL for the key-value pair"
      (let* ((location "test-location")
             (key :test-key)
             (value :test-value)
             (ttl 1) ; 1 second
             (default-value :default-value))
        (zenit-store-put key value ttl location)
        (sleep-for 2) ; wait for TTL to expire
        (expect (zenit-store-get key location default-value) :to-equal default-value)))

    (it "replaces an existing key-value pair with a new value"
      (let* ((location "test-location")
             (key :test-key)
             (old-value :old-value)
             (new-value :new-value))
        (zenit-store-put key old-value nil location)
        (zenit-store-put key new-value nil location)
        (expect (zenit-store-get key location) :to-equal new-value))))


  (describe "zenit-store-rem"
    (before-each
      ;; Ensure a clean test environment
      (setq zenit-store-dir (concat temporary-file-directory "zenit-test-store/"))
      (delete-directory zenit-store-dir t))

    (after-each
      ;; Clean up the test environment
      (delete-directory zenit-store-dir t))

    (it "removes a key-value pair from the store"
      (let* ((location "test-location")
             (key :test-key)
             (value :test-value)
             (default-value :default-value))
        (zenit-store-put key value nil location)
        (zenit-store-rem key location)
        (expect (zenit-store-get key location default-value) :to-equal default-value)))

    (it "does not affect other key-value pairs in the store"
      (let* ((location "test-location")
             (key1 :test-key1)
             (value1 :test-value1)
             (key2 :test-key2)
             (value2 :test-value2))
        (zenit-store-put key1 value1 nil location)
        (zenit-store-put key2 value2 nil location)
        (zenit-store-rem key1 location)
        (expect (zenit-store-get key1 location :default-value1) :to-equal :default-value1)
        (expect (zenit-store-get key2 location) :to-equal value2))))


  (describe "zenit-store-member-p"
    (before-each
      ;; Ensure a clean test environment
      (setq zenit-store-dir (concat temporary-file-directory "zenit-test-store/"))
      (delete-directory zenit-store-dir t))

    (after-each
      ;; Clean up the test environment
      (delete-directory zenit-store-dir t))

    (it "returns t when the key exists in the store"
      (let* ((location "test-location")
             (key :test-key)
             (value :test-value))
        (zenit-store-put key value nil location)
        (expect (zenit-store-member-p key location) :to-be t)))

    (it "returns nil when the key does not exist in the store"
      (let ((location "test-location")
            (key :foo))
        (expect (zenit-store-member-p key location) :to-be nil)))

    (it "returns nil when the key exists but has expired"
      (let* ((location "test-location")
             (key :test-key)
             (value :test-value))
        (zenit-store-put key value 0 location)
        (sleep-for 1)
        (expect (zenit-store-member-p key location) :to-be nil))))


  (describe "zenit-store-clear"
    (before-each
      ;; Ensure a clean test environment
      (setq zenit-store-dir (concat temporary-file-directory "zenit-test-store/"))
      (delete-directory zenit-store-dir t))

    (after-each
      ;; Clean up the test environment
      (delete-directory zenit-store-dir t))

    (it "clears the store and removes the associated file"
      (let* ((location "test-location")
             (key :test-key)
             (value :test-value))
        (zenit-store-put key value nil location)
        (expect (zenit-store-member-p key location) :to-be t)
        (zenit-store-clear location)
        (expect (file-exists-p (expand-file-name location zenit-store-dir)) :to-be nil)
        (expect (zenit-store-member-p key location) :to-be nil)))

    (it "clears the store but does not affect other stores"
      (let* ((location1 "test-location1")
             (location2 "test-location2")
             (key1 :test-key1)
             (key2 :test-key2)
             (value1 :test-value1)
             (value2 :test-value2))
        (zenit-store-put key1 value1 nil location1)
        (zenit-store-put key2 value2 nil location2)
        (expect (zenit-store-member-p key1 location1) :to-be t)
        (expect (zenit-store-member-p key2 location2) :to-be t)
        (zenit-store-clear location1)
        (expect (zenit-store-member-p key1 location1) :to-be nil)
        (expect (zenit-store-member-p key2 location2) :to-be t))))

  ;; Cleanup
  (remove-hook 'kill-emacs-hook #'zenit-save-persistent-store-h))

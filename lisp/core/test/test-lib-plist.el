;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-lib-plist.el

(describe "core/lib/plist"

  (load! "lib/plist" zenit-core-dir)

  (describe "zenit-plist-get"
    (it "returns the value of the specified property in the plist"
      (expect (zenit-plist-get '(:prop1 1 :prop2 2) :prop1) :to-equal 1)
      (expect (zenit-plist-get '(:prop1 1 :prop2 2) :prop2) :to-equal 2))

    (it "returns nil-value if the specified property does not exist in the plist"
      (expect (zenit-plist-get '(:prop1 1 :prop2 2) :prop3) :to-equal nil)
      (expect (zenit-plist-get '(:prop1 1 :prop2 2) :prop3 'default) :to-equal 'default)))


  (describe "zenit-plist-merge"
    (it "should merge two plists without changing the original from-plist"
      (let ((plist1 '(:foo 1 :bar 2))
            (plist2 '(:baz 3)))
        (expect (zenit-plist-merge plist1 plist2)
                :to-equal '(:baz 3 :foo 1 :bar 2))
        (expect plist1 :to-equal '(:foo 1 :bar 2))
        (expect plist2 :to-equal '(:baz 3 :foo 1 :bar 2))))

    (it "should overwrite properties in the target plist with properties from the source plist"
      (let ((plist1 '(:foo 1 :bar 2))
            (plist2 '(:bar 3 :baz 4)))
        (expect (zenit-plist-merge plist1 plist2)
                :to-equal '(:bar 2 :baz 4 :foo 1))
        (expect plist1 :to-equal '(:foo 1 :bar 2))
        (expect plist2 :to-equal '(:bar 2 :baz 4 :foo 1))))

    (it "should return the target plist"
      (let ((plist1 '(:foo 1 :bar 2))
            (plist2 '(:baz 3)))
        (expect (zenit-plist-merge plist1 plist2) :to-be plist2))))


  (describe "zenit-plist-delete-nil"
    (it "should delete all nil properties from a plist"
      (expect (zenit-plist-delete-nil '(nil nil :key2 "value2"))
              :to-equal '(:key2 "value2")))

    (it "should return an empty plist if all values are nil"
      (expect (zenit-plist-delete-nil '(nil nil))
              :to-equal nil)))


  (describe "zenit-plist-keys"
    (it "should return the keys of a given property list"
      (let ((plist '(:foo 1 :bar 2)))
        (expect (zenit-plist-keys plist) :to-equal '(:bar :foo))))

    (it "should return an empty list for an empty property list"
      (let ((plist '()))
        (expect (zenit-plist-keys plist) :to-equal '()))))


  (describe "zenit-plist-values"
    (it "should return the values in PLIST"
      (let ((plist '(:key1 "value1" :key2 "value2")))
        (expect (zenit-plist-values plist)
                :to-equal '("value2" "value1"))))

    (it "should return an empty list for an empty plist"
      (let ((plist nil))
        (expect (zenit-plist-values plist)
                :to-equal nil))))


  (describe "zenit-mplist-get-values"
    (describe "when passed a modified plist and a property"
      (it "returns the values associated with the property"
        (let ((mplist '(:foo 1 2 3 :bar 4 5 6)))
          (expect (zenit-mplist-get-values mplist :foo)
                  :to-equal '(1 2 3))))

      (it "returns an empty list when the property is not found"
        (let ((mplist '(:foo 1 2 3 :bar 4 5 6)))
          (expect (zenit-mplist-get-values mplist :baz)
                  :to-equal nil))))

    (describe "when passed a circular modified plist"
      (it "returns the values associated with the property"
        (let ((mplist '(:foo 1 2 3)))
          (setcdr (last mplist) mplist)
          (expect (zenit-mplist-get-values mplist :foo)
                  :to-equal '(1 2 3)))))))

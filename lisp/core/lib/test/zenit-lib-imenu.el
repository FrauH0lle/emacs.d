;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-imenu.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'imenu)

(zenit-deftest +imenu-create-nested-index ()
  ,test
  (test)
  :doc "`+imenu-create-nested-index' returns a nested index"
  (should
   (equal '(("Heading 1" ("." . 3) ("Heading 1.1" . 17)) ("Heading 2" . 34))
          (with-temp-buffer
            (insert ";;; Heading 1\n;;;; Heading 1.1\n;;; Heading 2\n")
            (+imenu-create-nested-index "^;;\\(?1:[;]+\\) \\(?2:[^\n]+\\)"))))
  :doc "`+imenu-create-nested-index' returns a nested index with menu title"
  (should
   (equal '(("Headings" ("Heading 1" ("." . 3) ("Heading 1.1" . 17)) ("Heading 2" . 34)))
          (with-temp-buffer
            (insert ";;; Heading 1\n;;;; Heading 1.1\n;;; Heading 2\n")
            (+imenu-create-nested-index "^;;\\(?1:[;]+\\) \\(?2:[^\n]+\\)" "Headings"))))
  :doc "`+imenu-create-nested-index' returns nil if nothing is found"
  (should
   (equal nil
          (with-temp-buffer
            (erase-buffer)
            (+imenu-create-nested-index "^;;\\(?1:[;]+\\) \\(?2:[^\n]+\\)" "Headings")))))

(zenit-deftest +imenu-extra-line-range
  (:doc "`+imenu-extra-line-range' returns correct line positions")
  (with-temp-buffer
    (erase-buffer)
    ,insert
    (should (equal ,out (+imenu-extra-line-range ,in))))
  (insert in out)
  (insert "Line one\nLine two\nLine three") 1 (cons 1 9)
  (insert "Line one\nLine two\nLine three") 10 (cons 10 18)
  (insert "Line one\nLine two\nLine three") 19 (cons 19 29)
  nil 1 (cons 1 1)
  (insert "Short\nMedium length line\nVery very very long line indeed") 1 (cons 1 6)
  (insert "Short\nMedium length line\nVery very very long line indeed") 7 (cons 7 25)
  (insert "Short\nMedium length line\nVery very very long line indeed") 26 (cons 26 57))

(zenit-deftest +imenu-extra-position ()
  ,test
  (test)
  :doc "`+imenu-extra-position' extracts positions: nil case"
  (should (eq nil (+imenu-extra-position nil)))

  :doc "`+imenu-extra-position' extracts positions: integer position"
  (should (eq 42 (+imenu-extra-position 42)))
  
  :doc "`+imenu-extra-position' extracts positions: marker position"
  (with-temp-buffer
    (insert "Test marker")
    (let ((marker (point-marker)))
      (should (eq 12 (+imenu-extra-position marker)))))

  :doc "`+imenu-extra-position' extracts positions: list position"
  (should (eq 42 (+imenu-extra-position '("test" 42 "value"))))
  
  :doc "`+imenu-extra-position' extracts positions: alist position"
  (should (eq 42 (+imenu-extra-position '("test" . 42))))
  
  :doc "`+imenu-extra-position' extracts positions: nested alist position"
  (should (eq 42 (+imenu-extra-position '("test" ("subtest1" . 42) ("subtest2" . 11)))))
  
  :doc "`+imenu-extra-position' extracts positions: list position"
  (should (eq 42 (+imenu-extra-position '("test" ("subtest1" 42 "value") ("subtest2" . 11))))))

(zenit-deftest +imenu-extract-ranges ()
  ,test
  (test)
  :doc "`+imenu-extract-ranges' extracts line ranges from input list"
  (with-temp-buffer
    (insert "Short\nMedium length line\nVery very very long line indeed")
    (should (zenit-test-same-items-p
             '((1 . 6) (1 . 6) (7 . 25) (26 . 57))
             (+imenu-extract-ranges '("Test" ("Head 1" ("." . 1) ("Head 1.1" . 2)) ("Head 2" . 8) ("Head 3" . 28)))
             :test #'equal)))
  :doc "`+imenu-extract-ranges' extracts line ranges from nested input list"
  (with-temp-buffer
    (insert "Short\nMedium length line\nVery very very long line indeed")
    (should (zenit-test-same-items-p
             '((1 . 6) (1 . 6) (7 . 25) (26 . 57))
             (+imenu-extract-ranges '((("Test" (("Head 1" (("." . 1)) (("Head 1.1" . 2)))) ((("Head 2" . 8))) ("Head 3" . 28)))))
             :test #'equal))))

(zenit-deftest +imenu-merge-new-items ()
  ,test
  (test)
  :doc "`+imenu-merge-new-items' returns a merged index - nested original, flat new"
  (with-temp-buffer
    (erase-buffer)
    (insert ";;; Heading 1\n;;;; Heading 1.1\n;;; Heading 2\n")
    (should
     (equal
      `(("Heading 3" . 64)
        ("Headers"
         ("Heading 1" . ,(set-marker (make-marker) 1))
         ("Heading 1.1" . ,(set-marker (make-marker) 15))
         ("Heading 2" . ,(set-marker (make-marker) 32))))
      (+imenu-merge-new-items
       '(("Heading 1" ("." . 3) ("Heading 1.1" . 15)) ("Heading 2" . 34) ("Heading 3" . 64))
       '(("Headers" "^;;[;]+ \\([^\n]+\\)" 1))))))
  :doc "`+imenu-merge-new-items' returns a merged index - flat original, nested new"
  (with-temp-buffer
    (erase-buffer)
    (insert ";;; Heading 1\n;;;; Heading 1.1\n;;; Heading 2\n")
    (should
     (equal
      '(("Heading 3" . 64)
        ("Headers"
         ("Heading 1"
          ("." . 3)
          ("Heading 1.1" . 17))
         ("Heading 2" . 34)))
      (+imenu-merge-new-items
       '(("Heading 1" . 3) ("Heading 1.1" . 15) ("Heading 2" . 34) ("Heading 3" . 64))
       '((+imenu-create-nested-index "^;;\\(?1:[;]+\\) \\(?2:[^\n]+\\)" "Headers"))))))
  :doc "`+imenu-merge-new-items' returns a merged index and keeps original items"
  (with-temp-buffer
    (erase-buffer)
    (insert ";;; Heading 1\n;;;; Heading 1.1\n;;; Heading 2\n")
    (should
     (equal
      `(("Heading 1" ("." . 3) ("Heading 1.1" . 15)) ("Heading 2" . 34) ("Heading 3" . 64)
        ("Headers"
         ("Heading 1" . ,(set-marker (make-marker) 1))
         ("Heading 1.1" . ,(set-marker (make-marker) 15))
         ("Heading 2" . ,(set-marker (make-marker) 32))))
      (+imenu-merge-new-items
       '(("Heading 1" ("." . 3) ("Heading 1.1" . 15)) ("Heading 2" . 34) ("Heading 3" . 64))
       '(("Headers" "^;;[;]+ \\([^\n]+\\)" 1)) t)))))

(zenit-deftest +imenu-filter-index ()
  (let ((input ',in)
        (positions
         '((4808 . 4810)
           (4902 . 4906))))
    (should (equal ',expected (+imenu-filter-index input positions))))
  (in expected)
  :doc "`+imenu-filter-index' returns filtered list"
  ("Test"
   ("Head 1" . 4809)
   ("Head 2"
    ("." . 4809)
    ("Head 2.1" . 4818))
   ("Head 3" . 4810))
  ("Test"
   ("Head 2"
    ("Head 2.1" . 4818)))
  :doc "`+imenu-filter-index' returns filtered list and merges single \".\" entries"
  ("Test"
   ("Head 1" . 4809)
   ("Head 2"
    ("." . 4818)
    ("Head 2.1" . 4905))
   ("Head 3" . 4810))
  ("Test"
   ("Head 2" . 4818)))

(zenit-deftest +imenu-add-items
  (:doc "`+imenu-add-items' is defined")
  (should (fboundp '+imenu-add-items)))
